use alloc::boxed::Box;
use alloc::sync::Arc;

/// Acts as polyfill for float methods
#[cfg(not(feature = "session"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use all_is_cubes::block::{self, AIR, Block, Composite, CompositeOperator};
use all_is_cubes::listen;
use all_is_cubes::math::{Face6, GridAab, GridCoordinate, GridSize, Rgb, ZeroOne};
use all_is_cubes::space::SpaceTransaction;

use crate::vui;
use crate::vui::widgets::{BoxStyle, WidgetTheme};

/// Widget which draws a progress bar.
#[derive(Clone, Debug)]
pub struct ProgressBar {
    empty_style: BoxStyle,
    filled_style: BoxStyle,
    direction: Face6,
    source: listen::DynSource<ProgressBarState>,
}

/// Information presented by a [`ProgressBar`] widget.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct ProgressBarState {
    /// A number from 0 to 1 which specifies how much of the progress bar is full.
    fraction: ZeroOne<f64>,
}

/// Identical to [`ProgressBarState`] except rounded to the finest granularity we distinguish.
#[derive(Clone, Debug, Eq, PartialEq)]
struct InternalState {
    sixteenths: GridCoordinate,
}

impl ProgressBar {
    /// Create a progress bar widget.
    ///
    /// * `theme` defines the style with which the progress bar is drawn.
    /// * `direction` is which direction the progress bar fills up in.
    /// * `source` is the data source.
    pub fn new(
        theme: &WidgetTheme,
        direction: Face6,
        source: listen::DynSource<ProgressBarState>,
    ) -> Arc<Self> {
        Arc::new(Self {
            empty_style: theme.progress_bar_empty.clone(),
            filled_style: theme.progress_bar_full.clone(),
            direction,
            source,
        })
    }
}

impl vui::Layoutable for ProgressBar {
    fn requirements(&self) -> vui::LayoutRequest {
        // Any size is permitted as long as it fits
        vui::LayoutRequest {
            minimum: GridSize::new(1, 1, 1),
        }
    }
}

impl vui::Widget for ProgressBar {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ProgressBarController {
            todo: listen::Flag::listening(true, &self.source),
            definition: self,
            position: position.bounds,
            last_drawn_state: None,
        })
    }
}

impl ProgressBarState {
    /// `fraction` should be a number from 0 to 1 which specifies how much of the progress bar is
    /// full. If it is `NaN`, zero is substituted.
    pub fn new(fraction: f64) -> Self {
        Self {
            fraction: ZeroOne::try_from(fraction.clamp(0.0, 1.0)).unwrap_or(ZeroOne::ZERO),
        }
    }
}

#[derive(Debug)]
struct ProgressBarController {
    definition: Arc<ProgressBar>,
    position: GridAab,
    todo: listen::Flag,
    last_drawn_state: Option<InternalState>,
}

impl ProgressBarController {
    fn convert_state(&self, requested_state: &ProgressBarState) -> InternalState {
        InternalState {
            sixteenths: (requested_state.fraction.into_inner()
                * f64::from(self.position.size()[self.definition.direction.axis()])
                * 16.0)
                .round() as GridCoordinate,
        }
    }

    fn paint_txn(&self, state: &InternalState) -> vui::WidgetTransaction {
        let d = &self.definition;
        let bounds = self.position;
        let axis = d.direction.axis();

        let mut txn = SpaceTransaction::default();
        for cube in bounds.interior_iter() {
            // TODO: respect requested direction
            let lb_in_sixteenths =
                (cube.lower_bounds()[axis] - bounds.lower_bounds()[axis]).saturating_mul(16);
            let ub_in_sixteenths =
                (cube.upper_bounds()[axis] - bounds.lower_bounds()[axis]).saturating_mul(16);

            //eprintln!("{cube:?} {lb_in_sixteenths}..{s}..{ub_in_sixteenths}", s=state.sixteenths);

            let block: Block = if ub_in_sixteenths <= state.sixteenths {
                // Bar is full up to this cube
                d.filled_style.cube_at(bounds, cube).unwrap_or(&AIR).clone()
            } else if state.sixteenths <= lb_in_sixteenths {
                // Bar is empty above this cube
                d.empty_style.cube_at(bounds, cube).unwrap_or(&AIR).clone()
            } else {
                // Bar's filled edge lands within this cube

                let partial_fill = state.sixteenths - lb_in_sixteenths;
                assert!((0..16).contains(&partial_fill));

                let mask_substance = block::from_color!(Rgb::ONE);
                let fill_mask = mask_substance.clone().with_modifier(block::Move::new(
                    Face6::NX,
                    u16::try_from((16 - partial_fill) * 16).unwrap(),
                    0,
                ));
                let empty_mask = mask_substance.with_modifier(block::Move::new(
                    Face6::PX,
                    u16::try_from(partial_fill * 16).unwrap(),
                    0,
                ));

                // Mask off the portions that shouldn't bve used
                let masked_fill = Composite::new(
                    d.filled_style.cube_at(bounds, cube).unwrap_or(&AIR).clone(),
                    CompositeOperator::In,
                )
                .compose_or_replace(fill_mask);
                let masked_empty = Composite::new(
                    d.empty_style.cube_at(bounds, cube).unwrap_or(&AIR).clone(),
                    CompositeOperator::In,
                )
                .compose_or_replace(empty_mask);

                // Combine the masked filled bar and the masked empty bar.
                Composite::new(masked_fill, CompositeOperator::Over)
                    .compose_or_replace(masked_empty)
            };

            txn.at(cube).overwrite(block);
        }

        txn
    }
}

impl vui::WidgetController for ProgressBarController {
    fn initialize(
        &mut self,
        _: &vui::WidgetContext<'_>,
    ) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let new_state = self.convert_state(&self.definition.source.get());
        let txn = self.paint_txn(&new_state);
        self.last_drawn_state = Some(new_state);
        Ok(txn)
    }

    fn step(
        &mut self,
        _context: &vui::WidgetContext<'_>,
    ) -> Result<vui::StepSuccess, vui::StepError> {
        if !self.todo.get_and_clear() {
            return Ok((SpaceTransaction::default(), vui::Then::Step));
        }

        let new_state = self.convert_state(&self.definition.source.get());

        // Don't redraw if the new state is visually identical.
        if self.last_drawn_state.as_ref() == Some(&new_state) {
            return Ok((SpaceTransaction::default(), vui::Then::Step));
        }

        let txn = self.paint_txn(&new_state);
        self.last_drawn_state = Some(new_state);
        Ok((txn, vui::Then::Step))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::space::{self, SpacePhysics};
    use all_is_cubes::transaction::Transaction as _;
    use all_is_cubes::util::yield_progress_for_testing;
    use all_is_cubes::{transaction, universe};
    use alloc::string::String;
    use alloc::vec::Vec;

    #[tokio::test]
    async fn progress_output() {
        // TODO: this theme setup logic should be part of a widget test setup helper
        let mut universe = universe::Universe::new();
        let mut install_txn = universe::UniverseTransaction::default();
        let widget_theme = WidgetTheme::new(
            universe.read_ticket(),
            &mut install_txn,
            yield_progress_for_testing(),
        )
        .await
        .unwrap();
        install_txn
            .execute(&mut universe, (), &mut transaction::no_outputs)
            .unwrap();

        let bounds = GridAab::from_lower_upper([0, 0, 0], [4, 1, 1]);

        let pb = |fraction: f64| -> String {
            let tree = vui::leaf_widget(ProgressBar::new(
                &widget_theme,
                Face6::PX,
                listen::constant(ProgressBarState::new(fraction)),
            ));

            let mut space = space::Builder::default()
                .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                .bounds(bounds)
                .build();

            vui::install_widgets(
                vui::LayoutGrant {
                    bounds: space.bounds(),
                    gravity: vui::Gravity::new(
                        vui::Align::Center,
                        vui::Align::Center,
                        vui::Align::Low,
                    ),
                },
                &tree,
            )
            .unwrap()
            .execute(
                &mut space,
                universe.read_ticket(),
                &mut transaction::no_outputs,
            )
            .unwrap();

            space
                .extract::<Vec<char>, _>(bounds, |e| {
                    e.block_data()
                        .evaluated()
                        .attributes()
                        .display_name
                        .chars()
                        .last()
                        .unwrap_or('\0')
                })
                .into_elements()
                .into_iter()
                .collect::<String>()
        };

        // TODO: we will need a cleverer strategy to test fractional blocks
        assert_eq!(
            [pb(0.0).as_str(), pb(0.5).as_str(), pb(1.0).as_str()],
            ["yyyy", "llyy", "llll"]
        );
    }
}
