use alloc::sync::Arc;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::block::{text, AIR};
use all_is_cubes::block::{Block, BlockAttributes, Resolution};
use all_is_cubes::camera;
use all_is_cubes::color_block;
use all_is_cubes::content::palette;
use all_is_cubes::euclid::{size2, Size2D};
use all_is_cubes::math::{Cube, Face6, FreeCoordinate, GridAab, GridCoordinate, GridSize, Rgba};
use all_is_cubes::space::{self, Space, SpaceBuilder, SpacePhysics};
use all_is_cubes::time;
use all_is_cubes::universe::{Handle, Universe};

use crate::vui::{
    self, install_widgets, widgets, Align, Gravity, InstallVuiError, LayoutGrant, LayoutRequest,
    LayoutTree, Layoutable, Widget, WidgetTree,
};

/// Bounds for UI display; a choice of scale and aspect ratio based on the viewport size
/// and aspect ratio (and maybe in the future, preferences).
///
///
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) struct UiSize {
    /// Two-dimensional size; individual pages may have their own choices of depth.
    size: Size2D<GridCoordinate, Cube>,
}

impl UiSize {
    pub(crate) const DEPTH_BEHIND_VIEW_PLANE: GridCoordinate = 5;

    /// Construct [`UiSize`] that suits the given viewport
    /// (based on pixel resolution and aspect ratio).
    pub fn new(viewport: camera::Viewport) -> Self {
        // Note: Dimensions are enforced to be odd so that the crosshair can work.
        // The toolbar is also designed to be odd width when it has an even number of positions.
        let width = 25;
        // we want to ceil() the height because the camera setup makes the height match
        // the viewport and ignores width, so we want to prefer too-narrow over too-wide
        let height = ((FreeCoordinate::from(width) / viewport.nominal_aspect_ratio()).ceil()
            as GridCoordinate)
            .max(8);
        let height = height / 2 * 2 + 1; // ensure odd
        Self {
            size: size2(width, height),
        }
    }

    /// TODO: depth should be up to the choice of the individual pages.
    pub(crate) fn space_bounds(self) -> GridAab {
        GridAab::from_lower_upper(
            [0, 0, -Self::DEPTH_BEHIND_VIEW_PLANE],
            [self.size.width, self.size.height, 5],
        )
    }

    /// Create a new space with the specified bounds and a standard lighting condition for UI.
    // TODO: validate this doesn't crash on wonky sizes.
    pub(crate) fn create_space(self) -> Space {
        let bounds = self.space_bounds();
        let Size2D {
            width: w,
            height: h,
            ..
        } = self.size;
        let mut space = Space::builder(bounds)
            .physics({
                let mut physics = SpacePhysics::default();
                physics.sky = space::Sky::Uniform(palette::HUD_SKY);
                physics
            })
            .build();

        if false {
            // Visualization of the bounds of the space we're drawing.
            // TODO: Use `BoxStyle` to draw this instead
            let mut add_frame = |z, color| {
                let frame_block = Block::from(color);
                space
                    .fill_uniform(GridAab::from_lower_size([0, 0, z], [w, h, 1]), &frame_block)
                    .unwrap();
                space
                    .fill_uniform(GridAab::from_lower_size([1, 1, z], [w - 2, h - 2, 1]), &AIR)
                    .unwrap();
            };
            add_frame(bounds.lower_bounds().z, Rgba::new(0.5, 0., 0., 1.));
            add_frame(-1, Rgba::new(0.5, 0.5, 0.5, 1.));
            add_frame(bounds.upper_bounds().z - 1, Rgba::new(0., 1., 1., 1.));
        }

        space
    }
}

/// Pair of a widget tree and a cached space it is instantiated in with a particular size,
/// which can be recreated with a different size as needed.
///
/// TODO: Give this a better name.
#[derive(Clone, Debug)]
pub(crate) struct PageInst {
    tree: WidgetTree,
    space: Option<Handle<Space>>,
}

impl PageInst {
    pub fn new(tree: WidgetTree) -> Self {
        Self { tree, space: None }
    }

    pub fn get_or_create_space(
        &mut self,
        mut size: UiSize,
        universe: &mut Universe,
    ) -> Handle<Space> {
        if let Some(space) = self.space.as_ref() {
            // TODO: We will need to be comparing the entire size if it gains other fields
            if space.read().unwrap().bounds() == size.space_bounds() {
                return space.clone();
            }
        }

        // If necessary, enlarge the proposed dimensions.
        // The camera may be bad but at least the widgets won't be missing parts.
        // TODO: We need overall better handling of this; for example, we should be able to
        // pan ("scroll") the camera over a tall dialog box.
        // Also, this doesn't handle Z size.
        let fitting_size = size.size.max(drop_depth(self.tree.requirements().minimum));
        if fitting_size != size.size {
            log::debug!("VUI page had to enlarge proposed size {size:?} to {fitting_size:?}");
            size.size = fitting_size;
        }

        // Size didn't match, so recreate the space.
        // TODO: Resize in-place instead, once `Space` supports that.
        match self.create_space(size, universe) {
            Ok(space) => {
                self.space = Some(space.clone());
                space
            }
            Err(error) => {
                // TODO: Recover by using old space or by popping the page state stack if possible
                panic!(
                    "VUI page construction failure:\n{error}",
                    error = all_is_cubes::util::ErrorChain(&error),
                );
            }
        }
    }

    fn create_space(
        &self,
        size: UiSize,
        universe: &mut Universe,
    ) -> Result<Handle<Space>, InstallVuiError> {
        let space = universe.insert_anonymous(size.create_space());
        // TODO: error handling for layout
        let txn = install_widgets(LayoutGrant::new(size.space_bounds()), &self.tree)?;
        space
            .execute(&txn)
            .map_err(|error| InstallVuiError::ExecuteInstallation { error })?;

        // Initialize lighting
        space
            .try_modify(|space| {
                space.fast_evaluate_light();
                space.evaluate_light::<time::NoTime>(10, |_| {});
            })
            .unwrap();

        Ok(space)
    }
}

// TODO: contribute this to euclid
fn drop_depth(size: GridSize) -> Size2D<GridCoordinate, Cube> {
    Size2D::new(size.width, size.height)
}

/// Wrap the given widget tree in a transparent screen-filling background.
pub(crate) fn page_modal_backdrop(foreground: WidgetTree) -> WidgetTree {
    Arc::new(LayoutTree::Stack {
        direction: Face6::PZ,
        children: vec![
            // TODO: have a better way to communicate our choice of "baseline" alignment
            Arc::new(LayoutTree::Spacer(LayoutRequest {
                // magic number 2 allows us to fill the edges of the viewport, ish
                // TODO: VUI camera positioning should give us the option of "overscan",
                // where all edges of the space spill off the window.
                minimum: GridSize::new(0, 0, UiSize::DEPTH_BEHIND_VIEW_PLANE + 2),
            })),
            vui::leaf_widget(widgets::Frame::with_block(color_block!(0., 0., 0., 0.7))),
            foreground,
        ],
    })
}

/// Helpers for assembling widget trees into dialog stuff.
pub(crate) mod parts {
    use super::*;

    /// Construct a [`Voxels`] widget around a widget tree containing [`LargeText`] or similar.
    ///
    /// TODO: Replace all uses of this with the new block-based text rendering.
    pub fn shrink(
        universe: &mut Universe,
        resolution: Resolution,
        large: &WidgetTree,
    ) -> Result<Arc<dyn Widget>, InstallVuiError> {
        let space = large.to_space(
            SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
            Gravity::new(Align::Center, Align::Center, Align::Low),
        )?;
        Ok(Arc::new(widgets::Voxels::new(
            space.bounds(),
            universe.insert_anonymous(space),
            resolution,
            BlockAttributes::default(),
        )))
    }

    pub fn heading(text: impl Into<ArcStr>) -> WidgetTree {
        vui::leaf_widget(widgets::Label::new(text.into()))
    }

    pub fn paragraph(text: impl Into<ArcStr>) -> WidgetTree {
        vui::leaf_widget(widgets::Label::with_font(
            text.into(),
            text::Font::SmallerBodyText,
            text::Positioning {
                x: text::PositioningX::Left,
                line_y: text::PositioningY::BodyTop,
                z: text::PositioningZ::Back,
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ui_size() {
        let cases: Vec<([u32; 2], [i32; 2])> =
            vec![([800, 600], [25, 19]), ([1000, 600], [25, 15])];
        let mut failed = 0;
        for (nominal_viewport, expected_size) in cases {
            let actual_size = UiSize::new(camera::Viewport::with_scale(1.0, nominal_viewport)).size;
            let actual_size: [i32; 2] = actual_size.into();
            if actual_size != expected_size {
                println!("{nominal_viewport:?} expected to produce {expected_size:?}; got {actual_size:?}");
                failed += 1;
            }
        }
        if failed > 0 {
            panic!("{failed} cases failed");
        }
    }
}
