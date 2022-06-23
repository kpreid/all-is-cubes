// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Specific UI widgets.

use std::error::Error;
use std::fmt::{self, Debug};
use std::sync::Arc;

use embedded_graphics::prelude::{Point, Primitive};
use embedded_graphics::primitives::{PrimitiveStyleBuilder, Rectangle};
use embedded_graphics::Drawable;
use exhaust::Exhaust;

use crate::behavior::BehaviorSetTransaction;
use crate::block::{Block, AIR};
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::inv::EphemeralOpaque;
use crate::listen::{DirtyFlag, ListenableSource};
use crate::math::{GridMatrix, GridPoint, GridVector};
use crate::space::{Grid, SpaceTransaction};
use crate::time::Tick;
use crate::vui::{
    ActivatableRegion, InstallVuiError, LayoutGrant, LayoutRequest, Layoutable, Widget,
    WidgetController, WidgetTransaction,
};

mod toolbar;
pub(crate) use toolbar::*;
mod tooltip;
pub(crate) use tooltip::*;

/// Generic widget controller that only does something initialize.
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
#[doc(hidden)] // TODO: widget API still in development and this is particularly dubious
pub struct OneshotController(pub Option<WidgetTransaction>);

impl WidgetController for OneshotController {
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        Ok(self.0.take().unwrap_or_default())
    }

    // TODO: Arrange somehow for this controller to be deleted since it doesn't need to be step()ped
}

/// Widget that fills a flat XY surface with a "dialog box" sort of background.
/// Also useful for identifying the bounds of a layout region.
///
/// TODO: Define what it does in 3D.
#[derive(Debug)]
pub(crate) struct FrameWidget {}

impl FrameWidget {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {})
    }
}

impl Layoutable for FrameWidget {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: GridVector::new(0, 0, 1),
        }
    }
}

impl Widget for FrameWidget {
    fn controller(self: Arc<Self>, position: &LayoutGrant) -> Box<dyn WidgetController> {
        let bounds = position.bounds;
        let mut txn = SpaceTransaction::default();

        if !bounds.is_empty() {
            let background_rect = Rectangle::with_corners(
                Point::new(bounds.lower_bounds().x, bounds.lower_bounds().y),
                Point::new(bounds.upper_bounds().x - 1, bounds.upper_bounds().y - 1),
            );

            let dt = &mut txn.draw_target(GridMatrix::from_translation([
                0,
                0,
                bounds.lower_bounds().z,
            ]));

            let background = VoxelBrush::single(Block::from(palette::MENU_BACK));
            let frame = VoxelBrush::single(Block::from(palette::MENU_FRAME));

            background_rect
                .into_styled(
                    PrimitiveStyleBuilder::new()
                        .stroke_width(1)
                        .stroke_color(&frame)
                        .fill_color(&background)
                        .build(),
                )
                .draw(dt)
                .unwrap();
        }

        Box::new(OneshotController(Some(txn)))
    }
}

/// A single-block button that displays a boolean state derived from a
/// [`ListenableSource`].
#[derive(Clone)]
pub(crate) struct ToggleButtonWidget<D> {
    states: [Block; 2],
    data_source: ListenableSource<D>,
    projection: Arc<dyn Fn(&D) -> bool + Send + Sync>,
    action: EphemeralOpaque<dyn Fn() + Send + Sync>,
}

impl<D: Clone + Sync + Debug> Debug for ToggleButtonWidget<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ToggleButtonWidget")
            .field("states", &self.states)
            .field("data_source", &self.data_source)
            .field(
                "projection(data_source)",
                &(self.projection)(&self.data_source.snapshot()),
            )
            .field("action", &self.action)
            .finish()
    }
}

impl<D> ToggleButtonWidget<D> {
    pub(crate) fn new(
        data_source: ListenableSource<D>,
        projection: impl Fn(&D) -> bool + Send + Sync + 'static,
        mut blocks: impl FnMut(ToggleButtonVisualState) -> Block,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            data_source,
            projection: Arc::new(projection),
            states: [
                blocks(ToggleButtonVisualState { value: false }),
                blocks(ToggleButtonVisualState { value: true }),
            ],
            action: EphemeralOpaque::from(Arc::new(action) as Arc<dyn Fn() + Send + Sync>),
        })
    }
}

impl<D> Layoutable for ToggleButtonWidget<D> {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }
    }
}

// TODO: Mess of generic bounds due to the combination of Widget and ListenableSource
// requirements -- should we make a trait alias for these?
impl<D: Clone + Debug + Send + Sync + 'static> Widget for ToggleButtonWidget<D> {
    fn controller(self: Arc<Self>, position: &LayoutGrant) -> Box<dyn WidgetController> {
        Box::new(ToggleButtonController::new(
            position
                .shrink_to(self.requirements().minimum)
                .bounds
                .lower_bounds(),
            self,
        ))
    }
}

/// Possible visual states of a `ToggleButtonWidget`.
///
/// The [`fmt::Display`] implementation of this type produces a string form suitable for
/// naming blocks depicting this state; the [`Exhaust`] implementation allows iterating
/// over all possible states.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
pub struct ToggleButtonVisualState {
    /// The on/off value depicted.
    pub value: bool,
    // TODO: add hover/press states
}

/// Represents this value as a string suitable for naming blocks depicting this state.
impl fmt::Display for ToggleButtonVisualState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self { value: true } => write!(f, "on"),
            Self { value: false } => write!(f, "off"),
        }
    }
}

/// [`WidgetController`] for [`ToggleButtonWidget`].
#[derive(Debug)]
pub(crate) struct ToggleButtonController<D: Clone + Send + Sync> {
    definition: Arc<ToggleButtonWidget<D>>,
    position: GridPoint,
    todo: DirtyFlag,
}

impl<D: Clone + Debug + Send + Sync + 'static> ToggleButtonController<D> {
    pub(crate) fn new(position: GridPoint, definition: Arc<ToggleButtonWidget<D>>) -> Self {
        Self {
            todo: DirtyFlag::listening(true, |l| definition.data_source.listen(l)),
            position,
            definition,
        }
    }
}

impl<D: Clone + Debug + Send + Sync + 'static> WidgetController for ToggleButtonController<D> {
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        Ok(SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            Arc::new(ActivatableRegion {
                region: Grid::single_cube(self.position),
                effect: self.definition.action.clone(),
            }),
        )))
    }

    fn step(&mut self, _: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        Ok(if self.todo.get_and_clear() {
            let value = (self.definition.projection)(&self.definition.data_source.get());
            SpaceTransaction::set_cube(
                self.position,
                None,
                Some(self.definition.states[usize::from(value)].clone()),
            )
        } else {
            SpaceTransaction::default()
        })
    }
}

/// Shows/hides the crosshair depending on mouselook mode.
#[derive(Debug)]
pub(crate) struct CrosshairController {
    icon: Block,
    todo: DirtyFlag,
    position: GridPoint,
    mouselook_mode: ListenableSource<bool>,
}

impl CrosshairController {
    pub fn new(position: GridPoint, icon: Block, mouselook_mode: ListenableSource<bool>) -> Self {
        Self {
            icon,
            todo: DirtyFlag::listening(false, |l| mouselook_mode.listen(l)),
            position,
            mouselook_mode,
        }
    }
}

impl WidgetController for CrosshairController {
    fn step(&mut self, _tick: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        Ok(if self.todo.get_and_clear() {
            SpaceTransaction::set_cube(
                self.position,
                None,
                Some(if *self.mouselook_mode.get() {
                    self.icon.clone()
                } else {
                    AIR
                }),
            )
        } else {
            SpaceTransaction::default()
        })
    }
}
