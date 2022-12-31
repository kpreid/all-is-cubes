use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::sync::Arc;

use exhaust::Exhaust;

use all_is_cubes::behavior::BehaviorSetTransaction;
use all_is_cubes::block::{
    Block,
    Resolution::{self, R32},
};
use all_is_cubes::content::load_image::{default_srgb, ImageAdapter};
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::{
    image::Image as EgImage,
    mono_font::{MonoFont, MonoTextStyle},
    prelude::{Dimensions, PixelColor, Point, Size},
    primitives::{
        Circle, Primitive, PrimitiveStyleBuilder, Rectangle, RoundedRectangle, StrokeAlignment,
    },
    text::{Alignment, Baseline, Text, TextStyleBuilder},
    Drawable,
};
use all_is_cubes::drawing::{DrawingPlane, VoxelBrush};
use all_is_cubes::inv::EphemeralOpaque;
use all_is_cubes::linking::InGenError;
use all_is_cubes::listen::{DirtyFlag, ListenableSource};
use all_is_cubes::math::{GridAab, GridCoordinate, GridMatrix, GridPoint, GridVector, Rgb, Rgba};
use all_is_cubes::space::{self, Space, SpaceBehaviorAttachment, SpacePhysics, SpaceTransaction};
use all_is_cubes::time::Tick;
use all_is_cubes::transaction::Merge;
use all_is_cubes::universe::Universe;

use crate::vui::{self, Layoutable as _};

type Action = EphemeralOpaque<dyn Fn() + Send + Sync>;

const REQUIREMENT: vui::LayoutRequest = vui::LayoutRequest {
    minimum: GridVector::new(1, 1, 1),
};

/// A single-block button that reacts to activations (clicks) but does not change
/// otherwise.
#[derive(Clone, Debug)]
pub struct ActionButton {
    // TODO: this will eventually want hover + pressed state blocks
    block: Block,
    action: Action,
}

impl ActionButton {
    pub fn new(
        mut blocks: impl FnMut(ActionButtonVisualState) -> Block,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            block: blocks(ActionButtonVisualState { _dummy: () }),
            action: EphemeralOpaque::from(Arc::new(action) as Arc<dyn Fn() + Send + Sync>),
        })
    }
}

impl vui::Layoutable for ActionButton {
    fn requirements(&self) -> vui::LayoutRequest {
        REQUIREMENT
    }
}

impl vui::Widget for ActionButton {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ActionButtonController {
            position: position
                .shrink_to(self.requirements().minimum, false)
                .bounds
                .lower_bounds(),
            definition: self,
        })
    }
}

/// Possible visual states of a [`ActionButton`].
///
/// The [`fmt::Display`] implementation of this type produces a string form suitable for
/// naming blocks depicting this state; the [`Exhaust`] implementation allows iterating
/// over all possible states.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
pub struct ActionButtonVisualState {
    // TODO: Add hover, pressed, disabled
    _dummy: (),
}

/// Represents this value as a string suitable for naming blocks depicting this state.
impl fmt::Display for ActionButtonVisualState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "idle")
    }
}

/// [`WidgetController`] for [`ActionButton`].
#[derive(Debug)]
struct ActionButtonController {
    definition: Arc<ActionButton>,
    position: GridPoint,
}

impl vui::WidgetController for ActionButtonController {
    fn initialize(&mut self) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let icon =
            SpaceTransaction::set_cube(self.position, None, Some(self.definition.block.clone()));
        let activatable = space::SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            space::SpaceBehaviorAttachment::new(GridAab::single_cube(self.position)),
            Arc::new(space::ActivatableRegion {
                effect: self.definition.action.clone(),
            }),
        ));
        icon.merge(activatable)
            .map_err(|error| vui::InstallVuiError::Conflict { error })
    }
}

/// A single-block button that displays a boolean state derived from a
/// [`ListenableSource`] and can be clicked.
#[derive(Clone)]
pub struct ToggleButton<D> {
    states: [Block; 2],
    data_source: ListenableSource<D>,
    projection: Arc<dyn Fn(&D) -> bool + Send + Sync>,
    action: Action,
}

impl<D: Clone + Sync + fmt::Debug> fmt::Debug for ToggleButton<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ToggleButton")
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

impl<D> ToggleButton<D> {
    pub fn new(
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

impl<D> vui::Layoutable for ToggleButton<D> {
    fn requirements(&self) -> vui::LayoutRequest {
        REQUIREMENT
    }
}

// TODO: Mess of generic bounds due to the combination of Widget and ListenableSource
// requirements -- should we make a trait alias for these?
impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::Widget for ToggleButton<D> {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ToggleButtonController {
            todo: DirtyFlag::listening(true, |l| self.data_source.listen(l)),
            position: position
                .shrink_to(self.requirements().minimum, false)
                .bounds
                .lower_bounds(),
            definition: self,
        })
    }
}

/// Possible visual states of a [`ToggleButton`].
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

impl ToggleButtonVisualState {
    /// Returns the “off” (false) or “on” (true) state.
    pub const fn new(value: bool) -> Self {
        Self { value }
    }
}

/// [`WidgetController`] for [`ToggleButton`].
#[derive(Debug)]
struct ToggleButtonController<D: Clone + Send + Sync> {
    definition: Arc<ToggleButton<D>>,
    position: GridPoint,
    todo: DirtyFlag,
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::WidgetController
    for ToggleButtonController<D>
{
    fn initialize(&mut self) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        Ok(SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            SpaceBehaviorAttachment::new(GridAab::single_cube(self.position)),
            Arc::new(space::ActivatableRegion {
                effect: self.definition.action.clone(),
            }),
        )))
    }

    fn step(&mut self, _: Tick) -> Result<vui::WidgetTransaction, Box<dyn Error + Send + Sync>> {
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

/// Allows drawing a button [`Block`] (a shape in one of several states with a label on it)
/// given a [`ButtonBase`] defining the style and state.
///
/// These blocks may then be used with button widgets like [`ActionButton`] and [`ToggleButton`].
#[derive(Debug)]
pub struct ButtonBlockBuilder {
    space: Space,
    active: bool,
    label_z: GridCoordinate,
    label_color: Rgba,
}

impl ButtonBlockBuilder {
    /// Resolution of the produced blocks.
    pub const RESOLUTION: Resolution = theme::RESOLUTION;
    /// Resolution of the produced blocks, as [`GridCoordinate`].
    pub const RESOLUTION_G: GridCoordinate = Self::RESOLUTION.to_grid();

    /// Construct a new [`ButtonBlockBuilder`] ready to draw into;
    /// equivalent to [`ButtonBase::button_builder()`].
    pub fn new<S: ButtonBase>(state: S) -> Result<Self, InGenError> {
        state.button_builder()
    }

    /// Create the [`Block`] from this builder.
    pub fn build(self, universe: &mut Universe, label: impl Into<Cow<'static, str>>) -> Block {
        Block::builder()
            .display_name(label)
            .light_emission(if self.active {
                palette::BUTTON_ACTIVATED_GLOW
            } else {
                Rgb::ZERO
            })
            .voxels_ref(Self::RESOLUTION, universe.insert_anonymous(self.space))
            .build()
    }

    /// Returns a [`DrawTarget`] for drawing the button label, with a
    /// Y-down coordinate system whose origin is centered on the button (or more precisely,
    /// (0, 0) is the lower-right pixel closest to the center, since e-g uses a convention
    /// where coordinates identify pixels, not their edges).
    ///
    /// Consult the [`ButtonBase`] in use for the appropriate size.
    ///
    /// [`DrawTarget`]: all_is_cubes::drawing::embedded_graphics::prelude::DrawTarget
    pub fn label_draw_target<C: PixelColor>(&mut self) -> DrawingPlane<'_, Space, C> {
        self.space.draw_target(
            GridMatrix::from_translation([
                Self::RESOLUTION_G / 2,
                Self::RESOLUTION_G / 2 - 1,
                self.label_z,
            ]) * GridMatrix::FLIP_Y,
        )
    }

    /// Draw the given image, centered, as the button label.
    ///
    /// Consult the [`ButtonBase`] in use for the appropriate size.
    pub fn draw_icon(&mut self, icon: &image::DynamicImage) -> Result<(), InGenError> {
        let id = &ImageAdapter::adapt(icon, default_srgb);
        EgImage::new(&id, -id.bounding_box().center() - Point::new(1, 1))
            .draw(&mut self.label_draw_target())?;
        Ok(())
    }

    /// Draw a text label.
    ///
    /// Consult the [`ButtonBase`] in use for the appropriate size.
    #[allow(unused)] // TODO: delete this if we continue to have only icon buttons
    pub fn draw_text(&mut self, font: &MonoFont<'_>, text: &str) -> Result<(), InGenError> {
        Text::with_text_style(
            text,
            Point::new(-1, -1),
            MonoTextStyle::new(font, self.label_color),
            TextStyleBuilder::new()
                .baseline(Baseline::Middle)
                .alignment(Alignment::Center)
                .build(),
        )
        .draw(&mut self.label_draw_target())?;
        Ok(())
    }

    fn create_space(max_z: GridCoordinate) -> Space {
        Space::builder(GridAab::from_lower_size(
            [0, 0, 0],
            [Self::RESOLUTION_G, Self::RESOLUTION_G, max_z],
        ))
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build()
    }
}

/// Common constants for button shapes.
/// TODO: public?
mod theme {
    use super::*;
    pub const RESOLUTION: Resolution = R32;
    pub const RESOLUTION_G: GridCoordinate = RESOLUTION.to_grid();
    pub const UNPRESSED_Z: GridCoordinate = 12;
    pub fn rim_lightening(color: Rgba) -> Rgba {
        color.map_rgb(|rgb| rgb * 1.1)
    }
}

/// A shape for a button; defining the elements which communicate the type of the button
/// (single action, toggle, radio button, etc.) and the state of the button (pressed,
/// toggled on, disabled, etc.), but not the label (symbol indicating what it does or
/// controls).
///
/// These shapes can then be turned into specific [`Block`]s using
/// [`ButtonBase::button_builder()`], and used in button widgets like [`ActionButton`]
/// and [`ToggleButton`].
pub trait ButtonBase {
    /// Returns a new [`ButtonBlockBuilder`] which can be used to draw a button label and
    /// construct a [`Block`].
    fn button_builder(&self) -> Result<ButtonBlockBuilder, InGenError>;
}

impl ButtonBase for ActionButtonVisualState {
    /// Returns a new [`ButtonBlockBuilder`] which can be used to construct [`Block`]s
    /// suitable for [`ActionButton`]s. The label drawn onto this button should fit
    /// within a 24 × 24 circle.
    fn button_builder(&self) -> Result<ButtonBlockBuilder, InGenError> {
        let label_z = theme::UNPRESSED_Z;
        let back_block = palette::BUTTON_BACK; // TODO: different color theme?
        let frame_brush = VoxelBrush::single(Block::from(palette::BUTTON_FRAME));
        let back_brush = VoxelBrush::with_thickness(back_block, 0..label_z);
        let cap_rim_brush = VoxelBrush::new([(
            [0, 0, label_z - 1],
            Block::from(theme::rim_lightening(palette::BUTTON_BACK)),
        )]);

        let outer_inset = 2; // TODO duplicate number
        let circle = |inset: i32| {
            let inset = outer_inset + inset;
            Circle::new(
                Point::new(inset, inset),
                (theme::RESOLUTION_G - inset * 2) as u32,
            )
        };

        let mut space = ButtonBlockBuilder::create_space(label_z + 1);
        let draw_target = &mut space.draw_target(
            GridMatrix::from_translation([0, theme::RESOLUTION_G - 1, 0]) * GridMatrix::FLIP_Y,
        );

        circle(0)
            .into_styled(
                PrimitiveStyleBuilder::new()
                    .fill_color(&back_brush)
                    .stroke_color(&frame_brush)
                    .stroke_width(2)
                    .stroke_alignment(StrokeAlignment::Inside)
                    .build(),
            )
            .draw(draw_target)?;
        circle(2)
            .into_styled(
                PrimitiveStyleBuilder::new()
                    .stroke_color(&cap_rim_brush)
                    .stroke_width(1)
                    .stroke_alignment(StrokeAlignment::Inside)
                    .build(),
            )
            .draw(draw_target)?;

        Ok(ButtonBlockBuilder {
            space,
            active: false,
            label_z,
            label_color: palette::BUTTON_LABEL,
        })
    }
}

impl ButtonBase for ToggleButtonVisualState {
    /// Returns a new [`ButtonBlockBuilder`] which can be used to construct [`Block`]s
    /// suitable for [`ToggleButton`]s. The label drawn onto this button should fit
    /// within a 24 × 24 square.
    fn button_builder(&self) -> Result<ButtonBlockBuilder, InGenError> {
        let label_z = theme::UNPRESSED_Z;
        let active = self.value;
        let back_block = Block::from(if active {
            palette::BUTTON_ACTIVATED_BACK
        } else {
            palette::BUTTON_BACK
        });
        let frame_brush = VoxelBrush::single(Block::from(palette::BUTTON_FRAME));
        let back_brush = VoxelBrush::with_thickness(back_block, 0..label_z);
        let cap_rim_brush = VoxelBrush::new([(
            [0, 0, label_z - 1],
            Block::from(theme::rim_lightening(palette::BUTTON_BACK)),
        )]);

        let outer_inset = 2;
        let outer_rectangle = Rectangle::with_corners(
            Point::new(outer_inset, outer_inset),
            Point::new(
                // - 1 because e-g rectangles are specified in terms of their outermost pixels
                theme::RESOLUTION_G - outer_inset - 1,
                theme::RESOLUTION_G - outer_inset - 1,
            ),
        );
        let rr = |inset: i32| {
            RoundedRectangle::with_equal_corners(
                outer_rectangle.offset(-inset),
                Size::new(5 - inset as u32, 5 - inset as u32),
            )
        };

        let mut space = ButtonBlockBuilder::create_space(label_z + 1);
        let draw_target = &mut space.draw_target(
            GridMatrix::from_translation([0, theme::RESOLUTION_G - 1, 0]) * GridMatrix::FLIP_Y,
        );

        // unwrap()s because if this drawing fails, tests will catch that — no parameters
        rr(0)
            .into_styled(
                PrimitiveStyleBuilder::new()
                    .fill_color(&back_brush)
                    .stroke_color(&frame_brush)
                    .stroke_width(2)
                    .stroke_alignment(StrokeAlignment::Inside)
                    .build(),
            )
            .draw(draw_target)?;
        rr(2)
            .into_styled(
                PrimitiveStyleBuilder::new()
                    .stroke_color(&cap_rim_brush)
                    .stroke_width(1)
                    .stroke_alignment(StrokeAlignment::Inside)
                    .build(),
            )
            .draw(draw_target)?;

        Ok(ButtonBlockBuilder {
            space,
            active,
            label_z,
            label_color: if active {
                palette::BUTTON_ACTIVATED_LABEL
            } else {
                palette::BUTTON_LABEL
            },
        })
    }
}
