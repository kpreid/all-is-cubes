use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::sync::Arc;

use embedded_graphics::image::Image as EgImage;
use embedded_graphics::mono_font::{MonoFont, MonoTextStyle};
use embedded_graphics::prelude::{Dimensions, PixelColor, Point, Size};
use embedded_graphics::primitives::{
    Primitive, PrimitiveStyleBuilder, Rectangle, RoundedRectangle, StrokeAlignment,
};
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use embedded_graphics::Drawable;
use exhaust::Exhaust;

use crate::behavior::BehaviorSetTransaction;
use crate::block::{
    Block,
    Resolution::{self, R32},
};
use crate::content::load_image::{default_srgb, ImageAdapter};
use crate::content::palette;
use crate::drawing::{DrawingPlane, VoxelBrush};
use crate::inv::EphemeralOpaque;
use crate::linking::InGenError;
use crate::listen::{DirtyFlag, ListenableSource};
use crate::math::{GridAab, GridCoordinate, GridMatrix, GridPoint, GridVector, Rgb, Rgba};
use crate::space::{Space, SpacePhysics, SpaceTransaction};
use crate::time::Tick;
use crate::universe::Universe;
use crate::vui::{self, Layoutable};

/// A single-block button that displays a boolean state derived from a
/// [`ListenableSource`].
#[derive(Clone)]
pub struct ToggleButton<D> {
    states: [Block; 2],
    data_source: ListenableSource<D>,
    projection: Arc<dyn Fn(&D) -> bool + Send + Sync>,
    action: EphemeralOpaque<dyn Fn() + Send + Sync>,
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
        vui::LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }
    }
}

// TODO: Mess of generic bounds due to the combination of Widget and ListenableSource
// requirements -- should we make a trait alias for these?
impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::Widget for ToggleButton<D> {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ToggleButtonController::new(
            position
                .shrink_to(self.requirements().minimum)
                .bounds
                .lower_bounds(),
            self,
        ))
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

/// [`WidgetController`] for [`ToggleButton`].
#[derive(Debug)]
struct ToggleButtonController<D: Clone + Send + Sync> {
    definition: Arc<ToggleButton<D>>,
    position: GridPoint,
    todo: DirtyFlag,
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> ToggleButtonController<D> {
    fn new(position: GridPoint, definition: Arc<ToggleButton<D>>) -> Self {
        Self {
            todo: DirtyFlag::listening(true, |l| definition.data_source.listen(l)),
            position,
            definition,
        }
    }
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::WidgetController
    for ToggleButtonController<D>
{
    fn initialize(&mut self) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        Ok(SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            Arc::new(vui::ActivatableRegion {
                region: GridAab::single_cube(self.position),
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
/// These blocks may then be used with button widgets like [`ToggleButton`].
#[derive(Debug)]
pub struct ButtonBlockBuilder {
    space: Space,
    active: bool,
    label_z: GridCoordinate,
    label_color: Rgba,
}

impl ButtonBlockBuilder {
    pub const RESOLUTION: Resolution = R32;
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
    /// [`DrawTarget`]: embedded_graphics::prelude::DrawTarget
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
}

/// A shape for a button; defining the elements which communicate the type of the button
/// (single action, toggle, radio button, etc.) and the state of the button (pressed,
/// toggled on, disabled, etc.), but not the label (symbol indicating what it does or
/// controls).
///
/// These shapes can then be turned into specific [`Block`]s using
/// [`ButtonBase::button_builder()`], and used in button widgets like [`ToggleButton`].
pub trait ButtonBase {
    /// Returns a new [`ButtonBlockBuilder`] which can be used to draw a button label and
    /// construct a [`Block`].
    fn button_builder(&self) -> Result<ButtonBlockBuilder, InGenError>;
}

impl ButtonBase for ToggleButtonVisualState {
    /// Returns a new [`ButtonBlockBuilder`] which can be used to construct [`Block`]s
    /// suitable for [`ToggleButton`]s. The label drawn onto this button should fit
    /// within a 24 × 24 square.
    fn button_builder(&self) -> Result<ButtonBlockBuilder, InGenError> {
        let label_z = 12;
        let active = self.value;
        let back_block = Block::from(if active {
            palette::BUTTON_ACTIVATED_BACK
        } else {
            palette::BUTTON_BACK
        });
        let cap_rim_block = Block::from(palette::BUTTON_BACK.map_rgb(|rgb| rgb * 1.1));

        let frame_brush = VoxelBrush::single(Block::from(palette::BUTTON_FRAME));
        let back_brush = VoxelBrush::with_thickness(back_block, 0..label_z);
        let cap_rim_brush = VoxelBrush::new([([0, 0, label_z - 1], &cap_rim_block)]);

        let outer_inset = 2;
        let outer_rectangle = Rectangle::with_corners(
            Point::new(outer_inset, outer_inset),
            Point::new(
                // - 1 because e-g rectangles are specified in terms of their outermost pixels
                ButtonBlockBuilder::RESOLUTION_G - outer_inset - 1,
                ButtonBlockBuilder::RESOLUTION_G - outer_inset - 1,
            ),
        );
        let rr = |inset: i32| {
            RoundedRectangle::with_equal_corners(
                outer_rectangle.offset(-inset),
                Size::new(5 - inset as u32, 5 - inset as u32),
            )
        };

        let mut space = Space::builder(GridAab::from_lower_size(
            [0, 0, 0],
            // this will need to be changed if we want to support thick labels
            [
                ButtonBlockBuilder::RESOLUTION_G,
                ButtonBlockBuilder::RESOLUTION_G,
                label_z + 1,
            ],
        ))
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build();
        let draw_target = &mut space.draw_target(
            GridMatrix::from_translation([0, ButtonBlockBuilder::RESOLUTION_G - 1, 0])
                * GridMatrix::FLIP_Y,
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