use alloc::sync::Arc;
use core::fmt;
use core::hash::Hash;
use core::sync::atomic::{AtomicU8, Ordering::Relaxed};
use std::error::Error;

use exhaust::Exhaust;

use all_is_cubes::behavior::BehaviorSetTransaction;
use all_is_cubes::block::builder::BlockBuilderVoxels;
use all_is_cubes::block::{
    self, Block, BlockBuilder,
    Resolution::{self, *},
};
use all_is_cubes::content::load_image::{default_srgb, DecodedPng, PngAdapter};
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::{
    image::Image as EgImage,
    mono_font::{MonoFont, MonoTextStyle},
    prelude::{Dimensions, PixelColor, Point, Size},
    primitives::{Primitive, PrimitiveStyleBuilder, Rectangle, RoundedRectangle, StrokeAlignment},
    text::{Alignment, Baseline, Text, TextStyleBuilder},
    Drawable,
};
use all_is_cubes::drawing::{DrawingPlane, VoxelBrush};
use all_is_cubes::inv::EphemeralOpaque;
use all_is_cubes::linking::{self, InGenError};
use all_is_cubes::listen::{DirtyFlag, ListenableSource};
use all_is_cubes::math::{Face6, GridAab, GridCoordinate, GridVector, Gridgid, Rgba};
use all_is_cubes::space::{self, Space, SpaceBehaviorAttachment, SpacePhysics, SpaceTransaction};
use all_is_cubes::transaction::Merge;
use all_is_cubes::universe::{URef, Universe};

use crate::vui;
use crate::vui::widgets::{BoxStyle, WidgetBlocks, WidgetTheme};

type Action = EphemeralOpaque<dyn Fn() + Send + Sync>;

/// Layout requirement for all buttons.
/// TODO: This will need to be dynamic once buttons have text labels.
const REQUIREMENT: vui::LayoutRequest = vui::LayoutRequest {
    minimum: GridVector::new(1, 1, 1),
};

fn shrink_button_bounds(grant: vui::LayoutGrant) -> vui::LayoutGrant {
    grant.shrink_to(REQUIREMENT.minimum, false)
}

/// Common elements of button widgets.
/// A button widget is a widget that displays a single clickable shape that can have a finite set
/// of possible appearances (on/off, pressed, etc).
///
/// TODO: Better name for this.
#[derive(Clone, Debug)]
struct ButtonCommon<St> {
    /// Button shape, indicating what kind of button it is.
    shape: linking::Provider<St, BoxStyle>,
    // TODO: need to separate shape from label
    // /// Label to be put on top of the shape.
    // /// TODO: Needs to be able to be text or text and icon.
    // label_block: Block,
}

impl<St: ButtonBase + Eq + Hash> ButtonCommon<St> {
    fn new(shape: linking::Provider<St, Block>, label: Block) -> Self
    where
        St: Exhaust + Clone + fmt::Debug,
    {
        let shape = shape
            .map(|state, base_block| assemble_button(state, base_block.clone(), label.clone()));
        Self { shape }
    }

    fn draw_txn(&self, grant: &vui::LayoutGrant, state: St) -> vui::WidgetTransaction {
        let grant = shrink_button_bounds(*grant);

        self.shape[state].create_box(grant.bounds)
    }
}

impl<St> vui::Layoutable for ButtonCommon<St> {
    fn requirements(&self) -> vui::LayoutRequest {
        REQUIREMENT
    }
}

/// A single-block button that reacts to activations (clicks) but does not change
/// otherwise.
#[derive(Clone, Debug)]
pub struct ActionButton {
    common: ButtonCommon<ButtonVisualState>,
    action: Action,
}

impl ActionButton {
    #[allow(missing_docs)]
    pub fn new(
        label: Block,
        theme: &WidgetTheme,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            common: ButtonCommon::new(
                theme.widget_blocks.subset(WidgetBlocks::ActionButton),
                label,
            ),
            action: EphemeralOpaque::new(Arc::new(action)),
        })
    }
}

impl vui::Layoutable for ActionButton {
    fn requirements(&self) -> vui::LayoutRequest {
        self.common.requirements()
    }
}

impl vui::Widget for ActionButton {
    fn controller(self: Arc<Self>, _: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ActionButtonController { definition: self })
    }
}

/// Possible visual states of a button.
///
/// [`ActionButton`] uses this directly, and other buttons' state may incorporate it.
///
/// The [`fmt::Display`] implementation of this type produces a string form suitable for
/// naming blocks depicting this state; the [`Exhaust`] implementation allows iterating
/// over all possible states.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
pub struct ButtonVisualState {
    // TODO: Add hover, disabled
    /// The button looks pushed in.
    pressed: bool,
}

/// Represents this value as a string suitable for naming blocks depicting this state.
impl fmt::Display for ButtonVisualState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.pressed { "pressed" } else { "idle" }.fmt(f)
    }
}

/// [`WidgetController`] for [`ActionButton`].
#[derive(Debug)]
struct ActionButtonController {
    definition: Arc<ActionButton>,
}

impl vui::WidgetController for ActionButtonController {
    fn initialize(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let grant = shrink_button_bounds(*context.grant());

        // TODO: we never draw the pressed state
        let draw = self
            .definition
            .common
            .draw_txn(&grant, ButtonVisualState { pressed: false });
        let activatable = space::SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            space::SpaceBehaviorAttachment::new(grant.bounds),
            Arc::new(space::ActivatableRegion {
                effect: self.definition.action.clone(),
            }),
        ));
        draw.merge(activatable)
            .map_err(|error| vui::InstallVuiError::Conflict { error })
    }
}

/// A single-block button that displays a boolean state derived from a
/// [`ListenableSource`] and can be clicked.
#[derive(Clone)]
pub struct ToggleButton<D> {
    common: ButtonCommon<ToggleButtonVisualState>,
    data_source: ListenableSource<D>,
    projection: Arc<dyn Fn(&D) -> bool + Send + Sync>,
    action: Action,
}

impl<D: Clone + Sync + fmt::Debug> fmt::Debug for ToggleButton<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ToggleButton")
            .field("common", &self.common)
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
    #[allow(missing_docs)]
    pub fn new(
        data_source: ListenableSource<D>,
        projection: impl Fn(&D) -> bool + Send + Sync + 'static,
        label: Block,
        theme: &WidgetTheme,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            common: ButtonCommon::new(
                theme.widget_blocks.subset(WidgetBlocks::ToggleButton),
                label,
            ),
            data_source,
            projection: Arc::new(projection),
            action: EphemeralOpaque::new(Arc::new(action)),
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
    fn controller(self: Arc<Self>, _: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ToggleButtonController {
            todo: DirtyFlag::listening(true, &self.data_source),
            definition: self,
            recently_pressed: Arc::new(AtomicU8::new(0)),
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
    common: ButtonVisualState,
    /// The on/off value depicted.
    pub value: bool,
}

/// Represents this value as a string suitable for naming blocks depicting this state.
impl fmt::Display for ToggleButtonVisualState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self { value, common } = self;
        write!(
            f,
            "{value}-{common}",
            value = match value {
                false => "off",
                true => "on",
            },
        )
    }
}

impl ToggleButtonVisualState {
    /// Returns the “off” (false) or “on” (true) state.
    pub const fn new(value: bool) -> Self {
        Self {
            value,
            common: ButtonVisualState { pressed: false },
        }
    }
}

/// [`WidgetController`] for [`ToggleButton`].
#[derive(Debug)]
struct ToggleButtonController<D: Clone + Send + Sync> {
    definition: Arc<ToggleButton<D>>,
    todo: DirtyFlag,
    recently_pressed: Arc<AtomicU8>,
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> ToggleButtonController<D> {
    fn draw_txn(&self, grant: &vui::LayoutGrant) -> vui::WidgetTransaction {
        let value = (self.definition.projection)(&self.definition.data_source.get());
        self.definition.common.draw_txn(
            grant,
            ToggleButtonVisualState {
                value,
                // TODO: once cursor/click system supports mousedown and up, use that instead
                // of this crude animation behavior (but maybe *also* have a post-press
                // animation, possibly based on block tick_actions instead).
                common: ButtonVisualState {
                    pressed: self
                        .recently_pressed
                        .fetch_update(Relaxed, Relaxed, |counter| Some(counter.saturating_sub(1)))
                        .unwrap()
                        > 1,
                },
            },
        )
    }
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::WidgetController
    for ToggleButtonController<D>
{
    fn initialize(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let grant = shrink_button_bounds(*context.grant());

        let activatable = SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            SpaceBehaviorAttachment::new(grant.bounds),
            Arc::new(space::ActivatableRegion {
                effect: {
                    let action = self.definition.action.clone();
                    let recently_pressed = self.recently_pressed.clone();

                    // TODO: awkward lack of composability here.
                    // Perhaps ActivatableRegion should be replaced with being able to
                    // activate any behavior, i.e. this WidgetBehavior?
                    EphemeralOpaque::new(Arc::new(move || {
                        recently_pressed.store(10, Relaxed);
                        if let Some(f) = action.try_ref() {
                            f();
                        }
                    }))
                },
            }),
        ));
        self.draw_txn(context.grant())
            .merge(activatable)
            .map_err(|error| vui::InstallVuiError::Conflict { error })
    }

    fn step(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<(vui::WidgetTransaction, vui::Then), Box<dyn Error + Send + Sync>> {
        Ok((
            if self.todo.get_and_clear() || self.recently_pressed.load(Relaxed) > 0 {
                self.draw_txn(context.grant())
            } else {
                SpaceTransaction::default()
            },
            vui::Then::Step,
        ))
    }
}

/// Composite a button shape multiblock and a button label to make the labeled button to be drawn.
///
/// Not public because it is only used by button widgets.
///
/// `base` must be the `ButtonBase` that produced `base_block`.
/// TODO: Find a non-redundant way to pass this information.
fn assemble_button(
    base: &dyn ButtonBase,
    base_multiblock: Block,
    label_block: Block,
    //grant: &vui::LayoutGrant,
) -> BoxStyle {
    let shifted_label = label_block.with_modifier(block::Move::new(
        Face6::PZ,
        (base.button_label_z() * 256 / theme::RESOLUTION_G) as u16,
        0,
    ));

    let unlabeled_box_style = BoxStyle::from_nine_and_thin(&base_multiblock);

    // TODO: We need to produce a transaction or VoxelBrush rather than a BoxStyle for this to work
    // properly. The label should be composed with the button at its final size and shape, not
    // duplicated into every block of the label.
    unlabeled_box_style.map_blocks(|base_block| {
        base_block.with_modifier(block::Composite::new(
            shifted_label.clone(),
            block::CompositeOperator::Over,
        ))
    })
}

/// Returns a [`DrawTarget`] for drawing the button label, with a
/// Y-down coordinate system whose origin is centered on the button (or more precisely,
/// (0, 0) is the lower-right pixel closest to the center, since e-g uses a convention
/// where coordinates identify pixels, not their edges).
///
/// TODO: explain expected size
///
/// [`DrawTarget`]: all_is_cubes::drawing::embedded_graphics::prelude::DrawTarget
pub(crate) fn draw_target_for_button_label<C: PixelColor>(
    space: &mut Space,
) -> DrawingPlane<'_, Space, C> {
    space.draw_target(
        Gridgid::from_translation([theme::RESOLUTION_G / 2, theme::RESOLUTION_G / 2 - 1, 0])
            * Gridgid::FLIP_Y,
    )
}

pub(crate) enum ButtonIcon<'a> {
    Icon(&'a DecodedPng),
    Text(&'a MonoFont<'a>, &'a str),
}

/// TODO: document, refine, and make public
pub(crate) fn make_button_label_block(
    universe: &mut Universe,
    name: &str,
    icon: ButtonIcon<'_>,
) -> Result<BlockBuilder<BlockBuilderVoxels>, InGenError> {
    let mut space = Space::builder(GridAab::from_lower_size(
        [0, 0, 0],
        [theme::RESOLUTION_G, theme::RESOLUTION_G, 1],
    ))
    .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
    .build();
    let mut draw_target = draw_target_for_button_label(&mut space);

    match icon {
        ButtonIcon::Icon(icon) => {
            let id = &PngAdapter::adapt(icon, default_srgb);
            EgImage::new(&id, -id.bounding_box().center() - Point::new(1, 1))
                .draw(&mut draw_target)?;
        }
        ButtonIcon::Text(font, text) => {
            Text::with_text_style(
                text,
                Point::new(-1, -1),
                MonoTextStyle::new(
                    font,
                    &VoxelBrush::single(Block::from(palette::BUTTON_LABEL)),
                ),
                TextStyleBuilder::new()
                    .baseline(Baseline::Middle)
                    .alignment(Alignment::Center)
                    .build(),
            )
            .draw(&mut draw_target)?;
        }
    }
    let space = universe.insert_anonymous(space);
    Ok(Block::builder()
        // .animation_hint(Replace)
        .display_name(name.to_owned())
        .voxels_ref(theme::RESOLUTION, space))
}

/// Common constants for button shapes.
/// TODO: public?
mod theme {
    use super::*;
    pub const RESOLUTION: Resolution = R32;
    pub const RESOLUTION_G: GridCoordinate = RESOLUTION.to_grid();
    /// Resolution to use for button base multiblock
    pub const MULTI_RESOLUTION: Resolution = R128 /* RESOLUTION * 4 */;
    pub const UNPRESSED_Z: GridCoordinate = 12;
    pub fn rim_lightening(color: Rgba) -> Rgba {
        color.map_rgb(|rgb| rgb * 1.1)
    }

    pub fn create_space(max_z: GridCoordinate) -> Space {
        let multi_resolution_g = MULTI_RESOLUTION.to_grid();
        Space::builder(GridAab::from_lower_size(
            [0, 0, 0],
            [multi_resolution_g, multi_resolution_g, max_z],
        ))
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build()
    }

    /// Build a [`Block`] for [`ButtonBase`].
    pub fn common_block(space: URef<Space>, name: &str) -> Block {
        Block::builder()
            .display_name(name.to_string())
            .voxels_ref(MULTI_RESOLUTION, space)
            .build()
    }
}

/// A shape for a button; defining the elements which communicate the type of the button
/// (single action, toggle, radio button, etc.) and the state of the button (pressed,
/// toggled on, disabled, etc.), but not the label (symbol indicating what it does or
/// controls).
///
/// These shapes are used in button widgets like [`ActionButton`] and [`ToggleButton`].
pub(crate) trait ButtonBase {
    /// Constructs the block shape for this kind of button in this state, without any label.
    ///
    /// The block is a multiblock shape suitable for [`BoxStyle::from_nine_and_thin()`].
    ///
    /// TODO: switch from `&mut Universe` to transactions
    ///
    /// TODO: more type-safe result while still cooperating with `linking`
    fn button_block(&self, universe: &mut Universe) -> Result<Block, InGenError>;

    /// Where within the [`Self::button_block()`] the label should be positioned.
    ///
    /// TODO: should be resolution-independent
    fn button_label_z(&self) -> GridCoordinate;
}

/// The label drawn onto this button should fit within a 24/32 × 24/32 circle.
impl ButtonBase for ButtonVisualState {
    fn button_label_z(&self) -> GridCoordinate {
        theme::UNPRESSED_Z + if self.pressed { -2 } else { 0 }
    }

    fn button_block(&self, universe: &mut Universe) -> Result<Block, InGenError> {
        let label_z = self.button_label_z();
        let back_block = palette::BUTTON_BACK; // TODO: different color theme for action than toggle?
        let frame_brush = VoxelBrush::single(Block::from(palette::BUTTON_FRAME));
        let back_brush = VoxelBrush::with_thickness(back_block, 0..label_z);
        let cap_rim_brush = VoxelBrush::new([(
            [0, 0, label_z - 1],
            Block::from(theme::rim_lightening(palette::BUTTON_BACK)),
        )]);

        let outer_inset = 2; // TODO duplicate number
        let rr = |bounding_box: GridAab, inset: i32| {
            RoundedRectangle::with_equal_corners(
                aab_xy_to_rectangle(bounding_box).offset(-(outer_inset + inset)),
                Size::new_equal((theme::RESOLUTION_G / 2 - outer_inset - inset) as u32),
            )
        };

        let mut space = theme::create_space(label_z);
        let draw_target = &mut space.draw_target(Gridgid::IDENTITY);

        for b in BoxStyle::nine_boxes(theme::RESOLUTION) {
            rr(b, 0)
                .into_styled(
                    PrimitiveStyleBuilder::new()
                        .fill_color(&back_brush)
                        .stroke_color(&frame_brush)
                        .stroke_width(2)
                        .stroke_alignment(StrokeAlignment::Inside)
                        .build(),
                )
                .draw(draw_target)?;
            rr(b, 2)
                .into_styled(
                    PrimitiveStyleBuilder::new()
                        .stroke_color(&cap_rim_brush)
                        .stroke_width(1)
                        .stroke_alignment(StrokeAlignment::Inside)
                        .build(),
                )
                .draw(draw_target)?;
        }

        Ok(theme::common_block(
            universe.insert_anonymous(space),
            "Action Button",
        ))
    }
}

/// The label drawn onto this button should fit within a 24/32 × 24/32 square.
impl ButtonBase for ToggleButtonVisualState {
    fn button_label_z(&self) -> GridCoordinate {
        self.common.button_label_z()
    }

    fn button_block(&self, universe: &mut Universe) -> Result<Block, InGenError> {
        let label_z = self.button_label_z();
        let active = self.value;
        let illuminate = move |builder: BlockBuilder<block::builder::BlockBuilderAtom>| {
            if active {
                builder.light_emission(palette::BUTTON_ACTIVATED_GLOW)
            } else {
                builder
            }
            .build()
        };
        let frame_brush = VoxelBrush::single(Block::from(palette::BUTTON_FRAME));
        let back_brush = VoxelBrush::with_thickness(
            illuminate(Block::builder().color(palette::BUTTON_ACTIVATED_BACK)),
            0..label_z,
        );
        let cap_rim_brush = VoxelBrush::new([(
            [0, 0, label_z - 1],
            illuminate(Block::builder().color(theme::rim_lightening(palette::BUTTON_BACK))),
        )]);

        let outer_inset = 2;
        let rr = |bounding_box: GridAab, inset: i32| {
            RoundedRectangle::with_equal_corners(
                aab_xy_to_rectangle(bounding_box).offset(-(outer_inset + inset)),
                Size::new(5 - inset as u32, 5 - inset as u32),
            )
        };

        let mut space = theme::create_space(label_z);
        let draw_target = &mut space.draw_target(Gridgid::IDENTITY);

        for b in BoxStyle::nine_boxes(theme::RESOLUTION) {
            rr(b, 0)
                .into_styled(
                    PrimitiveStyleBuilder::new()
                        .fill_color(&back_brush)
                        .stroke_color(&frame_brush)
                        .stroke_width(2)
                        .stroke_alignment(StrokeAlignment::Inside)
                        .build(),
                )
                .draw(draw_target)?;
            rr(b, 2)
                .into_styled(
                    PrimitiveStyleBuilder::new()
                        .stroke_color(&cap_rim_brush)
                        .stroke_width(1)
                        .stroke_alignment(StrokeAlignment::Inside)
                        .build(),
                )
                .draw(draw_target)?;
        }

        Ok(theme::common_block(
            universe.insert_anonymous(space),
            &format!("Toggle Button {self}"),
        ))
    }
}

fn aab_xy_to_rectangle(bounding_box: GridAab) -> Rectangle {
    Rectangle::with_corners(
        Point::new(bounding_box.lower_bounds().x, bounding_box.lower_bounds().y),
        Point::new(
            // - 1 because e-g rectangles are specified in terms of their outermost pixels
            bounding_box.upper_bounds().x - 1,
            bounding_box.upper_bounds().y - 1,
        ),
    )
}
