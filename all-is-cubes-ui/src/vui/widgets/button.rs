use alloc::borrow::ToOwned as _;
use alloc::boxed::Box;
use alloc::format;
use alloc::sync::Arc;
use core::fmt;
use core::hash::Hash;
use core::sync::atomic::{AtomicU8, Ordering::Relaxed};

use exhaust::Exhaust;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::behavior::BehaviorSetTransaction;
use all_is_cubes::block::AIR;
use all_is_cubes::block::{
    self, Block, Builder,
    Resolution::{self, *},
};
use all_is_cubes::content::load_image::{
    DecodedPng, default_srgb, include_image, space_from_image,
};
use all_is_cubes::content::palette;
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::euclid::{Vector2D, vec3};
use all_is_cubes::inv::EphemeralOpaque;
use all_is_cubes::linking::{self, InGenError};
use all_is_cubes::listen;
use all_is_cubes::math::{Cube, Face6, GridCoordinate, GridRotation, GridSize, GridVector, Rgba};
use all_is_cubes::space::{self, Space, SpaceBehaviorAttachment, SpaceTransaction};
use all_is_cubes::transaction::Merge;
use all_is_cubes::universe::{Handle, UniverseTransaction};

use crate::vui::widgets::{BoxStyle, WidgetBlocks, WidgetTheme};
use crate::vui::{self, Layoutable as _};

type Action = EphemeralOpaque<dyn Fn() + Send + Sync>;

/// What is displayed on the face of a button widget.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct ButtonLabel {
    /// Picture stuck to the front face of the button. It should be flat against the -Z face and
    /// only a few voxels thick.
    /// Specific button types have more specific requirements for margin.
    pub icon: Option<Block>,

    /// Text to display.
    pub text: Option<vui::widgets::Label>,
}

impl ButtonLabel {
    /// Return an iterator of the blocks in the label, increasing along the X axis.
    /// Always has as many elements as `requirements().minimum`.
    fn blocks(&self, mut gravity: vui::Gravity) -> impl Iterator<Item = Block> + '_ {
        // TODO: need a better plan for how gravity interacts with icons;
        // this is a kludge to get okay layout of the text for now, by left-aligning the
        // text to meet the icon.
        if self.icon.is_some() {
            gravity.x = vui::Align::Low;
        }

        self.icon
            .clone()
            .into_iter()
            .chain(
                self.text
                    .as_ref()
                    .into_iter()
                    .flat_map(move |label_widget| {
                        let text = label_widget.text(gravity);
                        let bb = text.bounding_blocks();
                        bb.x_range().map(move |x| {
                            Block::from_primitive(block::Primitive::Text {
                                text: text.clone(),
                                offset: GridVector::new(
                                    x,
                                    bb.lower_bounds().y,
                                    bb.lower_bounds().z,
                                ),
                            })
                        })
                    }),
            )
    }
}

impl From<Block> for ButtonLabel {
    fn from(icon: Block) -> Self {
        ButtonLabel {
            icon: Some(icon),
            text: None,
        }
    }
}
impl From<vui::widgets::Label> for ButtonLabel {
    fn from(text: vui::widgets::Label) -> Self {
        ButtonLabel {
            icon: None,
            text: Some(text),
        }
    }
}
impl From<ArcStr> for ButtonLabel {
    fn from(string: ArcStr) -> Self {
        ButtonLabel {
            icon: None,
            text: Some(vui::widgets::Label::new(string)),
        }
    }
}

impl vui::Layoutable for ButtonLabel {
    fn requirements(&self) -> vui::LayoutRequest {
        let Self { icon, text } = self;
        let text_size = text
            .as_ref()
            .map_or(GridSize::zero(), |text| text.requirements().minimum);
        let icon_size = if icon.is_some() {
            GridSize::new(1, 1, 1)
        } else {
            GridSize::zero()
        };
        vui::LayoutRequest {
            minimum: GridSize::new(
                // TODO: consider using LayoutTree to execute the layout
                text_size.width + icon_size.width,
                text_size.height.max(icon_size.height),
                text_size.depth.max(icon_size.depth),
            ),
        }
    }
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

    /// Label to be put on top of the shape.
    label: ButtonLabel,
}

impl<St: ButtonBase + Clone + Eq + Hash + Exhaust + fmt::Debug> ButtonCommon<St> {
    fn new(shape: &linking::Provider<St, Block>, label: ButtonLabel) -> Self {
        let shape = shape.map(|_, base_multiblock| BoxStyle::from_nine_and_thin(base_multiblock));
        Self { shape, label }
    }

    /// For a specific layout grant, generate the transaction which draws the button in a specific
    /// state.
    fn create_draw_txn(&self, grant: &vui::LayoutGrant, state: &St) -> vui::WidgetTransaction {
        let grant = self.shrink_bounds(*grant);

        // Create transaction for the button shape of the required size *without label*.
        // TODO: add Provider index impl to avoid this clone
        let mut shape_txn = self.shape[state.clone()].create_box(grant.bounds);

        // Composite label and shape
        for (x, label_block) in (0..).zip(self.label.blocks(grant.gravity)) {
            // TODO: centered in case the button is larger
            let cube = Cube::from(grant.bounds.lower_bounds() + vec3(x, 0, 0));

            if let Some(result_block) = shape_txn.at(cube).new_mut() {
                let shifted_label = shift_label_block(state, label_block);
                *result_block = result_block.clone().with_modifier(block::Composite::new(
                    shifted_label,
                    block::CompositeOperator::Over,
                ))
            }
        }

        shape_txn
    }

    fn create_draw_txns(
        &self,
        grant: &vui::LayoutGrant,
    ) -> linking::Provider<St, vui::WidgetTransaction> {
        self.shape
            .clone()
            .map(|state, _| self.create_draw_txn(grant, state))
    }

    fn shrink_bounds(&self, grant: vui::LayoutGrant) -> vui::LayoutGrant {
        grant.shrink_to(self.requirements().minimum, true)
    }
}

impl<St> vui::Layoutable for ButtonCommon<St> {
    fn requirements(&self) -> vui::LayoutRequest {
        let mut req = self.label.requirements();
        req.minimum.depth = req.minimum.depth.max(1);
        req
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
    #[allow(missing_docs)] // TODO
    pub fn new(
        label: impl Into<ButtonLabel>,
        theme: &WidgetTheme,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            common: ButtonCommon::new(
                &theme.widget_blocks.subset(WidgetBlocks::ActionButton),
                label.into(),
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
    fn controller(self: Arc<Self>, grant: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ActionButtonController {
            txns: self.common.create_draw_txns(grant),
            definition: self,
        })
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
    txns: linking::Provider<ButtonVisualState, vui::WidgetTransaction>,
}

impl vui::WidgetController for ActionButtonController {
    fn initialize(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let grant = self.definition.common.shrink_bounds(*context.grant());

        // TODO: we never draw the pressed state
        let draw = self.txns[ButtonVisualState { pressed: false }].clone();
        let activatable = SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            SpaceBehaviorAttachment::new(grant.bounds),
            Arc::new(space::ActivatableRegion {
                effect: self.definition.action.clone(),
            }),
        ));
        draw.merge(activatable)
            .map_err(|error| vui::InstallVuiError::Conflict { error })
    }
}

/// A single-block button that displays a boolean state derived from a
/// [`listen::DynSource`] and can be clicked.
#[derive(Clone)]
pub struct ToggleButton<D> {
    common: ButtonCommon<ToggleButtonVisualState>,
    data_source: listen::DynSource<D>,
    projection: Arc<dyn Fn(&D) -> bool + Send + Sync>,
    action: Action,
}

impl<D: Clone + Sync + fmt::Debug> fmt::Debug for ToggleButton<D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            common,
            data_source,
            projection,
            action,
        } = self;
        f.debug_struct("ToggleButton")
            .field("common", common)
            .field("data_source", data_source)
            .field("projection(data_source)", &projection(&data_source.get()))
            .field("action", action)
            .finish()
    }
}

impl<D> ToggleButton<D> {
    #[allow(missing_docs)]
    pub fn new(
        data_source: listen::DynSource<D>,
        projection: impl Fn(&D) -> bool + Send + Sync + 'static,
        label: impl Into<ButtonLabel>,
        theme: &WidgetTheme,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            common: ButtonCommon::new(
                &theme.widget_blocks.subset(WidgetBlocks::ToggleButton),
                label.into(),
            ),
            data_source,
            projection: Arc::new(projection),
            action: EphemeralOpaque::new(Arc::new(action)),
        })
    }
}

impl<D> vui::Layoutable for ToggleButton<D> {
    fn requirements(&self) -> vui::LayoutRequest {
        self.common.requirements()
    }
}

// TODO: Mess of generic bounds due to the combination of Widget and listen::DynSource
// requirements -- should we make a trait alias for these?
impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::Widget for ToggleButton<D> {
    fn controller(self: Arc<Self>, grant: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ToggleButtonController {
            todo: listen::Flag::listening(true, &self.data_source),
            txns: self.common.create_draw_txns(grant),
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
    txns: linking::Provider<ToggleButtonVisualState, vui::WidgetTransaction>,
    todo: listen::Flag,
    recently_pressed: Arc<AtomicU8>,
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> ToggleButtonController<D> {
    fn draw_txn(&self) -> vui::WidgetTransaction {
        let value = (self.definition.projection)(&self.definition.data_source.get());
        self.txns[ToggleButtonVisualState {
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
        }]
        .clone()
    }
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::WidgetController
    for ToggleButtonController<D>
{
    fn initialize(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let grant = self.definition.common.shrink_bounds(*context.grant());

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
        self.draw_txn()
            .merge(activatable)
            .map_err(|error| vui::InstallVuiError::Conflict { error })
    }

    fn step(&mut self, _: &vui::WidgetContext<'_>) -> Result<vui::StepSuccess, vui::StepError> {
        Ok((
            if self.todo.get_and_clear() || self.recently_pressed.load(Relaxed) > 0 {
                self.draw_txn()
            } else {
                SpaceTransaction::default()
            },
            // TODO: use waking
            vui::Then::Step,
        ))
    }
}

pub(crate) enum ButtonIcon<'a> {
    Icon(&'a DecodedPng),
}

/// TODO: document, refine, and make public
#[expect(clippy::needless_pass_by_value, reason = "convenience")]
pub(crate) fn make_button_label_block(
    txn: &mut UniverseTransaction,
    name: &str,
    icon: ButtonIcon<'_>,
) -> Result<Block, InGenError> {
    Ok(match icon {
        ButtonIcon::Icon(icon) => {
            let size = icon.size();
            let centering = ((Vector2D::splat(theme::RESOLUTION_G) - size.to_vector().to_i32())
                / 2)
            .extend(0)
            .cast_unit::<Cube>();

            let space = space_from_image(icon, GridRotation::RXyZ, &|color| {
                default_srgb(color).translate(centering)
            })?;

            // TODO: Implement the same bounds-shrinking feature as `Block::voxels_fn()` has.
            Block::builder()
                .voxels_handle(theme::RESOLUTION, txn.insert_anonymous(space))
                .display_name(name.to_owned())
                .build()
        }
    })
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

    /// Build a [`Block`] for [`ButtonBase`].
    pub fn common_block(space: Handle<Space>, name: &str) -> Block {
        Block::builder()
            .display_name(name)
            .voxels_handle(MULTI_RESOLUTION, space)
            .animation_hint(block::AnimationHint::replacement(
                block::AnimationChange::Shape,
            ))
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
    /// `txn` will be extended to insert the block's dependencies.
    /// It must be committed to the relevant universe afterward.
    ///
    /// TODO: more type-safe result while still cooperating with `linking`
    fn button_block(&self, txn: &mut UniverseTransaction) -> Result<Block, InGenError>;

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

    fn button_block(&self, txn: &mut UniverseTransaction) -> Result<Block, InGenError> {
        Ok(theme::common_block(
            txn.insert_anonymous(draw_button_multiblock_from_image(
                self,
                false,
                include_image!("theme/button-shape-action.png"),
            )?),
            "Action Button",
        ))
    }
}

/// The label drawn onto this button should fit within a 24/32 × 24/32 square.
impl ButtonBase for ToggleButtonVisualState {
    fn button_label_z(&self) -> GridCoordinate {
        self.common.button_label_z()
    }

    fn button_block(&self, txn: &mut UniverseTransaction) -> Result<Block, InGenError> {
        Ok(theme::common_block(
            txn.insert_anonymous(draw_button_multiblock_from_image(
                self,
                self.value,
                include_image!("theme/button-shape-toggle.png"),
            )?),
            &format!("Toggle Button {self}"),
        ))
    }
}

/// Colors expected in the image passed to [`draw_button_multiblock_from_image`].
/// These simple colors are chosen to be clearly distinct and will be replaced.
mod image_palette {
    pub(super) const OUTSIDE: [u8; 4] = [0, 0, 0, 0];
    pub(super) const FRAME: [u8; 4] = [0, 0, 0, 255];
    pub(super) const RIM: [u8; 4] = [255, 255, 255, 255];
    pub(super) const BACK: [u8; 4] = [0, 255, 255, 255];
}

/// Convert an image into a 3D button block for implementing [`ButtonBase::button_block()`].
///
/// The image must contain only colors in [`image_palette`],
/// be a multiblock arrangement for [`BoxStyle::from_nine_and_thin()`],
/// and have block resolution [`theme::RESOLUTION`].
fn draw_button_multiblock_from_image(
    state: &impl ButtonBase,
    active: bool,
    image: &DecodedPng,
) -> Result<Space, InGenError> {
    let label_z = state.button_label_z();
    let illuminate = move |builder: Builder<block::builder::Atom, ()>| {
        if active {
            builder.light_emission(palette::BUTTON_ACTIVATED_GLOW)
        } else {
            builder
        }
        .build()
    };
    // TODO: different color theme for action than toggle?
    let back_color = if active {
        palette::BUTTON_ACTIVATED_BACK
    } else {
        palette::BUTTON_BACK
    };
    let back_color_block = illuminate(Block::builder().color(back_color));
    let rim_color_block = illuminate(Block::builder().color(theme::rim_lightening(back_color)));
    let space = space_from_image(image, GridRotation::RXyZ, &|color| match color {
        image_palette::OUTSIDE => VoxelBrush::single(AIR),
        image_palette::FRAME => VoxelBrush::single(block::from_color!(palette::BUTTON_FRAME)),
        image_palette::RIM => VoxelBrush::new((0..label_z).map(|z| {
            // Highlight just the corner with a lighter color;
            // otherwise identical to BACK.
            (
                [0, 0, z],
                if z == label_z - 1 {
                    &rim_color_block
                } else {
                    &back_color_block
                },
            )
        })),
        image_palette::BACK => VoxelBrush::with_thickness(back_color_block.clone(), 0..label_z),
        _ => panic!("bad color in button image: {color:?}"),
    })?;
    Ok(space)
}

// Move a block that's part of a button label so that its z=0 aligns with the button's face.
fn shift_label_block(state: &impl ButtonBase, block: Block) -> Block {
    block.with_modifier(block::Move::new(
        Face6::PZ,
        (state.button_label_z() * 256 / theme::RESOLUTION_G) as u16,
        0,
    ))
}
