//! Drawing and laying out button widgets and their component blocks.

use alloc::borrow::ToOwned as _;
use alloc::format;
use core::fmt;
use core::hash::Hash;
use core::iter;

use exhaust::Exhaust;

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
use all_is_cubes::linking::{self, InGenError};
use all_is_cubes::math::{Cube, Face6, GridCoordinate, GridRotation, GridSize, GridVector, Rgba};
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, ReadTicket, UniverseTransaction};

use crate::vui::{self, Layoutable as _};

use super::{ButtonCommon, ButtonLabel};

// -------------------------------------------------------------------------------------------------

impl<St> vui::Layoutable for ButtonCommon<St> {
    fn requirements(&self) -> vui::LayoutRequest {
        let mut req = self.label.requirements();
        req.minimum.depth = req.minimum.depth.max(1);
        req
    }
}

impl<St: ButtonBase + Clone + Eq + Hash + Exhaust + fmt::Debug> ButtonCommon<St> {
    /// For a specific layout grant, generate the transaction which draws the button in a specific
    /// state.
    fn create_draw_txn(&self, grant: &vui::LayoutGrant, state: &St) -> vui::WidgetTransaction {
        let grant = self.shrink_bounds(*grant);

        // Create transaction for the button shape of the required size *without label*.
        // TODO: add Provider index impl to avoid this clone
        let mut shape_txn = self.shape[state.clone()].create_box(grant.bounds);

        // Composite label and shape
        for (x, label_block) in iter::zip(0.., self.label.blocks(grant.gravity)) {
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

    pub(in crate::vui::widgets::button) fn create_draw_txns(
        &self,
        grant: &vui::LayoutGrant,
    ) -> linking::Provider<St, vui::WidgetTransaction> {
        self.shape.clone().map(|state, _| self.create_draw_txn(grant, state))
    }

    pub(in crate::vui::widgets::button) fn shrink_bounds(
        &self,
        grant: vui::LayoutGrant,
    ) -> vui::LayoutGrant {
        grant.shrink_to(self.requirements().minimum, true)
    }
}

impl vui::Layoutable for ButtonLabel {
    fn requirements(&self) -> vui::LayoutRequest {
        let Self { icon, text } = self;
        let text_size = text.as_ref().map_or(GridSize::zero(), |text| text.requirements().minimum);
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

impl ButtonLabel {
    /// Return an iterator of the blocks in the label, increasing along the X axis.
    /// Always has as many elements as `requirements().minimum`.
    pub(crate) fn blocks(&self, mut gravity: vui::Gravity) -> impl Iterator<Item = Block> + '_ {
        // TODO: need a better plan for how gravity interacts with icons;
        // this is a kludge to get okay layout of the text for now, by left-aligning the
        // text to meet the icon.
        if self.icon.is_some() {
            gravity.x = vui::Align::Low;
        }

        self.icon.clone().into_iter().chain(self.text.as_ref().into_iter().flat_map(
            move |label_widget| {
                let text = label_widget.text(gravity);
                let bb = text.bounding_blocks();
                bb.x_range().into_iter().map(move |x| {
                    Block::from_primitive(block::Primitive::Text {
                        text: text.clone(),
                        offset: GridVector::new(x, bb.lower_bounds().y, bb.lower_bounds().z),
                    })
                })
            },
        ))
    }
}

pub(crate) enum ButtonIcon<'a> {
    Icon(&'a DecodedPng),
}

/// TODO: document, refine, and make public
/// so that callers can build their own button themes
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

            let space = space_from_image(ReadTicket::stub(), icon, GridRotation::RXyZ, &|color| {
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

// -------------------------------------------------------------------------------------------------

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
impl ButtonBase for super::ButtonVisualState {
    fn button_label_z(&self) -> GridCoordinate {
        theme::UNPRESSED_Z + if self.pressed { -4 } else { 0 }
    }

    fn button_block(&self, txn: &mut UniverseTransaction) -> Result<Block, InGenError> {
        Ok(theme::common_block(
            txn.insert_anonymous(draw_button_multiblock_from_image(
                self,
                self.pressed,
                include_image!("../theme/button-shape-action.png"),
            )?),
            "Action Button",
        ))
    }
}

/// The label drawn onto this button should fit within a 24/32 × 24/32 square.
impl ButtonBase for super::ToggleButtonVisualState {
    fn button_label_z(&self) -> GridCoordinate {
        self.common.button_label_z()
    }

    fn button_block(&self, txn: &mut UniverseTransaction) -> Result<Block, InGenError> {
        Ok(theme::common_block(
            txn.insert_anonymous(draw_button_multiblock_from_image(
                self,
                self.value,
                include_image!("../theme/button-shape-toggle.png"),
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
    let illuminate = move |builder: Builder<'static, block::builder::Atom, ()>| {
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
    let space = space_from_image(
        ReadTicket::stub(),
        image,
        GridRotation::RXyZ,
        &|color| match color {
            image_palette::OUTSIDE => VoxelBrush::single(AIR),
            image_palette::FRAME => VoxelBrush::single(block::from_color!(palette::BUTTON_FRAME)),
            image_palette::RIM => VoxelBrush::new((0..label_z).into_iter().map(|z| {
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
        },
    )?;
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
