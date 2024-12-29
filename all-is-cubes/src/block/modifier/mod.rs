use alloc::sync::Arc;
use alloc::vec::Vec;

use crate::block::{self, Block, BlockAttributes, Evoxels, MinEval};
use crate::inv;
use crate::math::{GridRotation, Vol};
use crate::universe::{HandleVisitor, VisitHandles};

mod composite;
pub use composite::*;
mod r#move;
pub use r#move::*;
mod quote;
pub use quote::*;
#[cfg(test)]
mod rotate_tests;
mod zoom;
pub use zoom::*;

/// Modifiers can be applied to a [`Block`] to change the result of
/// [`evaluate()`](Block::evaluate)ing it, and thus create variations, such as rotations
/// or combinations of multiple blocks.
///
/// # Usage
///
/// Most modifiers have their own dedicated structs, such as [`Composite`]; these may
/// be converted to [`Modifier`] using their [`From`] implementations, or by constructing
/// the enum variant ([`Modifier::Composite`]) explicitly. Some modifiers have specific
/// functions for constructing their typical usages, such as [`Block::rotate()`].
///
/// [`Block::with_modifier()`] is provided to conveniently add a single modifier to a block;
/// [`Block::modifiers()`] and [`Block::modifiers_mut()`] provide general direct access.
/// Note that [`Block`] is a clone-on-write type for when modifiers are changed.
///
/// # Arranging modifiers
///
/// Operations which add or remove modifiers, such as [`Block::rotate()`],
/// follow some general principles and special cases:
///
/// * There should not be consecutive [`Rotate`] modifiers, but a single
///   one with the combined rotation. [`Block::rotate()`] maintains this property.
/// * It is preferable to have [`Rotate`] appear last, since rotation and
///   [unrotation](Block::unspecialize) is part of player interaction, and the identity
///   of block modifiers, not just their final result, determines whether blocks are
///   equal for purposes of inventory management.
///     * [`Composite::compose_or_replace()`] avoids applying [`Composite`] after
///       [`Rotate`], so that rotated versions of the same combination are represented
///       identically.
///
/// There is not yet any general “algebra” defining all cases where combinations of
/// modifiers should be canonicalized to other forms. Future versions of All is Cubes may
/// do so; that will be a breaking change (particularly since [`Block::modifiers_mut()`]
/// exists, so no rules are currently enforceable).
///
/// [`Rotate`]: Self::Rotate
#[derive(Clone, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Modifier {
    /// Sets or overrides the [attributes](BlockAttributes) of the block.
    //---
    // Design note: Indirection is used here to keep `Modifier` small.
    // `Arc` specifically is used so cloning does not allocate.
    Attributes(Arc<BlockAttributes>),

    /// Suppresses all behaviors of the [`Block`] that might affect the space around it,
    /// (or itself).
    Quote(Quote),

    /// Rotate the block about its cube center by the given rotation.
    ///
    /// This modifier should normally be used by means of [`Block::rotate()`].
    Rotate(GridRotation),

    /// Combine the voxels of multiple blocks using some per-voxel rule.
    Composite(Composite),

    /// Zoom in on a portion of the block; become part of a multi-block structure whose
    /// parts are parts of the original block.
    Zoom(Zoom),

    /// Displace the block out of the grid, cropping it.
    Move(Move),

    /// The block has an inventory (e.g. a chest, a dropped item, a machine).
    Inventory(inv::Inventory),
}

impl core::fmt::Debug for Modifier {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        // Print most modifiers’ data without the enum variant, because their struct names
        // are identifying enough.
        match self {
            Self::Attributes(a) => a.fmt(f),
            Self::Quote(q) => q.fmt(f),
            Self::Rotate(r) => write!(f, "Rotate({r:?})"),
            Self::Composite(c) => c.fmt(f),
            Self::Zoom(z) => z.fmt(f),
            Self::Move(m) => m.fmt(f),
            Self::Inventory(i) => i.fmt(f),
        }
    }
}

impl Modifier {
    /// Compute the effect of this modifier.
    ///
    /// * `block` is the original block value (modifiers do not alter it).
    /// * `this_modifier_index` is the index in `block.modifiers()` of `self`.
    ///   In cases where the modifier needs to produce another block value,
    ///   such as generating a [`Modifier::Become`], all modifiers following
    ///   `this_modifier_index` should be omitted.
    ///   TODO: Make it easier to comply.
    /// * `value` is the output of the preceding modifier or primitive, which is what the
    ///   current modifier should be applied to.
    /// * `filter` controls evaluation options and listening, and its budget is decremented by
    ///   1 component (the modifier itself) plus as many voxels and additional components as the
    ///   particular modifier needs to calculate.
    pub(in crate::block) fn evaluate(
        &self,
        block: &Block,
        this_modifier_index: usize,
        mut value: MinEval,
        filter: &block::EvalFilter,
    ) -> Result<MinEval, block::InEvalError> {
        block::Budget::decrement_components(&filter.budget)?;

        Ok(match *self {
            // TODO: Eventually, we want to be able to override individual attributes.
            // We will need a new schema (possibly a set of individual modifiers) for that.
            Modifier::Attributes(ref attributes) => {
                value.set_attributes(BlockAttributes::clone(attributes));
                value
            }

            Modifier::Quote(ref quote) => quote.evaluate(value, filter)?,

            Modifier::Rotate(rotation) => {
                if filter.skip_eval
                    || rotation == GridRotation::IDENTITY
                    || value.rotationally_symmetric()
                {
                    // Skip computation of transforms
                    value
                } else {
                    let (attributes, voxels) = value.into_parts();
                    block::Budget::decrement_voxels(&filter.budget, voxels.count())?;

                    // It'd be nice if this rotation operation were in-place, but I've read that
                    // it's actually quite difficult to implement a 3D array rotation in-place.
                    // (But another possible improvement would be to have a spare buffer to reuse
                    // across multiple evaluations/steps.)
                    // TODO: But check if we can make the arithmetic simpler by using incrementing
                    // instead of running a general transform on every Cube.

                    let resolution = voxels.resolution();
                    let inner_to_outer = rotation.to_positive_octant_transform(resolution.into());
                    let outer_to_inner = rotation
                        .inverse()
                        .to_positive_octant_transform(resolution.into());

                    MinEval::new(
                        attributes.rotate(rotation),
                        Evoxels::from_many(
                            resolution,
                            Vol::from_fn(
                                voxels.bounds().transform(inner_to_outer).unwrap(),
                                |cube| voxels.get(outer_to_inner.transform_cube(cube)).unwrap(),
                            ),
                        ),
                    )
                }
            }

            Modifier::Composite(ref c) => c.evaluate(block, this_modifier_index, value, filter)?,

            Modifier::Zoom(ref z) => z.evaluate(value, filter)?,

            Modifier::Move(ref m) => m.evaluate(block, this_modifier_index, value, filter)?,

            // Inventories are rendered by compositing their icon blocks in.
            Modifier::Inventory(ref i) => render_inventory(value, i, filter)?,
        })
    }

    /// Given a [`Block`] whose last modifier is `self`, returns the block that
    /// [`Block::unspecialize`] should produce instead of the modified block.
    pub(crate) fn unspecialize(&self, block: &Block) -> ModifierUnspecialize {
        // When modifying this match, update the public documentation of `Block::unspecialize` too.
        match self {
            Modifier::Attributes(_) => ModifierUnspecialize::Keep,

            Modifier::Quote(_) => ModifierUnspecialize::Keep,

            Modifier::Rotate(_) => ModifierUnspecialize::Pop,

            Modifier::Composite(c) => c.unspecialize(block),

            // TODO: Implement removal of multiblock structures.
            // This will require awareness of neighboring blocks (so that the whole set
            // becomes one block) and probably a total replacement of the unspecialize() design.
            Modifier::Zoom(_) => ModifierUnspecialize::Keep,

            // TODO: Implement deletion of moving blocks.
            // This is essentially a 2-block multiblock situation.
            Modifier::Move(_) => ModifierUnspecialize::Keep,

            Modifier::Inventory(i) => {
                // TODO(inventory): Should be possible for the contents of the inventory to be
                // split off, depending on the block definition's desires and possibly on exactly
                // what role this unspecialize operation is playing.
                if i.is_empty() {
                    ModifierUnspecialize::Pop
                } else {
                    ModifierUnspecialize::Keep
                }
            }
        }
    }

    /// This is like `rotationally_symmetric()` on many other data types, except that it doesn't
    /// describe whether it _affects_ rotations — in particular, it does not mean “this modifier
    /// commutes with `Rotate` modifiers”, but rather “will it introduce asymmetry to a block that
    /// had none”.
    pub(crate) fn does_not_introduce_asymmetry(&self) -> bool {
        match self {
            Modifier::Attributes(attr) => attr.rotationally_symmetric(),
            // Quote has no asymmetry
            Modifier::Quote(_) => true,
            // Rotate may change existing asymmetry but does not introduce it
            Modifier::Rotate(_) => true,
            Modifier::Composite(c) => c.rotationally_symmetric(),
            // Technically, Zoom on a resolution-1 block is symmetric but that case is
            // hard to calculate without knowing something about the primitive.
            Modifier::Zoom(_) => false,

            Modifier::Move(_) => false,
            // This requires knowing the `InvInBlock` to answer true, and hence requires evaluation.
            // So don't try.
            Modifier::Inventory(_) => false,
        }
    }

    /// Rotates this modifier, so that whatever asymmetric effects it has, it has them in
    /// different directions.
    ///
    /// If [`Self::does_not_introduce_asymmetry()`] returns `true`, then this has no
    /// effect.
    pub(crate) fn rotate(self, rotation: GridRotation) -> Self {
        match self {
            Modifier::Attributes(a) => {
                Modifier::Attributes(Arc::new((*a).clone().rotate(rotation)))
            }
            Modifier::Rotate(r) => Modifier::Rotate(rotation * r * rotation.inverse()), // TODO: no test this is correct yet
            Modifier::Composite(c) => Modifier::Composite(c.rotate(rotation)),
            Modifier::Zoom(z) => Modifier::Zoom(z.rotate(rotation)),
            Modifier::Move(m) => Modifier::Move(m.rotate(rotation)),
            m @ (Modifier::Inventory(_) | Modifier::Quote(_)) => m,
        }
    }
}

impl VisitHandles for Modifier {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        match self {
            Modifier::Attributes(a) => a.visit_handles(visitor),
            Modifier::Quote(m) => m.visit_handles(visitor),
            Modifier::Rotate(_) => {}
            Modifier::Composite(m) => m.visit_handles(visitor),
            Modifier::Zoom(m) => m.visit_handles(visitor),
            Modifier::Move(m) => m.visit_handles(visitor),
            Modifier::Inventory(i) => i.visit_handles(visitor),
        }
    }
}

/// Result of [`Modifier::unspecialize()`] returned to [`Block::unspecialize()`].
#[derive(Debug)]
pub(crate) enum ModifierUnspecialize {
    /// Produce the block unchanged.
    Keep,
    /// Pop the modifier.
    Pop,
    /// Replace with a different set of blocks.
    /// `unspecialize()` will be called on each of those automatically.
    Replace(Vec<Block>),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::BlockAttributes;
    use pretty_assertions::assert_eq;

    /// Track the size of the `Modifier` enum to make sure we don't accidentally make it bigger
    /// by giving one variant more data.
    #[test]
    fn size_of_modifier() {
        // The largest modifier, currently, is `Composite`, which contains a `Block` plus
        // additional data, and a `Block` is a pointer plus additional data;
        // in each case the additional data does not exceed 4 bytes, so on both 32 and
        // 64-bit systems, the size will be rounded up to three pointers
        // (unless the alignment of pointers is less than their size).
        assert_eq!(size_of::<Modifier>(), 3 * size_of::<*const ()>());
    }

    #[test]
    fn modifier_debug() {
        let modifiers: Vec<Modifier> = vec![
            BlockAttributes {
                display_name: arcstr::literal!("hello"),
                ..Default::default()
            }
            .into(),
            Modifier::Quote(Quote::new()),
            Modifier::Rotate(GridRotation::RXyZ),
            Modifier::Composite(Composite::new(block::AIR, CompositeOperator::Over)),
            Modifier::Inventory(inv::Inventory::from_slots([])),
        ];
        assert_eq!(
            format!("{modifiers:#?}"),
            indoc::indoc! {
                r#"[
                    BlockAttributes {
                        display_name: "hello",
                    },
                    Quote {
                        suppress_ambient: false,
                    },
                    Rotate(RXyZ),
                    Composite {
                        source: Block {
                            primitive: Air,
                        },
                        operator: Over,
                        reverse: false,
                        disassemblable: false,
                    },
                    Inventory {
                        slots: [],
                    },
                ]"#
            }
        );
    }
}
