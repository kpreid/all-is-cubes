use crate::block::{self, Block, Evoxels, MinEval};
use crate::inv;
use crate::math::{GridRotation, Vol};
use crate::universe::{HandleVisitor, VisitHandles};

mod composite;
use alloc::vec::Vec;
pub use composite::*;
mod r#move;
pub use r#move::*;
mod quote;
pub use quote::*;
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
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Modifier {
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
    ///
    /// TODO(inventory): Define means for a block definition to specify the properties the inventory
    /// should have (its size, at least), and how it is rendered into the block.
    Inventory(inv::Inventory),
}

impl Modifier {
    /// Compute the effect of this modifier.
    ///
    /// * `block` is the original block value (modifiers do not alter it).
    /// * `this_modifier_index` is the index in `block.modifiers()` of `self`.
    /// * `value` is the output of the preceding modifier or primitive, which is what the
    ///   current modifier should be applied to.
    /// * `filter` controls evaluation options and listening, and its budget is decremented by
    ///   1 component (the modifier itself) plus as many voxels and additional components as the
    ///   particular modifier needs to calculate.
    pub(in crate::block) fn evaluate(
        &self,
        block: &Block,
        this_modifier_index: usize,
        value: MinEval,
        filter: &block::EvalFilter,
    ) -> Result<MinEval, block::InEvalError> {
        block::Budget::decrement_components(&filter.budget)?;

        Ok(match *self {
            Modifier::Quote(ref quote) => quote.evaluate(value, filter)?,

            Modifier::Rotate(rotation) => {
                if filter.skip_eval || value.rotationally_symmetric() {
                    // Skip computation of transforms
                    value
                } else {
                    block::Budget::decrement_voxels(&filter.budget, value.voxels.count())?;

                    // It'd be nice if this rotation operation were in-place, but I've read that
                    // it's actually quite difficult to implement a 3D array rotation in-place.
                    // (But another possible improvement would be to have a spare buffer to reuse
                    // across multiple evaluations/steps.)
                    // TODO: But check if we can make the arithmetic simpler by using incrementing
                    // instead of running a general transform on every Cube.

                    let resolution = value.resolution();
                    let inner_to_outer = rotation.to_positive_octant_transform(resolution.into());
                    let outer_to_inner = rotation
                        .inverse()
                        .to_positive_octant_transform(resolution.into());

                    MinEval {
                        voxels: Evoxels::Many(
                            resolution,
                            Vol::from_fn(
                                value.voxels.bounds().transform(inner_to_outer).unwrap(),
                                |cube| {
                                    value
                                        .voxels
                                        .get(outer_to_inner.transform_cube(cube))
                                        .unwrap()
                                },
                            ),
                        ),
                        attributes: value.attributes.rotate(rotation),
                    }
                }
            }

            Modifier::Composite(ref c) => c.evaluate(value, filter)?,

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
                // what role this unspecialize operation is playingh.
                if i.is_empty() {
                    ModifierUnspecialize::Pop
                } else {
                    ModifierUnspecialize::Keep
                }
            }
        }
    }
}

impl VisitHandles for Modifier {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        match self {
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
    use crate::block::{BlockAttributes, TickAction};
    use crate::block::{BlockCollision, EvaluatedBlock, Evoxel, Primitive, Resolution::R2};
    use crate::content::make_some_voxel_blocks;
    use crate::math::{Cube, Face6, FaceMap, GridAab, OpacityCategory, Rgb, Rgba};
    use crate::op::Operation;
    use crate::universe::Universe;
    use core::mem::size_of;
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
    fn rotate_evaluation() {
        let resolution = R2;
        let block_bounds = GridAab::for_block(resolution);
        let rotation = GridRotation::RYXZ;
        let mut universe = Universe::new();
        let [replacement] = make_some_voxel_blocks(&mut universe);
        let color_fn = |cube: Cube| {
            Rgba::new(
                cube.x as f32,
                cube.y as f32,
                cube.z as f32,
                if cube.y == 0 { 1.0 } else { 0.0 },
            )
        };
        let rotated_color_fn = |cube: Cube| {
            color_fn(
                rotation
                    .to_positive_octant_transform(resolution.into())
                    .transform_cube(cube),
            )
        };
        let block = Block::builder()
            .display_name("foo")
            .voxels_fn(resolution, |cube| {
                // Construct a lower half block with all voxels distinct
                Block::from(color_fn(cube))
            })
            .unwrap()
            .rotation_rule(block::RotationPlacementRule::Attach { by: Face6::PX })
            .tick_action(Some(TickAction::from(Operation::Become(
                replacement.clone(),
            ))))
            .build_into(&mut universe);
        let be = block.evaluate().unwrap();

        let rotated = block.clone().rotate(rotation);
        let re = rotated.evaluate().unwrap();

        assert_eq!(
            re,
            EvaluatedBlock {
                block: rotated.clone(),
                color: be.color,
                face_colors: be.face_colors.rotate(rotation),
                light_emission: Rgb::ZERO,
                voxels: Evoxels::Many(
                    R2,
                    Vol::from_fn(block_bounds, |cube| {
                        Evoxel {
                            color: rotated_color_fn(cube),
                            emission: Rgb::ZERO,
                            selectable: true,
                            collision: BlockCollision::Hard,
                        }
                    })
                ),
                opaque: FaceMap::repeat(false).with(rotation.transform(Face6::NY), true),
                visible: true,
                uniform_collision: Some(BlockCollision::Hard),
                voxel_opacity_mask: Some(Vol::from_fn(block_bounds, |cube| {
                    if cube.x == 0 {
                        OpacityCategory::Opaque
                    } else {
                        OpacityCategory::Invisible
                    }
                })),
                attributes: BlockAttributes {
                    display_name: "foo".into(),
                    tick_action: Some(TickAction::from(Operation::Become(
                        replacement.rotate(rotation).clone()
                    ))),
                    rotation_rule: block::RotationPlacementRule::Attach { by: Face6::PY },
                    ..BlockAttributes::default()
                },
                cost: block::Cost {
                    components: 2,
                    voxels: 2u32.pow(3) * 2, // original + rotation
                    recursion: 0
                }
            }
        );
    }

    /// Check that [`Block::rotate`]'s pre-composition is consistent with the interpretation
    /// used by evaluating [`Modifier::Rotate`].
    #[test]
    fn rotate_rotated_consistency() {
        let mut universe = Universe::new();
        let [block] = make_some_voxel_blocks(&mut universe);
        assert!(matches!(block.primitive(), Primitive::Recur { .. }));

        // Two rotations not in the same plane, so they are not commutative.
        let rotation_1 = GridRotation::RyXZ;
        let rotation_2 = GridRotation::RXyZ;

        let rotated_twice = block.clone().rotate(rotation_1).rotate(rotation_2);
        let mut two_rotations = block.clone();
        two_rotations
            .modifiers_mut()
            .extend([Modifier::Rotate(rotation_1), Modifier::Rotate(rotation_2)]);
        assert_ne!(rotated_twice, two_rotations, "Oops; test is ineffective");

        let ev_rotated_twice = rotated_twice.evaluate().unwrap();
        let ev_two_rotations = two_rotations.evaluate().unwrap();

        assert_eq!(
            EvaluatedBlock {
                block: rotated_twice,
                cost: ev_rotated_twice.cost,
                ..ev_two_rotations
            },
            ev_rotated_twice,
        );
    }

    /// `.rotate(IDENTITY)` does nothing.
    #[test]
    fn rotate_by_identity() {
        let universe = &mut Universe::new();
        let [block] = make_some_voxel_blocks(universe);
        assert_eq!(block, block.clone().rotate(GridRotation::IDENTITY));
        // prove that the test didn't trivially pass by applying to a symmetric block
        assert_ne!(block, block.clone().rotate(GridRotation::CLOCKWISE));
    }
}
