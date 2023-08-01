use crate::block::{self, Block, Evoxels, MinEval};
use crate::math::{GridArray, GridRotation, Rgb};
use crate::universe::{RefVisitor, VisitRefs};

mod composite;
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
}

impl Modifier {
    /// Compute the effect of this modifier.
    ///
    /// * `block` is the original block value (modifiers do not alter it).
    /// * `this_modifier_index` is the index in `block.modifiers()` of `self`.
    /// * `value` is the output of the preceding modifier or primitive, which is what the
    ///   current modifier should be applied to.
    /// * `depth` is the current block evaluation recursion depth (which is *not*
    ///   incremented by modifiers; TODO: define a computation limit strategy).
    pub(crate) fn evaluate(
        &self,
        block: &Block,
        this_modifier_index: usize,
        mut value: MinEval,
        depth: u8,
        filter: &block::EvalFilter,
    ) -> Result<MinEval, block::EvalBlockError> {
        Ok(match *self {
            Modifier::Quote(Quote { suppress_ambient }) => {
                value.attributes.tick_action = None;
                if suppress_ambient {
                    value.attributes.light_emission = Rgb::ZERO;
                }
                value
            }

            Modifier::Rotate(rotation) => {
                if matches!(value.voxels, Evoxels::One(_)) || filter.skip_eval {
                    // Skip computation of transforms
                    value
                } else {
                    // TODO: Add a shuffle-in-place rotation operation to GridArray and try implementing this using that, which should have less arithmetic involved than these matrix ops
                    let resolution = value.resolution();
                    let inner_to_outer = rotation.to_positive_octant_transform(resolution.into());
                    let outer_to_inner = rotation
                        .inverse()
                        .to_positive_octant_transform(resolution.into());

                    MinEval {
                        voxels: Evoxels::Many(
                            resolution,
                            GridArray::from_fn(
                                value.voxels.bounds().transform(inner_to_outer).unwrap(),
                                |cube| {
                                    value
                                        .voxels
                                        .get(outer_to_inner.transform_cube(cube))
                                        .unwrap()
                                },
                            ),
                        ),
                        attributes: value.attributes,
                    }
                }
            }

            Modifier::Composite(ref c) => c.evaluate(value, depth, filter)?,

            Modifier::Zoom(ref z) => z.evaluate(value)?,

            Modifier::Move(ref m) => {
                m.evaluate(block, this_modifier_index, value, depth, filter)?
            }
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
        }
    }
}

impl VisitRefs for Modifier {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        match self {
            Modifier::Quote(m) => m.visit_refs(visitor),
            Modifier::Rotate(_) => {}
            Modifier::Composite(m) => m.visit_refs(visitor),
            Modifier::Zoom(m) => m.visit_refs(visitor),
            Modifier::Move(m) => m.visit_refs(visitor),
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
    use crate::block::{BlockCollision, EvaluatedBlock, Evoxel, Primitive, Resolution::R2};
    use crate::content::make_some_voxel_blocks;
    use crate::math::{Face6, FaceMap, GridAab, GridPoint, OpacityCategory, Rgba};
    use crate::universe::Universe;
    use pretty_assertions::assert_eq;

    #[test]
    fn rotate_evaluation() {
        let resolution = R2;
        let block_bounds = GridAab::for_block(resolution);
        let rotation = GridRotation::RYXZ;
        let mut universe = Universe::new();
        let color_fn = |cube: GridPoint| {
            Rgba::new(
                cube.x as f32,
                cube.y as f32,
                cube.z as f32,
                if cube.y == 0 { 1.0 } else { 0.0 },
            )
        };
        let rotated_color_fn = |cube: GridPoint| {
            color_fn(
                rotation
                    .to_positive_octant_transform(resolution.into())
                    .transform_cube(cube),
            )
        };
        let block = Block::builder()
            .display_name("foo")
            .voxels_fn(&mut universe, resolution, |cube| {
                // Construct a lower half block with all voxels distinct
                Block::from(color_fn(cube))
            })
            .unwrap()
            .build();

        let oe = block.evaluate().unwrap();
        let rotated = block.clone().rotate(rotation);
        let re = rotated.evaluate().unwrap();

        // TODO: We don't quite yet have rotation for any attributes, but we *will*
        // (rotation_rule in particular might need to adapt).
        assert_eq!(
            re,
            EvaluatedBlock {
                attributes: oe.attributes,
                color: rgba_const!(0.5, 0.5, 0.5, 0.5),
                voxels: Evoxels::Many(
                    R2,
                    GridArray::from_fn(block_bounds, |cube| {
                        Evoxel {
                            color: rotated_color_fn(cube),
                            selectable: true,
                            collision: BlockCollision::Hard,
                        }
                    })
                ),
                opaque: FaceMap::repeat(false).with(rotation.transform(Face6::NY), true),
                visible: true,
                uniform_collision: Some(BlockCollision::Hard),
                voxel_opacity_mask: Some(GridArray::from_fn(block_bounds, |cube| {
                    if cube.x == 0 {
                        OpacityCategory::Opaque
                    } else {
                        OpacityCategory::Invisible
                    }
                })),
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

        assert_eq!(
            rotated_twice.evaluate().unwrap(),
            two_rotations.evaluate().unwrap()
        );
    }
}
