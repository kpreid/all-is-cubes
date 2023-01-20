use crate::block::{Block, BlockChange, EvalBlockError, EvaluatedBlock, Evoxels};
use crate::listen::Listener;
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
/// [`Modifier::attach()`] is provided to conveniently add a single modifier to a block;
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
    /// Return the given `block` with `self` added as the outermost/last modifier.
    pub fn attach(self, mut block: Block) -> Block {
        block.modifiers_mut().push(self);
        block
    }

    /// Compute the effect of this modifier.
    ///
    /// * `block` is the original block value (modifiers do not alter it).
    /// * `this_modifier_index` is the index in `block.modifiers()` of `self`.
    /// * `value` is the output of the preceding modifier or primitive, which is what the
    ///   current modifier should be applied to.
    /// * `depth` is the current block evaluation recursion depth (which is *not*
    ///   incremented by modifiers; TODO: define a computation limit strategy).
    ///
    /// TODO: Arrange some way to not end up re-computing the `voxel_opacity_mask` and other
    /// derived properties (i.e. we should have some kind of `IncompleteEvaluatedBlock` to pass
    /// through modifiers)
    pub(crate) fn evaluate(
        &self,
        block: &Block,
        this_modifier_index: usize,
        mut value: EvaluatedBlock,
        depth: u8,
    ) -> Result<EvaluatedBlock, EvalBlockError> {
        Ok(match *self {
            Modifier::Quote(Quote { suppress_ambient }) => {
                value.attributes.tick_action = None;
                if suppress_ambient {
                    value.attributes.light_emission = Rgb::ZERO;
                }
                value
            }

            Modifier::Rotate(rotation) => {
                if matches!(value.voxels, Evoxels::One(_)) && value.voxel_opacity_mask.is_none() {
                    // Skip computation of transforms
                    value
                } else {
                    // TODO: Add a shuffle-in-place rotation operation to GridArray and try implementing this using that, which should have less arithmetic involved than these matrix ops
                    let resolution = value.resolution();
                    let inner_to_outer = rotation.to_positive_octant_matrix(resolution.into());
                    let outer_to_inner = rotation
                        .inverse()
                        .to_positive_octant_matrix(resolution.into());

                    EvaluatedBlock {
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
                        voxel_opacity_mask: value.voxel_opacity_mask.map(|mask| {
                            GridArray::from_fn(
                                mask.bounds().transform(inner_to_outer).unwrap(),
                                |cube| mask[outer_to_inner.transform_cube(cube)],
                            )
                        }),

                        // Unaffected
                        attributes: value.attributes,
                        color: value.color,
                        opaque: value.opaque.rotate(rotation),
                        visible: value.visible,
                    }
                }
            }

            Modifier::Composite(ref c) => c.evaluate(value, depth)?,

            Modifier::Zoom(ref z) => z.evaluate(value)?,

            Modifier::Move(ref m) => m.evaluate(block, this_modifier_index, value, depth)?,
        })
    }

    /// Called by [`Block::listen()`]; not designed to be used otherwise.
    pub(crate) fn listen_impl(
        &self,
        listener: &(impl Listener<BlockChange> + Clone + Send + Sync + 'static),
        depth: u8,
    ) -> Result<(), EvalBlockError> {
        match self {
            Modifier::Quote { .. } => {}
            Modifier::Rotate(_) => {}
            Modifier::Composite(Composite {
                source,
                operator: _,
                reverse: _,
            }) => source.listen_impl(listener.clone(), super::next_depth(depth)?)?,
            Modifier::Zoom(_) => {}
            Modifier::Move { .. } => {}
        }
        Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::R2;
    use crate::block::{BlockAttributes, BlockCollision, Evoxel, Primitive};
    use crate::content::make_some_voxel_blocks;
    use crate::math::{Face6, FaceMap, GridAab, GridPoint, OpacityCategory, Rgba};
    use crate::universe::Universe;
    use pretty_assertions::assert_eq;

    // Unlike other tests, this one asserts the entire `EvaluatedBlock` value because
    // a new field is a potential bug.
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
                    .to_positive_octant_matrix(resolution.into())
                    .transform_cube(cube),
            )
        };
        let block = Block::builder()
            .voxels_fn(&mut universe, resolution, |cube| {
                // Construct a lower half block with all voxels distinct
                Block::from(color_fn(cube))
            })
            .unwrap()
            .build();
        let rotated = block.clone().rotate(rotation);
        assert_eq!(
            rotated.evaluate().unwrap(),
            EvaluatedBlock {
                attributes: BlockAttributes::default(),
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
