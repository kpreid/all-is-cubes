use std::mem;

use cgmath::{EuclideanSpace as _, Point3, Zero as _};
use ordered_float::NotNan;

use crate::block::{
    next_depth, Block, BlockAttributes, BlockChange, BlockCollision, EvalBlockError,
    EvaluatedBlock, Evoxel,
    Resolution::{self, R1, R16},
    AIR,
};
use crate::drawing::VoxelBrush;
use crate::listen::Listener;
use crate::math::{Face6, GridAab, GridArray, GridCoordinate, GridPoint, GridRotation, Rgb, Rgba};
use crate::universe::{RefVisitor, VisitRefs};

/// Modifiers can be applied to a [`Block`] to change the result of
/// [`evaluate()`](Block::evaluate)ing it.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Modifier {
    /// Suppresses all behaviors of the [`Block`] that might affect the space around it,
    /// (or itself).
    // TODO: figure out how to publicly construct this given that we want to still have
    // the option to add variants
    #[non_exhaustive]
    Quote {
        /// If true, also suppress light and sound effects.
        ambient: bool,
    },

    /// Rotate the block about its cube center by the given rotation.
    Rotate(GridRotation),

    /// Combine the voxels of multiple blocks using some per-voxel rule.
    Composite(Composite),

    /// Zoom in on a portion of the block; become part of a multi-block structure whose
    /// parts are parts of the original block.
    Zoom(Zoom),

    /// Displace the block out of the grid, cropping it. A pair of `Move`s can depict a
    /// block moving between two cubes.
    ///
    /// **Animation:**
    ///
    /// * If the `distance` is zero then the modifier will remove itself, if possible,
    ///   on the next tick.
    /// * If the `distance` and `velocity` are such that the block is out of view and will
    ///   never strt being in view, the block will be replaced with [`AIR`].
    ///
    /// (TODO: Define the conditions for “if possible”.)
    // #[non_exhaustive] // TODO: needs a constructor
    Move {
        /// The direction in which the block is displaced.
        direction: Face6,
        /// The distance, in 1/256ths, by which it is displaced.
        distance: u16,
        /// The velocity **per tick** with which the displacement is changing.
        ///
        /// TODO: "Per tick" is a bad unit.
        velocity: i16,
    },
}

impl Modifier {
    /// Return the given `block` with `self` added as the outermost modifier.
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
            Modifier::Quote { ambient } => {
                value.attributes.tick_action = None;
                if ambient {
                    value.attributes.light_emission = Rgb::ZERO;
                }
                value
            }

            Modifier::Rotate(rotation) => {
                if value.voxels.is_none() && value.voxel_opacity_mask.is_none() {
                    // Skip computation of transforms
                    value
                } else {
                    // TODO: Add a shuffle-in-place rotation operation to GridArray and try implementing this using that, which should have less arithmetic involved than these matrix ops
                    let resolution = value.resolution;
                    let inner_to_outer = rotation.to_positive_octant_matrix(resolution.into());
                    let outer_to_inner = rotation
                        .inverse()
                        .to_positive_octant_matrix(resolution.into());

                    EvaluatedBlock {
                        voxels: value.voxels.map(|voxels| {
                            GridArray::from_fn(
                                voxels.bounds().transform(inner_to_outer).unwrap(),
                                |cube| voxels[outer_to_inner.transform_cube(cube)],
                            )
                        }),
                        voxel_opacity_mask: value.voxel_opacity_mask.map(|mask| {
                            GridArray::from_fn(
                                mask.bounds().transform(inner_to_outer).unwrap(),
                                |cube| mask[outer_to_inner.transform_cube(cube)],
                            )
                        }),

                        // Unaffected
                        attributes: value.attributes,
                        color: value.color,
                        resolution,
                        opaque: value.opaque,
                        visible: value.visible,
                    }
                }
            }

            Modifier::Composite(Composite {
                ref source,
                operator,
                reverse,
            }) => {
                // The destination block is already evaluated (it is the input to this
                // modifier), but we need to evaluate the source block.
                let mut src_evaluated = source.evaluate_impl(next_depth(depth)?)?;
                // Apply the reverse option by swapping everything.
                if reverse {
                    mem::swap(&mut src_evaluated, &mut value);
                }
                // Unpack blocks.
                let EvaluatedBlock {
                    attributes,
                    color: dst_color,
                    voxels: dst_voxels,
                    resolution: dst_resolution,
                    opaque: _,
                    visible: _,
                    voxel_opacity_mask: _,
                } = value;
                let EvaluatedBlock {
                    attributes: _, // TODO: merge attributes
                    color: src_color,
                    voxels: src_voxels,
                    resolution: src_resolution,
                    opaque: _,
                    visible: _,
                    voxel_opacity_mask: _,
                } = src_evaluated;

                let effective_resolution = src_resolution.max(dst_resolution);
                let src_scale = GridCoordinate::from(effective_resolution)
                    / GridCoordinate::from(src_resolution);
                let dst_scale = GridCoordinate::from(effective_resolution)
                    / GridCoordinate::from(dst_resolution);

                if effective_resolution == R1 {
                    EvaluatedBlock::from_color(
                        attributes,
                        operator.blend_color(src_color, dst_color),
                    )
                } else {
                    // TODO: avoid needing to allocate here. Define a GridArrayView type?
                    let src_voxels = src_voxels.unwrap_or_else(|| {
                        GridArray::from_fn(GridAab::for_block(R1), |_| {
                            Evoxel::from_color(src_color)
                        })
                    });
                    let dst_voxels = dst_voxels.unwrap_or_else(|| {
                        GridArray::from_fn(GridAab::for_block(R1), |_| {
                            Evoxel::from_color(src_color)
                        })
                    });
                    EvaluatedBlock::from_voxels(
                        // TODO: merge attributes
                        attributes,
                        effective_resolution,
                        // TODO: use narrower array bounds (union of both inputs' bounds)
                        GridArray::from_fn(GridAab::for_block(effective_resolution), |p| {
                            operator.blend_evoxel(
                                src_voxels
                                    .get(p / src_scale)
                                    .copied()
                                    .unwrap_or(Evoxel::AIR),
                                dst_voxels
                                    .get(p / dst_scale)
                                    .copied()
                                    .unwrap_or(Evoxel::AIR),
                            )
                        }),
                    )
                }
            }

            Modifier::Zoom(Zoom {
                offset: offset_in_zoomed_blocks,
                scale,
            }) => {
                // TODO: To efficiently implement this, we should be able to run in a phase
                // *before* the `Primitive` evaluation, which allows us to reduce how many
                // of the primitive voxels are evaluated. (Modifier::Move will also help.)

                let EvaluatedBlock {
                    attributes,
                    color,
                    voxels,
                    resolution: original_resolution,
                    opaque: _,
                    visible: _,
                    voxel_opacity_mask: _,
                } = value;

                // TODO: write test cases for what happens if the division fails
                // (this is probably wrong in that we need to duplicate voxels if it happens)
                let zoom_resolution = (original_resolution / scale).unwrap_or(R1);

                if let Some(voxels) = voxels {
                    let voxel_offset = offset_in_zoomed_blocks.map(GridCoordinate::from).to_vec()
                        * GridCoordinate::from(zoom_resolution);
                    match GridAab::for_block(zoom_resolution)
                        .intersection(voxels.bounds().translate(-voxel_offset))
                    {
                        // This case occurs when the voxels' actual bounds (which may be smaller
                        // than the block bounding box) don't intersect the zoom region.
                        None => EvaluatedBlock::from_color(attributes, Rgba::TRANSPARENT),
                        Some(intersected_bounds) => EvaluatedBlock::from_voxels(
                            attributes,
                            zoom_resolution,
                            GridArray::from_fn(intersected_bounds, |p| voxels[p + voxel_offset]),
                        ),
                    }
                } else {
                    // Atom block.
                    // Zoom::new() checks that the region is not outside the block's unit cube,
                    // so we can just unconditionally return the original color.
                    EvaluatedBlock::from_color(attributes, color)
                }
            }

            Modifier::Move {
                direction,
                distance,
                velocity,
            } => {
                // Apply Quote to ensure that the block's own `tick_action` and other effects
                // don't interfere with movement or cause duplication.
                // (In the future we may want a more nuanced policy that allows internal changes,
                // but that will probably involve refining tick_action processing.)
                value = Modifier::Quote { ambient: false }.evaluate(
                    block,
                    this_modifier_index,
                    value,
                    depth,
                )?;

                let (original_bounds, effective_resolution) = match value.voxels.as_ref() {
                    Some(array) => (array.bounds(), value.resolution),
                    // Treat color blocks as having a resolution of 16. TODO: Improve on this hardcoded constant
                    None => (GridAab::for_block(R16), R16),
                };

                // For now, our strategy is to work in units of the block's resolution.
                // TODO: Generalize to being able to increase resolution to a chosen minimum.
                let distance_in_res = GridCoordinate::from(distance)
                    * GridCoordinate::from(effective_resolution)
                    / 256;
                let translation_in_res = direction.normal_vector() * distance_in_res;

                // This will be None if the displacement puts the block entirely out of view.
                let displaced_bounds: Option<GridAab> = original_bounds
                    .translate(translation_in_res)
                    .intersection(GridAab::for_block(effective_resolution));

                let animation_action = if displaced_bounds.is_none() && velocity >= 0 {
                    // Displaced to invisibility; turn into just plain air.
                    Some(VoxelBrush::single(AIR))
                } else if translation_in_res.is_zero() && velocity == 0
                    || distance == 0 && velocity < 0
                {
                    // Either a stationary displacement which is invisible, or an animated one which has finished its work.
                    assert_eq!(&block.modifiers()[this_modifier_index], self);
                    let mut new_block = block.clone();
                    new_block.modifiers_mut().remove(this_modifier_index); // TODO: What if other modifiers want to do things?
                    Some(VoxelBrush::single(new_block))
                } else if velocity != 0 {
                    // Movement in progress.
                    assert_eq!(&block.modifiers()[this_modifier_index], self);
                    let mut new_block = block.clone();
                    if let Modifier::Move {
                        distance, velocity, ..
                    } = &mut new_block.modifiers_mut()[this_modifier_index]
                    {
                        *distance = i32::from(*distance)
                            .saturating_add(i32::from(*velocity))
                            .clamp(0, i32::from(u16::MAX))
                            .try_into()
                            .unwrap(/* clamped to range */);
                    }
                    Some(VoxelBrush::single(new_block))
                } else {
                    // Stationary displacement; take no action
                    None
                };

                // Used by the solid color case; we have to do this before we move `attributes`
                // out of `value`.
                let voxel = Evoxel::from_block(&value);

                let attributes = BlockAttributes {
                    // Switch to `Recur` collision so that the displacement collides as expected.
                    // TODO: If the collision was `Hard` then we may need to edit the collision
                    // values of the individual voxels to preserve expected behavior.
                    collision: match value.attributes.collision {
                        BlockCollision::None => BlockCollision::None,
                        BlockCollision::Hard | BlockCollision::Recur => {
                            if displaced_bounds.is_some() {
                                BlockCollision::Recur
                            } else {
                                // Recur treats no-voxels as Hard, which is not what we want
                                BlockCollision::None
                            }
                        }
                    },
                    tick_action: animation_action,
                    ..value.attributes
                };

                match displaced_bounds {
                    Some(displaced_bounds) => {
                        let displaced_voxels = match value.voxels.as_ref() {
                            Some(voxels) => GridArray::from_fn(displaced_bounds, |cube| {
                                voxels[cube - translation_in_res]
                            }),
                            None => {
                                // Input block is a solid color; synthesize voxels.
                                GridArray::from_fn(displaced_bounds, |_| voxel)
                            }
                        };
                        EvaluatedBlock::from_voxels(
                            attributes,
                            effective_resolution,
                            displaced_voxels,
                        )
                    }
                    None => EvaluatedBlock::from_color(attributes, Rgba::TRANSPARENT),
                }
            }
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

    /// Create a pair of [`Modifier::Move`]s to displace a block.
    /// The first goes on the block being moved and the second on the air
    /// it's moving into.
    ///
    /// TODO: This is going to need to change again in order to support
    /// moving one block in and another out at the same time.
    pub fn paired_move(direction: Face6, distance: u16, velocity: i16) -> [Modifier; 2] {
        [
            Modifier::Move {
                direction,
                distance,
                velocity,
            },
            Modifier::Move {
                direction: direction.opposite(),
                distance: 256 - distance,
                velocity: -velocity,
            },
        ]
    }
}

impl VisitRefs for Modifier {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        match self {
            Modifier::Quote { .. } => {}
            Modifier::Rotate(..) => {}
            Modifier::Composite(m) => m.visit_refs(visitor),
            Modifier::Zoom(_) => todo!(),
            Modifier::Move {
                direction: _,
                distance: _,
                velocity: _,
            } => {}
        }
    }
}

/// Data for [`Modifier::Composite`], describing how to combine the voxels of another
/// block with the original one.
///
/// TODO: This modifier is not complete. It needs additional rules, particularly about combining
/// the blocks' attributes (right now it always chooses the destination), and the ability to
/// systematically combine or break apart the composite when applicable.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct Composite {
    pub source: Block,
    pub operator: CompositeOperator,
    /// Swap the roles of “source” and “destination” for the [`operator`](Self::operator).
    pub reverse: bool,
    // TODO: allow specifying another block to substitute the alpha, so as to be able to
    // make things become transparent? (That isn't strictly necessary since the “out” operator
    // will handle it, but a single unit might be useful)
}

impl Composite {
    pub fn new(source: Block, operator: CompositeOperator) -> Self {
        Self {
            source,
            operator,
            reverse: false,
        }
    }
}

impl From<Composite> for Modifier {
    fn from(value: Composite) -> Self {
        Modifier::Composite(value)
    }
}

impl VisitRefs for Composite {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        let Self {
            source,
            operator: _,
            reverse: _,
        } = self;
        source.visit_refs(visitor);
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Composite {
    // This noop manual implementation is necessary because there there is no `Arbitrary for Block`,
    // because blocks can't directly own further voxel blocks. TODO: We should put block building
    // behind some `ArbitraryBlock` type that is a bundle for a Universe, or better, make
    // limited owning URefs possible.
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Err(arbitrary::Error::EmptyChoose)
    }

    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        (0, Some(0))
    }
}

/// Compositing operators, mostly as per Porter-Duff.
///
/// The “source” block is the [`Composite`]'s stored block, and the “destination” block
/// is the block the modifier is attached to.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum CompositeOperator {
    /// Porter-Duff “over”. If both source and destination are opaque, the source is taken;
    /// otherwise the destination is taken.
    Over,
    // /// Split the volume in half on the plane perpendicular to `[1, 0, 1]`; all voxels
    // /// on the side nearer to the origin are taken from the destination, and all voxels
    // /// on the farther side or exactly on the plane are taken from the source.
    // Bevel,
}

impl CompositeOperator {
    fn blend_color(&self, source: Rgba, destination: Rgba) -> Rgba {
        match self {
            Self::Over => {
                // TODO: Surely this is not the only place we have implemented rgba blending?
                // Note that this math would be simpler if we used premultiplied alpha.
                let sa = source.alpha();
                let sa_complement = NotNan::new(1. - sa.into_inner()).unwrap();
                let rgb = source.to_rgb() * sa + destination.to_rgb() * sa_complement;
                rgb.with_alpha(sa + sa_complement * destination.alpha())
            }
        }
    }

    fn blend_evoxel(&self, src_ev: Evoxel, dst_ev: Evoxel) -> Evoxel {
        use BlockCollision as Coll;
        Evoxel {
            color: self.blend_color(src_ev.color, dst_ev.color),
            // TODO: specific operator should control all of these; we need an idea of what mask to
            // apply to discrete attributes.
            selectable: src_ev.selectable | dst_ev.selectable,
            collision: match (src_ev.collision, dst_ev.collision) {
                (Coll::Hard | Coll::Recur, _) | (_, Coll::Hard | Coll::Recur) => Coll::Hard,
                (Coll::None, Coll::None) => Coll::None,
            },
        }
    }
}

/// Data for [`Modifier::Zoom`], describing a portion of the original block that is scaled
/// up to become the whole block.
///
/// Design note: This is a struct separate from [`Modifier`] so that it can have a
/// constructor accepting only valid bounds.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Zoom {
    /// Scale factor to zoom in by.
    scale: Resolution,

    /// Which portion of the block/space will be used, specified in terms of an offset
    /// in the grid of zoomed blocks (that is, this should have coordinates between `0`
    /// and `scale - 1`).
    offset: Point3<u8>,
    // /// If present, a space to extract voxels from _instead of_ the underlying
    // /// [`Primitive`]. This may be used so that the before-zooming block can be a
    // /// custom preview rather than an exact miniature of the multi-block
    // /// structure.
    // space: Option<URef<Space>>,
}

impl Zoom {
    /// Construct a [`Zoom`] which enlarges the original block's voxels by `scale` and
    /// selects the region of them whose lower corner is `offset * scale`.
    ///
    /// Panics if any of `offset`'s components are out of bounds, i.e. less than 0 or
    /// greater than `scale - 1`.
    #[track_caller]
    pub fn new(scale: Resolution, offset: GridPoint) -> Self {
        if !GridAab::for_block(scale).contains_cube(offset) {
            panic!("Zoom offset {offset:?} out of bounds for {scale}");
        }

        Self {
            scale,
            offset: offset.map(|c| c as u8),
        }
    }
}

impl From<Zoom> for Modifier {
    fn from(value: Zoom) -> Self {
        Modifier::Zoom(value)
    }
}

impl VisitRefs for Zoom {
    fn visit_refs(&self, _visitor: &mut dyn RefVisitor) {
        let Zoom {
            scale: _,
            offset: _,
        } = self;
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for Zoom {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let scale = u.arbitrary()?;
        let max_offset = GridCoordinate::from(scale) - 1;
        Ok(Self::new(
            scale,
            GridPoint::new(
                u.int_in_range(0..=max_offset)?,
                u.int_in_range(0..=max_offset)?,
                u.int_in_range(0..=max_offset)?,
            ),
        ))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        use arbitrary::{size_hint::and_all, Arbitrary};
        and_all(&[
            <Resolution as Arbitrary>::size_hint(depth),
            <[GridCoordinate; 3] as Arbitrary>::size_hint(depth),
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::{R16, R2};
    use crate::block::{BlockAttributes, BlockCollision, Evoxel, Primitive, AIR};
    use crate::content::{make_some_blocks, make_some_voxel_blocks};
    use crate::drawing::VoxelBrush;
    use crate::math::{GridAab, GridPoint, GridVector, OpacityCategory, Rgba};
    use crate::space::Space;
    use crate::time::Tick;
    use crate::universe::Universe;
    use cgmath::EuclideanSpace;
    use pretty_assertions::assert_eq;

    #[test]
    fn quote_evaluation() {
        let l = Rgb::new(1.0, 2.0, 3.0);
        let mut block = Block::builder()
            .light_emission(l)
            .color(Rgba::WHITE)
            .build();
        assert_eq!(block.evaluate().unwrap().attributes.light_emission, l);
        block
            .modifiers_mut()
            .push(Modifier::Quote { ambient: true });
        assert_eq!(
            block.evaluate().unwrap().attributes.light_emission,
            Rgb::ZERO
        );
    }

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
                voxels: Some(GridArray::from_fn(block_bounds, |cube| {
                    Evoxel {
                        color: rotated_color_fn(cube),
                        selectable: true,
                        collision: BlockCollision::Hard,
                    }
                })),
                resolution: R2,
                opaque: false,
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

    #[test]
    #[should_panic(expected = "Zoom offset Point3 [2, 1, 1] out of bounds for 2")]
    fn zoom_construction_out_of_range_high() {
        Zoom::new(R2, Point3::new(2, 1, 1));
    }

    #[test]
    #[should_panic(expected = "Zoom offset Point3 [-1, 1, 1] out of bounds for 2")]
    fn zoom_construction_out_of_range_low() {
        Zoom::new(R2, Point3::new(-1, 1, 1));
    }

    #[test]
    fn zoom_evaluation() {
        let mut universe = Universe::new();
        let [original_block] = make_some_voxel_blocks(&mut universe);

        let ev_original = original_block.evaluate().unwrap();
        let zoom_resolution = ev_original.resolution.halve().unwrap();
        let original_voxels = ev_original.voxels.as_ref().unwrap();

        // Try zoom at multiple offset steps.
        for x in 0i32..2 {
            dbg!(x);
            let mut zoomed = original_block.clone();
            zoomed.modifiers_mut().push(
                Zoom::new(
                    R2, // scale up by two = divide resolution by two
                    Point3::new(x, 0, 0),
                )
                .into(),
            );
            let ev_zoomed = zoomed.evaluate().unwrap();
            assert_eq!(
                ev_zoomed,
                if x >= 2 {
                    // out of range
                    EvaluatedBlock::from_color(ev_original.attributes.clone(), Rgba::TRANSPARENT)
                } else {
                    EvaluatedBlock::from_voxels(
                        ev_original.attributes.clone(),
                        zoom_resolution,
                        GridArray::from_fn(GridAab::for_block(zoom_resolution), |p| {
                            original_voxels[p + GridVector::new(
                                GridCoordinate::from(zoom_resolution) * x,
                                0,
                                0,
                            )]
                        }),
                    )
                }
            );
        }
    }

    #[test]
    fn zoom_atom_in_bounds() {
        let [original] = make_some_blocks();
        let mut zoomed = original.clone();
        zoomed.modifiers_mut().push(Modifier::Zoom(Zoom {
            scale: R2,
            offset: Point3::new(1, 0, 0),
        }));
        assert_eq!(zoomed.evaluate().unwrap().color, original.color());
    }

    #[test]
    fn move_atom_block_evaluation() {
        let color = rgba_const!(1.0, 0.0, 0.0, 1.0);
        let original = Block::from(color);
        let modifier = Modifier::Move {
            direction: Face6::PY,
            distance: 128, // distance 1/2 block × scale factor of 256
            velocity: 0,
        };
        let moved = modifier.attach(original.clone());

        let expected_bounds = GridAab::from_lower_size([0, 8, 0], [16, 8, 16]);

        let ev_original = original.evaluate().unwrap();
        assert_eq!(
            moved.evaluate().unwrap(),
            EvaluatedBlock {
                attributes: BlockAttributes {
                    collision: BlockCollision::Recur,
                    ..ev_original.attributes.clone()
                },
                color: color.to_rgb().with_alpha(notnan!(0.5)),
                voxels: Some(GridArray::repeat(
                    expected_bounds,
                    Evoxel::from_block(&ev_original)
                )),
                resolution: R16,
                opaque: false,
                visible: true,
                voxel_opacity_mask: Some(GridArray::repeat(
                    expected_bounds,
                    OpacityCategory::Opaque
                )),
            }
        );
    }

    #[test]
    fn move_voxel_block_evaluation() {
        let mut universe = Universe::new();
        let resolution = R2;
        let color = rgba_const!(1.0, 0.0, 0.0, 1.0);
        let original = Block::builder()
            .voxels_fn(&mut universe, resolution, |_| Block::from(color))
            .unwrap()
            .build();

        let modifier = Modifier::Move {
            direction: Face6::PY,
            distance: 128, // distance 1/2 block × scale factor of 256
            velocity: 0,
        };
        let moved = modifier.attach(original.clone());

        let expected_bounds = GridAab::from_lower_size([0, 1, 0], [2, 1, 2]);

        let ev_original = original.evaluate().unwrap();
        assert_eq!(
            moved.evaluate().unwrap(),
            EvaluatedBlock {
                attributes: BlockAttributes {
                    collision: BlockCollision::Recur,
                    ..ev_original.attributes.clone()
                },
                color: color.to_rgb().with_alpha(notnan!(0.5)),
                voxels: Some(GridArray::repeat(
                    expected_bounds,
                    Evoxel::from_block(&ev_original)
                )),
                resolution,
                opaque: false,
                visible: true,
                voxel_opacity_mask: Some(GridArray::repeat(
                    expected_bounds,
                    OpacityCategory::Opaque
                )),
            }
        );
    }

    /// [`Modifier::Move`] incorporates [`Modifier::Quote`] to ensure that no conflicting
    /// effects happen.
    #[test]
    fn move_also_quotes() {
        let original = Block::builder()
            .color(Rgba::WHITE)
            .tick_action(Some(VoxelBrush::single(AIR)))
            .build();
        let moved = Modifier::Move {
            direction: Face6::PY,
            distance: 128,
            velocity: 0,
        }
        .attach(original);

        assert_eq!(moved.evaluate().unwrap().attributes.tick_action, None);
    }

    /// Set up a `Modifier::Move`, let it run, and then allow assertions to be made about the result.
    fn move_block_test(direction: Face6, velocity: i16, checker: impl FnOnce(&Space, &Block)) {
        let [block] = make_some_blocks();
        let mut space = Space::empty(GridAab::from_lower_upper([-1, -1, -1], [2, 2, 2]));
        let [move_out, move_in] = Modifier::paired_move(direction, 0, velocity);
        space
            .set([0, 0, 0], move_out.attach(block.clone()))
            .unwrap();
        space
            .set(
                GridPoint::origin() + direction.normal_vector(),
                move_in.attach(block.clone()),
            )
            .unwrap();
        let mut universe = Universe::new();
        let space = universe.insert_anonymous(space);
        // TODO: We need a "step until idle" function, or for the UniverseStepInfo to convey how many blocks were updated / are waiting
        // TODO: Some tests will want to look at the partial results
        for _ in 0..257 {
            universe.step(Tick::arbitrary());
        }
        checker(&space.borrow(), &block);
    }

    #[test]
    fn move_zero_velocity() {
        move_block_test(Face6::PX, 0, |space, block| {
            assert_eq!(&space[[0, 0, 0]], block);
            assert_eq!(&space[[1, 0, 0]], &AIR);
        });
    }

    #[test]
    fn move_slowly() {
        move_block_test(Face6::PX, 1, |space, block| {
            assert_eq!(&space[[0, 0, 0]], &AIR);
            assert_eq!(&space[[1, 0, 0]], block);
        });
    }

    #[test]
    fn move_instant_velocity() {
        move_block_test(Face6::PX, 256, |space, block| {
            assert_eq!(&space[[0, 0, 0]], &AIR);
            assert_eq!(&space[[1, 0, 0]], block);
        });
    }
}
