use std::mem;

use ordered_float::NotNan;

use crate::block::{
    self, Block, BlockCollision, EvaluatedBlock, Evoxel, Modifier, Resolution::R1, AIR,
};
use crate::math::{GridAab, GridArray, GridCoordinate, GridRotation, Rgba};
use crate::universe;

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

    /// Compose `self` and `destination`, except that:
    ///
    /// * If `destination` is [`AIR`], then the `self.source` block will be returned.
    /// * If `self.source` is [`AIR`], then `destination` will be returned.
    /// * If `destination` has a rotation modifier, it will be rearranged to be last.
    ///   (In this way, there won't be any unequal-but-equivalent blocks generated due
    ///   to rotation.)
    ///
    /// This operation is of limited use and is designed for world-generation purposes, not
    /// player action (since it has no restrictions on what it can compose). Its particular
    /// use is to build corner joint blocks.
    ///
    /// TODO: Generalize this so it has a filter on which things should be composed,
    /// replaced, or left unchanged (failure).
    ///
    /// TODO: Figure out a way to express "sorting order" rules for swapping self and
    /// destination, because for corner joints we don't care which is on top but we want
    /// there to be only one kind of corner block, not two depending on operation order.
    pub fn compose_or_replace(mut self, mut destination: Block) -> Block {
        // If the destination had a rotation, extract it.
        let dest_rot = if let Some(&Modifier::Rotate(dest_rot)) = destination.modifiers().last() {
            destination.modifiers_mut().pop();
            dest_rot
        } else {
            GridRotation::IDENTITY
        };

        if destination == AIR {
            // If the destination is AIR, discard it.
            // Note: Since we removed rotation, this is currently equivalent to
            // testing against Block::unspecialize(), but it might not be in the future.
            // We could use a better solution.
            self.source
        } else if self.source == AIR {
            // If the source is AIR, produce the original destination block.
            destination.rotate(dest_rot)
        } else {
            self.source = self.source.rotate(dest_rot.inverse());
            Modifier::from(self).attach(destination).rotate(dest_rot)
        }
    }

    /// Called by [`Modifier::evaluate`].
    pub(super) fn evaluate(
        &self,
        mut dst_evaluated: EvaluatedBlock,
        depth: u8,
    ) -> Result<EvaluatedBlock, block::EvalBlockError> {
        let Composite {
            ref source,
            operator,
            reverse,
        } = *self;

        // The destination block is already evaluated (it is the input to this
        // modifier), but we need to evaluate the source block.
        let mut src_evaluated = source.evaluate_impl(block::next_depth(depth)?)?;
        // Apply the reverse option by swapping everything.
        if reverse {
            mem::swap(&mut src_evaluated, &mut dst_evaluated);
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
        } = dst_evaluated;
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
        let src_scale =
            GridCoordinate::from(effective_resolution) / GridCoordinate::from(src_resolution);
        let dst_scale =
            GridCoordinate::from(effective_resolution) / GridCoordinate::from(dst_resolution);

        Ok(if effective_resolution == R1 {
            EvaluatedBlock::from_color(attributes, operator.blend_color(src_color, dst_color))
        } else {
            // TODO: avoid needing to allocate here. Define a GridArrayView type?
            let src_voxels = src_voxels.unwrap_or_else(|| {
                GridArray::from_fn(GridAab::for_block(R1), |_| Evoxel::from_color(src_color))
            });
            let dst_voxels = dst_voxels.unwrap_or_else(|| {
                GridArray::from_fn(GridAab::for_block(R1), |_| Evoxel::from_color(src_color))
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
        })
    }
}

impl From<Composite> for Modifier {
    fn from(value: Composite) -> Self {
        Modifier::Composite(value)
    }
}

impl universe::VisitRefs for Composite {
    fn visit_refs(&self, visitor: &mut dyn universe::RefVisitor) {
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
        let source = source.clamp();
        let destination = destination.clamp();
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use pretty_assertions::assert_eq;

    #[test]
    fn composite_silly_floats() {
        // We just want to see this does not panic on NaN.
        CompositeOperator::Over.blend_color(
            Rgba::new(2e25, 2e25, 2e25, 2e25),
            Rgba::new(2e25, 2e25, 2e25, 2e25),
        );
    }

    #[test]
    fn compose_or_replace_source_is_air() {
        let [block] = make_some_blocks();
        assert_eq!(
            Composite::new(AIR, CompositeOperator::Over).compose_or_replace(block.clone()),
            block
        );
    }

    #[test]
    fn compose_or_replace_destination_is_air() {
        let [block] = make_some_blocks();
        assert_eq!(
            Composite::new(block.clone(), CompositeOperator::Over).compose_or_replace(AIR),
            block
        );
    }
}
