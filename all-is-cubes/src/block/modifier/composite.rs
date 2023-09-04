use std::mem;

use ordered_float::NotNan;

use crate::block::{
    self, Block, BlockCollision, Evoxel, Evoxels, MinEval, Modifier, Resolution::R1, AIR,
};
use crate::math::{Cube, GridAab, GridArray, GridCoordinate, GridRotation, Rgb};
use crate::universe;

/// Data for [`Modifier::Composite`], describing how to combine the voxels of another
/// block with the original one.
///
/// TODO: This modifier is not complete. It needs additional rules, particularly about combining
/// the blocks' attributes (right now it always chooses the destination), and the ability to
/// systematically combine or break apart the composite when applicable.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct Composite {
    /// The “source” input to the compositing operator.
    /// (The “destination” input is the block this modifier is attached to.)
    pub source: Block,

    /// The compositing operator used to combine the source and destination blocks.
    pub operator: CompositeOperator,

    /// Swap the roles of “source” and “destination” for the [`operator`](Self::operator).
    pub reverse: bool,

    /// Whether the block should come apart into its components when removed from its place.
    pub disassemblable: bool,
    // TODO: allow specifying another block to substitute the alpha, so as to be able to
    // make things become transparent? (That isn't strictly necessary since the “out” operator
    // will handle it, but a single unit might be useful)
}

impl Composite {
    /// Construct a new [`Composite`] modifier with the given source and operator, and
    /// `reverse: false`.
    pub fn new(source: Block, operator: CompositeOperator) -> Self {
        Self {
            source,
            operator,
            reverse: false,
            disassemblable: false,
        }
    }

    /// Toggle the reversed flag, which swaps the roles of the two blocks in the operator.
    #[must_use]
    pub fn reversed(mut self) -> Self {
        self.reverse = !self.reverse;
        self
    }

    /// Set the disassemblable flag to true.
    ///
    /// This will allow the composite to be taken apart by player action.
    /// TODO: explain further
    #[must_use]
    pub fn with_disassemblable(mut self) -> Self {
        self.disassemblable = true;
        self
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
            destination.with_modifier(self).rotate(dest_rot)
        }
    }

    /// Use [`Composite::compose_or_replace()`] repeatedly to assemble a block from parts.
    pub fn stack(destination: Block, parts: impl IntoIterator<Item = Composite>) -> Block {
        parts
            .into_iter()
            .fold(destination, |block, part| part.compose_or_replace(block))
    }

    /// Called by [`Modifier::evaluate`].
    pub(super) fn evaluate(
        &self,
        mut dst_evaluated: MinEval,
        depth: u8,
        filter: &block::EvalFilter,
    ) -> Result<MinEval, block::EvalBlockError> {
        let Composite {
            ref source,
            operator,
            reverse,
            disassemblable: _,
        } = *self;

        // The destination block is already evaluated (it is the input to this
        // modifier), but we need to evaluate the source block.
        let mut src_evaluated = source.evaluate_impl(block::next_depth(depth)?, filter)?;

        if filter.skip_eval {
            return Ok(dst_evaluated);
        }

        // Apply the reverse option by swapping everything.
        if reverse {
            mem::swap(&mut src_evaluated, &mut dst_evaluated);
        }
        // Unpack blocks.
        let MinEval {
            attributes: dst_att,
            voxels: dst_voxels,
        } = dst_evaluated;
        let MinEval {
            attributes: src_att,
            voxels: src_voxels,
        } = src_evaluated;

        let src_resolution = src_voxels.resolution();
        let dst_resolution = dst_voxels.resolution();
        let effective_resolution = src_resolution.max(dst_resolution);
        let src_scale =
            GridCoordinate::from(effective_resolution) / GridCoordinate::from(src_resolution);
        let dst_scale =
            GridCoordinate::from(effective_resolution) / GridCoordinate::from(dst_resolution);

        let attributes = block::BlockAttributes {
            display_name: dst_att.display_name, // TODO merge
            selectable: src_att.selectable | dst_att.selectable,
            rotation_rule: dst_att.rotation_rule, // TODO merge
            // TODO: summing is kinda correct for "this contains a light source", but isn't
            // very justifiable; we should probably use per-voxel light emission instead
            tick_action: dst_att.tick_action,       // TODO: merge
            animation_hint: dst_att.animation_hint, // TODO: merge
        };

        Ok(if effective_resolution == R1 {
            MinEval {
                attributes,
                voxels: Evoxels::One(operator.blend_evoxel(
                    src_voxels.single_voxel().unwrap(),
                    dst_voxels.single_voxel().unwrap(),
                )),
            }
        } else {
            MinEval {
                attributes,
                // TODO: use narrower array bounds (union of both inputs' bounds)
                voxels: Evoxels::Many(
                    effective_resolution,
                    GridArray::from_fn(GridAab::for_block(effective_resolution), |cube| {
                        let p = cube.lower_bounds();
                        operator.blend_evoxel(
                            src_voxels
                                .get(Cube::from(p / src_scale))
                                .unwrap_or(Evoxel::AIR),
                            dst_voxels
                                .get(Cube::from(p / dst_scale))
                                .unwrap_or(Evoxel::AIR),
                        )
                    }),
                ),
            }
        })
    }

    /// Called by [`Modifier::unspecialize()`].
    pub(super) fn unspecialize(&self, entire_block: &Block) -> block::ModifierUnspecialize {
        if self.disassemblable {
            let mut destination = entire_block.clone();
            destination
                .modifiers_mut()
                .pop()
                .expect("Missing Composite modifier");
            block::ModifierUnspecialize::Replace(vec![self.source.clone(), destination])
        } else {
            block::ModifierUnspecialize::Keep
        }
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
            disassemblable: _,
        } = self;
        source.visit_refs(visitor);
    }
}

/// Compositing operators, mostly as per Porter-Duff.
///
/// The “source” block is the [`Composite`]'s stored block, and the “destination” block
/// is the block the modifier is attached to.
///
/// TODO: Document behavior of `collision` and `selectable` properties.
///
#[doc = include_str!("../../save/serde-warning.md")]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum CompositeOperator {
    /// Porter-Duff “over”. If both source and destination are opaque, the source is taken;
    /// otherwise the destination is taken.
    Over,

    /// Porter-Duff “in”. If both source and destination are opaque, the source is taken;
    /// otherwise the result is transparent. Thus the destination acts as a mask constraining
    /// where the source is present; the source is “in” the destination.
    /// The destination's color is not used.
    In,

    /// Porter-Duff “atop”. If both source and destination are opaque, the source is taken;
    /// otherwise the destination is taken. Thus the source is painted onto the destination's
    /// substance.
    Atop,
    //
    // /// Split the volume in half on the plane perpendicular to `[1, 0, 1]`; all voxels
    // /// on the side nearer to the origin are taken from the destination, and all voxels
    // /// on the farther side or exactly on the plane are taken from the source.
    // Bevel,
}

impl CompositeOperator {
    /// Entry point by which [`Composite::evaluate()`] uses [`Self`].
    fn blend_evoxel(&self, src_ev: Evoxel, dst_ev: Evoxel) -> Evoxel {
        use BlockCollision as Coll;
        Evoxel {
            color: {
                // Clamp to avoid silly outcomes of the arithmetic.
                let source = src_ev.color.clamp();
                let destination = dst_ev.color.clamp();
                let (rgb, a) = self.alpha_blend(
                    source.to_rgb(),
                    source.alpha(),
                    destination.to_rgb(),
                    destination.alpha(),
                );
                rgb.with_alpha(a)
            },

            // TODO: This doesn't work correctly when something is transparent and emissive.
            // We need to define the semantics of that in terms of volumetric rendering.
            emission: {
                let (color_blend, alpha) = self.alpha_blend(
                    src_ev.emission,
                    src_ev.color.clamp().alpha(),
                    dst_ev.emission,
                    dst_ev.color.clamp().alpha(),
                );
                // effectively “premultiplying” in order to apply the intended effect of
                // alpha on the intensity
                color_blend * alpha
            },

            selectable: self.blend_binary(src_ev.selectable, dst_ev.selectable),

            collision: {
                let src_is_something = !matches!(src_ev.collision, Coll::None);
                let dst_is_something = !matches!(dst_ev.collision, Coll::None);
                if self.blend_binary(src_is_something, dst_is_something) {
                    // TODO: this is probably not a sufficient condition and we will
                    // eventually need some kind of “ranking” of collision types so that
                    // depending the operator a "soft" collision (e.g. "high viscosity")
                    // might entirely override a "hard" one or might not.
                    if src_is_something {
                        src_ev.collision
                    } else {
                        dst_ev.collision
                    }
                } else {
                    Coll::None
                }
            },
        }
    }

    /// Called by [`Self::blend_evoxel()`] to handle diffuse and emissive colors.
    ///
    /// Note that this does not accept and return `Rgba` because the output is not necessarily
    /// in the 0-1 range; it might work but that's not an intended use of the type.
    fn alpha_blend(
        &self,
        source: Rgb,
        sa: NotNan<f32>,
        destination: Rgb,
        da: NotNan<f32>,
    ) -> (Rgb, NotNan<f32>) {
        match self {
            Self::Over => {
                // TODO: Surely this is not the only place we have implemented rgba blending?
                // Note that this math would be simpler if we used premultiplied alpha.
                let sa_complement = NotNan::new(1. - sa.into_inner()).unwrap();
                let rgb = source * sa + destination * sa_complement;
                (rgb, sa + sa_complement * da)
            }

            Self::In => (source, sa * da),

            Self::Atop => {
                let sa_complement = NotNan::new(1. - sa.into_inner()).unwrap();
                let rgb = source * sa + destination * sa_complement;

                let out_alpha = da;
                if out_alpha == 0.0 {
                    // we wouldn't have to do this if we used premultiplied alpha :/
                    (Rgb::ZERO, out_alpha)
                } else {
                    (rgb, out_alpha)
                }
            }
        }
    }

    /// Called by [`Self::blend_evoxel()`] to handle properties that can be described as
    /// “present or absent” binary flags.
    fn blend_binary(&self, source: bool, destination: bool) -> bool {
        match self {
            Self::Over => source | destination,
            Self::In => source & destination,
            Self::Atop => destination,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::EvaluatedBlock;
    use crate::content::make_some_blocks;
    use crate::math::{Rgb, Rgba};
    use pretty_assertions::assert_eq;
    use BlockCollision::{Hard, None as CNone};
    use CompositeOperator::{Atop, In, Over};

    // --- Helpers

    /// Check the result of applying an operator to a single `Evoxel`
    #[track_caller]
    fn assert_blend(src: Evoxel, operator: CompositeOperator, dst: Evoxel, outcome: Evoxel) {
        // TODO: Replace this direct call with going through the full block evaluation.

        assert_eq!(
            operator.blend_evoxel(src, dst),
            outcome,
            "\nexpecting {operator:?}.blend(\n  {src:?},\n  {dst:?}\n) == {outcome:?}"
        );
    }

    fn evcolor(color: Rgba) -> Evoxel {
        Evoxel {
            color,
            emission: Rgb::ZERO,
            selectable: true,
            collision: Hard,
        }
    }

    /// Construct a voxel with light emission.
    /// Alpha is taken too, because alpha is used to control blending.
    fn evemit(emission: Rgb, alpha: f32) -> Evoxel {
        Evoxel {
            // color doesn't matter, except that at zero alpha it should be the canonical zero
            // for convenience of testing. (TODO: maybe `Rgba` should enforce that or be premultiplied.)
            color: Rgb::ZERO.with_alpha(NotNan::new(alpha).unwrap()),
            emission,
            selectable: true,
            collision: Hard,
        }
    }

    fn evcoll(collision: BlockCollision) -> Evoxel {
        Evoxel {
            color: Rgba::WHITE, // no effect
            emission: Rgb::ZERO,
            selectable: false, // no effect
            collision,
        }
    }

    #[track_caller]
    fn eval_compose(src: &Block, operator: CompositeOperator, dst: &Block) -> EvaluatedBlock {
        dst.clone()
            .with_modifier(Composite::new(src.clone(), operator))
            .evaluate()
            .expect("failed to evaluate in eval_compose()")
    }

    // --- Tests ---

    #[test]
    fn blend_over_silly_floats() {
        // We just want to see this does not panic on NaN.
        CompositeOperator::Over.blend_evoxel(
            evcolor(Rgba::new(2e25, 2e25, 2e25, 2e25)),
            evcolor(Rgba::new(2e25, 2e25, 2e25, 2e25)),
        );
    }

    #[test]
    fn blend_over_emission() {
        let red_1 = evemit(Rgb::new(1., 0., 0.), 1.0);
        let green_0 = evemit(Rgb::new(0., 1., 0.), 0.0);
        let green_05 = evemit(Rgb::new(0., 1., 0.), 0.5);
        let none_1 = evemit(Rgb::ZERO, 1.0);
        let none_0 = evemit(Rgb::ZERO, 0.0);

        // Simple 100% blending cases
        assert_blend(red_1, Over, none_1, red_1);
        assert_blend(none_1, Over, red_1, none_1);
        assert_blend(none_1, Over, none_1, none_1);
        assert_blend(red_1, Over, red_1, red_1);
        assert_blend(red_1, Over, none_0, red_1);
        assert_blend(none_0, Over, red_1, red_1);

        // Partial alpha
        assert_blend(red_1, Over, green_05, red_1);
        assert_blend(green_05, Over, red_1, evemit(Rgb::new(0.5, 0.5, 0.0), 1.0));
        assert_blend(
            green_05,
            Over,
            green_05,
            evemit(Rgb::new(0.0, 0.75, 0.0), 0.75),
        );
        // assert_blend(green_05, Over, none_0, green_05); // TODO: broken, too dim

        // What if emission with zero alpha is blended in?
        assert_blend(green_0, Over, none_1, none_1);
        // assert_blend(green_0, Over, none_0, green_0); // TODO
        assert_blend(none_1, Over, green_0, none_1);
        // assert_blend(green_0, Over, green_0, green_0); // TODO: goes to zero
    }

    #[test]
    fn blend_over_collision() {
        assert_blend(evcoll(Hard), Over, evcoll(Hard), evcoll(Hard));
        assert_blend(evcoll(CNone), Over, evcoll(CNone), evcoll(CNone));
        assert_blend(evcoll(Hard), Over, evcoll(CNone), evcoll(Hard));
        assert_blend(evcoll(CNone), Over, evcoll(Hard), evcoll(Hard));
    }

    #[test]
    fn blend_in_emission() {
        let red_1 = evemit(Rgb::new(1., 0., 0.), 1.0);
        let green_1 = evemit(Rgb::new(0., 1., 0.), 1.0);
        let green_0 = evemit(Rgb::new(0., 1., 0.), 0.0);
        let green_05 = evemit(Rgb::new(0., 1., 0.), 0.5);
        let none_1 = evemit(Rgb::ZERO, 1.0);
        let none_0 = evemit(Rgb::ZERO, 0.0);

        // Simple 100% blending cases
        assert_blend(red_1, In, none_1, red_1);
        assert_blend(red_1, In, red_1, red_1);
        assert_blend(red_1, In, green_1, red_1);
        assert_blend(red_1, In, none_0, none_0);
        assert_blend(none_1, In, red_1, none_1);
        assert_blend(none_0, In, red_1, none_0);
        assert_blend(none_1, In, none_1, none_1);
        assert_blend(none_0, In, none_1, none_0);

        // Partial alpha
        assert_blend(red_1, In, green_05, evemit(Rgb::new(0.5, 0.0, 0.0), 0.5));
        assert_blend(green_05, In, red_1, evemit(Rgb::new(0.0, 0.5, 0.0), 0.5));
        assert_blend(
            green_05,
            In,
            green_05,
            evemit(Rgb::new(0.0, 0.25, 0.0), 0.25),
        );
        assert_blend(green_05, In, none_0, none_0); // TODO: broken, too dim

        // What if emission with zero alpha is blended in?
        assert_blend(green_0, In, none_1, none_0);
        assert_blend(green_0, In, none_0, none_0);
        assert_blend(none_1, In, green_0, none_0);
        assert_blend(green_0, In, green_0, none_0); // TODO: this should plausibly stay
    }

    #[test]
    fn blend_in_collision() {
        assert_blend(evcoll(Hard), In, evcoll(Hard), evcoll(Hard));
        assert_blend(evcoll(CNone), In, evcoll(CNone), evcoll(CNone));
        assert_blend(evcoll(Hard), In, evcoll(CNone), evcoll(CNone));
        assert_blend(evcoll(CNone), In, evcoll(Hard), evcoll(CNone));
    }

    #[test]
    fn blend_atop_color() {
        let opaque1 = evcolor(Rgba::new(1.0, 0.0, 0.0, 1.0));
        let opaque2 = evcolor(Rgba::new(0.0, 1.0, 0.0, 1.0));
        let half_red = evcolor(Rgba::new(1.0, 0.0, 0.0, 0.5));
        let clear = evcolor(Rgba::TRANSPARENT);

        assert_blend(opaque1, Atop, opaque2, opaque1);
        assert_blend(
            half_red,
            Atop,
            opaque2,
            evcolor(Rgba::new(0.5, 0.5, 0.0, 1.0)),
        );
        assert_blend(opaque1, Atop, clear, clear);
        assert_blend(clear, Atop, opaque2, opaque2);
        assert_blend(clear, Atop, clear, clear);
    }

    #[test]
    fn blend_atop_emission() {
        let red_1 = evemit(Rgb::new(1., 0., 0.), 1.0);
        let green_1 = evemit(Rgb::new(0., 1., 0.), 1.0);
        let green_0 = evemit(Rgb::new(0., 1., 0.), 0.0);
        let green_05 = evemit(Rgb::new(0., 1., 0.), 0.5);
        let none_1 = evemit(Rgb::ZERO, 1.0);
        let none_0 = evemit(Rgb::ZERO, 0.0);

        // Simple 100% blending cases
        assert_blend(red_1, Atop, none_1, red_1);
        assert_blend(red_1, Atop, red_1, red_1);
        assert_blend(red_1, Atop, green_1, red_1);
        assert_blend(red_1, Atop, none_0, none_0);
        assert_blend(none_1, Atop, red_1, none_1);
        assert_blend(none_0, Atop, red_1, red_1);
        assert_blend(none_1, Atop, none_1, none_1);
        assert_blend(none_0, Atop, none_1, none_1);

        // Partial alpha
        assert_blend(red_1, Atop, green_05, evemit(Rgb::new(0.5, 0.0, 0.0), 0.5));
        assert_blend(green_05, Atop, red_1, evemit(Rgb::new(0.5, 0.5, 0.0), 1.0));
        assert_blend(
            green_05,
            Atop,
            green_05,
            evemit(Rgb::new(0.0, 0.5, 0.0), 0.5),
        );
        assert_blend(green_05, Atop, none_0, none_0);

        // What if emission with zero alpha is blended in?
        assert_blend(green_0, Atop, none_1, none_1);
        assert_blend(green_0, Atop, none_0, none_0);
        assert_blend(none_1, Atop, green_0, none_0);
        assert_blend(green_0, Atop, green_0, none_0); // TODO: this should plausibly stay
    }

    #[test]
    fn blend_atop_collision() {
        assert_blend(evcoll(Hard), Atop, evcoll(Hard), evcoll(Hard));
        assert_blend(evcoll(CNone), Atop, evcoll(CNone), evcoll(CNone));
        assert_blend(evcoll(Hard), Atop, evcoll(CNone), evcoll(CNone));
        assert_blend(evcoll(CNone), Atop, evcoll(Hard), evcoll(Hard));
    }

    #[test]
    fn attribute_selectable_if_either_is_selectable() {
        // TODO: make this a more thorough test by making the two blocks slabs so that
        // all four types of voxels are involved. This currently doesn't matter but it may.
        let is_sel = &Block::builder().color(Rgba::WHITE).selectable(true).build();
        let not_sel = &Block::builder()
            .color(Rgba::WHITE)
            .selectable(false)
            .build();

        assert!(eval_compose(is_sel, Over, is_sel).attributes.selectable);
        assert!(eval_compose(is_sel, Over, not_sel).attributes.selectable);
        assert!(eval_compose(not_sel, Over, is_sel).attributes.selectable);
        assert!(!eval_compose(not_sel, Over, not_sel).attributes.selectable);

        assert!(eval_compose(is_sel, In, is_sel).attributes.selectable);
        assert!(eval_compose(is_sel, In, not_sel).attributes.selectable);
        assert!(eval_compose(not_sel, In, is_sel).attributes.selectable);
        assert!(!eval_compose(not_sel, In, not_sel).attributes.selectable);
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

    #[test]
    fn unspecialize_no() {
        let [b1, b2] = make_some_blocks();
        let composed = b1
            .clone()
            .with_modifier(Composite::new(b2.clone(), CompositeOperator::Over));
        assert_eq!(composed.unspecialize(), vec![composed]);
    }

    #[test]
    fn unspecialize_yes() {
        let [b1, b2] = make_some_blocks();
        let composed = b1.clone().with_modifier(
            Composite::new(b2.clone(), CompositeOperator::Over).with_disassemblable(),
        );
        assert_eq!(composed.unspecialize(), vec![b2, b1]);
    }
}
