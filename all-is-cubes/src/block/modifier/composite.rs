use core::mem;

use alloc::vec;
use ordered_float::NotNan;

use crate::block::{
    self, Block, BlockCollision, Evoxel, Evoxels, MinEval, Modifier, Resolution::R1, AIR,
};
use crate::math::{Cube, GridAab, GridCoordinate, GridRotation, GridSize, Rgb, Vol};
use crate::op::Operation;
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
        filter: &block::EvalFilter,
    ) -> Result<MinEval, block::InEvalError> {
        let Composite {
            ref source,
            operator,
            reverse,
            disassemblable: _,
        } = *self;

        // The destination block is already evaluated (it is the input to this
        // modifier), but we need to evaluate the source block.
        let mut src_evaluated = {
            let _recursion_scope = block::Budget::recurse(&filter.budget)?;
            source.evaluate_impl(filter)?
        };

        if filter.skip_eval {
            return Ok(dst_evaluated);
        }

        // Apply the reverse option by swapping everything.
        if reverse {
            mem::swap(&mut src_evaluated, &mut dst_evaluated);
        }

        evaluate_composition(src_evaluated, dst_evaluated, operator, filter)
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

/// Implementation of [`Composite::evaluate()`], without the requirement that the source
/// be a [`Block`] rather than a [`MinEval`].
fn evaluate_composition(
    src_evaluated: MinEval,
    dst_evaluated: MinEval,
    operator: CompositeOperator,
    filter: &block::EvalFilter,
) -> Result<MinEval, block::InEvalError> {
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

    let src_bounds_scaled = bounds_excluding_air(&src_voxels, src_scale);
    let dst_bounds_scaled = bounds_excluding_air(&dst_voxels, dst_scale);

    let output_bounds = operator.bounds(src_bounds_scaled, dst_bounds_scaled);

    block::Budget::decrement_voxels(&filter.budget, output_bounds.volume().unwrap())?;

    let attributes = block::BlockAttributes {
        display_name: dst_att.display_name, // TODO merge
        selectable: src_att.selectable | dst_att.selectable,
        inventory: src_att.inventory.concatenate(dst_att.inventory),
        rotation_rule: dst_att.rotation_rule, // TODO merge
        tick_action: dst_att.tick_action,     // TODO: merge
        activation_action: operator.blend_operations(
            src_att.activation_action.as_ref(),
            dst_att.activation_action.as_ref(),
        ),
        animation_hint: src_att.animation_hint | dst_att.animation_hint, // TODO: some operators should ignore some hints (e.g. `In` should ignore destination color changes)
    };

    let voxels = if let (true, Some(src_voxel), Some(dst_voxel)) = (
        output_bounds == GridAab::ORIGIN_CUBE,
        src_voxels.single_voxel(),
        dst_voxels.single_voxel(),
    ) {
        // The output is nonempty
        // TODO: Do we need the `output_bounds == ORIGIN_CUBE` test?
        // It skips this branch to keep the bounds empty, but is that good?
        Evoxels::One(operator.blend_evoxel(src_voxel, dst_voxel))
    } else {
        Evoxels::Many(
            effective_resolution,
            Vol::from_fn(output_bounds, |cube| {
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
        )
    };

    Ok(MinEval { attributes, voxels })
}

/// Rescale the bounds of the input to the resolution of the output, but also, if the voxels are
/// [`Evoxel::AIR`] and thus equivalent to out-of-bounds, substitute empty bounds.
/// This way, we produce suitably tight bounds when one of the blocks is AIR.
fn bounds_excluding_air(voxels: &Evoxels, src_scale: i32) -> GridAab {
    if voxels.single_voxel() == Some(Evoxel::AIR) {
        GridAab::ORIGIN_EMPTY
    } else {
        voxels.bounds().multiply(src_scale)
    }
}

impl From<Composite> for Modifier {
    fn from(value: Composite) -> Self {
        Modifier::Composite(value)
    }
}

impl universe::VisitHandles for Composite {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            source,
            operator: _,
            reverse: _,
            disassemblable: _,
        } = self;
        source.visit_handles(visitor);
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

    /// Porter-Duff “out”. If both source and destination are opaque, the result is transparent;
    /// otherwise the source is taken. Thus the destination acts as a mask removing portions
    /// of the source.
    /// The destination's color is not used.
    Out,

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
    /// Entry point by which [`evaluate_composition()`] uses [`Self`].
    fn blend_evoxel(self, src_ev: Evoxel, dst_ev: Evoxel) -> Evoxel {
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
        self,
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
            Self::Out => {
                let da_complement = NotNan::new(1. - da.into_inner()).unwrap();
                (source, sa * da_complement)
            }

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
    #[allow(clippy::needless_bitwise_bool)] // ideally this would be branchless…
    fn blend_binary(self, source: bool, destination: bool) -> bool {
        match self {
            Self::Over => source | destination,
            Self::In => source & destination,
            Self::Out => source & !destination,
            Self::Atop => destination,
        }
    }

    fn blend_operations(
        self,
        source: Option<&Operation>,
        destination: Option<&Operation>,
    ) -> Option<Operation> {
        // TODO: Actually implement merging of multiple operations.
        _ = self;
        _ = source;
        destination.cloned()
    }

    /// Compute the bounds of the result given the bounds of the source and destination.
    fn bounds(self, source: GridAab, destination: GridAab) -> GridAab {
        match self {
            Self::Over => source.union_cubes(destination),
            // We could equally well use intersection_cubes() here, but prefer the one that
            // more often returns a box related to the input.
            Self::In => source
                .intersection_box(destination)
                .unwrap_or(GridAab::ORIGIN_EMPTY),
            Self::Out => source,
            Self::Atop => destination,
        }
    }
}

// Inventories are rendered by compositing their icon blocks in.
pub(in crate::block) fn render_inventory(
    mut input: MinEval,
    inventory: &crate::inv::Inventory,
    filter: &block::EvalFilter,
) -> Result<MinEval, block::InEvalError> {
    if filter.skip_eval {
        return Ok(input);
    }

    // TODO(inventory): clone necessary to avoid a borrow conflict
    let config = input.attributes.inventory.clone();

    for (slot_index, icon_position) in config.icon_positions() {
        let Some(placed_icon_bounds) = GridAab::from_lower_size(
            icon_position,
            GridSize::splat(
                (config.icon_resolution / config.icon_scale)
                    .unwrap_or(R1)
                    .into(),
            ),
        )
        .intersection_cubes(GridAab::for_block(config.icon_resolution)) else {
            // Icon's position doesn't intersect the block's bounds.
            continue;
        };

        // TODO(inventory): icon_only_if_intrinsic is a kludge
        let Some(icon): Option<&Block> = inventory
            .slots
            .get(slot_index)
            .and_then(|slot| slot.icon_only_if_intrinsic())
        else {
            // No slot to render at this position.
            continue;
        };

        let mut icon_evaluated = {
            let _recursion_scope = block::Budget::recurse(&filter.budget)?;
            // this is the wrong cost value but it doesn't matter
            icon.evaluate_impl(filter)?
                .finish(icon.clone(), filter.budget.get().to_cost())
        };

        // TODO(inventory): We should be downsampling the icons (or more precisely,
        // asking evaluation to generate a lower resolution as per `config.icon_resolution`).
        // For now, we just always generate the resolution-1 form.
        let icon_voxel = Evoxel::from_block(&icon_evaluated);
        icon_evaluated.voxels = Evoxels::Many(
            config.icon_resolution,
            Vol::repeat(placed_icon_bounds, icon_voxel),
        );

        input = evaluate_composition(
            icon_evaluated.into(),
            input,
            CompositeOperator::Over,
            filter,
        )?;
    }

    Ok(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{EvaluatedBlock, Resolution::*};
    use crate::content::{make_slab, make_some_blocks};
    use crate::math::Rgba;
    use crate::space::Space;
    use crate::universe::Universe;
    use pretty_assertions::assert_eq;
    use BlockCollision::{Hard, None as CNone};
    use CompositeOperator::{Atop, In, Over};

    // --- Helpers ---

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
    fn bounding_volume_combination() {
        let universe = &mut Universe::new();
        // Two spaces for blocks that overlap in Venn diagram fashion
        let bounds1 = GridAab::from_lower_size([0, 0, 0], [2, 1, 1]);
        let bounds2 = GridAab::from_lower_size([1, 0, 0], [2, 1, 1]);
        let space1 = universe.insert_anonymous(Space::builder(bounds1).build());
        let space2 = universe.insert_anonymous(Space::builder(bounds2).build());
        let block1 = Block::builder().voxels_handle(R4, space1).build();
        let block2 = Block::builder().voxels_handle(R4, space2).build();

        let union = GridAab::from_lower_size([0, 0, 0], [3, 1, 1]);
        let intersection = GridAab::from_lower_size([1, 0, 0], [1, 1, 1]);

        assert_eq!(
            eval_compose(&block1, Over, &block2).voxels_bounds(),
            union,
            "Over"
        );
        assert_eq!(
            eval_compose(&block1, In, &block2).voxels_bounds(),
            intersection,
            "In"
        );
        assert_eq!(
            eval_compose(&block1, Atop, &block2).voxels_bounds(),
            bounds2,
            "Atop"
        );
    }

    #[test]
    fn bounding_volume_when_one_is_air() {
        let universe = &mut Universe::new();
        let slab = make_slab(universe, 1, R2);
        let slab_bounds = slab.evaluate().unwrap().voxels_bounds();

        assert_eq!(
            eval_compose(&slab, Over, &AIR).voxels_bounds(),
            slab_bounds,
            "Over",
        );
        assert_eq!(
            eval_compose(&slab, In, &AIR).voxels_bounds(),
            GridAab::ORIGIN_EMPTY,
            "In",
        );
        assert_eq!(
            eval_compose(&slab, Atop, &AIR).voxels_bounds(),
            GridAab::ORIGIN_EMPTY,
            "Atop AIR",
        );
        assert_eq!(
            eval_compose(&AIR, Atop, &slab).voxels_bounds(),
            slab_bounds,
            "AIR Atop",
        );
    }

    /// Test each operator’s treatment of input blocks’ individual voxels (not attributes).
    mod voxel {
        use super::*;

        #[test]
        fn over_silly_floats() {
            // We just want to see this does not panic on NaN.
            Over.blend_evoxel(
                evcolor(Rgba::new(2e25, 2e25, 2e25, 2e25)),
                evcolor(Rgba::new(2e25, 2e25, 2e25, 2e25)),
            );
        }

        #[test]
        fn over_emission() {
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
        fn over_collision() {
            assert_blend(evcoll(Hard), Over, evcoll(Hard), evcoll(Hard));
            assert_blend(evcoll(CNone), Over, evcoll(CNone), evcoll(CNone));
            assert_blend(evcoll(Hard), Over, evcoll(CNone), evcoll(Hard));
            assert_blend(evcoll(CNone), Over, evcoll(Hard), evcoll(Hard));
        }

        #[test]
        fn in_emission() {
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
        fn in_collision() {
            assert_blend(evcoll(Hard), In, evcoll(Hard), evcoll(Hard));
            assert_blend(evcoll(CNone), In, evcoll(CNone), evcoll(CNone));
            assert_blend(evcoll(Hard), In, evcoll(CNone), evcoll(CNone));
            assert_blend(evcoll(CNone), In, evcoll(Hard), evcoll(CNone));
        }

        #[test]
        fn atop_color() {
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
        fn atop_emission() {
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
    }

    /// Test each operator’s treatment of input blocks’ attributes (not voxels).
    mod attributes {
        use super::{assert_eq, *};

        #[test]
        fn selectable_if_either_is_selectable() {
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
        #[ignore = "TODO: implement operation merge to make this pass"]
        fn activation_action_is_composed() {
            let [result1, result2] = make_some_blocks();
            let b1 = &Block::builder()
                .color(Rgba::WHITE)
                .activation_action(Operation::Become(result1.clone()))
                .build();
            let b2 = &Block::builder()
                .color(Rgba::WHITE)
                .activation_action(Operation::Become(result2.clone()))
                .build();

            assert_eq!(
                eval_compose(b1, Over, b2).attributes.activation_action,
                Some(Operation::Become(
                    result2.with_modifier(Composite::new(result1, Over))
                ))
            );

            // TODO: add other tests for when there is only one operation
        }
    }

    /// Operations on the `Composite` modifier or block themselves.
    mod ops {
        use super::{assert_eq, *};

        #[test]
        fn compose_or_replace_source_is_air() {
            let [block] = make_some_blocks();
            assert_eq!(
                Composite::new(AIR, Over).compose_or_replace(block.clone()),
                block
            );
        }

        #[test]
        fn compose_or_replace_destination_is_air() {
            let [block] = make_some_blocks();
            assert_eq!(
                Composite::new(block.clone(), Over).compose_or_replace(AIR),
                block
            );
        }

        #[test]
        fn unspecialize_no() {
            let [b1, b2] = make_some_blocks();
            let composed = b1.clone().with_modifier(Composite::new(b2.clone(), Over));
            assert_eq!(composed.unspecialize(), vec![composed]);
        }

        #[test]
        fn unspecialize_yes() {
            let [b1, b2] = make_some_blocks();
            let composed = b1
                .clone()
                .with_modifier(Composite::new(b2.clone(), Over).with_disassemblable());
            assert_eq!(composed.unspecialize(), vec![b2, b1]);
        }
    }
}
