use crate::block;
use crate::math::Rgb;
use crate::universe;

/// Data for [`Modifier::Quote`](block::Modifier::Quote).
/// Suppresses all behaviors of the [`Block`](block::Block) that might affect the space
/// around it, (or itself).
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct Quote {
    /// If true, also suppress light and sound effects.
    pub suppress_ambient: bool,
}

impl Quote {
    /// Construct an instance of [`Quote`], the same as [`Quote::default()`].
    pub fn new() -> Self {
        Self::default()
    }

    pub(in crate::block) fn evaluate(
        &self,
        value: block::MinEval,
        filter: &block::EvalFilter<'_>,
    ) -> Result<block::MinEval, block::InEvalError> {
        // Fully destructure so we don't forget anything.
        let &Quote { suppress_ambient } = self;
        let (
            block::BlockAttributes {
                display_name,
                selectable,
                inventory,
                mut ambient_sound,
                rotation_rule,
                mut placement_action,
                mut tick_action,
                mut activation_action,
                animation_hint,
            },
            mut voxels,
        ) = value.into_parts();

        if !filter.skip_eval {
            // All `Operation`s must be disabled.
            tick_action = None;
            placement_action = None;
            activation_action = None;

            // If `suppress_ambient`, then avoid the block having any light or sound emission.
            if suppress_ambient {
                block::Budget::decrement_voxels(&filter.budget, voxels.count())?;
                for voxel in voxels.as_vol_mut().as_linear_mut().iter_mut() {
                    voxel.emission = Rgb::ZERO;
                }

                ambient_sound = crate::sound::Ambient::SILENT;
            }
        }

        Ok(block::MinEval::new(
            block::BlockAttributes {
                display_name,
                selectable,
                inventory,
                ambient_sound,
                rotation_rule,
                placement_action,
                tick_action,
                activation_action,
                animation_hint,
            },
            voxels,
        ))
    }
}

impl From<Quote> for block::Modifier {
    fn from(value: Quote) -> Self {
        block::Modifier::Quote(value)
    }
}

impl universe::VisitHandles for Quote {
    fn visit_handles(&self, _visitor: &mut dyn universe::HandleVisitor) {
        let Quote {
            suppress_ambient: _,
        } = self;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Block;
    use crate::math::Rgba;
    use crate::{op, time};
    use pretty_assertions::assert_eq;

    #[test]
    fn quote_evaluation() {
        let light = Rgb::new(1.0, 2.0, 3.0);

        let block = Block::builder()
            .color(Rgba::WHITE)
            .light_emission(light)
            .placement_action(block::PlacementAction {
                operation: op::Operation::Become(block::AIR),
                in_front: true,
            })
            .tick_action(block::TickAction {
                operation: op::Operation::Become(block::AIR),
                schedule: time::Schedule::EVERY_TICK,
            })
            .activation_action(op::Operation::Become(block::AIR))
            .build();

        assert_eq!(
            eval_without_metadata(&block.clone().with_modifier(Quote {
                suppress_ambient: false,
            })),
            eval_without_metadata(
                &Block::builder().color(Rgba::WHITE).light_emission(light).build()
            ),
            "suppress_ambient = false"
        );

        assert_eq!(
            eval_without_metadata(&block.with_modifier(Quote {
                suppress_ambient: true,
            })),
            eval_without_metadata(&Block::builder().color(Rgba::WHITE).build()),
            "suppress_ambient = true"
        );
    }

    // TODO: there should be a less one-off way to express this narrowing of EvaluatedBlock
    // to only the data and not the metadata (cost and original block).
    // `MinEval` is not quite it.
    #[inline(never)]
    fn eval_without_metadata(block: &Block) -> (block::BlockAttributes, block::Evoxels) {
        let evaluated = block.evaluate(universe::ReadTicket::stub()).unwrap();
        (evaluated.attributes().clone(), evaluated.voxels().clone())
    }
}
