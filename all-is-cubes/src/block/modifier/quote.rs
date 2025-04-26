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
        let &Quote { suppress_ambient } = self;
        let (mut attributes, mut voxels) = value.into_parts();

        attributes.tick_action = None;
        if suppress_ambient && !filter.skip_eval {
            block::Budget::decrement_voxels(&filter.budget, voxels.count())?;
            for voxel in voxels.as_vol_mut().as_linear_mut().iter_mut() {
                voxel.emission = Rgb::ZERO;
            }
        }

        Ok(block::MinEval::new(attributes, voxels))
    }

    // The evaluation implementation is simple enough that it's in `Modifier::evaluate`
    // directly.
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
    use crate::block::{Block, Modifier};
    use crate::math::Rgba;
    use pretty_assertions::assert_eq;

    #[test]
    fn quote_evaluation() {
        let read_ticket = universe::ReadTicket::stub();
        let l = Rgb::new(1.0, 2.0, 3.0);
        let mut block = Block::builder()
            .color(Rgba::WHITE)
            .light_emission(l)
            .build();
        assert_eq!(
            block
                .evaluate(read_ticket)
                .unwrap()
                .voxels
                .single_voxel()
                .unwrap()
                .emission,
            l
        );
        block.modifiers_mut().push(Modifier::Quote(Quote {
            suppress_ambient: true,
        }));
        assert_eq!(
            block
                .evaluate(read_ticket)
                .unwrap()
                .voxels
                .single_voxel()
                .unwrap()
                .emission,
            Rgb::ZERO
        );
    }
}
