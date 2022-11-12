use crate::block;
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
    pub fn new() -> Self {
        Self::default()
    }

    // The evaluation implementation is simple enough that it's in `Modifier::evaluate`
    // directly.
}

impl From<Quote> for block::Modifier {
    fn from(value: Quote) -> Self {
        block::Modifier::Quote(value)
    }
}

impl universe::VisitRefs for Quote {
    fn visit_refs(&self, _visitor: &mut dyn universe::RefVisitor) {
        let Quote {
            suppress_ambient: _,
        } = self;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Block, Modifier};
    use crate::math::{Rgb, Rgba};
    use pretty_assertions::assert_eq;

    #[test]
    fn quote_evaluation() {
        let l = Rgb::new(1.0, 2.0, 3.0);
        let mut block = Block::builder()
            .light_emission(l)
            .color(Rgba::WHITE)
            .build();
        assert_eq!(block.evaluate().unwrap().attributes.light_emission, l);
        block.modifiers_mut().push(Modifier::Quote(Quote {
            suppress_ambient: true,
        }));
        assert_eq!(
            block.evaluate().unwrap().attributes.light_emission,
            Rgb::ZERO
        );
    }
}
