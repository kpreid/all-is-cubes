use core::fmt;

use crate::block::{Block, Primitive};
use crate::math::{Rgb, Rgb01, Rgba};
use crate::universe;

// -------------------------------------------------------------------------------------------------

/// Data of [`Primitive::Atom`]. The definition of a single [block][Block] that has uniform
/// material properties rather than spatially varying ones; a single voxel.
///
/// All properties of an atom are [intensive properties].
///
/// [intensive properties]: https://en.wikipedia.org/wiki/Intensive_and_extensive_properties
#[derive(Clone, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[expect(clippy::exhaustive_structs)]
pub struct Atom {
    /// The color exhibited by diffuse reflection from this block.
    ///
    /// The RGB components of this color are the *[reflectance]:* the fraction of incoming light
    /// that is reflected rather than absorbed.
    ///
    /// The alpha (<var>α</var>) component of this color specifies the opacity of this block,
    /// that is, the fraction of light that is reflected or absorbed rather than transmitted.
    ///
    /// Whenever <var>α</var> is neither 1 nor 0 (and, trivially, in those cases too),
    /// the reflectance and opacity
    /// should be interpreted as being of **a unit thickness of** this material. Thus, they may be
    /// modified by the length of material through which a light ray passes, either due to
    /// viewing angle or due to using this block as a voxel in a [`Primitive::Recur`].
    ///
    /// This transformation is best understood in terms of _transmittance_ <var>T</var>,
    /// defined as 1 &minus; <var>α</var>.
    /// The transmittance of a given thickness of material, <var>T</var><sub><var>d</var></sub>,
    /// is defined in terms of the transmittance of a unit thickness,
    /// <var>T</var><sub><var>1</var></sub>, as:
    ///
    /// <p style="text-align: center">
    /// <var>T</var><sub><var>d</var></sub> = (<var>T</var><sub>1</sub>)<sup><var>d</var></sup>.
    /// </p>
    ///
    /// Therefore,
    ///
    /// <p style="text-align: center">
    /// <var>α</var><sub>d</sub> =
    /// 1 &minus; (1 &minus; <var>α</var><sub>1</sub>)<sup><var>d</var></sup>.
    /// </p>
    ///
    /// [reflectance]: https://en.wikipedia.org/wiki/Reflectance
    pub color: Rgba,

    /// Light emitted (not reflected) by the block.
    ///
    /// This quantity is the emitted portion of the *[luminance]* of this material, in unspecified
    /// units where 1.0 is the display white level (except for the effects of tone mapping).
    /// In the future this may be redefined in terms of a physical unit, but with the same
    /// dimensions.
    ///
    /// Because we are describing a volume, not a surface, the physical
    /// interpretation of this value depends on the opacity of the material.
    /// If `self.color.alpha()` is 1.0, then this light escaping a surface must have been emitted at
    /// the surface; if the alpha is 0.0, then it must have been emitted throughout the volume; and
    /// in intermediate cases, then the light emitted within the volume must be greater per unit
    /// volume to compensate for internal absorption. Still, these are not distinct cases but form
    /// a continuum.
    ///
    /// The emission <var>E</var><sub><var>d</var></sub> of a particular thickness <var>d</var>
    /// of this material is
    ///
    /// <p style="text-align: center">
    /// <var>E</var><sub><var>d</var></sub> = <var>E</var><sub>1</sub> ·
    ///     ∫<sub>0</sub><sup><var>d</var></sup>
    ///         (<var>T</var><sub>1</sub>)<sup><var>x</var></sup>
    ///     <var>dx</var>
    /// </p>
    ///
    /// where <var>E</var><sub>1</sub> = `self.emission` and
    /// <var>T</var><sub>1</sub> = `1.0 - self.color.alpha()`.
    /// When integrated, this becomes
    ///
    /// <p style="text-align: center">
    /// <var>E</var><sub><var>d</var></sub> = <var>E</var><sub>1</sub> · <var>d</var>
    /// </p>
    ///
    /// when <var>α</var> = 0 (<var>T</var> = 1) and
    ///
    /// <p style="text-align: center">
    /// <var>E</var><sub><var>d</var></sub> = <var>E</var><sub>1</sub> ·
    /// (<var>T</var><sub><var>d</var></sub> - 1) / (<var>T</var><sub>1</sub> - 1)
    /// </p>
    ///
    /// otherwise.
    ///
    ///
    /// [luminance]: https://en.wikipedia.org/wiki/Luminance
    pub emission: Rgb,

    /// The effect on a [`Body`](crate::physics::Body) of colliding with this block.
    ///
    /// The default value is [`Collision::Hard`].
    pub collision: Collision,
}

impl Atom {
    pub(in crate::block) fn rotationally_symmetric(&self) -> bool {
        let Self {
            color: _,
            emission: _,
            collision: _,
        } = self;
        // I'm planning to eventually have non-uniform collision behaviors
        // or visual effects such as normal mapping,
        // at which point this will be sometimes false.
        true
    }
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self {
            color,
            emission,
            collision,
        } = self;
        let mut s = f.debug_struct("Atom");
        s.field("color", &color);
        if emission != Rgb::ZERO {
            s.field("emission", &emission);
        }
        s.field("collision", &collision);
        s.finish()
    }
}

impl universe::VisitHandles for Atom {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        let Self {
            color: _,
            emission: _,
            collision: _,
        } = self;
    }
}

mod conversions_for_atom {
    use super::*;

    impl Atom {
        /// Construct an [`Atom`] with the given reflectance color.
        ///
        /// This function is equivalent to `Atom::from(color)` but it can be used in const contexts.
        pub const fn from_color(color: Rgba) -> Self {
            Atom {
                color,
                emission: Rgb::ZERO,
                collision: Collision::DEFAULT_FOR_FROM_COLOR,
            }
        }
    }

    const impl From<Rgb01> for Atom {
        /// Construct an [`Atom`] with the given reflectance color, and default attributes.
        fn from(color: Rgb01) -> Self {
            Self::from_color(color.with_alpha_one())
        }
    }
    const impl From<Rgba> for Atom {
        /// Construct an [`Atom`] with the given reflectance color, and default attributes.
        fn from(color: Rgba) -> Self {
            Self::from_color(color)
        }
    }

    const impl From<Atom> for Primitive {
        fn from(value: Atom) -> Self {
            Primitive::Atom(value)
        }
    }

    impl From<Atom> for Block {
        fn from(value: Atom) -> Self {
            Block::from_primitive(Primitive::Atom(value))
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Specifies the effect on a [`Body`](crate::physics::Body) of colliding with a
/// `Atom` block or voxel having this property.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Collision {
    /// The atom can be passed through; it is not an obstacle (though intersecting it
    /// might cause other effects not directly part of collision response).
    None,
    /// The atom is a perfectly solid obstacle occupying its entire bounding cube.
    ///
    /// This is the default value used for most blocks.
    Hard,
    // Future values might include bouncy solid, water-like resistance, force fields, etc.
}

impl Collision {
    /// Value used when we are constructing a block from a color with default other
    /// characteristics. This is not just a [`Default`] impl so that if we later decide
    /// that e.g. transparent atoms automatically become non-colliding, we can replace
    /// uses of this constant with that.
    pub(crate) const DEFAULT_FOR_FROM_COLOR: Self = Self::Hard;
}
