use core::fmt;
use core::ops;

#[cfg(doc)]
use crate::planar::Vertex;

/// A bit-mask-like enum identifying which of the four quadrants around a [`Vertex`] should be
/// covered by triangles.
///
/// The orientation/identification of these quadrants is defined relative to
/// [`Basis`][super::Basis], rather than in any fixed relationship to the vertex coordinates.
///
/// <pre style="line-height: 1.0em">↑ perpendicular direction
/// ┆
/// ┆    ↖↑           ┆           ↑↗
/// ┆    ← Mask::BSFP ┃ Mask::FSFP →
/// ┆                 ┃
/// ┆ ···━━━━━━━━━━━━━╋━━━━━━━━━━━━━···
/// ┆                 ┃
/// ┆    ← Mask::BSBP ┃ Mask::FSBP →
/// ┆    ↙↓           ┆           ↓↘
/// ┆
/// └┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄→ sweep direction</pre>
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[repr(u8)]
#[expect(clippy::exhaustive_enums)]
pub enum Mask {
    // Bit values corresponding to quadrants:
    // * 0b0001 = 1 = forward sweep & forward perpendicular
    // * 0b0010 = 2 = forward sweep & backward perpendicular
    // * 0b0100 = 4 = backward sweep & forward perpendicular
    // * 0b1000 = 8 = backward sweep & backward perpendicular
    //
    // Implementation note: If it were possible to have a “u4” type containing bitflags, that would
    // be a much simpler definition. However, it is not, and we want the exhaustiveness we get from
    // using an enum with 16 variants instead of a u8 with unused values.
    //
    /// No coverage.
    Empty = 0b0000,
    /// Coverage in the quadrant which is forward of the vertex in the sweep direction,
    /// and forward of the vertex in the perpendicular direction; an outside corner.
    Fsfp = 0b0001,
    /// Coverage in the quadrant which is forward of the vertex in the sweep direction,
    /// and backward of the vertex in the perpendicular direction; an outside corner.
    Fsbp = 0b0010,
    /// Coverage in the forward sweep half-plane; a mid-edge vertex.
    Fs = 0b0011,
    /// Coverage in the quadrant which is backward of the vertex in the sweep direction,
    /// and forward of the vertex in the perpendicular direction; an outside corner.
    Bsfp = 0b0100,
    /// Coverage in the forward perpendicular half-plane; a mid-edge vertex.
    Fp = 0b0101,
    /// Coverage in the checkerboard pattern covering [`Fsbp`][Self::Fsbp] and [`Bsfp`][Self::Bsfp].
    Fbbf = 0b0110,
    /// Coverage of all quadrants except [`Bsbp`][Self::Bsbp]; an inside corner.
    NotBsbp = 0b0111,
    /// Coverage in the quadrant which is backward of the vertex in the sweep direction,
    /// and backward of the vertex in the perpendicular direction; an outside corner.
    Bsbp = 0b1000,
    /// Coverage in the checkerboard pattern covering [`Fsfp`][Self::Fsfp] and [`Bsbp`][Self::Bsbp].
    Ffbb = 0b1001,
    /// Coverage in the backward perpendicular half-plane; a mid-edge vertex.
    Bp = 0b1010,
    /// Coverage of all quadrants except [`Bsfp`][Self::Bsfp]; an inside corner.
    NotBsfp = 0b1011,
    /// Coverage in the backward sweep half-plane; a mid-edge vertex.
    Bs = 0b1100,
    /// Coverage of all quadrants except [`Fsbp`][Self::Fsbp]; an inside corner.
    NotFsbp = 0b1101,
    /// Coverage of all quadrants except [`Fsfp`][Self::Fsfp]; an inside corner.
    NotFsfp = 0b1110,
    /// Coverage of all quadrants; a mid-face vertex.
    All = 0b1111,
}

impl Mask {
    #[inline]
    pub(crate) fn contains_any_of(self, test: Mask) -> bool {
        self & test != Self::Empty
    }

    #[inline]
    fn from_flags(flags: u8) -> Self {
        // This match should compile down to at most a bounds check.
        match flags {
            0b0000 => Self::Empty,
            0b0001 => Self::Fsfp,
            0b0010 => Self::Fsbp,
            0b0011 => Self::Fs,
            0b0100 => Self::Bsfp,
            0b0101 => Self::Fp,
            0b0110 => Self::Fbbf,
            0b0111 => Self::NotBsbp,
            0b1000 => Self::Bsbp,
            0b1001 => Self::Ffbb,
            0b1010 => Self::Bp,
            0b1011 => Self::NotBsfp,
            0b1100 => Self::Bs,
            0b1101 => Self::NotFsbp,
            0b1110 => Self::NotFsfp,
            0b1111 => Self::All,
            _ => panic!("computed an invalid Mask value"),
        }
    }

    /// Returns whether this vertex is a corner (or two corners touching), thus forming a
    /// significant part of the shape.
    //---
    // TODO: make this public but only after adding some tests.
    pub(crate) fn is_corner(self) -> bool {
        match self {
            // No edges
            Mask::Empty => false,
            Mask::All => false,

            // Straight lines
            Mask::Fs => false,
            Mask::Fp => false,
            Mask::Bp => false,
            Mask::Bs => false,

            // Inside corners
            Mask::Fsfp => true,
            Mask::Fsbp => true,
            Mask::Bsfp => true,
            Mask::Bsbp => true,

            // Outside corners
            Mask::NotBsbp => true,
            Mask::NotBsfp => true,
            Mask::NotFsbp => true,
            Mask::NotFsfp => true,

            // Checkerboards
            Mask::Fbbf => true,
            Mask::Ffbb => true,
        }
    }
}

/// When formatted, [`Mask`] is depicted as if the sweep direction is rightward
/// and the perpendicular direction is upward.
/// Alignment and width options are respected.
//---
// Note: Why so many features?
// So that anyone stuck debugging why masks are wrong has plenty of textual
// and visual information to work with (but also for fun).
impl fmt::Debug for Mask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[rustfmt::skip]
        let (graphic, name) = match self {
            Self::Empty   => (" ", "EMPTY",                    ),
            Self::Fsfp    => ("▝", "FSFP",                     ),
            Self::Fsbp    => ("▗", "FSBP",                     ),
            Self::Fs      => ("▐", "FSFP | FSBP",              ),
            Self::Bsfp    => ("▘", "BSFP",                     ),
            Self::Fp      => ("▀", "FSFP | BSFP",              ),
            Self::Fbbf    => ("▚", "FSBP | BSFP",              ),
            Self::NotBsbp => ("▜", "FSFP | FSBP | BSFP",       ),
            Self::Bsbp    => ("▖", "BSBP",                     ),
            Self::Ffbb    => ("▞", "FSFP | BSBP",              ),
            Self::Bp      => ("▄", "FSBP | BSBP",              ),
            Self::NotBsfp => ("▟", "FSFP | FSBP | BSBP",       ),
            Self::Bs      => ("▌", "BSFP | BSBP",              ),
            Self::NotFsbp => ("▛", "FSFP | BSFP | BSBP",       ),
            Self::NotFsfp => ("▙", "FSBP | BSFP | BSBP",       ),
            Self::All     => ("█", "FSFP | FSBP | BSFP | BSBP",),
        };
        match f.align() {
            Some(fmt::Alignment::Right) => {
                f.pad(name)?;
                write!(f, " ({graphic})")?;
            }
            // otherwise use left alignment (center is not supported)
            _ => {
                write!(f, "({graphic}) ")?;
                f.pad(name)?;
            }
        }
        Ok(())
    }
}

impl ops::BitAnd for Mask {
    type Output = Self;
    #[inline]
    fn bitand(self, rhs: Self) -> Self::Output {
        Self::from_flags(self as u8 & rhs as u8)
    }
}
impl ops::BitOr for Mask {
    type Output = Self;
    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self::from_flags(self as u8 | rhs as u8)
    }
}
impl ops::BitXor for Mask {
    type Output = Self;
    #[inline]
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::from_flags(self as u8 ^ rhs as u8)
    }
}

impl ops::Not for Mask {
    type Output = Self;
    #[inline]
    fn not(self) -> Self::Output {
        Self::from_flags(self as u8 ^ 0b1111)
    }
}

impl ops::BitAndAssign for Mask {
    #[inline]
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}
impl ops::BitOrAssign for Mask {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}
impl ops::BitXorAssign for Mask {
    #[inline]
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::format;
    use alloc::string::String;

    fn all_masks() -> impl Iterator<Item = Mask> {
        // We could have `Mask` implement `Exhaust` instead, but I can’t think of a reason why
        // having that in the public API would ever be useful for anything.
        (0..=0b1111).map(Mask::from_flags)
    }

    #[test]
    fn debug() {
        fn f(mask: Mask) -> String {
            format!("{mask:>11?} {mask:<11?}")
        }
        assert_eq!(f(Mask::Empty), "      EMPTY ( ) ( ) EMPTY      ");
        assert_eq!(f(Mask::Fsfp), "       FSFP (▝) (▝) FSFP       ");
        assert_eq!(f(Mask::Fsbp), "       FSBP (▗) (▗) FSBP       ");
        assert_eq!(f(Mask::Bsfp), "       BSFP (▘) (▘) BSFP       ");
        assert_eq!(f(Mask::Bsbp), "       BSBP (▖) (▖) BSBP       ");
        assert_eq!(f(Mask::Ffbb), "FSFP | BSBP (▞) (▞) FSFP | BSBP");
    }

    #[test]
    fn bitwise_ops_equivalence() {
        for a in all_masks() {
            assert_eq!((!a) as u8, a as u8 ^ 0b1111);
            for b in all_masks() {
                assert_eq!((a & b) as u8, a as u8 & b as u8);
                assert_eq!((a | b) as u8, a as u8 | b as u8);
                assert_eq!((a ^ b) as u8, a as u8 ^ b as u8);
                {
                    let mut mut_a = a;
                    mut_a &= b;
                    assert_eq!(mut_a, a & b);
                }
                {
                    let mut mut_a = a;
                    mut_a |= b;
                    assert_eq!(mut_a, a | b);
                }
                {
                    let mut mut_a = a;
                    mut_a ^= b;
                    assert_eq!(mut_a, a ^ b);
                }
            }
        }
    }

    /// Test that the variant names corresponding to multiple bits match the combinations of those
    /// bits.
    #[test]
    fn combined_value_consistency() {
        assert_eq!(Mask::Fs, Mask::Fsfp | Mask::Fsbp);
        assert_eq!(Mask::Bs, Mask::Bsfp | Mask::Bsbp);
        assert_eq!(Mask::Fp, Mask::Fsfp | Mask::Bsfp);
        assert_eq!(Mask::Bp, Mask::Fsbp | Mask::Bsbp);
        assert_eq!(Mask::Fbbf, Mask::Fsbp | Mask::Bsfp);
        assert_eq!(Mask::Ffbb, Mask::Fsfp | Mask::Bsbp);
        assert_eq!(Mask::NotBsbp, !Mask::Bsbp);
        assert_eq!(Mask::NotBsfp, !Mask::Bsfp);
        assert_eq!(Mask::NotFsbp, !Mask::Fsbp);
        assert_eq!(Mask::NotFsfp, !Mask::Fsfp);
        assert_eq!(Mask::All, !Mask::Empty);
    }
}
