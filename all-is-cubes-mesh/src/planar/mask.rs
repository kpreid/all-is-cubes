use core::fmt;
use core::ops;

#[cfg(doc)]
use crate::planar::Vertex;

/// A bit-mask identifying which of the four quadrants around a [`Vertex`] should be covered by
/// triangles.
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
pub struct Mask {
    /// Bit values corresponding to quadrants:
    /// * 1 = forward sweep & forward perpendicular
    /// * 2 = forward sweep & backward perpendicular
    /// * 4 = backward sweep & forward perpendicular
    /// * 8 = backward sweep & backward perpendicular
    flags: u8,
}

impl Mask {
    /// No quadrants.
    pub const EMPTY: Self = Self { flags: 0 };
    /// The quadrant which is forward of the vertex in the sweep direction,
    /// and forward of the vertex in the perpendicular direction.
    pub const FSFP: Self = Self { flags: 1 };
    /// The quadrant which is forward of the vertex in the sweep direction,
    /// and backward of the vertex in the perpendicular direction.
    pub const FSBP: Self = Self { flags: 2 };
    /// The quadrant which is backward of the vertex in the sweep direction,
    /// and forward of the vertex in the perpendicular direction.
    pub const BSFP: Self = Self { flags: 4 };
    /// The quadrant which is backward of the vertex in the sweep direction,
    /// and backward of the vertex in the perpendicular direction.
    pub const BSBP: Self = Self { flags: 8 };

    #[inline]
    pub(crate) fn contains_any_of(self, test: Mask) -> bool {
        self & test != Self::EMPTY
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
        let name = match self.flags {
            0b0000 => "EMPTY",
            0b0001 => "FSFP",
            0b0010 => "FSBP",
            0b0011 => "FSFP | FSBP",
            0b0100 => "BSFP",
            0b0101 => "FSFP | BSFP",
            0b0110 => "FSBP | BSFP",
            0b0111 => "FSFP | FSBP | BSFP",
            0b1000 => "BSBP",
            0b1001 => "FSFP | BSBP",
            0b1010 => "FSBP | BSBP",
            0b1011 => "FSFP | FSBP | BSBP",
            0b1100 => "BSFP | BSBP",
            0b1101 => "FSFP | BSFP | BSBP",
            0b1110 => "FSBP | BSFP | BSBP",
            0b1111 => "FSFP | FSBP | BSFP | BSBP",
            _ => unreachable!(),
        };
        let graphic = match self.flags {
            0b0000 => " ",
            0b0001 => "▝",
            0b0010 => "▗",
            0b0011 => "▐",
            0b0100 => "▘",
            0b0101 => "▀",
            0b0110 => "▚",
            0b0111 => "▜",
            0b1000 => "▖",
            0b1001 => "▞",
            0b1010 => "▄",
            0b1011 => "▟",
            0b1100 => "▌",
            0b1101 => "▛",
            0b1110 => "▙",
            0b1111 => "█",
            _ => unreachable!(),
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

impl ops::BitOr for Mask {
    type Output = Self;
    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            flags: self.flags | rhs.flags,
        }
    }
}
impl ops::BitAnd for Mask {
    type Output = Self;
    #[inline]
    fn bitand(self, rhs: Self) -> Self::Output {
        Self {
            flags: self.flags & rhs.flags,
        }
    }
}
impl ops::Not for Mask {
    type Output = Self;
    #[inline]
    fn not(self) -> Self::Output {
        Self { flags: !self.flags }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::string::String;

    #[test]
    fn debug() {
        fn f(mask: Mask) -> String {
            format!("{mask:>11?} {mask:<11?}")
        }
        assert_eq!(f(Mask::EMPTY), "      EMPTY ( ) ( ) EMPTY      ");
        assert_eq!(f(Mask::FSFP), "       FSFP (▝) (▝) FSFP       ");
        assert_eq!(f(Mask::FSBP), "       FSBP (▗) (▗) FSBP       ");
        assert_eq!(f(Mask::BSFP), "       BSFP (▘) (▘) BSFP       ");
        assert_eq!(f(Mask::BSBP), "       BSBP (▖) (▖) BSBP       ");
        assert_eq!(
            f(Mask::FSFP | Mask::BSBP),
            "FSFP | BSBP (▞) (▞) FSFP | BSBP"
        );
    }
}
