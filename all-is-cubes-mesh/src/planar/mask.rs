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
// TODO: custom Debug
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
