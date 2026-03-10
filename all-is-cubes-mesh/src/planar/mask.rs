use core::ops;

/// A bit-mask of quadrants, which, in the context of the triangulator,
/// refers to which of the four quadrants around a vertex should be covered by triangles.
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
    pub const EMPTY: Self = Self { flags: 0 };
    pub const FSFP: Self = Self { flags: 1 };
    pub const FSBP: Self = Self { flags: 2 };
    pub const BSFP: Self = Self { flags: 4 };
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
