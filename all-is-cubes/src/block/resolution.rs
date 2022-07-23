#[cfg(doc)]
use crate::block::{EvaluatedBlock, Primitive};

/// Scale factor between a [recursive block](Primitive::Recur) and its component voxels.
///
/// This resolution cubed is the number of voxels making up a block.
///
/// Resolutions are always powers of 2. This ensures that the arithmetic is well-behaved
/// (no division by zero, exact floating-point representation, and the potential of
/// fixed-point representation),
/// and that it is always possible to subdivide a block further (up to the limit) without
/// shifting the existing voxel boundaries.
///
/// Note that while quite high resolutions are permitted, this does not mean that it is
/// practical to construct a full block at that resolution. For example, 256 × 256 × 256
/// = 16,777,216 voxels, occupying 335 MB when [evaluated](EvaluatedBlock). Higher
/// resolutions are permitted so that *thin* blocks (e.g. 256 × 256 × 1) can display high
/// resolution text and other 2D images.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[repr(u8)]
#[non_exhaustive] // unlikely to change but on general principle: not supposed to match this
pub enum Resolution {
    R1 = 0,
    R2 = 1,
    R4 = 2,
    R8 = 3,
    R16 = 4,
    R32 = 5,
    R64 = 6,
    R128 = 7,
    R256 = 8,
}
use std::fmt;

use crate::math::GridCoordinate;

impl Resolution {
    /// Returns the [`Resolution`] that’s twice this one, or [`None`] at the limit.
    pub const fn double(self) -> Option<Self> {
        match self {
            Self::R1 => Some(Self::R2),
            Self::R2 => Some(Self::R4),
            Self::R4 => Some(Self::R8),
            Self::R8 => Some(Self::R16),
            Self::R16 => Some(Self::R32),
            Self::R32 => Some(Self::R64),
            Self::R64 => Some(Self::R128),
            Self::R128 => Some(Self::R256),
            Self::R256 => None,
        }
    }

    /// Returns the [`Resolution`] that’s half this one, or [`None`] if `self` is
    /// [`R1`](Self::R1).
    pub const fn halve(self) -> Option<Self> {
        match self {
            Self::R1 => None,
            Self::R2 => Some(Self::R1),
            Self::R4 => Some(Self::R2),
            Self::R8 => Some(Self::R4),
            Self::R16 => Some(Self::R8),
            Self::R32 => Some(Self::R16),
            Self::R64 => Some(Self::R32),
            Self::R128 => Some(Self::R64),
            Self::R256 => Some(Self::R128),
        }
    }

    #[doc(hidden)] // interim while waiting for better const-eval support in Rust
    pub const fn to_grid(self) -> GridCoordinate {
        1 << self as GridCoordinate
    }
}

impl fmt::Debug for Resolution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        GridCoordinate::from(*self).fmt(f)
    }
}
impl fmt::Display for Resolution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        GridCoordinate::from(*self).fmt(f)
    }
}

macro_rules! impl_try_from {
    ($t:ty) => {
        impl TryFrom<$t> for Resolution {
            type Error = (); // TODO
            fn try_from(value: $t) -> Result<Self, Self::Error> {
                match value {
                    1 => Ok(Self::R1),
                    2 => Ok(Self::R2),
                    4 => Ok(Self::R4),
                    8 => Ok(Self::R8),
                    16 => Ok(Self::R16),
                    32 => Ok(Self::R32),
                    64 => Ok(Self::R64),
                    128 => Ok(Self::R128),
                    256 => Ok(Self::R256),
                    _ => Err(()),
                }
            }
        }
    };
}
impl_try_from!(i16);
impl_try_from!(i32);
impl_try_from!(i64);
impl_try_from!(i128);
impl_try_from!(isize);
impl_try_from!(u16);
impl_try_from!(u32);
impl_try_from!(u64);
impl_try_from!(u128);
impl_try_from!(usize);

impl From<Resolution> for i32 {
    /// ```
    /// use all_is_cubes::block::Resolution;
    ///
    /// assert_eq!(64, i32::from(Resolution::R64));
    /// ```
    fn from(r: Resolution) -> i32 {
        1 << (r as i32)
    }
}
impl From<Resolution> for u16 {
    fn from(r: Resolution) -> u16 {
        1 << (r as u16)
    }
}
impl From<Resolution> for u32 {
    fn from(r: Resolution) -> u32 {
        1 << (r as u32)
    }
}
impl From<Resolution> for usize {
    fn from(r: Resolution) -> usize {
        1 << (r as usize)
    }
}
impl From<Resolution> for f32 {
    fn from(r: Resolution) -> f32 {
        u16::from(r).into()
    }
}
impl From<Resolution> for f64 {
    fn from(r: Resolution) -> f64 {
        u16::from(r).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Resolution::*;

    const RS: [Resolution; 9] = [R1, R2, R4, R8, R16, R32, R64, R128, R256];

    #[test]
    fn resolution_steps() {
        for i in 0..RS.len() - 1 {
            assert_eq!(RS[i].double().unwrap(), RS[i + 1]);
            assert_eq!(RS[i + 1].halve().unwrap(), RS[i]);
        }
    }

    #[test]
    fn resolution_values() {
        assert_eq!(RS.map(i32::from), [1, 2, 4, 8, 16, 32, 64, 128, 256]);
        assert_eq!(RS.map(u16::from), [1, 2, 4, 8, 16, 32, 64, 128, 256]);
        assert_eq!(RS.map(u32::from), [1, 2, 4, 8, 16, 32, 64, 128, 256]);
        assert_eq!(RS.map(usize::from), [1, 2, 4, 8, 16, 32, 64, 128, 256]);
    }
}
