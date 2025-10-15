use core::ops;

// Note: Public documentation for this is in its re-export from `all_is_cubes::block`.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd, exhaust::Exhaust)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[allow(missing_docs)]
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
}
use core::fmt;

use crate::math::GridCoordinate;

impl Resolution {
    /// The maximum available resolution.
    pub const MAX: Resolution = Resolution::R128;

    /// Returns the [`Resolution`] that’s twice this one, or [`None`] at the limit.
    #[inline]
    pub const fn double(self) -> Option<Self> {
        match self {
            Self::R1 => Some(Self::R2),
            Self::R2 => Some(Self::R4),
            Self::R4 => Some(Self::R8),
            Self::R8 => Some(Self::R16),
            Self::R16 => Some(Self::R32),
            Self::R32 => Some(Self::R64),
            Self::R64 => Some(Self::R128),
            Self::R128 => None,
        }
    }

    /// Returns the [`Resolution`] that’s half this one, or [`None`] if `self` is
    /// [`R1`](Self::R1).
    #[inline]
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
        }
    }

    #[inline]
    #[doc(hidden)] // interim while waiting for better const-eval support in Rust
    pub const fn to_grid(self) -> GridCoordinate {
        1 << self as GridCoordinate
    }

    /// Returns the logarithm base 2 of this resolution.
    ///
    /// This is always an exact integer value; all resolutions are powers of 2.
    ///
    /// # Example
    ///
    /// ```
    /// # use all_is_cubes_base::resolution::Resolution;
    ///
    /// assert_eq!(Resolution::R1.log2(), 0);
    /// assert_eq!(Resolution::R16.log2(), 4);
    /// ```
    #[inline]
    pub const fn log2(self) -> u8 {
        self as u8
    }

    /// Returns the reciprocal of this resolution; that is, the scale factor from
    /// voxels to blocks of this resolution.
    ///
    /// Equivalent to `f32::from(self).recip()` but does not perform division.
    #[inline]
    pub const fn recip_f32(self) -> f32 {
        match self {
            Self::R1 => const { 1.0f32.recip() },
            Self::R2 => const { 2.0f32.recip() },
            Self::R4 => const { 4.0f32.recip() },
            Self::R8 => const { 8.0f32.recip() },
            Self::R16 => const { 16.0f32.recip() },
            Self::R32 => const { 32.0f32.recip() },
            Self::R64 => const { 64.0f32.recip() },
            Self::R128 => const { 128.0f32.recip() },
        }
    }

    /// Returns the reciprocal of this resolution; that is, the scale factor from
    /// voxels to blocks of this resolution.
    ///
    /// Equivalent to `f64::from(self).recip()` but does not perform division.
    #[inline]
    pub const fn recip_f64(self) -> f64 {
        match self {
            Self::R1 => const { 1.0f64.recip() },
            Self::R2 => const { 2.0f64.recip() },
            Self::R4 => const { 4.0f64.recip() },
            Self::R8 => const { 8.0f64.recip() },
            Self::R16 => const { 16.0f64.recip() },
            Self::R32 => const { 32.0f64.recip() },
            Self::R64 => const { 64.0f64.recip() },
            Self::R128 => const { 128.0f64.recip() },
        }
    }
}

impl fmt::Debug for Resolution {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        GridCoordinate::from(*self).fmt(f)
    }
}
impl fmt::Display for Resolution {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        GridCoordinate::from(*self).fmt(f)
    }
}

macro_rules! impl_try_from {
    ($t:ty) => {
        impl TryFrom<$t> for Resolution {
            type Error = IntoResolutionError<$t>;
            #[inline]
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
                    _ => Err(IntoResolutionError(value)),
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
    /// # mod all_is_cubes { pub mod block { pub use all_is_cubes_base::resolution::Resolution; } }
    /// use all_is_cubes::block::Resolution;
    ///
    /// assert_eq!(64, i32::from(Resolution::R64));
    /// ```
    #[inline]
    fn from(r: Resolution) -> i32 {
        1 << (r as i32)
    }
}
impl From<Resolution> for u16 {
    #[inline]
    fn from(r: Resolution) -> u16 {
        1 << (r as u16)
    }
}
impl From<Resolution> for u32 {
    #[inline]
    fn from(r: Resolution) -> u32 {
        1 << (r as u32)
    }
}
impl From<Resolution> for usize {
    #[inline]
    fn from(r: Resolution) -> usize {
        1 << (r as usize)
    }
}
impl From<Resolution> for f32 {
    #[inline]
    fn from(r: Resolution) -> f32 {
        u16::from(r).into()
    }
}
impl From<Resolution> for f64 {
    #[inline]
    fn from(r: Resolution) -> f64 {
        u16::from(r).into()
    }
}

impl ops::Mul<Resolution> for Resolution {
    type Output = Option<Resolution>;

    #[inline]
    fn mul(self, rhs: Resolution) -> Self::Output {
        // not the most efficient way to implement this, but straightforward
        Self::try_from(u32::from(self) * u32::from(rhs)).ok()
    }
}

impl ops::Div<Resolution> for Resolution {
    type Output = Option<Resolution>;

    #[inline]
    fn div(self, rhs: Resolution) -> Self::Output {
        // not the most efficient way to implement this, but straightforward
        Self::try_from(u32::from(self) / u32::from(rhs)).ok()
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Resolution {
    #[allow(clippy::missing_inline_in_public_items)]
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        u16::from(*self).serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Resolution {
    #[allow(clippy::missing_inline_in_public_items)]
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        u16::deserialize(deserializer)?
            .try_into()
            .map_err(serde::de::Error::custom)
    }
}

/// Error type produced by [`TryFrom`] for [`Resolution`], and deserializing resolutions,
/// when the number is not a permitted resolution value.
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub struct IntoResolutionError<N>(N);

impl<N: fmt::Display + fmt::Debug> core::error::Error for IntoResolutionError<N> {}

impl<N: fmt::Display> fmt::Display for IntoResolutionError<N> {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{number} is not a permitted resolution; must be a power of 2 between 1 and 127",
            number = self.0
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Resolution::*;
    use alloc::vec::Vec;
    use exhaust::Exhaust as _;

    const RS: [Resolution; 8] = [R1, R2, R4, R8, R16, R32, R64, R128];

    #[test]
    fn test_list_is_complete() {
        assert_eq!(
            Vec::from(RS),
            Resolution::exhaust().collect::<Vec<Resolution>>()
        );
    }

    #[test]
    fn max_is_max() {
        assert_eq!(Resolution::MAX, *RS.last().unwrap());
        assert_eq!(None, Resolution::MAX.double());
    }

    #[test]
    fn resolution_steps() {
        for i in 0..RS.len() - 1 {
            assert_eq!(RS[i].double().unwrap(), RS[i + 1]);
            assert_eq!(RS[i + 1].halve().unwrap(), RS[i]);
        }
    }

    #[test]
    fn resolution_values() {
        assert_eq!(RS.map(i32::from), [1, 2, 4, 8, 16, 32, 64, 128]);
        assert_eq!(RS.map(u16::from), [1, 2, 4, 8, 16, 32, 64, 128]);
        assert_eq!(RS.map(u32::from), [1, 2, 4, 8, 16, 32, 64, 128]);
        assert_eq!(RS.map(usize::from), [1, 2, 4, 8, 16, 32, 64, 128]);
    }

    #[test]
    fn mul() {
        assert_eq!(R4 * R2, Some(R8));
        assert_eq!(R128 * R2, None);
        assert_eq!(R2 * R128, None);
    }

    #[test]
    fn div() {
        assert_eq!(R8 / R2, Some(R4));
        assert_eq!(R128 / R128, Some(R1));
        assert_eq!(R1 / R2, None);
        assert_eq!(R64 / R128, None);
    }

    #[test]
    fn recip() {
        for resolution in RS {
            assert_eq!(resolution.recip_f32(), f32::from(resolution).recip());
            assert_eq!(resolution.recip_f64(), f64::from(resolution).recip());
        }
    }
}
