use std::ops;

#[cfg(doc)]
use crate::block::{EvaluatedBlock, Modifier, Primitive};

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
/// practical to routinely use full blocks at that resolution. For example, 64 × 64 × 64
/// = 262,144 voxels, occupying several megabytes just for color data.
/// High resolutions are permitted for special purposes that do not necessarily use the
/// full cube volume:
///
/// * *Thin* blocks (e.g. 128 × 128 × 1) can display high resolution text and other 2D
///   images.
/// * Multi-block structures can be defined using [`Modifier::Zoom`]; their total size
///   is limited by the resolution limit.
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
use std::fmt;

use crate::math::GridCoordinate;

impl Resolution {
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

    fn mul(self, rhs: Resolution) -> Self::Output {
        // not the most efficient way to implement this, but straightforward
        Self::try_from(u32::from(self) * u32::from(rhs)).ok()
    }
}

impl ops::Div<Resolution> for Resolution {
    type Output = Option<Resolution>;

    fn div(self, rhs: Resolution) -> Self::Output {
        // not the most efficient way to implement this, but straightforward
        Self::try_from(u32::from(self) / u32::from(rhs)).ok()
    }
}

impl serde::Serialize for Resolution {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        u16::from(*self).serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Resolution {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        u16::deserialize(deserializer)?
            .try_into()
            .map_err(serde::de::Error::custom)
    }
}

/// Error type produced by [`TryFrom`] for [`Resolution`], and deserializing resolutions,
/// when the number is not a permitted resolution value.
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq, thiserror::Error)]
pub struct IntoResolutionError<N>(N);

impl<N: fmt::Display> fmt::Display for IntoResolutionError<N> {
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

    const RS: [Resolution; 8] = [R1, R2, R4, R8, R16, R32, R64, R128];

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
    fn ser_ok() {
        use serde_json::{json, to_value};
        assert_eq!(to_value(R1).unwrap(), json!(1));
        assert_eq!(to_value(R2).unwrap(), json!(2));
        assert_eq!(to_value(R4).unwrap(), json!(4));
        assert_eq!(to_value(R8).unwrap(), json!(8));
        assert_eq!(to_value(R16).unwrap(), json!(16));
        assert_eq!(to_value(R32).unwrap(), json!(32));
        assert_eq!(to_value(R64).unwrap(), json!(64));
        assert_eq!(to_value(R128).unwrap(), json!(128));
    }

    #[test]
    fn de_ok() {
        use serde_json::{from_value, json};
        assert_eq!(from_value::<Resolution>(json!(1)).unwrap(), R1);
        assert_eq!(from_value::<Resolution>(json!(2)).unwrap(), R2);
        assert_eq!(from_value::<Resolution>(json!(4)).unwrap(), R4);
        assert_eq!(from_value::<Resolution>(json!(8)).unwrap(), R8);
        assert_eq!(from_value::<Resolution>(json!(16)).unwrap(), R16);
        assert_eq!(from_value::<Resolution>(json!(32)).unwrap(), R32);
        assert_eq!(from_value::<Resolution>(json!(64)).unwrap(), R64);
        assert_eq!(from_value::<Resolution>(json!(128)).unwrap(), R128);
    }

    #[test]
    fn de_err() {
        use serde_json::{from_value, json};
        assert_eq!(
            from_value::<Resolution>(json!(-16))
                .unwrap_err()
                .to_string(),
            "invalid value: integer `-16`, expected u16"
        );
        assert_eq!(
            from_value::<Resolution>(json!(0)).unwrap_err().to_string(),
            "0 is not a permitted resolution; must be a power of 2 between 1 and 127"
        );
        assert_eq!(
            from_value::<Resolution>(json!(1.5))
                .unwrap_err()
                .to_string(),
            "invalid type: floating point `1.5`, expected u16"
        );
    }
}
