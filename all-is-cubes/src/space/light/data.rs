//! Data structures for light storage and algorithms.

use core::fmt;

use euclid::default::Vector3D;
use euclid::vec3;

/// Acts as polyfill for float methods
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use num_traits::float::Float as _;

use crate::math::{PositiveSign, Rgb};

#[cfg(doc)]
use crate::space::{self, Space};

/// One component of a `PackedLight`.
pub(crate) type PackedLightScalar = u8;

/// Special reasons for a cube having zero light in it.
/// These may be used to help compute smoothed lighting across blocks.
///
/// The numeric values of this enum are used to transmit it to shaders by packing
/// it into an "RGBA" color value. They should not be considered a stable API element.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum LightStatus {
    /// The cube's light value has never been computed, or it has been computed
    /// as a wild guess that should be used only if no better data is available.
    ///
    /// This value may appear when:
    ///
    /// * [`space::Builder::palette_and_contents()`] was called without including light data.
    /// * The light updater is speculatively copying from neighboring cubes.
    Uninitialized = 0,
    /// The cube has no surfaces to catch light and therefore the light value is not tracked.
    NoRays = 1,
    /// The cube contains an opaque block and therefore does not have any light entering.
    Opaque = 128,
    /// No special situation: if it's black then it's just dark.
    Visible = 255,
}

/// Lighting within a [`Space`]; an [`Rgb`] value stored with reduced precision and range.
///
/// TODO: This now stores additional information. Rename to '`SpaceLight`' or some such.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PackedLight {
    // LightStatus being other than Visible is mutually exclusive with value being nonzero,
    // so we could in theory make this an enum, but that wouldn't actually compact the
    // representation, and this representation maps to 8-bit-per-component RGBA which is
    // what the shader expects.
    value: Vector3D<PackedLightScalar>,
    status: LightStatus,
}
// TODO: Once we've built out the rest of the game, do some performance testing and
// decide whether having colored lighting is worth the compute and storage cost.
// If memory vs. bit depth is an issue, consider switching to something like YCbCr
// representation, or possibly something that GPUs specifically do well with.

impl PackedLight {
    const LOG_SCALE: f32 = 16.0;
    const LOG_OFFSET: f32 = 128.0;

    // pub(crate) const ZERO: Self = Self::none(LightStatus::Visible);
    pub(crate) const OPAQUE: Self = Self::none(LightStatus::Opaque);
    pub(crate) const NO_RAYS: Self = Self::none(LightStatus::NoRays);
    pub(crate) const UNINITIALIZED_AND_BLACK: Self = Self::none(LightStatus::Uninitialized);
    pub(crate) const ONE: PackedLight = {
        let one_scalar = Self::LOG_OFFSET as PackedLightScalar;
        PackedLight {
            status: LightStatus::Visible,
            value: Vector3D::new(one_scalar, one_scalar, one_scalar),
        }
    };

    pub(crate) fn some(value: Rgb) -> Self {
        PackedLight {
            value: Vector3D::new(
                Self::scalar_in(value.red()),
                Self::scalar_in(value.green()),
                Self::scalar_in(value.blue()),
            ),
            status: LightStatus::Visible,
        }
    }

    pub(crate) const fn none(status: LightStatus) -> Self {
        PackedLight {
            value: Vector3D::new(0, 0, 0),
            status,
        }
    }

    pub(crate) fn guess(value: Rgb) -> Self {
        PackedLight {
            value: Vector3D::new(
                Self::scalar_in(value.red()),
                Self::scalar_in(value.green()),
                Self::scalar_in(value.blue()),
            ),
            status: LightStatus::Uninitialized,
        }
    }

    /// Returns the light level.
    #[inline]
    pub fn value(&self) -> Rgb {
        Rgb::new_ps(
            Self::scalar_out_ps(self.value.x),
            Self::scalar_out_ps(self.value.y),
            Self::scalar_out_ps(self.value.z),
        )
    }

    // TODO: Expose LightStatus once we are more confident in its API stability
    pub(crate) fn status(self) -> LightStatus {
        self.status
    }

    /// Returns true if the light value is meaningful, or false if it is
    /// inside an opaque block or in empty unlit air (in which case [`Self::value`]
    /// always returns zero).
    pub(crate) fn valid(self) -> bool {
        self.status == LightStatus::Visible
    }

    /// RGB color plus a fourth component which is a “weight” value which indicates how
    /// much this color should actually contribute to the surface color. It is usually
    /// 0 or 1, but is set slightly above zero for opaque blocks to create the ambient
    /// occlusion effect.
    pub(crate) fn value_with_ambient_occlusion(self) -> [f32; 4] {
        [
            Self::scalar_out(self.value.x),
            Self::scalar_out(self.value.y),
            Self::scalar_out(self.value.z),
            match self.status {
                LightStatus::Uninitialized => 0.0,
                LightStatus::NoRays => 0.0,
                // TODO: Make this a graphics option
                LightStatus::Opaque => 0.25,
                LightStatus::Visible => 1.0,
            },
        ]
    }

    #[inline]
    #[doc(hidden)] // TODO: used by all_is_cubes_gpu; but it should be doable equivalently using public functions
    pub fn as_texel(self) -> [u8; 4] {
        let Self {
            value: Vector3D {
                x: r, y: g, z: b, ..
            },
            status,
        } = self;
        [r, g, b, status as u8]
    }

    /// Undoes the transformation of [`Self::as_texel()`].
    /// For testing only.
    #[doc(hidden)]
    #[track_caller]
    pub fn from_texel([r, g, b, s]: [u8; 4]) -> Self {
        Self {
            value: vec3(r, g, b),
            status: match s {
                0 => LightStatus::Uninitialized,
                1 => LightStatus::NoRays,
                128 => LightStatus::Opaque,
                255 => LightStatus::Visible,
                _ => panic!("invalid status value {s}"),
            },
        }
    }

    /// Computes a degree of difference between two [`PackedLight`] values, used to decide
    /// update priority.
    /// The value is zero if and only if the two inputs are equal.
    #[inline]
    pub(crate) fn difference_priority(self, other: PackedLight) -> PackedLightScalar {
        fn abs_diff(a: PackedLightScalar, b: PackedLightScalar) -> PackedLightScalar {
            a.max(b) - a.min(b)
        }
        let mut difference = abs_diff(self.value.x, other.value.x)
            .max(abs_diff(self.value.y, other.value.y))
            .max(abs_diff(self.value.z, other.value.z));

        if other.status != self.status {
            // A non-opaque block changing to an opaque one, or similar, changes the
            // results of the rest of the algorithm so should be counted as a difference
            // even if it's still changing zero to zero.
            // TODO: Tune this number for fast settling and good results.
            difference = difference.saturating_add(PackedLightScalar::MAX / 4);
        }

        difference
    }

    #[inline(always)]
    fn scalar_in(value: PositiveSign<f32>) -> PackedLightScalar {
        // Note that `as` is a saturating cast.
        (value.into_inner().log2() * Self::LOG_SCALE + Self::LOG_OFFSET) as PackedLightScalar
    }

    /// Convert a `PackedLightScalar` value to a linear color component value.
    /// This function is guaranteed (and tested) to only return finite floats.
    #[inline(always)]
    fn scalar_out(value: PackedLightScalar) -> f32 {
        // Special representation to ensure we don't "round" zero up to a small nonzero value.
        if value == 0 {
            0.0
        } else {
            ((f32::from(value) - Self::LOG_OFFSET) / Self::LOG_SCALE).exp2()
        }
    }

    #[inline(always)]
    fn scalar_out_ps(value: PackedLightScalar) -> PositiveSign<f32> {
        // Safety: a test verifies that `scalar_out` can never return NaN.
        unsafe { PositiveSign::new_unchecked(Self::scalar_out(value)) }
    }
}

impl fmt::Debug for PackedLight {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "PackedLight({}, {}, {}, {:?})",
            self.value.x, self.value.y, self.value.z, self.status
        )
    }
}

impl From<Rgb> for PackedLight {
    #[inline]
    fn from(value: Rgb) -> Self {
        PackedLight::some(value)
    }
}

#[cfg(feature = "save")]
impl From<PackedLight> for crate::save::schema::LightSerV1 {
    fn from(p: PackedLight) -> Self {
        use crate::save::schema::LightStatusSerV1 as S;
        crate::save::schema::LightSerV1 {
            value: p.value.into(),
            status: match p.status {
                LightStatus::Uninitialized => S::Uninitialized,
                LightStatus::NoRays => S::NoRays,
                LightStatus::Opaque => S::Opaque,
                LightStatus::Visible => S::Visible,
            },
        }
    }
}

#[cfg(feature = "save")]
impl From<crate::save::schema::LightSerV1> for PackedLight {
    fn from(ls: crate::save::schema::LightSerV1) -> Self {
        use crate::save::schema::LightStatusSerV1 as S;
        PackedLight {
            value: Vector3D::from(ls.value),
            status: match ls.status {
                S::Uninitialized => LightStatus::Uninitialized,
                S::NoRays => LightStatus::NoRays,
                S::Opaque => LightStatus::Opaque,
                S::Visible => LightStatus::Visible,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::ps32;
    use core::iter::once;

    fn packed_light_test_values() -> impl Iterator<Item = PackedLight> {
        (PackedLightScalar::MIN..PackedLightScalar::MAX)
            .flat_map(|s| {
                vec![
                    PackedLight {
                        value: Vector3D::new(s, 0, 0),
                        status: LightStatus::Visible,
                    },
                    PackedLight {
                        value: Vector3D::new(0, s, 0),
                        status: LightStatus::Visible,
                    },
                    PackedLight {
                        value: Vector3D::new(0, 0, s),
                        status: LightStatus::Visible,
                    },
                    PackedLight {
                        value: Vector3D::new(s, 127, 255),
                        status: LightStatus::Visible,
                    },
                ]
                .into_iter()
            })
            .chain(once(PackedLight::OPAQUE))
            .chain(once(PackedLight::NO_RAYS))
    }

    /// Test that unpacking and packing doesn't shift the value, which could lead
    /// to runaway light values.
    #[test]
    fn packed_light_roundtrip() {
        for i in PackedLightScalar::MIN..PackedLightScalar::MAX {
            assert_eq!(i, PackedLight::scalar_in(PackedLight::scalar_out_ps(i)));
        }
    }

    /// Safety test: we want to skip the NaN/sign checks for constructing `Rgb`
    /// from `PackedLight`, so it had better be valid for any possible input.
    #[test]
    fn packed_light_always_positive() {
        for i in PackedLightScalar::MIN..PackedLightScalar::MAX {
            let value = PackedLight::scalar_out(i);
            assert!(value.is_finite() && value.is_sign_positive(), "{}", i);
        }
    }

    /// Test out-of-range floats.
    #[test]
    fn packed_light_clipping_in() {
        assert_eq!(
            [
                PackedLight::scalar_in(ps32(-0.0)),
                PackedLight::scalar_in(ps32(0.0)),
                PackedLight::scalar_in(ps32(1e-30)),
                PackedLight::scalar_in(ps32(1e+30)),
                PackedLight::scalar_in(ps32(f32::INFINITY)),
            ],
            [0, 0, 0, 255, 255],
        );
    }

    #[test]
    fn packed_light_is_packed() {
        // Technically this is not guaranteed by the compiler, but if it's false something probably went wrong.
        assert_eq!(size_of::<PackedLight>(), 4);
    }

    /// Demonstrate what range and step sizes we get out of the encoding.
    #[test]
    fn packed_light_extreme_values_out() {
        assert_eq!(
            [
                PackedLight::scalar_out(0),
                PackedLight::scalar_out(1),
                PackedLight::scalar_out(2),
                PackedLight::scalar_out(254),
                PackedLight::scalar_out(255),
            ],
            [0.0, 0.0040791943, 0.004259796, 234.75304, 245.14644],
        );
    }

    #[test]
    fn packed_light_difference_vs_eq() {
        for v1 in packed_light_test_values() {
            for v2 in packed_light_test_values() {
                assert_eq!(
                    v1 == v2,
                    v1.difference_priority(v2) == 0,
                    "v1={v1:?} v2={v2:?}"
                );
            }
        }
    }
}
