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

use crate::math::{ps32, PositiveSign, Rgb};

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

/// A single cube of light within a [`Space`]; an [`Rgb`] value stored with reduced precision and
/// range, plus metadata about the applicability of the result.
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
        // Note that `as` is a saturating cast, so out of range values will be clamped.
        (value.into_inner().log2() * Self::LOG_SCALE + Self::LOG_OFFSET).round()
            as PackedLightScalar
    }

    /// Convert a `PackedLightScalar` value to a linear color component value.
    /// This function is guaranteed (and tested) to only return finite non-negative floats.
    #[inline(always)]
    fn scalar_out(value: PackedLightScalar) -> f32 {
        PACKED_LIGHT_SCALAR_LOOKUP_TABLE[usize::from(value)].into_inner()
    }

    /// Convert a `PackedLightScalar` value to a linear color component value.
    /// This function is guaranteed (and tested) to only return finite non-negative floats.
    #[inline(always)]
    fn scalar_out_ps(value: PackedLightScalar) -> PositiveSign<f32> {
        PACKED_LIGHT_SCALAR_LOOKUP_TABLE[usize::from(value)]
    }

    /// Convert a `PackedLightScalar` value to a linear color component value.
    /// This function is guaranteed (and tested) to only return finite non-negative floats.
    ///
    /// This implementation is only used for testing and regenerating
    /// [`PACKED_LIGHT_SCALAR_LOOKUP_TABLE`].
    #[cfg(test)]
    fn scalar_out_arithmetic(value: PackedLightScalar) -> f32 {
        // Special representation to ensure we don't "round" zero up to a small nonzero value.
        if value == 0 {
            0.0
        } else {
            ((f32::from(value) - Self::LOG_OFFSET) / Self::LOG_SCALE).exp2()
        }
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

/// Precomputed lookup table of the results of [`PackedLight::scalar_out_arithmetic()`].
/// This is more efficient than computing the function every time.
/// 
/// This table is validated and can be regenerated using the test `check_packed_light_table`.
#[rustfmt::skip]
#[allow(clippy::approx_constant)]
const PACKED_LIGHT_SCALAR_LOOKUP_TABLE: [PositiveSign<f32>; 256] = [
    ps32(0.0), ps32(0.0040791943), ps32(0.004259796), ps32(0.004448393), ps32(0.00464534),
    ps32(0.004851007), ps32(0.0050657797), ps32(0.005290061), ps32(0.0055242716), ps32(0.0057688523),
    ps32(0.006024261), ps32(0.006290978), ps32(0.0065695033), ps32(0.00686036), ps32(0.007164094),
    ps32(0.0074812756), ps32(0.0078125), ps32(0.008158389), ps32(0.008519592), ps32(0.008896786),
    ps32(0.00929068), ps32(0.009702014), ps32(0.010131559), ps32(0.010580122), ps32(0.011048543),
    ps32(0.011537705), ps32(0.012048522), ps32(0.012581956), ps32(0.013139007), ps32(0.01372072),
    ps32(0.014328188), ps32(0.014962551), ps32(0.015625), ps32(0.016316777), ps32(0.017039184),
    ps32(0.017793572), ps32(0.01858136), ps32(0.019404028), ps32(0.020263119), ps32(0.021160243),
    ps32(0.022097087), ps32(0.02307541), ps32(0.024097044), ps32(0.025163911), ps32(0.026278013),
    ps32(0.02744144), ps32(0.028656377), ps32(0.029925102), ps32(0.03125), ps32(0.032633554),
    ps32(0.034078367), ps32(0.035587143), ps32(0.03716272), ps32(0.038808055), ps32(0.040526237),
    ps32(0.042320486), ps32(0.044194173), ps32(0.04615082), ps32(0.048194088), ps32(0.050327823),
    ps32(0.052556027), ps32(0.05488288), ps32(0.057312753), ps32(0.059850205), ps32(0.0625),
    ps32(0.06526711), ps32(0.068156734), ps32(0.07117429), ps32(0.07432544), ps32(0.07761611),
    ps32(0.081052475), ps32(0.08464097), ps32(0.088388346), ps32(0.09230164), ps32(0.096388176),
    ps32(0.100655645), ps32(0.10511205), ps32(0.10976576), ps32(0.114625506), ps32(0.11970041),
    ps32(0.125), ps32(0.13053422), ps32(0.13631347), ps32(0.14234857), ps32(0.14865088),
    ps32(0.15523222), ps32(0.16210495), ps32(0.16928194), ps32(0.17677669), ps32(0.18460327),
    ps32(0.19277635), ps32(0.20131129), ps32(0.2102241), ps32(0.21953152), ps32(0.22925101),
    ps32(0.23940082), ps32(0.25), ps32(0.26106843), ps32(0.27262694), ps32(0.28469715),
    ps32(0.29730177), ps32(0.31046444), ps32(0.3242099), ps32(0.3385639), ps32(0.35355338),
    ps32(0.36920655), ps32(0.3855527), ps32(0.40262258), ps32(0.4204482), ps32(0.43906304),
    ps32(0.45850202), ps32(0.47880164), ps32(0.5), ps32(0.52213687), ps32(0.5452539),
    ps32(0.5693943), ps32(0.59460354), ps32(0.6209289), ps32(0.6484198), ps32(0.6771278),
    ps32(0.70710677), ps32(0.7384131), ps32(0.7711054), ps32(0.80524516), ps32(0.8408964),
    ps32(0.8781261), ps32(0.91700405), ps32(0.9576033), ps32(1.0), ps32(1.0442737),
    ps32(1.0905077), ps32(1.1387886), ps32(1.1892071), ps32(1.2418578), ps32(1.2968396),
    ps32(1.3542556), ps32(1.4142135), ps32(1.4768262), ps32(1.5422108), ps32(1.6104903),
    ps32(1.6817929), ps32(1.7562522), ps32(1.8340081), ps32(1.9152066), ps32(2.0),
    ps32(2.0885475), ps32(2.1810155), ps32(2.2775772), ps32(2.3784142), ps32(2.4837155),
    ps32(2.5936792), ps32(2.708511), ps32(2.828427), ps32(2.9536524), ps32(3.0844216),
    ps32(3.2209806), ps32(3.3635857), ps32(3.5125043), ps32(3.6680162), ps32(3.830413),
    ps32(4.0), ps32(4.177095), ps32(4.362031), ps32(4.5551543), ps32(4.7568283),
    ps32(4.967431), ps32(5.1873584), ps32(5.417022), ps32(5.656854), ps32(5.907305),
    ps32(6.1688433), ps32(6.4419613), ps32(6.7271714), ps32(7.0250087), ps32(7.3360324),
    ps32(7.660826), ps32(8.0), ps32(8.35419), ps32(8.724062), ps32(9.110309),
    ps32(9.513657), ps32(9.934862), ps32(10.374717), ps32(10.834044), ps32(11.313708),
    ps32(11.81461), ps32(12.337687), ps32(12.883923), ps32(13.454343), ps32(14.050017),
    ps32(14.672065), ps32(15.321652), ps32(16.0), ps32(16.70838), ps32(17.448124),
    ps32(18.220617), ps32(19.027313), ps32(19.869724), ps32(20.749434), ps32(21.668089),
    ps32(22.627417), ps32(23.62922), ps32(24.675373), ps32(25.767845), ps32(26.908686),
    ps32(28.100035), ps32(29.34413), ps32(30.643305), ps32(32.0), ps32(33.41676),
    ps32(34.896248), ps32(36.441235), ps32(38.054626), ps32(39.73945), ps32(41.498867),
    ps32(43.336178), ps32(45.254833), ps32(47.25844), ps32(49.350746), ps32(51.53569),
    ps32(53.81737), ps32(56.20007), ps32(58.68826), ps32(61.28661), ps32(64.0),
    ps32(66.83352), ps32(69.792496), ps32(72.88247), ps32(76.10925), ps32(79.4789),
    ps32(82.997734), ps32(86.672356), ps32(90.50967), ps32(94.51688), ps32(98.70149),
    ps32(103.07138), ps32(107.63474), ps32(112.40014), ps32(117.37652), ps32(122.57322),
    ps32(128.0), ps32(133.66704), ps32(139.58499), ps32(145.76494), ps32(152.2185),
    ps32(158.9578), ps32(165.99547), ps32(173.34471), ps32(181.01933), ps32(189.03375),
    ps32(197.40298), ps32(206.14276), ps32(215.26949), ps32(224.80028), ps32(234.75304),
    ps32(245.14644),
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::ps32;
    use alloc::vec::Vec;
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

    #[test]
    fn check_packed_light_table() {
        let generated_table: Vec<f32> = (0..=u8::MAX)
            .map(PackedLight::scalar_out_arithmetic)
            .collect();
        print!("const PACKED_LIGHT_SCALAR_LOOKUP_TABLE: [PositiveSign<f32>; 256] = [");
        for i in 0..=u8::MAX {
            if i % 5 == 0 {
                print!("\n   ");
            }
            print!(" ps32({:?}),", generated_table[i as usize]);
        }
        println!("\n];");

        pretty_assertions::assert_eq!(
            PACKED_LIGHT_SCALAR_LOOKUP_TABLE
                .iter()
                .copied()
                .map(PositiveSign::<f32>::into_inner)
                .collect::<Vec<_>>(),
            generated_table
        );
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
    fn packed_light_rounds_to_nearest() {
        for i in PackedLightScalar::MIN..PackedLightScalar::MAX {
            let value = PackedLight::scalar_out_ps(i);
            assert_eq!(
                (
                    PackedLight::scalar_in(value * ps32(0.9999)),
                    i,
                    PackedLight::scalar_in(value * ps32(1.0001)),
                ),
                (i, i, i)
            );
        }
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
