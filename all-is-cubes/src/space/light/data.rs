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

use crate::math::{PositiveSign, Rgb, ps32};

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
    // Note: When changing these scaling parameters, don't forget to update:
    // * `PACKED_LIGHT_SCALAR_LOOKUP_TABLE`
    // * The decoder in `all-is-cubes-gpu/src/in_wgpu/shaders/blocks-and-lines.wgsl`
    const LOG_SCALE: f32 = 10.0;
    const LOG_OFFSET: f32 = 144.0;

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
    pub fn value(self) -> Rgb {
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
            // Use pure-Rust implementation from `libm` to avoid platform-dependent rounding
            // which would be inconsistent with our hardcoded lookup table.
            // This function is supposed to
            // *define* what belongs in the lookup table.
            libm::exp2f((f32::from(value) - Self::LOG_OFFSET) / Self::LOG_SCALE)
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
    ps32(0.0), ps32(4.9575945e-5), ps32(5.3134198e-5), ps32(5.69478e-5), ps32(6.1035156e-5),
    ps32(6.541588e-5), ps32(7.0110975e-5), ps32(7.51431e-5), ps32(8.053635e-5), ps32(8.6316744e-5),
    ps32(9.251202e-5), ps32(9.915189e-5), ps32(0.000106268395), ps32(0.0001138956), ps32(0.00012207031),
    ps32(0.00013083176), ps32(0.00014022195), ps32(0.0001502862), ps32(0.0001610727), ps32(0.00017263349),
    ps32(0.00018502404), ps32(0.00019830378), ps32(0.00021253679), ps32(0.0002277912), ps32(0.00024414063),
    ps32(0.00026166352), ps32(0.0002804439), ps32(0.0003005724), ps32(0.0003221454), ps32(0.00034526698),
    ps32(0.00037004807), ps32(0.00039660756), ps32(0.00042507358), ps32(0.0004555824), ps32(0.00048828125),
    ps32(0.00052332703), ps32(0.0005608878), ps32(0.0006011448), ps32(0.0006442908), ps32(0.00069053395),
    ps32(0.00074009615), ps32(0.0007932151), ps32(0.00085014716), ps32(0.0009111648), ps32(0.0009765625),
    ps32(0.0010466541), ps32(0.0011217756), ps32(0.0012022896), ps32(0.0012885816), ps32(0.0013810679),
    ps32(0.0014801923), ps32(0.0015864302), ps32(0.0017002943), ps32(0.0018223296), ps32(0.001953125),
    ps32(0.0020933081), ps32(0.0022435512), ps32(0.0024045792), ps32(0.0025771633), ps32(0.0027621358),
    ps32(0.0029603846), ps32(0.0031728605), ps32(0.0034005886), ps32(0.0036446592), ps32(0.00390625),
    ps32(0.004186615), ps32(0.0044871024), ps32(0.0048091584), ps32(0.005154328), ps32(0.0055242716),
    ps32(0.0059207673), ps32(0.006345721), ps32(0.0068011773), ps32(0.0072893207), ps32(0.0078125),
    ps32(0.00837323), ps32(0.008974205), ps32(0.009618317), ps32(0.010308656), ps32(0.011048543),
    ps32(0.011841535), ps32(0.012691442), ps32(0.013602355), ps32(0.014578641), ps32(0.015625),
    ps32(0.01674646), ps32(0.01794841), ps32(0.019236634), ps32(0.020617312), ps32(0.022097087),
    ps32(0.02368307), ps32(0.025382884), ps32(0.02720471), ps32(0.029157283), ps32(0.03125),
    ps32(0.03349292), ps32(0.03589682), ps32(0.038473267), ps32(0.041234624), ps32(0.044194173),
    ps32(0.04736614), ps32(0.050765768), ps32(0.05440942), ps32(0.058314566), ps32(0.0625),
    ps32(0.06698584), ps32(0.07179365), ps32(0.07694653), ps32(0.08246925), ps32(0.088388346),
    ps32(0.09473228), ps32(0.10153155), ps32(0.108818814), ps32(0.11662913), ps32(0.125),
    ps32(0.13397168), ps32(0.1435873), ps32(0.15389305), ps32(0.1649385), ps32(0.17677669),
    ps32(0.18946455), ps32(0.2030631), ps32(0.21763763), ps32(0.23325826), ps32(0.25),
    ps32(0.26794338), ps32(0.2871746), ps32(0.3077861), ps32(0.32987696), ps32(0.35355338),
    ps32(0.37892914), ps32(0.4061262), ps32(0.43527526), ps32(0.4665165), ps32(0.5),
    ps32(0.53588676), ps32(0.57434916), ps32(0.6155722), ps32(0.6597539), ps32(0.70710677),
    ps32(0.7578583), ps32(0.8122524), ps32(0.8705506), ps32(0.933033), ps32(1.0),
    ps32(1.0717734), ps32(1.1486983), ps32(1.2311444), ps32(1.319508), ps32(1.4142135),
    ps32(1.5157166), ps32(1.6245048), ps32(1.7411011), ps32(1.866066), ps32(2.0),
    ps32(2.143547), ps32(2.297397), ps32(2.4622889), ps32(2.6390157), ps32(2.828427),
    ps32(3.031433), ps32(3.2490096), ps32(3.482202), ps32(3.732132), ps32(4.0),
    ps32(4.2870936), ps32(4.594794), ps32(4.9245777), ps32(5.278032), ps32(5.656854),
    ps32(6.0628657), ps32(6.498019), ps32(6.964404), ps32(7.4642644), ps32(8.0),
    ps32(8.574187), ps32(9.189588), ps32(9.849155), ps32(10.556064), ps32(11.313708),
    ps32(12.125731), ps32(12.996038), ps32(13.928808), ps32(14.928529), ps32(16.0),
    ps32(17.148375), ps32(18.379171), ps32(19.698313), ps32(21.112127), ps32(22.627417),
    ps32(24.251463), ps32(25.992073), ps32(27.857622), ps32(29.857058), ps32(32.0),
    ps32(34.29675), ps32(36.758343), ps32(39.396626), ps32(42.224255), ps32(45.254833),
    ps32(48.502926), ps32(51.984146), ps32(55.715244), ps32(59.714115), ps32(64.0),
    ps32(68.5935), ps32(73.516685), ps32(78.79325), ps32(84.44851), ps32(90.50967),
    ps32(97.00585), ps32(103.96829), ps32(111.43049), ps32(119.42823), ps32(128.0),
    ps32(137.187), ps32(147.03337), ps32(157.5865), ps32(168.89702), ps32(181.01933),
    ps32(194.0117), ps32(207.93658), ps32(222.86098), ps32(238.85646), ps32(256.0),
    ps32(274.37408), ps32(294.06674), ps32(315.173), ps32(337.79395), ps32(362.03867),
    ps32(388.02353), ps32(415.87317), ps32(445.72195), ps32(477.71277), ps32(512.0),
    ps32(548.74817), ps32(588.1335), ps32(630.346), ps32(675.5879), ps32(724.07733),
    ps32(776.04706), ps32(831.74634), ps32(891.4439), ps32(955.42554), ps32(1024.0),
    ps32(1097.4963), ps32(1176.267), ps32(1260.692), ps32(1351.1758), ps32(1448.1547),
    ps32(1552.0941), ps32(1663.4927), ps32(1782.8878), ps32(1910.8511), ps32(2048.0),
    ps32(2194.9927),
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
            if i.is_multiple_of(5) {
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
                PackedLight::scalar_in(PositiveSign::<f32>::INFINITY),
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
            [0.0, 4.9575945e-5, 5.3134198e-5, 2048.0, 2194.9927],
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
