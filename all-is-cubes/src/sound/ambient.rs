//! Ambient sound.
//!
//! Ambient sound is sound that is not explicitly emitted by a simulated event, but is presented
//! to any listener based on their position in space.

use core::fmt;
use core::ops;

use euclid::{Vector3D, vec3};
use exhaust::Exhaust;
use num_traits::ConstZero as _;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use crate::camera;
use crate::math::{FreeCoordinate, PositiveSign, ZeroOne};
use crate::universe;

// -------------------------------------------------------------------------------------------------

// Center frequency of the lowest band
const FIRST_BAND: f32 = 20.0;
// Spacing between bands, in fractions of an octave.
const BAND_STEP_OCT: f32 = 0.5;

/// This is a macro so it can be used in documentation text.
macro_rules! band_list_for_doc {
    () => {
        "20 Hz, 28 Hz, 40 Hz, 57 Hz, 80 Hz, 113 Hz, 160 Hz, 226 Hz, 320 Hz, 453 Hz, 640 Hz, \
        905 Hz, 1280 Hz, 1810 Hz, 2560 Hz, 3620 Hz, 5120 Hz, 7241 Hz, 10240 Hz, and 14482 Hz"
    };
}

/// A frequency band, used in ambient sound processing.
///
/// These bands are chosen to be sufficiently coarse that it is feasible for many game objects
/// to store a complete spectrum of them. The bands’ center frequencies, rounded, are
#[doc = concat!(band_list_for_doc!(), ".")]
// TODO: Decide whether the bands are promised not to change, or not, and document that.
#[derive(Clone, Copy, Eq, PartialEq, PartialOrd)]
pub struct Band {
    index: u8,
}

impl Band {
    /// Number of bands that exist.
    pub const COUNT: usize = 20;

    /// The band of the lowest frequency.
    pub const MIN: Self = Self { index: 0 };

    /// The band of the highest  frequency.
    pub const MAX: Self = Self {
        index: (Self::COUNT - 1) as u8,
    };

    /// Converts a frequency in Hz to the nearest band.
    pub fn from_frequency(frequency: PositiveSign<f32>) -> Self {
        Self {
            index: (((frequency.into_inner() / FIRST_BAND).log2() / BAND_STEP_OCT).round() as u8)
                .min(Band::MAX.index),
        }
    }

    /// Returns the [`Band`] value corresponding to the given array index.
    ///
    /// Panics if `index >= Band::COUNT`.
    #[track_caller]
    pub fn from_index(index: usize) -> Self {
        if index >= Self::COUNT {
            panic!("band index out of range");
        } else {
            Self { index: index as u8 }
        }
    }

    /// Returns the array index corresponding to this band.
    ///
    /// The value is always less than [`Band::COUNT`].
    pub fn to_index(self) -> usize {
        self.index.into()
    }

    /// Returns the frequency of the middle of this band.
    pub fn center_frequency(self) -> PositiveSign<f32> {
        PositiveSign::<f32>::new_strict((f32::from(self.index) * BAND_STEP_OCT).exp2() * FIRST_BAND)
    }
}

impl fmt::Display for Band {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // zero decimal places is adequate precision for identification
        let frequency = self.center_frequency().round();
        write!(f, "{frequency} Hz")
    }
}
impl fmt::Debug for Band {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // zero decimal places is adequate precision for identification
        let frequency = self.center_frequency().round();
        let index = self.index;
        write!(f, "{frequency} Hz [{index}]")
    }
}

impl Exhaust for Band {
    type Iter = ops::Range<u8>;
    type Factory = u8;

    fn exhaust_factories() -> Self::Iter {
        Self::MIN.index..(Self::MAX.index + 1)
    }

    fn from_factory(index: Self::Factory) -> Self {
        Self::from_index(index as usize)
    }
}

// -------------------------------------------------------------------------------------------------

/// Stores a value (usually a number) per frequency [`Band`].
#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[allow(clippy::exhaustive_structs)]
pub struct Spectrum<T>(pub [T; Band::COUNT]);

impl<T> Spectrum<T> {
    /// Creates a [`Spectrum`] with a nonzero value at one position.
    pub fn narrow(value: T, frequency: PositiveSign<f32>) -> Self
    where
        T: Copy + Default,
    {
        let mut new_self = Self::default();
        new_self[Band::from_frequency(frequency)] = value;
        new_self
    }
}

impl<T: fmt::Debug> fmt::Debug for Spectrum<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // always single-line formatting
        write!(f, "Spectrum {:?}", self.0)
    }
}

impl<T> ops::Index<Band> for Spectrum<T> {
    type Output = T;

    fn index(&self, index: Band) -> &Self::Output {
        &self.0[usize::from(index.index)]
    }
}
impl<T> ops::IndexMut<Band> for Spectrum<T> {
    fn index_mut(&mut self, index: Band) -> &mut Self::Output {
        &mut self.0[usize::from(index.index)]
    }
}

impl<T: ops::AddAssign> ops::Add for Spectrum<T> {
    type Output = Self;
    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}
impl<T: ops::AddAssign> ops::AddAssign for Spectrum<T> {
    fn add_assign(&mut self, rhs: Self) {
        for (l, r) in self.0.iter_mut().zip(rhs.0.into_iter()) {
            *l += r;
        }
    }
}

impl<T: ops::MulAssign<U>, U: Copy> ops::Mul<U> for Spectrum<T> {
    type Output = Self;
    fn mul(mut self, rhs: U) -> Self::Output {
        self *= rhs;
        self
    }
}
impl<T: ops::MulAssign<U>, U: Copy> ops::MulAssign<U> for Spectrum<T> {
    fn mul_assign(&mut self, rhs: U) {
        for elem in &mut self.0 {
            *elem *= rhs;
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Mix of background sound occurring in some space or emitted by some block.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct Ambient {
    /// Noise emitted by this source.
    ///
    /// TODO: specify units for these values
    pub noise_bands: Spectrum<PositiveSign<f32>>,
    // TODO: Add other properties such as absorption/reflection and non-white-noise emission
}

impl Ambient {
    /// No sound emission; defaults for other parameters.
    pub const SILENT: Self = Self {
        noise_bands: Spectrum([PositiveSign::ZERO; _]),
    };

    /// Constructs an [`Ambient`] denoting noise output of the given power and center frequency.
    ///
    /// TODO: should be dB instead of power. at what distance etc?
    /// TODO: Specify bandwidth
    pub fn noise_at_frequency(
        power: PositiveSign<f32>,
        center_frequency: PositiveSign<f32>,
    ) -> Self {
        let mut new_self = Self::SILENT;
        let band = Band::from_frequency(center_frequency);
        new_self.noise_bands[band] = power;
        new_self
    }

    /// Given a direction in which this sound lies, in the [`camera::Eye`] listener-relative
    /// coordinate system,
    /// construct the corresponding [`SpatialAmbient`] for the listener.
    pub(crate) fn pan(self, direction: Vector3D<f64, camera::Eye>) -> SpatialAmbient {
        const LEFT_EAR: Vector3D<f64, camera::Eye> = vec3(-1., 0., 0.);
        const RIGHT_EAR: Vector3D<f64, camera::Eye> = vec3(1., 0., 0.);

        fn panning_function(direction_dot_product: FreeCoordinate) -> PositiveSign<f32> {
            // Transforms the dot product from -1 .. 1 to 0.33 .. 1
            // TODO: replace this with a real HRTF(-ish) function
            ZeroOne::<f32>::new_clamped((direction_dot_product as f32 + 2.0) / 3.0).into()
        }

        SpatialAmbient {
            left: Ambient {
                noise_bands: self.noise_bands * panning_function(direction.dot(LEFT_EAR)),
            },
            right: Ambient {
                noise_bands: self.noise_bands * panning_function(direction.dot(RIGHT_EAR)),
            },
        }
    }
}

impl Default for Ambient {
    fn default() -> Self {
        Self::SILENT
    }
}

impl universe::VisitHandles for Ambient {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        let Self { noise_bands: _ } = self;
    }
}

// -------------------------------------------------------------------------------------------------

/// Aggregation of [`Ambient`] sound into stereo.
// TODO: Eventually we will want some way to express doppler shift?
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs, reason = "TODO: add appropriate accessors")]
pub struct SpatialAmbient {
    /// Left channel, relative to the listener.
    pub left: Ambient,
    /// Right channel, relative to the listener.
    pub right: Ambient,
}

impl SpatialAmbient {
    /// [`Ambient::SILENT`] in stereo.
    pub const SILENT: Self = Self {
        left: Ambient::SILENT,
        right: Ambient::SILENT,
    };

    /// Replace the value of `self` with a weighted sum of `self` and all elements of `iter`,
    /// divided by the total weight.
    ///
    /// The weight of `self` is `existing_value_weight`, and
    /// the weight of each element of `iter` is 1.
    pub(crate) fn set_to_weighted_sum_of<'i>(
        &mut self,
        existing_value_weight: PositiveSign<f32>,
        iter: impl Iterator<Item = &'i SpatialAmbient> + Clone,
    ) {
        let Self {
            left: Ambient {
                noise_bands: left_noise,
            },
            right: Ambient {
                noise_bands: right_noise,
            },
        } = self;

        // Apply existing_value_weight
        *left_noise *= existing_value_weight;
        *right_noise *= existing_value_weight;

        // Sum elements
        let mut count: f32 = existing_value_weight.into_inner();
        for element in iter {
            *left_noise += element.left.noise_bands;
            *right_noise += element.right.noise_bands;
            count += 1.0;
        }

        // Normalize
        let scale = PositiveSign::<f32>::try_from(count.recip()).unwrap_or(PositiveSign::ZERO);
        *left_noise *= scale;
        *right_noise *= scale;
    }
}

impl<'a> From<&'a Ambient> for SpatialAmbient {
    /// Mono-to-stereo conversion.
    fn from(value: &'a Ambient) -> Self {
        Self {
            left: value.clone(),
            right: value.clone(),
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::ps32;
    use alloc::string::String;
    use alloc::vec::Vec;
    use core::fmt::Write as _;

    #[test]
    fn band_frequencies_in_documentation() {
        let bands: Vec<Band> = Band::exhaust().collect();
        let mut text = String::new();
        for &band in &bands[0..bands.len() - 1] {
            write!(text, "{band}, ").unwrap();
        }
        write!(text, "and {}", bands.last().unwrap()).unwrap();

        assert_eq!(text, band_list_for_doc!());
    }

    #[test]
    fn band_roundtrip() {
        for band in Band::exhaust() {
            dbg!(band);
            let frequency = dbg!(band.center_frequency());
            let rt_band = Band::from_frequency(frequency);
            assert_eq!(band, rt_band);
        }
    }

    #[test]
    fn pan() {
        let ambient = Ambient {
            noise_bands: Spectrum::narrow(ps32(1.0), ps32(100.0)),
        };
        let spatial: SpatialAmbient = ambient.pan(vec3(1.0, 0.0, 0.0));

        assert_eq!(
            spatial,
            SpatialAmbient {
                left: Ambient {
                    noise_bands: Spectrum::narrow(ps32(1.0 / 3.0), ps32(100.0)),
                },
                right: Ambient {
                    noise_bands: Spectrum::narrow(ps32(1.0), ps32(100.0)),
                }
            }
        );
    }
}
