//! Ambient sound.
//!
//! Ambient sound is sound that is not emitted at a time by a simulated event,
//! but is derived from the listener’s immediate environment.
//!
//! TODO: This module and functionality is highly incomplete.

use core::fmt;
use core::ops;

use exhaust::Exhaust;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use crate::math::PositiveSign;
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

impl<T> ops::Index<Band> for [T; Band::COUNT] {
    type Output = T;

    fn index(&self, index: Band) -> &Self::Output {
        &self[usize::from(index.index)]
    }
}
impl<T> ops::IndexMut<Band> for [T; Band::COUNT] {
    fn index_mut(&mut self, index: Band) -> &mut Self::Output {
        &mut self[usize::from(index.index)]
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

/// Mix of background sound occurring in some space or emitted by some block.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct Ambient {
    // No fields yet because the ambient sound system is not implemented yet.
}

impl Ambient {
    /// No sound emission. TODO: Define and explain default absorption assumptions
    pub const SILENT: Self = Self {};
}

impl Default for Ambient {
    fn default() -> Self {
        Self::SILENT
    }
}

impl universe::VisitHandles for Ambient {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        let Self {} = self;
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
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
}
