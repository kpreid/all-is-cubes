//! Options controlling recording.

use std::convert::TryFrom;
use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::time::Duration;

use all_is_cubes::camera::Viewport;
use all_is_cubes::cgmath::Vector2;

/// Options for recording and output in [`record_main`].
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct RecordOptions {
    pub output_path: PathBuf,
    pub output_format: RecordFormat,
    pub image_size: Vector2<u32>,
    pub animation: Option<RecordAnimationOptions>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct RecordAnimationOptions {
    pub frame_count: usize,
    pub frame_period: Duration,
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum RecordFormat {
    // PNG and Animated-PNG are (currently) the same implementation, just with multiple frames or not
    PngOrApng,
    Gltf,
}

impl RecordOptions {
    pub(crate) fn viewport(&self) -> Viewport {
        Viewport::with_scale(1.0, self.image_size)
    }

    pub(crate) fn frame_range(&self) -> RangeInclusive<usize> {
        match &self.animation {
            None => 0..=0,
            Some(animation) => 1..=animation.frame_count,
        }
    }
}

impl RecordAnimationOptions {
    pub(crate) fn total_duration(&self) -> Duration {
        self.frame_period * u32::try_from(self.frame_count).unwrap_or(u32::MAX)
    }
}
