//! Options controlling recording.

use std::ops::RangeInclusive;
use std::path::PathBuf;
use std::time::Duration;

use all_is_cubes_render::camera::{self, Viewport};

#[cfg(doc)]
use all_is_cubes::space::Space;

// -------------------------------------------------------------------------------------------------

/// Specifies a destination and format to write a recording.
///
/// Part of [`InnerMainParams`](crate::InnerMainParams).
#[derive(Clone, Debug, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)] // TODO: make fields private again
#[allow(missing_docs)]
pub struct RecordOptions {
    pub output_path: PathBuf,
    pub output_format: RecordFormat,
    pub save_all: bool,
    pub image_size: camera::ImageSize,
    pub animation: Option<RecordAnimationOptions>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)] // TODO: make fields private again
#[allow(missing_docs)]
pub struct RecordAnimationOptions {
    pub frame_count: usize,
    pub frame_period: Duration,
}

/// File format to write a recording as.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum RecordFormat {
    /// PNG and Animated-PNG are the same except for having multiple frames or not.
    PngOrApng,

    /// glTF recordings contain a scene with an animated world.
    Gltf,

    /// Any [`all_is_cubes_port`] export format may be written via the recording feature.
    Export(all_is_cubes_port::Format),
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

impl RecordFormat {
    /// Whether this format is capable of including [`Space`] light data.
    ///
    /// This may be used to decide whether to wait for light calculations before starting the
    /// recording.
    #[doc(hidden)]
    pub fn includes_light(&self) -> bool {
        match self {
            RecordFormat::PngOrApng => true,
            RecordFormat::Gltf => false,
            RecordFormat::Export(f) => f.includes_light(),
        }
    }
}
