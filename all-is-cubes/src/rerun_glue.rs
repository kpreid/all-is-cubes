use alloc::vec::Vec;
use core::fmt;

use re_sdk::{datatypes, RecordingStream};

use crate::math;

// To support concise conditional debugging, this module re-exports many items from rerun.
pub use re_log_types::{entity_path, EntityPath, Index};
pub use re_sdk::{archetypes, components};

/// Information that an entity or parent of entities can store in order to know where to
/// send their Rerun logging data.
#[derive(Clone)]
#[allow(clippy::exhaustive_structs)]
pub struct Destination {
    pub stream: RecordingStream,
    pub path: EntityPath,
}

impl fmt::Debug for Destination {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Destination")
            .field("path", &self.path)
            .finish_non_exhaustive()
    }
}

impl Default for Destination {
    fn default() -> Self {
        Self {
            stream: RecordingStream::disabled(),
            path: EntityPath::root(),
        }
    }
}

impl Destination {
    pub fn log(&self, path_suffix: &EntityPath, data: &impl re_sdk::AsComponents) {
        match self.stream.log(self.path.join(path_suffix), data) {
            Ok(()) => (),
            Err(e) => log::error!("Rerun logging failed: {e}", e = crate::util::ErrorChain(&e)),
        }
    }

    pub fn clear_recursive(&self, path_suffix: &EntityPath) {
        // TODO: this is no longer necessary
        self.log(path_suffix, &archetypes::Clear::new(true));
    }

    #[must_use]
    pub fn child(&self, path_suffix: &EntityPath) -> Self {
        Self {
            stream: self.stream.clone(),
            path: self.path.join(path_suffix),
        }
    }
}

// --- Entities ---

// --- Components ---

pub fn convert_point<S, U>(point: euclid::Point3D<S, U>) -> components::Position3D
where
    S: num_traits::NumCast + Copy,
{
    let array: [f32; 3] = point.cast::<f32>().into();
    components::Position3D::from(array)
}
pub fn convert_vec<S, U>(v: euclid::Vector3D<S, U>) -> datatypes::Vec3D
where
    S: num_traits::NumCast + Copy,
{
    let array: [f32; 3] = v.cast::<f32>().into();
    datatypes::Vec3D::from(array)
}

pub fn convert_aabs(
    aabs: impl IntoIterator<Item = math::Aab>,
    offset: math::FreeVector,
) -> archetypes::Boxes3D {
    let (half_sizes, centers): (Vec<components::HalfSizes3D>, Vec<components::Position3D>) = aabs
        .into_iter()
        .map(|aab| {
            (
                components::HalfSizes3D(convert_vec(aab.size() / 2.0)),
                convert_point(aab.center() + offset),
            )
        })
        .unzip();
    archetypes::Boxes3D::from_half_sizes(half_sizes).with_centers(centers)
}

impl From<math::Face6> for re_sdk::coordinates::SignedAxis3 {
    fn from(face: math::Face6) -> Self {
        use math::Face6;
        use re_sdk::coordinates::{Axis3, Sign, SignedAxis3};
        match face {
            Face6::NX => SignedAxis3 {
                sign: Sign::Negative,
                axis: Axis3::X,
            },
            Face6::NY => SignedAxis3 {
                sign: Sign::Negative,
                axis: Axis3::Y,
            },
            Face6::NZ => SignedAxis3 {
                sign: Sign::Negative,
                axis: Axis3::Z,
            },
            Face6::PX => SignedAxis3 {
                sign: Sign::Positive,
                axis: Axis3::X,
            },
            Face6::PY => SignedAxis3 {
                sign: Sign::Positive,
                axis: Axis3::Y,
            },
            Face6::PZ => SignedAxis3 {
                sign: Sign::Positive,
                axis: Axis3::Z,
            },
        }
    }
}

impl From<math::Rgb> for components::Color {
    fn from(value: math::Rgb) -> Self {
        value.with_alpha_one().into()
    }
}
impl From<math::Rgba> for components::Color {
    fn from(value: math::Rgba) -> Self {
        let [r, g, b, a] = value.to_srgb8();
        components::Color::from_unmultiplied_rgba(r, g, b, a)
    }
}
