use core::fmt;

use re_sdk::RecordingStream;

use crate::math;

// To support concise conditional debugging, this module re-exports many items from rerun.
pub use re_log_types::{entity_path, EntityPath, Index};
pub use re_sdk::{components, MsgSender};

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
    pub fn send(
        &self,
        path_suffix: &EntityPath,
        f: impl FnOnce(MsgSender) -> Result<MsgSender, re_sdk::MsgSenderError>,
    ) {
        let sender = MsgSender::new(self.path.join(path_suffix));
        match f(sender) {
            Err(e) => log::error!("Rerun logging failed: {e}", e = crate::util::ErrorChain(&e)),
            Ok(sender) => match sender.send(&self.stream) {
                Ok(()) => (),
                Err(e) => log::error!("Rerun logging failed: {e}", e = crate::util::ErrorChain(&e)),
            },
        }
    }

    pub fn clear_recursive(&self, path_suffix: &EntityPath) {
        self.stream
            .record_path_op(re_log_types::PathOp::ClearRecursive(
                self.path.join(path_suffix),
            ))
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

pub fn convert_point<S, U>(point: euclid::Point3D<S, U>) -> components::Point3D
where
    S: num_traits::NumCast + Copy,
{
    let array: [f32; 3] = point.cast::<f32>().into();
    components::Point3D::from(array)
}
pub fn convert_vec<S, U>(point: euclid::Vector3D<S, U>) -> components::Vec3D
where
    S: num_traits::NumCast + Copy,
{
    let array: [f32; 3] = point.cast::<f32>().into();
    components::Vec3D::from(array)
}

pub fn convert_aab(
    aab: math::Aab,
    offset: math::FreeVector,
) -> (components::Box3D, components::Vec3D) {
    let euclid::Vector3D {
        x: sx,
        y: sy,
        z: sz,
        _unit: _,
    } = (aab.size() * 0.5).cast::<f32>();

    // Box3D is *always* centered, so we need to separately extract the Aab's center as a
    // translation.
    let center = aab.center();
    let combined_offset: [f32; 3] = (center + offset).cast::<f32>().into();

    (
        components::Box3D {
            x: sx,
            y: sy,
            z: sz,
        },
        components::Vec3D::from(combined_offset),
    )
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

impl From<math::Rgb> for components::ColorRGBA {
    fn from(value: math::Rgb) -> Self {
        value.with_alpha_one().into()
    }
}
impl From<math::Rgba> for components::ColorRGBA {
    fn from(value: math::Rgba) -> Self {
        let [r, g, b, a] = value.to_srgb8();
        components::ColorRGBA::from_unmultiplied_rgba(r, g, b, a)
    }
}

impl From<crate::raycast::Ray> for components::Arrow3D {
    fn from(value: crate::raycast::Ray) -> Self {
        let crate::raycast::Ray { origin, direction } = value;
        components::Arrow3D {
            origin: convert_vec(origin.to_vector()),
            vector: convert_vec(direction),
        }
    }
}
