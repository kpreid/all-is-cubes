use alloc::vec::Vec;
use core::fmt;

use re_sdk::{RecordingStream, RecordingStreamResult};

use crate::math;

// To support concise conditional debugging, this module re-exports many items from rerun.
pub use re_log_types::{entity_path, EntityPath};
pub use re_types::datatypes;
pub use re_types::external::arrow2::types::f16;
pub use re_types::{archetypes, components, view_coordinates};

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
    /// Log the initial data every stream should have.
    pub fn log_initialization(&self) {
        let path = EntityPath::from(vec![]);
        self.catch(|| {
            self.stream
                .log_timeless(path.clone(), &annotation_context())?;
            self.stream.log_timeless(
                path.clone(),
                &archetypes::ViewCoordinates::new(OUR_VIEW_COORDINATES),
            )?;
            Ok(())
        });
    }

    pub fn is_enabled(&self) -> bool {
        self.stream.is_enabled()
    }

    #[allow(clippy::unused_self)]
    fn catch(&self, f: impl FnOnce() -> RecordingStreamResult<()>) {
        match f() {
            Ok(()) => (),
            Err(e) => log::error!("Rerun logging failed: {e}", e = crate::util::ErrorChain(&e)),
        }
    }

    pub fn log(&self, path_suffix: &EntityPath, data: &impl re_sdk::AsComponents) {
        self.catch(|| self.stream.log(self.path.join(path_suffix), data))
    }

    pub fn log_timeless(&self, path_suffix: &EntityPath, data: &impl re_sdk::AsComponents) {
        self.catch(|| self.stream.log_timeless(self.path.join(path_suffix), data))
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
    #[must_use]
    pub fn into_child(self, path_suffix: &EntityPath) -> Self {
        Self {
            stream: self.stream,
            path: self.path.join(path_suffix),
        }
    }
}

// --- Timeless configuration ---

const OUR_VIEW_COORDINATES: components::ViewCoordinates = components::ViewCoordinates::RUB;

/// Enum describing class id numbers we use in our rerun streams.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
#[repr(u16)]
pub enum ClassId {
    SpaceBlock,
    BodyCollisionBox,
    CollisionContactWithin,
    CollisionContactAgainst,
}
impl From<ClassId> for datatypes::ClassId {
    fn from(value: ClassId) -> Self {
        Self(value as u16)
    }
}

fn annotation_context() -> archetypes::AnnotationContext {
    use crate::content::palette as p;
    use ClassId as C;

    #[rustfmt::skip]
    let descs = [
        (C::BodyCollisionBox, "body box", p::DEBUG_COLLISION_BOX),
        (C::CollisionContactWithin, "within", p::DEBUG_COLLISION_CUBE_WITHIN),
        (C::CollisionContactAgainst, "against", p::DEBUG_COLLISION_CUBE_AGAINST),
        (C::SpaceBlock, "", rgba_const!(0.15, 0.15, 0.15, 1.0)),
    ];

    archetypes::AnnotationContext::new(descs.into_iter().map(|(id, label, color)| {
        datatypes::AnnotationInfo {
            id: id as u16,
            label: Some(label)
                .filter(|label| !label.is_empty())
                .map(datatypes::Utf8::from),
            color: Some(datatypes::Rgba32::from(color)),
        }
    }))
}

// --- Data conversion ---

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
fn convert_half_sizes<S, U>(size: euclid::Size3D<S, U>) -> components::HalfSizes3D
where
    S: num_traits::NumCast + Copy,
{
    components::HalfSizes3D(convert_vec(size.to_vector().cast::<f32>() / 2.0))
}
pub fn convert_quaternion<S, Src, Dst>(
    rot: euclid::Rotation3D<S, Src, Dst>,
) -> datatypes::Quaternion
where
    S: num_traits::NumCast,
{
    let rot = [rot.i, rot.j, rot.k, rot.r];
    datatypes::Quaternion(rot.map(|c| c.to_f32().unwrap()))
}

pub fn convert_transform<S, Src, Dst>(
    t: euclid::RigidTransform3D<S, Src, Dst>,
    from_parent: bool,
) -> datatypes::Transform3D
where
    S: num_traits::NumCast + Copy,
{
    datatypes::Transform3D::TranslationRotationScale(datatypes::TranslationRotationScale3D {
        translation: Some(convert_vec(t.translation)),
        rotation: Some(datatypes::Rotation3D::Quaternion(convert_quaternion(
            t.rotation,
        ))),
        scale: None,
        from_parent,
    })
}

pub fn convert_aabs(
    aabs: impl IntoIterator<Item = math::Aab>,
    offset: math::FreeVector,
) -> archetypes::Boxes3D {
    let (half_sizes, centers): (Vec<components::HalfSizes3D>, Vec<components::Position3D>) = aabs
        .into_iter()
        .map(|aab| {
            (
                convert_half_sizes(aab.size()),
                convert_point(aab.center() + offset),
            )
        })
        .unzip();
    archetypes::Boxes3D::from_half_sizes(half_sizes).with_centers(centers)
}

pub fn convert_camera_to_pinhole(
    camera: &crate::camera::Camera,
) -> (archetypes::Pinhole, archetypes::Transform3D) {
    let size = camera.viewport().framebuffer_size.to_f32();
    let half = size * 0.5;
    let aspect = size.width / size.height;
    // tangent of the half-field of view, which is the ratio of the image half-size to the focal length
    let tan_fov_y = (camera.fov_y() * 0.5).to_radians().tan() as f32;
    let tan_fov_x = tan_fov_y * aspect;
    #[rustfmt::skip]
    let pinhole_matrix = datatypes::Mat3x3([
        // column major matrix (i.e. transposed in this textual display)
        half.width / tan_fov_x, 0.,                       0.,
        0.,                     half.height / tan_fov_y,  0.,
        half.width,             half.height,              1.,
    ]);
    (
        archetypes::Pinhole {
            image_from_camera: components::PinholeProjection(pinhole_matrix),
            resolution: Some(components::Resolution(datatypes::Vec2D(size.into()))),
            camera_xyz: Some(OUR_VIEW_COORDINATES),
        },
        archetypes::Transform3D {
            // TODO: shouldn't from_parent be true?
            transform: convert_transform(camera.view_transform(), false).into(),
        },
    )
}

pub fn milliseconds(d: core::time::Duration) -> archetypes::Scalar {
    archetypes::Scalar::new(d.as_secs_f64() * 1000.0)
}

impl From<math::Face6> for view_coordinates::SignedAxis3 {
    fn from(face: math::Face6) -> Self {
        use math::Face6;
        use view_coordinates::{Axis3, Sign, SignedAxis3};
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
// impl From<math::Rgba> for components::Color {
//     fn from(value: math::Rgba) -> Self {
//         let [r, g, b, a] = value.to_srgb8();
//         components::Color::from_unmultiplied_rgba(r, g, b, a)
//     }
// }
impl From<math::Rgba> for datatypes::Rgba32 {
    fn from(value: math::Rgba) -> Self {
        let [r, g, b, a] = value.to_srgb8();
        datatypes::Rgba32::from_unmultiplied_rgba(r, g, b, a)
    }
}
