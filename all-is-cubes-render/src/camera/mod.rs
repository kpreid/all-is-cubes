//! Projection and view matrices, viewport and aspect ratio, visibility,
//! projecting camera rays into the scene, graphics options, etc.

use all_is_cubes::euclid::Size2D;

#[cfg(doc)]
use all_is_cubes::space::Space;

// -------------------------------------------------------------------------------------------------

/// Defines a perspective view in/of the world.
///
/// A [`Camera`] has the following independently controllable properties.
///
/// * A [`ViewTransform`], which specifies the viewpoint (eye position) and
///   direction.
/// * A [`Viewport`], whose aspect ratio is used in the projection matrix.
/// * A [`GraphicsOptions`], whose `fov_y` is used in the projection matrix.
/// * A “measured exposure” (luminance scale factor) value which should be provided by examining
///   the world, and which is carried through to [the output](Camera::exposure).
///
/// From this information, it derives [view](Camera::view_matrix) and
/// [projection](Camera::projection_matrix) matrices.
///
/// It does not specify *what* [`Space`] is to be viewed; more broadly, it is
/// plain data structure that does some calculations, and knows nothing outside of that.
/// See [`StandardCameras`] for going further.
pub use all_is_cubes::camera::Camera;

// As a workaround for <https://github.com/rust-lang/rust/issues/127445>,
// we list some items explicitly even though they should be redundant with
// the glob re-exports.
pub use all_is_cubes::camera::{GraphicsOptions, ToneMappingOperator, ViewTransform, Viewport, *};

mod stdcam;
pub use stdcam::*;

// -------------------------------------------------------------------------------------------------

/// Calculate area and convert to `usize`, which is a common operation for image data lengths.
#[inline]
#[doc(hidden)] // intended as a utility for our code, not public API
pub fn area_usize<T: TryInto<usize>, U>(size: Size2D<T, U>) -> Option<usize> {
    let width = size.width.try_into().ok()?;
    let height = size.height.try_into().ok()?;
    width.checked_mul(height)
}
