//! Projection and view matrices, viewport and aspect ratio, visibility,
//! projecting camera rays into the scene, graphics options, etc.

use all_is_cubes::euclid::Size2D;

// -------------------------------------------------------------------------------------------------

// As a workaround for <https://github.com/rust-lang/rust/issues/127445>,
// we list all items explicitly and avoid cross-crate glob re-exports.
pub use all_is_cubes::camera::{Eye, ImagePixel, ImageSize, ViewTransform, eye_for_look_at};

mod camera_struct;
pub use camera_struct::{Camera, Ndc, NdcPoint2, NdcPoint3};

mod graphics_options;
pub use graphics_options::{
    AntialiasingOption, ExposureOption, FogOption, GraphicsOptions, LightingOption, RenderMethod,
    ToneMappingOperator, TransparencyOption,
};

mod stdcam;
pub use stdcam::*;

mod viewport;
pub use viewport::{NominalPixel, Viewport};

#[cfg(test)]
mod tests;

// -------------------------------------------------------------------------------------------------

/// Calculate area and convert to `usize`, which is a common operation for image data lengths.
#[inline]
#[doc(hidden)] // intended as a utility for our code, not public API
pub fn area_usize<T: TryInto<usize>, U>(size: Size2D<T, U>) -> Option<usize> {
    let width = size.width.try_into().ok()?;
    let height = size.height.try_into().ok()?;
    width.checked_mul(height)
}
