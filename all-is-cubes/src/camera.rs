//! Note: This module is hidden, and its contents re-exported under `all_is_cubes_render::camera`.
//!
//! The contents of this module are just those camera-related items which are also used by:
//! * loading images
//! * `Character`’s eye positioning

use crate::math::{Axis, Cube, FreeCoordinate, FreePoint, FreeVector, GridAab};
use euclid::{RigidTransform3D, Size2D};

// -------------------------------------------------------------------------------------------------

/// Unit-of-measure/coordinate-system type for points/vectors in “eye space”,
/// the space of camera-relative coordinates that are *not* perspective-projected.
///
/// +X is right, +Y is up, +Z is towards-the-viewer (right-handed coordinates).
#[expect(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum Eye {}

/// Representation of a camera viewpoint and orientation.
///
/// Note that this is treated as a transform **from** the origin looking in the &minus;Z
/// direction (eye space) **to** the camera position and orientation (world space).
/// This is done so that the [`RigidTransform3D::translation`] vector is equal to the
/// world position, rather than needing to be rotated by the view direction.
pub type ViewTransform = RigidTransform3D<FreeCoordinate, Eye, Cube>;

/// Calculate an “eye position” (camera position) to view the entire given `bounds`.
///
/// `direction` points in the direction the camera should be relative to the space.
///
/// TODO: This function does not yet consider the effects of field-of-view,
/// and it will need additional parameters to do so.
pub fn eye_for_look_at(bounds: GridAab, direction: FreeVector) -> FreePoint {
    let mut space_radius: FreeCoordinate = 0.0;
    for axis in Axis::ALL {
        space_radius = space_radius.max(bounds.size()[axis].into());
    }
    bounds.center() + direction.normalize() * space_radius // TODO: allow for camera FoV
}

/// Width and height of an image, framebuffer, or window, as measured in actual distinct
/// image pixels.
///
/// For sizes that are in nominal, or “logical” pixel units that have become separated from
/// actual image or display resolution, use `Size2D<T, NominalPixel>`; there is no type
/// alias for that.
pub type ImageSize = Size2D<u32, ImagePixel>;

/// Unit-of-measure type for vectors representing the width and height of an image.
#[expect(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum ImagePixel {}
