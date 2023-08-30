//! Numeric types used for coordinates and related quantities.

use cgmath::{Point3, Vector3};

/// Coordinates that are locked to the cube grid.
pub type GridCoordinate = i32;

/// Positions that are locked to the cube grid.
pub type GridPoint = Point3<GridCoordinate>;

/// Vectors that are locked to the cube grid.
pub type GridVector = Vector3<GridCoordinate>;

/// Coordinates that are not locked to the cube grid.
///
/// Note: Because `GridCoordinate = i32` and `FreeCoordinate = f64`, which has
/// more than 32 bits of mantissa, the infallible conversion
/// `From<GridCoordinate> for FreeCoordinate` exists, which is often convenient.
pub type FreeCoordinate = f64;

/// Compute the squared magnitude of a [`GridVector`].
///
/// [`cgmath::InnerSpace::magnitude2`] would do the same but only for floats.
#[inline(always)] // trivial arithmetic that almost certainly should be inlined
pub(crate) fn int_magnitude_squared(v: GridVector) -> GridCoordinate {
    v.x * v.x + v.y * v.y + v.z * v.z
}
