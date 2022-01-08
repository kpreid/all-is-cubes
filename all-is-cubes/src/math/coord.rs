// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

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

/// Convert a `GridPoint` used to identify a cube to the midpoint of that cube.
/// That is, convert the number type and add 0.5.
pub fn cube_to_midpoint(cube: GridPoint) -> Point3<FreeCoordinate> {
    cube.map(|component| FreeCoordinate::from(component) + 0.5)
}

/// Compute the squared magnitude of a [`GridVector`].
///
/// [`cgmath::InnerSpace::magnitude2`] would do the same but only for floats.
#[inline]
pub(crate) fn int_magnitude_squared(v: GridVector) -> GridCoordinate {
    v.x * v.x + v.y * v.y + v.z * v.z
}
