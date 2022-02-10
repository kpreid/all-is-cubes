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
#[inline(always)] // trivial arithmetic that almost certainly should be inlined
pub fn cube_to_midpoint(cube: GridPoint) -> Point3<FreeCoordinate> {
    cube.map(|component| FreeCoordinate::from(component) + 0.5)
}

/// Convert a point in space to the unit cube that encloses it.
///
/// Such cubes are defined to be half-open intervals on each axis; that is,
/// an integer coordinate is counted as part of the cube extending positively
/// from that coordinate.
///
/// ```
/// use all_is_cubes::cgmath::Point3;
/// use all_is_cubes::math::point_to_enclosing_cube;
///
/// assert_eq!(point_to_enclosing_cube(Point3::new(1.0, 1.5, -2.5)), Point3::new(1, 1, -3));
/// ```
#[inline(always)] // trivial arithmetic that almost certainly should be inlined
pub fn point_to_enclosing_cube(point: Point3<FreeCoordinate>) -> GridPoint {
    point.map(|component| component.floor() as GridCoordinate)
}

/// Compute the squared magnitude of a [`GridVector`].
///
/// [`cgmath::InnerSpace::magnitude2`] would do the same but only for floats.
#[inline(always)] // trivial arithmetic that almost certainly should be inlined
pub(crate) fn int_magnitude_squared(v: GridVector) -> GridCoordinate {
    v.x * v.x + v.y * v.y + v.z * v.z
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn point_to_enclosing_cube_inf_nan() {
        assert_eq!(
            point_to_enclosing_cube(Point3::new(
                FreeCoordinate::INFINITY,
                -FreeCoordinate::INFINITY,
                FreeCoordinate::NAN
            )),
            Point3::new(GridCoordinate::MAX, GridCoordinate::MIN, 0)
        );
    }
}
