//! Numeric types used for coordinates and related quantities.

use euclid::{Point3D, Vector3D};

use crate::math::Cube;

/// Coordinates that are locked to the cube grid.
pub type GridCoordinate = i32;

/// Positions that are locked to the cube grid.
pub type GridPoint = Point3D<GridCoordinate, Cube>;

/// Vectors that are locked to the cube grid.
pub type GridVector = Vector3D<GridCoordinate, Cube>;

/// Coordinates that are not locked to the cube grid.
///
/// Note: Because `GridCoordinate = i32` and `FreeCoordinate = f64`, which has
/// more than 32 bits of mantissa, the infallible conversion
/// `From<GridCoordinate> for FreeCoordinate` exists, which is often convenient.
pub type FreeCoordinate = f64;

/// Positions that are not locked to the cube grid but may interact with it.
pub type FreePoint = Point3D<FreeCoordinate, Cube>;

/// Vectors that are not locked to the cube grid but may interact with it.
pub type FreeVector = Vector3D<FreeCoordinate, Cube>;

/// Additional operations, mostly element-wise, on `euclid` vector/point types.
pub trait VectorOps<O> {
    /// Input vector element type.
    type Elem;
    /// Output vector type.
    type Output;
    /// Apply the function to each element.
    fn map<F: FnMut(Self::Elem) -> O>(self, f: F) -> Self::Output;
    /// Apply the function to each element of the two vectors, pairwise.
    fn zip<F: FnMut(Self::Elem, Self::Elem) -> O>(self, rhs: Self, f: F) -> Self::Output;
}

macro_rules! impl_vector_ops {
    ($vec:ident, ($( $field:ident )*)) => {
        impl<T, O, U> VectorOps<O> for $vec<T, U> {
            type Elem = T;
            type Output = $vec<O, U>;

            fn map<F: FnMut(Self::Elem) -> O>(self, mut f: F) -> Self::Output {
                $vec::new($(f(self.$field),)*)
            }
            fn zip<F: FnMut(Self::Elem, Self::Elem) -> O>(
                self,
                rhs: Self,
                mut f: F,
            ) -> Self::Output {
                $vec::new($(f(self.$field, rhs.$field),)*)
            }
        }
    };
}
mod impl_euclid {
    use super::*;
    use euclid::*;
    impl_vector_ops!(Vector2D, (x y));
    impl_vector_ops!(Vector3D, (x y z));
    impl_vector_ops!(Point2D, (x y));
    impl_vector_ops!(Point3D, (x y z));
    impl_vector_ops!(Size2D, (width height));
    impl_vector_ops!(Size3D, (width height depth));
}
