//! Numeric types used for coordinates and related quantities.

use euclid::{Box3D, Point3D, Size2D, Size3D, Vector3D};

use crate::math::Cube;

/// Coordinates that are locked to the cube grid.
pub type GridCoordinate = i32;

/// Numeric type in a [`GridSize`].
///
/// TODO: This needs a cleaner name.
pub type GridSizeCoord = u32;

/// Positions that are locked to the cube grid.
pub type GridPoint = Point3D<GridCoordinate, Cube>;

/// Vectors that are locked to the cube grid.
pub type GridVector = Vector3D<GridCoordinate, Cube>;

/// Sizes of grid-aligned objects.
pub type GridSize = Size3D<GridSizeCoord, Cube>;

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

/// Additional element-wise operations on `euclid` types.
// TODO: Contribute these to `euclid` itself
pub trait VectorOps<O> {
    /// Input vector element type.
    type Elem;
    /// Output vector type.
    type Output;
    /// Apply the function to each element.
    fn map<F: FnMut(Self::Elem) -> O>(self, f: F) -> Self::Output;
    /// Apply the function to each element of the two inputs, pairwise.
    fn zip<F: FnMut(Self::Elem, Self::Elem) -> O>(self, rhs: Self, f: F) -> Self::Output;
}

mod impl_euclid {
    use super::*;

    macro_rules! impl_vector_ops {
        ($vec:ident, ($( $field:ident )*)) => {
            impl<T, O, U> VectorOps<O> for $vec<T, U> {
                type Elem = T;
                type Output = $vec<O, U>;

                #[inline]
                fn map<F: FnMut(Self::Elem) -> O>(self, mut f: F) -> Self::Output {
                    $vec::new($(f(self.$field),)*)
                }

                #[inline]
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

    // TODO: contribute inherent methods for this to euclid
    impl_vector_ops!(Size2D, (width height));
    impl_vector_ops!(Size3D, (width height depth));

    // TODO: contribute inherent methods for this (at least map()) to euclid
    impl<T, O: Copy, U> VectorOps<O> for Box3D<T, U> {
        type Elem = T;
        type Output = Box3D<O, U>;

        #[inline]
        fn map<F: FnMut(Self::Elem) -> O>(self, mut f: F) -> Self::Output {
            Box3D {
                min: self.min.map(&mut f),
                max: self.max.map(&mut f),
            }
        }

        #[inline]
        fn zip<F: FnMut(Self::Elem, Self::Elem) -> O>(self, rhs: Self, mut f: F) -> Self::Output {
            Box3D {
                min: self.min.zip(rhs.min, &mut f).to_point(),
                max: self.max.zip(rhs.max, &mut f).to_point(),
            }
        }
    }
}
