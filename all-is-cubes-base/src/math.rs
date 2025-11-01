//! Mathematical utilities and decisions.

use euclid::Vector3D;
use num_traits::identities::Zero;
pub use ordered_float::{FloatIsNan, NotNan};

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::util::ConciseDebug;

mod aab;
pub use aab::*;
mod axis;
pub use axis::*;
#[macro_use]
mod color;
pub use color::*;
mod coord;
pub use coord::*;
mod cube;
pub use cube::Cube;
mod face;
pub use face::*;
mod grid_aab;
pub use grid_aab::*;
mod grid_iter;
pub use grid_iter::*;
pub mod lines;
mod rigid;
pub use rigid::*;
mod restricted_number;
pub use restricted_number::*;
mod matrix;
pub use matrix::*;
mod octant;
pub use octant::*;
mod rotation;
pub use rotation::*;
#[cfg(feature = "serde")]
mod serde_impls;
mod vol;
pub use vol::*;

// We make an assumption in several places that `usize` is at least 32 bits.
// It's likely that compilation would not succeed anyway, but let's make it explicit.
#[cfg(target_pointer_width = "16")]
compile_error!("all-is-cubes does not support platforms with less than 32-bit `usize`");

/// Allows writing a [`NotNan`] value as a constant expression (which is not currently
/// a feature provided by the [`ordered_float`] crate itself).
///
/// Note that if the expression does not need to be constant, this macro may not be
/// needed; infallible construction can be written using `NotNan::from(an_integer)`,
/// `NotNan::zero()`, and `NotNan::one()`.
///
/// # Examples
///
/// ```
/// # extern crate all_is_cubes_base as all_is_cubes;
/// use all_is_cubes::{notnan, math::NotNan};
///
/// const X: NotNan<f32> = notnan!(1.234);
/// ```
///
/// If anything other than a floating-point literal is used, the code will not compile:
///
/// ```compile_fail
/// # extern crate all_is_cubes_base as all_is_cubes;
/// # use all_is_cubes::math::{NotNan, notnan};
/// // Not a literal; will not compile
/// const X: NotNan<f32> = notnan!(f32::NAN);
/// ```
///
/// ```compile_fail
/// # extern crate all_is_cubes_base as all_is_cubes;
/// # use all_is_cubes::math::{NotNan, notnan};
/// // Not a literal; will not compile
/// const X: NotNan<f32> = notnan!(0.0 / 0.0);
/// ```
///
/// ```compile_fail
/// # extern crate all_is_cubes_base as all_is_cubes;
/// # use all_is_cubes::math::{NotNan, notnan};
/// const N0N: f32 = f32::NAN;
/// // Not a literal; will not compile
/// const X: NotNan<f32> = notnan!(N0N);
/// ```
///
/// ```compile_fail
/// # extern crate all_is_cubes_base as all_is_cubes;
/// # use all_is_cubes::math::{NotNan, notnan};
/// // Not a float; will not compile
/// const X: NotNan<char> = notnan!('a');
/// ```
#[doc(hidden)] // reexported publicly within the math module by `all_is_cubes`
#[macro_export]
macro_rules! notnan {
    ($value:literal) => {
        match $value {
            value => {
                // Safety: Only literal values are allowed, which will either be a non-NaN
                // float or (as checked below) a type mismatch.
                let result = unsafe { $crate::math::NotNan::new_unchecked(value) };

                // Ensure that the type is one which could have resulted from a float literal,
                // by requiring type unification with a literal. This prohibits char, &str, etc.
                let _ = if false {
                    // Safety: Statically never NaN, and is also never executed.
                    unsafe { $crate::math::NotNan::new_unchecked(0.0) }
                } else {
                    result
                };

                result
            }
        }
    };
}

#[cfg(not(any(feature = "std", test)))]
#[allow(dead_code, reason = "unclear why this warns even though it is needed")]
/// Identical to [`num_traits::Euclid`] except that its signatures are compatible with
/// `std` versions.
///
/// Note: this code is duplicated between `all-is-cubes` and
/// `all-is-cubes-base` so that it doesn't need to be public.
pub(crate) trait Euclid {
    fn div_euclid(self, rhs: Self) -> Self;
    fn rem_euclid(self, rhs: Self) -> Self;
}
#[cfg(not(any(feature = "std", test)))]
impl<T: num_traits::Euclid + Copy> Euclid for T {
    fn div_euclid(self, rhs: Self) -> Self {
        <T as num_traits::Euclid>::div_euclid(&self, &rhs)
    }
    fn rem_euclid(self, rhs: Self) -> Self {
        <T as num_traits::Euclid>::rem_euclid(&self, &rhs)
    }
}

/// Sort exactly two items; swap them if `a > b`.
#[inline]
#[doc(hidden)]
pub fn sort_two<T: PartialOrd>(a: &mut T, b: &mut T) {
    if *a > *b {
        core::mem::swap(a, b);
    }
}

/// Returns the length of the vector in [Chebyshev distance].
///
/// Returns the maximum length of the vector’s projections onto all 3 coordinate axes.
/// This has the property that all points forming a cube centered on the origin will have the
/// same `chebyshev_length()`.
///
/// This is useful for procedurally generating layered cubical shapes.
/// It can also be used to generate squares if the third axis is set to zero.
///
/// If any element is NaN (more precisely, if [`PartialOrd`] returns [`None`]),
/// the result will be the absolute value of an arbitrary one of the inputs.
///
/// [Chebyshev distance]: <https://en.wikipedia.org/w/index.php?title=Chebyshev_distance&oldid=1314633868>.
///
/// # Examples
///
/// ```
/// # extern crate all_is_cubes_base as all_is_cubes;
/// use all_is_cubes::math::{chebyshev_length, FreeVector, GridVector};
///
/// assert_eq!(chebyshev_length(GridVector::new(1, 3, 2)), 3);
/// assert_eq!(chebyshev_length(GridVector::new(2, -1, -3)), 3);
/// assert_eq!(chebyshev_length(FreeVector::new(1.0, -2.5, 3.75)), 3.75);
/// ```
#[inline]
#[allow(clippy::needless_pass_by_value)]
pub fn chebyshev_length<S: num_traits::Signed + PartialOrd, U>(v: Vector3D<S, U>) -> S {
    let abs_x = v.x.abs();
    let abs_y = v.y.abs();
    let abs_z = v.z.abs();

    // 3-way max without requiring `Ord` or `Float` traits.
    let mut result = abs_x;
    if abs_y > result {
        result = abs_y;
    }
    if abs_z > result {
        result = abs_z;
    }
    result
}
