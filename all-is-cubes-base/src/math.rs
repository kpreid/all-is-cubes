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

/// Geometric objects that can be drawn as wireframes.
pub trait Wireframe {
    /// Represent this object as a line drawing, or wireframe.
    ///
    /// The generated points should be in pairs, each pair defining a line segment.
    /// If there are an odd number of vertices, the caller should ignore the last.
    ///
    /// Design note: This method accepts a destination to write to, rather than returning an
    /// iterator, because if it did return an iterator, it would be difficult to compose in
    /// ways like allocating a temporary `Wireframe` and delegating to that, if it borrowed
    /// its input, and would risk composing a very large yet unnecessary iterator struct
    /// if it owned its input.
    /// This way, composition is simply calling further functions.
    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>;
}

impl<T: Wireframe> Wireframe for Option<T> {
    #[allow(clippy::missing_inline_in_public_items)]
    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>,
    {
        if let Some(value) = self {
            value.wireframe_points(output)
        }
    }
}

/// One end of a line to be drawn.
///
/// These are the output of [`Wireframe::wireframe_points()`].
#[derive(Clone, Copy, Debug, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct LineVertex {
    /// Position of the vertex.
    pub position: FreePoint,

    /// Color in which to draw the line.
    ///
    /// If [`None`], a color set by the context/parent should be used instead.
    ///
    /// If the ends of a line are different colors, color should be interpolated along
    /// the line.
    pub color: Option<Rgba>,
}

impl From<FreePoint> for LineVertex {
    #[inline]
    fn from(position: FreePoint) -> Self {
        Self {
            position,
            color: None,
        }
    }
}
