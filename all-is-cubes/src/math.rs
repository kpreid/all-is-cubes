//! Mathematical utilities and decisions.

use euclid::Vector3D;
use num_traits::identities::Zero;
pub use ordered_float::{FloatIsNan, NotNan};

use crate::util::{ConciseDebug, CustomFormat};

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
mod rigid;
pub use rigid::*;
mod matrix;
pub use matrix::*;
mod rotation;
pub use rotation::*;

/// Allows writing a [`NotNan`] value as a constant expression (which is not currently
/// a feature provided by the [`ordered_float`] crate itself).
///
/// Note that if the expression does not need to be constant, this macro may not be
/// needed; infallible construction can be written using `NotNan::from(an_integer)`,
/// `NotNan::zero()`, and `NotNan::one()`.
///
/// ```
/// use all_is_cubes::{notnan, math::NotNan};
///
/// const X: NotNan<f32> = notnan!(1.234);
/// ```
///
/// ```compile_fail
/// # use all_is_cubes::{notnan, math::NotNan};
/// // Not a literal; will not compile
/// const X: NotNan<f32> = notnan!(f32::NAN);
/// ```
///
/// ```compile_fail
/// # use all_is_cubes::{notnan, math::NotNan};
/// // Not a literal; will not compile
/// const X: NotNan<f32> = notnan!(0.0 / 0.0);
/// ```
///
/// ```compile_fail
/// # use all_is_cubes::{notnan, math::NotNan};
/// const N0N: f32 = f32::NAN;
/// // Not a literal; will not compile
/// const X: NotNan<f32> = notnan!(N0N);
/// ```
///
/// ```compile_fail
/// # use all_is_cubes::{notnan, math::NotNan};
/// // Not a float; will not compile
/// const X: NotNan<char> = notnan!('a');
/// ```
#[doc(hidden)]
#[macro_export] // used by all-is-cubes-content
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

#[inline]
pub(crate) fn smoothstep(x: f64) -> f64 {
    let x = x.clamp(0.0, 1.0);
    3. * x.powi(2) - 2. * x.powi(3)
}

/// Sort exactly two items; swap them if `a > b`.
#[inline]
#[doc(hidden)]
pub fn sort_two<T: PartialOrd>(a: &mut T, b: &mut T) {
    if *a > *b {
        std::mem::swap(a, b);
    }
}

/// Common features of objects that have a location and shape in space.
pub trait Geometry {
    /// Type of coordinates; generally determines whether this object can be translated by a
    /// non-integer amount.
    type Coord;

    /// Translate (move) this object by the specified offset.
    #[must_use]
    fn translate(self, offset: Vector3D<Self::Coord, Cube>) -> Self;

    /// Represent this object as a line drawing, or wireframe.
    ///
    /// The generated points should be in pairs, each pair defining a line segment.
    /// If there are an odd number of vertices, the caller should ignore the last.
    ///
    /// TODO: This should probably return an iterator instead, but defining the type
    /// will be awkward until `type_alias_impl_trait` is stable.
    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>;
}

/// One end of a line to be drawn.
///
/// Used for debugging visualizations and not for game content, with the current exception
/// of [`Cursor`](crate::character::Cursor).
///
/// The primary way in which these are used in this crate is
/// [`Geometry::wireframe_points()`].
#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct LineVertex {
    /// Position of the vertex.
    pub position: FreePoint,

    /// Color in which to draw the line.
    ///
    /// If [`None`], a color set by the context/parent should be used instead.
    ///
    /// If the ends of a line are different colors, color should be interpolated along
    /// the line.
    pub color: Option<crate::math::Rgba>,
}

impl From<FreePoint> for LineVertex {
    fn from(position: FreePoint) -> Self {
        Self {
            position,
            color: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoothstep_test() {
        assert_eq!(smoothstep(0.0), 0.0);
        assert_eq!(smoothstep(0.5), 0.5);
        assert_eq!(smoothstep(1.0), 1.0);
    }
}
