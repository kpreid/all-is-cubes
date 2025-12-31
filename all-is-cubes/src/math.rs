//! Mathematical utilities and decisions.

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

// -------------------------------------------------------------------------------------------------

// Most of the content of this module is implemented in `all_is_cubes_base::math`.
#[doc(inline)]
pub use all_is_cubes_base::math::*;

// A crate defining a macro can't export it except at the root,
// but when we re-export, we can put them in their right place (this module).
#[doc(inline)]
pub use all_is_cubes_base::{notnan, rgb_const, rgb01, rgba_const};

/// An axis-aligned box with integer coordinates.
/// [`GridAab`]s are used to specify the coordinate extent of [`Space`](crate::space::Space)s, and
/// regions within them.
///
/// When we refer to “a cube” in a [`GridAab`], that is a unit cube which is identified by the
/// integer coordinates of its most negative corner, in the fashion of [`Cube`].
///
/// A [`GridAab`] may have a zero-size range in any direction, thus making its total volume zero.
/// The different possibilities are not considered equal; thus, points, lines, and planes may be
/// represented, which may be useful for procedural-generation purposes.
///
#[doc = include_str!("save/serde-warning.md")]
pub use all_is_cubes_base::math::GridAab;

// -------------------------------------------------------------------------------------------------

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

// -------------------------------------------------------------------------------------------------

/// Converts a vector with float components into a vector with finite [`NotNan`] components.
///
/// Note that this is stricter than [`NotNan::new()`] in that it rejects infinities.
#[inline]
pub(crate) fn try_into_finite_vector<T: ordered_float::FloatCore, U>(
    input: euclid::Vector3D<T, U>,
) -> Option<euclid::Vector3D<NotNan<T>, U>> {
    Some(euclid::Vector3D::new(
        try_into_finite(input.x)?,
        try_into_finite(input.y)?,
        try_into_finite(input.z)?,
    ))
}

/// Converts a point with float components into a pointwith finite [`NotNan`] components.
///
/// Note that this is stricter than [`NotNan::new()`] in that it rejects infinities.
#[inline]
pub(crate) fn try_into_finite_point<T: ordered_float::FloatCore, U>(
    input: euclid::Point3D<T, U>,
) -> Option<euclid::Point3D<NotNan<T>, U>> {
    Some(euclid::Point3D::new(
        try_into_finite(input.x)?,
        try_into_finite(input.y)?,
        try_into_finite(input.z)?,
    ))
}

#[inline]
pub(crate) fn try_into_finite<T: ordered_float::FloatCore>(value: T) -> Option<NotNan<T>> {
    if !value.is_finite() {
        None
    } else {
        NotNan::new(value).ok()
    }
}
