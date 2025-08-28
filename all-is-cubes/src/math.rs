//! Mathematical utilities and decisions.

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

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

#[inline]
pub(crate) fn smoothstep(x: f64) -> f64 {
    let x = x.clamp(0.0, 1.0);
    3. * x.powi(2) - 2. * x.powi(3)
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
