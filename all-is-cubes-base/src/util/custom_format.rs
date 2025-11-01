#![allow(clippy::missing_inline_in_public_items)]

use core::fmt;
use core::marker::PhantomData;
use core::time::Duration;

use manyfmt::{Fmt, Refmt as _};

/// Format type for [`manyfmt::Fmt`] which prints the name of a type.
/// The value is a `PhantomData` to avoid requiring an actual instance of the type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[doc(hidden)] // too specific to be good public API ... arguably should be part of refmt itself.
#[expect(clippy::exhaustive_structs)]
pub struct TypeName;
impl<T> Fmt<TypeName> for PhantomData<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &TypeName) -> fmt::Result {
        write!(fmt, "{}", core::any::type_name::<T>())
    }
}

/// Format type for [`manyfmt::Fmt`] which is similar to [`fmt::Debug`], but uses an
/// alternate concise format.
///
/// This format may be on one line despite the pretty-printing option, and may lose
/// precision or Rust syntax in favor of a short at-a-glance representation.
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ConciseDebug;

impl<T: Fmt<ConciseDebug>, const N: usize> Fmt<ConciseDebug> for [T; N] {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &ConciseDebug) -> fmt::Result {
        fmt.debug_list().entries(self.iter().map(|item| item.refmt(fopt))).finish()
    }
}

impl<T: fmt::Debug, U> Fmt<ConciseDebug> for euclid::Point2D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?})", self.x, self.y)
    }
}
impl<T: fmt::Debug, U> Fmt<ConciseDebug> for euclid::Point3D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}
impl<T: fmt::Debug, U> Fmt<ConciseDebug> for euclid::Vector2D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?})", self.x, self.y)
    }
}
impl<T: fmt::Debug, U> Fmt<ConciseDebug> for euclid::Vector3D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?}, {:+.3?})", self.x, self.y, self.z)
    }
}
impl<T: fmt::Debug, U> Fmt<ConciseDebug> for euclid::Size2D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &ConciseDebug) -> fmt::Result {
        write!(fmt, "({:+.3?}, {:+.3?})", self.width, self.height)
    }
}
impl<T: fmt::Debug, U> Fmt<ConciseDebug> for euclid::Size3D<T, U> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &ConciseDebug) -> fmt::Result {
        write!(
            fmt,
            "({:+.3?}, {:+.3?}, {:+.3?})",
            self.width, self.height, self.depth
        )
    }
}

/// Makes the assumption that [`Duration`]s are per-frame timings and hence the
/// interesting precision is in the millisecond-to-microsecond range.
impl Fmt<ConciseDebug> for Duration {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &ConciseDebug) -> fmt::Result {
        write!(fmt, "{:5.2?} ms", (self.as_micros() as f32) / 1000.0)
    }
}
