use core::fmt;
use core::marker::PhantomData;
use core::time::Duration;

use manyfmt::{Fmt, Refmt as _};

/// Format type for [`manyfmt::Fmt`] which prints the name of a type.
/// The value is a `PhantomData` to avoid requiring an actual instance of the type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct TypeName;
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
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ConciseDebug;

impl<T: Fmt<ConciseDebug>, const N: usize> Fmt<ConciseDebug> for [T; N] {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, fopt: &ConciseDebug) -> fmt::Result {
        fmt.debug_list()
            .entries(self.iter().map(|item| item.refmt(fopt)))
            .finish()
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
impl Fmt<StatusText> for Duration {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        <Self as Fmt<ConciseDebug>>::fmt(self, fmt, &ConciseDebug)
    }
}

/// Format type for [`manyfmt::Fmt`] which provides an highly condensed, ideally constant-width
/// or constant-height, user-facing format for live-updating textual status messages.
/// This format does not follow Rust [`fmt::Debug`] syntax, and when implemented
/// for standard Rust types may have quirks. Values may have multiple lines.
#[allow(clippy::exhaustive_structs)]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct StatusText {
    /// Types of information to include or exclude.
    pub show: ShowStatus,
}

impl StatusText {
    #[allow(missing_docs)]
    pub const ALL: Self = Self {
        show: ShowStatus::all(),
    };
}

bitflags::bitflags! {
    /// Different kinds of information which [`StatusText`] may include or exclude.
    ///
    ///  I apologize for this being so specific to All is Cubes internals.
    #[derive(Clone, Copy, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
    // TODO: deserialization should be lenient and ignore unknown textual flags
    #[cfg_attr(feature = "save", derive(serde::Serialize, serde::Deserialize))]
    pub struct ShowStatus: u32 {
        /// The “game world” universe (not the UI).
        const WORLD = 1 << 0;
        /// The UI universe (not the game world).
        const UI = 1 << 1;

        /// Simulation; advancing time.
        const STEP = 1 << 2;
        /// Drawing the current state.
        const RENDER = 1 << 3;
        /// Information about what the cursor is targeting.
        const CURSOR = 1 << 4;

        /// Things related to the processing of blocks.
        const BLOCK = 1 << 5;
        /// Things related to character control and physics.
        const CHARACTER = 1 << 6;
        /// Things related to [`Space`](crate::space::Space)s.
        const SPACE = 1 << 7;
    }
}

impl ShowStatus {
    pub(crate) const DEFAULT: Self = Self::WORLD
        .union(Self::STEP)
        .union(Self::RENDER)
        .union(Self::CURSOR);
}

impl Default for ShowStatus {
    /// A partial set of flags which makes a reasonable default for introducing users to the
    /// status text.
    fn default() -> Self {
        Self::DEFAULT
    }
}
