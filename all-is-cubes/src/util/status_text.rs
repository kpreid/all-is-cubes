use core::fmt;

use manyfmt::Fmt;

use all_is_cubes_base::util::ConciseDebug;

/// Format type for [`manyfmt::Fmt`] which provides an highly condensed, ideally constant-width
/// or constant-height, user-facing format for live-updating textual status messages.
///
/// This format does not follow Rust [`fmt::Debug`] syntax, and when implemented
/// for standard Rust types may have quirks. Values may have multiple lines.
#[expect(clippy::exhaustive_structs)]
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
    #[cfg_attr(feature = "_serde_math_and_graphics_options", derive(serde::Serialize, serde::Deserialize))]
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
    #[doc(hidden)] // just a substitute for const trait impl
    pub const DEFAULT: Self = Self::WORLD.union(Self::STEP).union(Self::RENDER).union(Self::CURSOR);
}

impl Default for ShowStatus {
    /// A partial set of flags which makes a reasonable default for introducing users to the
    /// status text.
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl Fmt<StatusText> for core::time::Duration {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        <Self as Fmt<ConciseDebug>>::fmt(self, fmt, &ConciseDebug)
    }
}
