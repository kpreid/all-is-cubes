use core::fmt;

const UNFINISHED_BIT: u16 = 1 << 2;
const UNSUPPORTED_BIT: u16 = 1 << 3;

bitflags::bitflags! {
    /// Deficiencies of a rendering.
    ///
    /// This type describes the ways in which a rendered image could fail to accurately
    /// represent the scene, or fail to comply with requested [`GraphicsOptions`].
    ///
    /// It is a [`bitflags`] generated bit-flag type. *Note: We make no guarantees that
    /// the numeric value of flags will stay the same across versions*; please treat this
    /// as a set of named values only.
    ///
    /// The [empty](Self::empty) set means no flaws are present.
    ///
    /// [`GraphicsOptions`]: super::GraphicsOptions
    #[derive(Clone, Copy, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
    pub struct Flaws: u16 {
        /// The rendering has a flaw not otherwise classified.
        const OTHER = 1 << 0;

        /// The scene contains too much complexity for the renderer to ever succeed in rendering it.
        ///
        /// This may refer to numeric overflow or to complexity limits set by configuration.
        const TOO_COMPLEX = 1 << 1;

        /// The rendering is incomplete, but could have been complete if circumstances were
        /// different.
        ///
        /// Several more specific flags include this bit.
        const UNFINISHED = UNFINISHED_BIT;

        /// The graphics options in use requested a feature that was not supported.
        ///
        /// Several more specific flags include this bit.
        const UNSUPPORTED = UNSUPPORTED_BIT;

        /// The rendering is incomplete due to the renderer not having had enough
        /// time to finish initialization or catch up to changes.
        ///
        /// This error may appear non-deterministically, but should never appear when
        /// [`Deadline::Whenever`][all_is_cubes::time::Deadline::Whenever] is used.
        ///
        /// This always includes [`UNFINISHED`][Self::UNFINISHED].
        const OUT_OF_TIME = UNFINISHED_BIT | (1 << 4);

        /// A memory allocation required to complete this rendering failed.
        ///
        /// Since this refers to a shared resource, this error may appear non-deterministically.
        ///
        /// This always includes [`UNFINISHED`][Self::UNFINISHED].
        const OUT_OF_MEMORY = UNFINISHED_BIT | 1 << 5;

        /// The renderer was invoked incorrectly and the rendering is therefore incomplete or
        /// not up to date.
        ///
        /// This always includes [`UNFINISHED`][Self::UNFINISHED].
        const INVOCATION = UNFINISHED_BIT | 1 << 6;

        // TODO: The NO_* feature support flaws are kind of overly specific.
        // thinking that they should be replaced by a general mechanism where we store at most one
        // pointer to a string specifying an unusual condition, which we can use for these
        // and also for reporting unusual errors in `OTHER` and `INVOCATION`.

        /// Antialiasing has not been used,
        /// despite being requested by the graphics options.
        const NO_ANTIALIASING = UNSUPPORTED_BIT | 1 << 7;

        /// Bloom has not been rendered,
        /// despite being requested by the graphics options.
        const NO_BLOOM = UNSUPPORTED_BIT | 1 << 8;

        /// A cursor has not been rendered, despite one being given.
        const NO_CURSOR = UNSUPPORTED_BIT | 1 << 9;

        /// View-distance fog has not been rendered, despite being requested by the
        /// graphics options.
        ///
        /// This does not refer to explicitly semitransparent objects within the scene.
        const NO_FOG = UNSUPPORTED_BIT | 1 << 10;

        /// Surfaces that should have textures rather than a solid color don't.
        ///
        /// This flaw may appear either because the renderer does not support, or has been
        /// configured not to allow, texturing, or because available texture storage has been
        /// exhausted.
        const MISSING_TEXTURES = 1 << 11;
    }
}

impl Flaws {
    /// Whether at least one flaw is present as a result of limits on the resources available during
    /// rendering, rather than a limitation of the rendering algorithm.
    ///
    /// Currently, this means [`Flaws::UNFINISHED`], which is also set by more specific flags.
    pub fn contains_resource_limitation(self) -> bool {
        self.intersects(Flaws::UNFINISHED)
    }
}

const impl Default for Flaws {
    /// Equivalent to [`Self::empty()`].
    fn default() -> Self {
        Self::empty()
    }
}

impl fmt::Display for Flaws {
    /// Displays the flags as text like “`UNFINISHED | NO_FOG`".
    ///
    /// TODO: Change this to “English” text like “unfinished, no fog”?
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::ToString;

    #[test]
    fn display() {
        // TODO: Add something other than the empty string when empty?
        assert_eq!(Flaws::default().to_string(), "");

        assert_eq!(Flaws::UNFINISHED.to_string(), "UNFINISHED");
        assert_eq!(
            (Flaws::UNFINISHED | Flaws::NO_FOG).to_string(),
            "UNFINISHED | UNSUPPORTED | NO_FOG"
        );
    }
}
