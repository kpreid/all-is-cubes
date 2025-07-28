use core::fmt;

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
        /// The rendering is incomplete due to the renderer not having had enough
        /// time to finish initialization or catch up to changes.
        const UNFINISHED = 1 << 0;

        /// The rendering has a flaw not otherwise classified.
        const OTHER = 1 << 1;

        /// Antialiasing has not been used,
        /// despite being requested by the graphics options.
        const NO_ANTIALIASING = 1 << 2;

        /// Bloom has not been rendered,
        /// despite being requested by the graphics options.
        const NO_BLOOM = 1 << 3;

        /// A cursor has not been rendered, despite one being given.
        const NO_CURSOR = 1 << 4;

        /// View-distance fog has not been rendered, despite being requested by the
        /// graphics options.
        ///
        /// This does not refer to explicitly semitransparent objects within the scene.
        const NO_FOG = 1 << 5;

        /// Surfaces that should have textures rather than a solid color don't.
        // TODO: Should this just be one of the things TOO_MUCH means?
        const MISSING_TEXTURES = 1 << 6;

        // TODO:
        // /// The rendering is incomplete due to insufficient resources to
        // /// support the full complexity of the scene.
        // const TOO_MUCH = 1 << ;

    }
}

impl const Default for Flaws {
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
            "UNFINISHED | NO_FOG"
        );
    }
}
