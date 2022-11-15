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
    pub struct Flaws: u16 {
        /// Whether the scene is incomplete due to the renderer not having had enough
        /// time to finish initialization or catch up to changes.
        const UNFINISHED = 1 << 0;

        // TODO:
        // /// Whether the rendering is incomplete due to insufficient resources to
        // /// support the full complexity of the scene.
        // const TOO_MUCH = 1 << 1;

        /// Whether antialiasing has not been used,
        /// despite being requested by the graphics options.
        const NO_ANTIALIASING = 1 << 2;

        /// Whether a cursor has not been rendered,
        /// despite one being given.
        const NO_CURSOR = 1 << 3;

        /// Whether view-distance fog has not been rendered,
        /// despite being requested by the graphics options.
        ///
        /// This does not cover explicitly semitransparent objects within the scene.
        const NO_FOG = 1 << 4;
    }
}

impl Default for Flaws {
    /// Equivalent to [`Self::empty()`].
    fn default() -> Self {
        Self::empty()
    }
}
