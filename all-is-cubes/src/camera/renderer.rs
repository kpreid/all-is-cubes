// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use futures_core::future::BoxFuture;
use image::RgbaImage;

use crate::character::Cursor;
use crate::universe::RefError;

/// Rendering a previously-specified scene to an in-memory image.
pub trait HeadlessRenderer {
    /// Update the renderer's internal copy of the scene from the data sources
    /// (`URef<Character>` etc.) it is tracking.
    ///
    /// Returns [`RenderError::Read`] if said sources are in use or if some other
    /// prohibitive failure occurred. The resulting state of the renderer in such cases
    /// is not specified, but a good implementation should attempt recovery on a future
    /// call.
    ///
    /// TODO: provide for returning performance info?
    ///
    /// TODO: we may want more dynamic information than the cursor
    fn update<'a>(
        &'a mut self,
        cursor: Option<&'a Cursor>,
    ) -> BoxFuture<'a, Result<(), RenderError>>;

    /// Produce an image of the current state of the scene this renderer was created to
    /// track, as of the last call to [`Self::update()`], with the given overlaid text.
    ///
    /// This operation does not attempt to access the scene objects and therefore may be
    /// called while the [`Universe`] is being stepped, etc.
    ///
    /// The returned image is always 8 bits per component and should be in the sRGB color
    /// space. (This trait may be revised in the future to support HDR rendering.)
    ///
    /// TODO: provide for returning performance info?
    ///
    /// [`Universe`]: crate::universe::Universe
    fn draw<'a>(&'a mut self, info_text: &'a str) -> BoxFuture<'a, Result<RgbaImage, RenderError>>;
}

/// An error indicating that a [`HeadlessRenderer`] failed to operate.
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
#[non_exhaustive]
pub enum RenderError {
    /// A component of the [`Universe`] that is to be rendered was not available
    /// for reading.
    ///
    /// TODO: The renderers currently implemented don't return this error but panic.
    ///
    /// [`Universe`]: crate::universe::Universe
    #[error("scene to be rendered was not available for reading")]
    Read(RefError),
    // TODO: add errors for out of memory, lost GPU, etc.
}
