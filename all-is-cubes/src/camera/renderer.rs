use embedded_graphics::mono_font::iso_8859_1::FONT_7X13_BOLD;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::prelude::{PixelColor, Point};
use embedded_graphics::text::{Baseline, Text};
use embedded_graphics::Drawable;
use futures_core::future::BoxFuture;
use image::RgbaImage;

use crate::camera::Flaws;
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
    /// This operation should not attempt to access the scene objects and therefore may be
    /// called while the [`Universe`] is being stepped, etc.
    /// (TODO: Not all implementations obey this yet.)
    ///
    /// The returned image is always 8 bits per component and should be in the sRGB color
    /// space. (This trait may be revised in the future to support HDR rendering.)
    ///
    /// TODO: provide for returning performance info?
    ///
    /// [`Universe`]: crate::universe::Universe
    fn draw<'a>(
        &'a mut self,
        info_text: &'a str,
    ) -> BoxFuture<'a, Result<(RgbaImage, Flaws), RenderError>>;
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

/// Provides the standard text style and positioning to draw the “debug info text”
/// (as in [`HeadlessRenderer::draw()`]'s parameter).
///
/// Note: The conventional color is white with a black drop-shadow, but the exact color
/// format and means by which the shadow is accomplished depends on the specific renderer,
/// so this function makes no assumption about color.
#[doc(hidden)] // TODO: decide whether to make public
pub fn info_text_drawable<C: PixelColor + 'static>(
    text: &str,
    color_value: C,
) -> impl Drawable<Color = C> + '_ {
    Text::with_baseline(
        text,
        Point::new(5, 5),
        MonoTextStyle::new(&FONT_7X13_BOLD, color_value),
        Baseline::Top,
    )
}
