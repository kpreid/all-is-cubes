use alloc::vec::Vec;

use embedded_graphics::mono_font::iso_8859_1::FONT_7X13_BOLD;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::prelude::{PixelColor, Point};
use embedded_graphics::text::{Baseline, Text};
use embedded_graphics::Drawable;

use crate::camera::{Flaws, ImageSize};
use crate::character::Cursor;
use crate::universe::HandleError;
use crate::util::maybe_sync::BoxFuture;

/// Rendering a previously-specified scene to an in-memory image.
///
/// This trait is object-safe so that different renderers can be used without generics.
/// Therefore, all its `async` methods use boxed futures.
pub trait HeadlessRenderer {
    /// Update the renderer's internal copy of the scene from the data sources
    /// (`Handle<Character>` etc.) it is tracking.
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
    /// called while the [`Universe`] is being stepped on another thread.
    ///
    /// [`Universe`]: crate::universe::Universe
    fn draw<'a>(&'a mut self, info_text: &'a str) -> BoxFuture<'a, Result<Rendering, RenderError>>;
}

/// An error indicating that a [`HeadlessRenderer`] or other renderer failed to operate.
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum RenderError {
    /// A component of the [`Universe`] that is to be rendered was not available
    /// for reading.
    ///
    /// [`Universe`]: crate::universe::Universe
    #[displaydoc("scene to be rendered was not available for reading")]
    Read(HandleError),
    // TODO: add errors for out of memory, lost GPU, etc.
}

cfg_should_impl_error! {
    impl std::error::Error for RenderError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            match self {
                RenderError::Read(e) => Some(e),
            }
        }
    }
}

/// Image container produced by a [`HeadlessRenderer`].
// ---
// TODO: This is not very compatible with future changes, but it's not clear how to
// improve extensibility. We would also like to have renderer-specific performance info.
#[derive(Clone, Debug)]
#[allow(clippy::exhaustive_structs)]
pub struct Rendering {
    /// Width and height of the image.
    pub size: ImageSize,
    /// Image data, RGBA, 8 bits per component, in the sRGB color space.
    pub data: Vec<[u8; 4]>,
    /// Deficiencies of the rendering; ways in which it fails to accurately represent the
    /// scene or apply the renderer’s configuration.
    pub flaws: Flaws,
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

#[cfg(test)]
mod tests {
    use super::*;

    fn _headless_renderer_is_object_safe(_: &dyn HeadlessRenderer) {}
}
