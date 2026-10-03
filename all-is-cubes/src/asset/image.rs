//! Loading images embedded in the binary for use as game assets.
//!
//! The images are lazily decompressed from PNG.
//! This has the disadvantage of requiring a decoder, but makes up for it in the
//! compactness of individual images.

use core::fmt;
use core::ops;

use imgref::ImgVec;

use bevy_platform::sync::OnceLock;

use crate::math::{Srgba8, u32size};

// -------------------------------------------------------------------------------------------------

/// Decode data in PNG format.
///
/// Ordinarily, you should use [`include_image!`] instead of this function, which provides
/// lazy loading (memoization of decoding).
/// This function is visible for cases where built-in memoization is unwanted, such as if
/// further work is going to be done and the image discarded.
///
/// # Panics
///
/// Panics if the data is not a valid PNG.
#[track_caller]
pub(crate) fn decode_static(png_data: &'static [u8], path: &'static str) -> ImgVec<Srgba8> {
    match png_decoder::decode(png_data) {
        Ok((header, data)) => ImgVec::new(data, u32size(header.width), u32size(header.height)),
        Err(error) => panic!("Error loading image asset {path:?}: {error:?}"),
    }
}

// -------------------------------------------------------------------------------------------------

/// Data type produced by [`include_image!`].
///
/// Dereferences to [`ImgVec`] of [`Srgba8`].
pub struct LazyImage {
    /// Lazily decoded image data.
    decoded_data: OnceLock<ImgVec<Srgba8>>,

    /// PNG image data for decoding.
    encoded_data: &'static [u8],

    /// (File) name of the image, for printing in case of errors.
    path: &'static str,
}

impl fmt::Debug for LazyImage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LazyImage").field("path", &self.path).finish_non_exhaustive()
    }
}

impl LazyImage {
    #[doc(hidden)]
    pub const fn private_include_image_macro_new(
        path: &'static str,
        encoded_data: &'static [u8],
    ) -> Self {
        Self {
            decoded_data: OnceLock::new(),
            path,
            encoded_data,
        }
    }

    /// The path of the image, exposed for diagnostic purposes.
    ///
    /// This path is not guaranteed to be absolute or to be relative to any particular directory.
    pub fn path(&self) -> &'static str {
        self.path
    }
}

impl ops::Deref for LazyImage {
    type Target = ImgVec<Srgba8>;
    #[track_caller] // attribute decoding error to the lazy site
    fn deref(&self) -> &Self::Target {
        self.decoded_data.get_or_init(|| decode_static(self.encoded_data, self.path))
    }
}

// -------------------------------------------------------------------------------------------------

#[macro_export]
#[doc(hidden)]
macro_rules! _asset_include_image {
    ( $path:literal ) => {{
        static IMAGE: $crate::asset::LazyImage =
            $crate::asset::LazyImage::private_include_image_macro_new(
                $path,
                ::core::include_bytes!($path),
            );
        &IMAGE
    }};
}

/// Embed a PNG image.
///
/// This macro takes one argument, which must be a string literal (suitable for [`include_bytes!`])
/// that is the path of a PNG image file relative to this source file,
/// and expands to an expression of type [`&'static LazyImage`][LazyImage],
/// which lazily  dereferences to [`ImgVec`] of [`Srgba8`].
#[doc(inline)] // required for this name to be documented
pub use _asset_include_image as include_image;

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use imgref::ImgRef;

    #[test]
    fn include_image() {
        // Putting this in a `const` item shows that `include_image!` can be called from a
        // const context.
        const IMAGE: &LazyImage = include_image!("load_image_test.png");

        let decoded: &ImgVec<Srgba8> = IMAGE;
        assert_eq!(
            decoded.as_ref(),
            ImgRef::new(
                [
                    [0, 0, 0, 0],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255],
                    [255, 0, 0, 255]
                ]
                .as_slice(),
                3,
                2,
            )
        )
    }
}
