use alloc::boxed::Box;

use bevy_platform::sync::OnceLock;
use euclid::{Point2D, Size2D, Translation2D, point2, size2, vec2};
use itertools::iproduct;

use crate::block::text::{self, layout::InGlyph};
use crate::camera::{ImagePixel, ImageSize};
use crate::content::load_image::DecodedPng;
use crate::math::{GridAab, GridCoordinate};
use crate::universe;

#[cfg(doc)]
use crate::block::text::Text;

// -------------------------------------------------------------------------------------------------

/// A font that may be used with [`Text`] blocks.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Font {
    /// A font whose characteristics are unspecified, other than that it is general-purpose and
    /// has a line height (vertical distance from a point on one line to the corresponding
    /// point of the next line) of 16 voxels.
    ///
    /// This is a placeholder for further improvement in the font system.
    System16,

    #[doc(hidden)]
    // experimental while we figure things out. Probably to be replaced with fonts stored in Universe
    Logo,

    #[doc(hidden)]
    // experimental while we figure things out. Probably to be replaced with fonts stored in Universe
    SmallerBodyText,
}

impl Font {
    /// Returns the static (compile-time constant) data for this font.
    pub(in crate::block::text) fn font_decl(&self) -> &'static FontDecl {
        match self {
            Self::System16 | Self::Logo => {
                static DECL: FontDecl = FontDecl {
                    png_data: include_bytes!("font-system-7x16.png"),
                    png_path: "new-system-font.png",
                    binary_image: OnceLock::new(),
                    character_size: size2(7, 16),
                    baseline: 12,
                };

                &DECL
            }
            Self::SmallerBodyText => {
                static DECL: FontDecl = FontDecl {
                    png_data: include_bytes!("font-body-text-6x14.png"),
                    png_path: "body-text-fot.png",
                    binary_image: OnceLock::new(),
                    character_size: size2(6, 14),
                    baseline: 10,
                };
                &DECL
            }
        }
    }

    /// Returns the size of a character cell that should be used when this font is used with a
    /// strictly monospaced renderer.
    ///
    /// Currently, this will always agree with the spacing that [`Font::draw_str_monospaced()`]
    /// produces.
    pub fn character_cell_size(&self) -> ImageSize {
        let s = self.font_decl().character_size;
        size2(u32::from(s.width), u32::from(s.height))
    }

    /// Draws text in this font, calling `set_pixel` for each pixel that should be set.
    ///
    /// The output coordinates are X-right Y-down starting from `(0, 0)` at the top-left of the
    /// first line of text.
    ///
    //---
    // Design note: In most cases, “set pixel” is an inefficient way of drawing images.
    // In this case, we are using it somewhat for historical reasons, but also because:
    // * our text generally has few foreground pixels relative to the area it covers
    // * all of our users want to track bounding boxes or otherwise work with the foreground
    //   area distinct from the background.
    //
    // TODO: This should support drawing outlines, because it can do it more efficiently with
    // access to the glyph.
    //
    // TODO: Ideally this should return an iterator, but a non-borrowing iterator over
    // `Arc<[PositionedGlyph]>` is tricky.
    //
    // TODO: Return a more appropriate unit.
    pub fn draw_str_monospaced(
        &self,
        text: &str,
        mut set_pixel: impl FnMut(Point2D<i32, euclid::UnknownUnit>),
    ) {
        let decl = self.font_decl();
        let pixels = decl.binary_image();
        let layout = text::compute_layout(
            text,
            decl,
            false,
            GridAab::ORIGIN_CUBE,
            text::Positioning {
                x: text::PositioningX::Left,
                line_y: text::PositioningY::BodyTop,
                z: text::PositioningZ::Back,
            },
        );
        for glyph in layout.glyphs.iter() {
            // TODO: change the output convention so that we do not have to flip coords here?
            let translation: Translation2D<i32, InGlyph, euclid::UnknownUnit> = Translation2D::from(
                glyph.position.to_vector().cast_unit().component_mul(vec2(1, -1)),
            );
            glyph_from_binary_image(pixels, decl, glyph.glyph_index).for_each(|p| {
                set_pixel(translation.transform_point(p));
            });
        }
    }
}

impl universe::VisitHandles for Font {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        match self {
            Self::System16 | Self::SmallerBodyText | Self::Logo => {}
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Number of glyphs which we place on each row of our .png and bitmap images.
///
/// TODO: Instead of this constant being used in many places, we should, when loading the font
/// definition, reorganize it into individual glyph images. (It will also need to go away
/// when we support non-monospaced fonts.)
const GLYPHS_PER_ROW: u32 = 16;

const GLYPHS_PER_ROW_USIZE: usize = GLYPHS_PER_ROW as usize;

/// Data structure defining a font, that can go in a `static` even though the image needs decoding.
///
/// For convenience of the implementation, glyph sizes are not allowed to exceed 255, allowing
/// all dimensions to be `u8`.
pub(in crate::block::text) struct FontDecl {
    png_data: &'static [u8],
    png_path: &'static str,

    /// Lazily decoded from `png_data` and bit-packed.
    binary_image: OnceLock<Box<[u8]>>,

    pub(in crate::block::text) character_size: Size2D<u8, InGlyph>,

    /// Y position of the baseline in the glyph.
    ///
    /// TODO: Clarify interpretation of this coordinate
    pub(in crate::block::text) baseline: u8,
}

impl FontDecl {
    /// Ensures the 1bpp packed font bitmap is loaded (decoding the PNG if needed)
    /// and returns a reference to it.
    pub fn binary_image(&'static self) -> &'static [u8] {
        self.binary_image.get_or_init(|| {
            let decoded_png = DecodedPng::decode_static(self.png_data, self.png_path);
            assert_eq!(
                decoded_png.size().width,
                u32::from(self.character_size.width) * GLYPHS_PER_ROW
            );
            rgba_image_to_glyphs(&decoded_png, self.character_size)
        })
    }
}

// -------------------------------------------------------------------------------------------------

/// Given [`FontDecl::binary_image()`] data, iterate over all the pixels making up one glyph.
///
/// TODO: the results of this should typed, not as points, but as unit squares like `Cube`
/// is a unit cube (or literally `Cube` if we choose to denote voxels this early).
pub(in crate::block::text) fn glyph_from_binary_image(
    all_glyph_pixels: &[u8],
    decl: &FontDecl,
    glyph_index: usize,
) -> impl Iterator<Item = Point2D<GridCoordinate, InGlyph>> {
    let glyph_byte_size =
        (decl.character_size.width as usize * decl.character_size.height as usize).div_ceil(8);
    let this_glyph_pixels = &all_glyph_pixels[glyph_index * glyph_byte_size..][..glyph_byte_size];

    iproduct!(0..decl.character_size.height, 0..decl.character_size.width)
        .map(move |(y, x)| point2(GridCoordinate::from(x), GridCoordinate::from(y)))
        .filter(move |&position_in_glyph| {
            let pixel = position_in_glyph.to_usize();
            let bit_index = pixel.y * usize::from(decl.character_size.width) + pixel.x;
            let byte_index = bit_index / 8;
            let bit_pos = bit_index % 8;
            this_glyph_pixels.get(byte_index).is_some_and(|&b| b & (1 << bit_pos) != 0)
        })
}

/// Convert image data provided by [`png_decoder`] into the bit-packed format expected by
/// [`glyph_from_binary_image()`].
///
/// This involves reorganizing the image from having glyphs arrayed in two dimensions, to
/// separate blocks of data for each glyph. This is intended to simplfy usage and improve locality
/// of reference.
fn rgba_image_to_glyphs(image: &DecodedPng, glyph_size: Size2D<u8, InGlyph>) -> Box<[u8]> {
    let glyph_size = glyph_size.to_u32();
    let row_count = image.size().height / glyph_size.height;
    let bytes_per_glyph = glyph_size.area().div_ceil(u8::BITS) as usize;
    let glyph_count = row_count * GLYPHS_PER_ROW;

    assert_eq!(
        image.size(),
        size2(
            glyph_size.width * GLYPHS_PER_ROW,
            row_count * glyph_size.height
        ),
        "image not consistently sized"
    );

    let mut output = vec![0u8; bytes_per_glyph * glyph_count as usize].into_boxed_slice();

    // This is hardly a highly efficient image copying operation, but it's done only once per font.
    for glyph_index_u in 0..(glyph_count as usize) {
        let input_glyph_pixel_offset: Translation2D<u32, InGlyph, ImagePixel> = Translation2D::new(
            (glyph_index_u % GLYPHS_PER_ROW_USIZE) as u32 * glyph_size.width,
            (glyph_index_u / GLYPHS_PER_ROW_USIZE) as u32 * glyph_size.height,
        );
        let output_glyph_byte_offset = glyph_index_u * bytes_per_glyph;

        for position_in_glyph in iproduct!(0..glyph_size.height, 0..glyph_size.width)
            .map(|(y, x)| <Point2D<u32, InGlyph>>::new(x, y))
        {
            let input_position = input_glyph_pixel_offset.transform_point(position_in_glyph);
            let [r, _g, _b, a] = image.get_pixel(input_position.cast_unit()).unwrap();
            if !(r > 0 && a > 0) {
                continue;
            }

            let output_bit_in_glyph = position_in_glyph.x + position_in_glyph.y * glyph_size.width;
            output[output_glyph_byte_offset + (output_bit_in_glyph / 8) as usize] |=
                1 << (output_bit_in_glyph % 8);
        }
    }

    output
}
