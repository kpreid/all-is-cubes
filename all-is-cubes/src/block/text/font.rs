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
                    glyphs: OnceLock::new(),
                    character_size: size2(7, 16),
                    baseline: 12,
                };

                &DECL
            }
            Self::SmallerBodyText => {
                static DECL: FontDecl = FontDecl {
                    png_data: include_bytes!("font-body-text-6x14.png"),
                    png_path: "body-text-fot.png",
                    glyphs: OnceLock::new(),
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

    /// Draws text in this font, calling `set_pixel` for each pixel that should be set
    /// according to some [`Value`].
    ///
    /// The output coordinates are X-right Y-down starting from `(0, 0)` at the top-left of the
    /// first line of text.
    ///
    /// **Caution:**
    /// Glyphs may overlap, and in particular, the [`Value::Outline`] of one glyph may be produced
    /// before or after an adjacent glyph’s [`Value::Foreground`]. Therefore, you must adopt one
    /// of these strategies to handle this case:
    ///
    /// * Ignore outline pixels.
    /// * If an outline pixel is drawn after a foreground pixel, do not overwrite the foreground
    ///   pixel (draw to different layers, or have some priority mechanism such as taking the
    ///   maximum or minimum).
    /// * Call this function twice, first taking the outline pixels only, and then taking the
    ///   foreground pixels only.
    ///
    //---
    // Design note: In most cases, “set pixel” is an inefficient way of drawing images.
    // In this case, we are using it somewhat for historical reasons, but also because:
    // * our text generally has few foreground pixels relative to the area it covers
    // * all of our users want to track bounding boxes or otherwise work with the foreground
    //   area distinct from the background.
    //
    // TODO: Ideally this should return an iterator, but a non-borrowing iterator over
    // `Arc<[PositionedGlyph]>` is tricky.
    //
    // TODO: Return a more appropriate unit.
    pub fn draw_str_monospaced(
        &self,
        text: &str,
        mut set_pixel: impl FnMut(Point2D<i32, euclid::UnknownUnit>, Value),
    ) {
        let decl = self.font_decl();
        let glyphs = decl.glyphs();
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
            glyphs.get(decl, glyph.glyph_index).for_each(|(position, value)| {
                set_pixel(translation.transform_point(position), value);
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

    /// Lazily decoded from `png_data`.
    glyphs: OnceLock<Glyphs>,

    pub(in crate::block::text) character_size: Size2D<u8, InGlyph>,

    /// Y position of the baseline in the glyph.
    ///
    /// TODO: Clarify interpretation of this coordinate
    pub(in crate::block::text) baseline: u8,
}

impl FontDecl {
    /// Returns the glyph data for this font, loading it if necessary.
    pub(crate) fn glyphs(&self) -> &Glyphs {
        self.glyphs.get_or_init(|| {
            let decoded_png = DecodedPng::decode_static(self.png_data, self.png_path);
            assert_eq!(
                decoded_png.size().width,
                u32::from(self.character_size.width) * GLYPHS_PER_ROW
            );
            Glyphs::new(&decoded_png, self.character_size)
        })
    }
}

// -------------------------------------------------------------------------------------------------

/// Values a pixel of a glyph or piece of text can have.
///
/// These are not specific colors, but identify which color role (“foreground”, “outline”)
/// to use at a specific position, given the particular piece of text’s style.
///
/// This enum is produced by [`Font::draw_str_monospaced()`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[expect(clippy::exhaustive_enums)]
pub enum Value {
    /// Part of the shape of the glyph itself.
    Foreground = 0b11,

    /// Part of the outline of the glyph.
    /// Every pixel 8-way-adjacent to a `Foreground` pixel is part of the outline.
    Outline = 0b01,
}

// -------------------------------------------------------------------------------------------------

/// All the glyphs of one font, ready for use by the renderer.
pub(crate) struct Glyphs {
    pixels: Box<[u8]>,
}

const BITS_PER_PIXEL: u32 = 2;
const PIXELS_PER_BYTE: u32 = u8::BITS / BITS_PER_PIXEL;
const PIXEL_MASK: u8 = 0b11;

/// Set bits in [`Glyphs`] bit-packed data.
fn set_glyph_bits(data: &mut [u8], pixel_index: usize, value: u8) {
    data[pixel_index / PIXELS_PER_BYTE as usize] |=
        value << ((pixel_index % PIXELS_PER_BYTE as usize) as u32 * BITS_PER_PIXEL);
}

impl Glyphs {
    /// Convert an atlas image provided by [`png_decoder`] into glyphs prepared for rendering.
    ///
    /// This involves reorganizing the image from having glyphs arrayed in two dimensions, to
    /// separate blocks of data for each glyph. This is intended to simplify usage and improve
    /// locality of reference.
    ///
    /// It also precalculates which pixels are adjacent to the glyph for outline drawing.
    fn new(image: &DecodedPng, glyph_size: Size2D<u8, InGlyph>) -> Self {
        let loaded_glyph_rect_size = glyph_data_rect_size(glyph_size);
        let glyph_size = glyph_size.to_u32();
        let row_count = image.size().height / glyph_size.height;
        let bytes_per_glyph = loaded_glyph_rect_size.area().div_ceil(PIXELS_PER_BYTE) as usize;
        let glyph_count = row_count * GLYPHS_PER_ROW;

        // Bits that should be set in the output in the neighborhood of the input,
        // and their offsets in terms of pixel indices.
        // This forms the 3×3 pattern of outline pixels around a foreground pixel:
        //      O O O
        //      O F O
        //      O O O
        #[rustfmt::skip]
        let brush: [(u8, usize); 9] = {
            let o = Value::Outline as u8;
            let f = Value::Foreground as u8;
            let row = loaded_glyph_rect_size.width as usize;
            [
                (o, 0      ), (o,           1), (o,           2),
                (o, row    ), (f, row     + 1), (o, row     + 2),
                (o, row * 2), (o, row * 2 + 1), (o, row * 2 + 2),
            ]
        };

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
            let input_glyph_pixel_offset: Translation2D<u32, InGlyph, ImagePixel> =
                Translation2D::new(
                    (glyph_index_u % GLYPHS_PER_ROW_USIZE) as u32 * glyph_size.width,
                    (glyph_index_u / GLYPHS_PER_ROW_USIZE) as u32 * glyph_size.height,
                );
            let output_glyph_data =
                &mut output[glyph_index_u * bytes_per_glyph..][..bytes_per_glyph];

            for position_in_glyph in iproduct!(0..glyph_size.height, 0..glyph_size.width)
                .map(|(y, x)| <Point2D<u32, InGlyph>>::new(x, y))
            {
                let input_position = input_glyph_pixel_offset.transform_point(position_in_glyph);
                let [r, _g, _b, a] = image.get_pixel(input_position.cast_unit()).unwrap();
                if !(r > 0 && a > 0) {
                    continue;
                }

                let first_output_pixel = position_in_glyph.x as usize
                    + position_in_glyph.y as usize * loaded_glyph_rect_size.width as usize;
                for (value, offset) in brush {
                    set_glyph_bits(output_glyph_data, first_output_pixel + offset, value);
                }
            }
        }

        Self { pixels: output }
    }

    /// Returns an iterator over all the pixels making up one glyph.
    ///
    /// TODO: the results of this should typed, not as points, but as unit squares like `Cube`
    /// is a unit cube (or literally `Cube` if we choose to denote voxels this early).
    pub(in crate::block::text) fn get(
        &self,
        decl: &FontDecl,
        glyph_index: usize,
    ) -> impl Iterator<Item = (Point2D<GridCoordinate, InGlyph>, Value)> {
        let glyph_data_rect_size = glyph_data_rect_size(decl.character_size);
        let glyph_byte_size =
            (glyph_data_rect_size.area() as usize).div_ceil(PIXELS_PER_BYTE as usize);
        let this_glyph_pixels = &self.pixels[glyph_index * glyph_byte_size..][..glyph_byte_size];

        iproduct!(
            0..glyph_data_rect_size.height,
            0..glyph_data_rect_size.width
        )
        // can't overflow due to ranges of the original character size
        .map(move |(y, x)| point2(x.cast_signed(), y.cast_signed()))
        .filter_map(move |position_in_data| {
            let pixel = position_in_data.to_usize();
            let pixel_index = pixel.y * glyph_data_rect_size.width as usize + pixel.x;

            let byte_index = pixel_index / PIXELS_PER_BYTE as usize;
            let shift = (pixel_index % PIXELS_PER_BYTE as usize) * 2;

            let byte_value = *this_glyph_pixels.get(byte_index)?;
            let bit_value = (byte_value >> shift) & PIXEL_MASK;

            // offset of -1 accounts for the thickness of the outline area
            let position_in_glyph = position_in_data - vec2(1, 1);
            match bit_value {
                0b00 => None,
                0b01 => Some((position_in_glyph, Value::Outline)),
                0b11 => Some((position_in_glyph, Value::Foreground)),
                _ => unreachable!(),
            }
        })
    }
}

/// Compute the size of a glyph as loaded into [`Glyphs`] with its pre-calculated outline,
/// which is 1 pixel larger on all sides than the original image size.
fn glyph_data_rect_size(character_size: Size2D<u8, InGlyph>) -> ImageSize {
    size2(
        u32::from(character_size.width) + 2,
        u32::from(character_size.height) + 2,
    )
}
