use alloc::vec::Vec;
use core::fmt;

use euclid::{Box2D, Length, Point2D, Size2D, Translation2D, point2, size2, vec2};
use itertools::iproduct;

use crate::camera::ImagePixel;
use crate::content::load_image::DecodedPng;
use crate::math::{GridAab, GridCoordinate, u32size};
use crate::text;
use crate::transaction;
use crate::universe;
#[cfg_attr(not(any(doc, feature = "arbitrary")), allow(unused_imports))]
use crate::universe::Builtin;

#[cfg(doc)]
use crate::block::Text;

// -------------------------------------------------------------------------------------------------

// The following `FontDecl` constants are used via `Builtin` handles.

pub(crate) static FONT_SYSTEM_16: FontDecl = FontDecl {
    png_data: include_bytes!("font-system-7x16.png"),
    png_path: "font-system-7x16.png",
    metrics: Metrics {
        character_size: size2(7, 16),
        baseline: Length::new(13),
    },
};

pub(crate) static FONT_BODY_TEXT: FontDecl = FontDecl {
    png_data: include_bytes!("font-body-text-6x14.png"),
    png_path: "font-body-text-6x14.png",
    metrics: Metrics {
        character_size: size2(6, 14),
        baseline: Length::new(11),
    },
};

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for universe::Handle<FontDef> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        // This is an appropriate implementation because fonts currently can't be constructed from
        // user-provided data. In the future it will need to be replaced.

        // This is a list of all fonts available as builtins.
        u.choose(&[Builtin::font_system16, Builtin::font_body_text])
            .map(|f| f().clone())
    }
}

/// A font handle.
///
/// You can obtain font handles from [`Builtin`], then use them in [`Text`] blocks.
// TODO: When user-defined fonts are a thing, revise the mention of `Builtin`.
///
/// This type alias is a convenient shorthand for code working with text; additionally,
/// using it may help with migration in case future versions of All is Cubes no longer have
/// a 1:1 relationship between fonts and handles.
pub type Font = universe::Handle<FontDef>;

// -------------------------------------------------------------------------------------------------

/// [`euclid`] coordinate system type for coordinates within a single glyph image.
///
/// X increases rightward and Y increases downward.
/// There is no Z.
/// The origin is the top-left corner of the abstract character cell,
/// not counting the effect of outlining or any other transformation or combination of glyphs
/// that may spill out of the cell.
#[derive(Debug, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct InGlyph;

/// The metrics of a [`FontDef`]: measurements of dimensions which all characters in the font share.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Metrics {
    /// Size of a whole monospaced character cell.
    ///
    /// Currently, All is Cubes uses strictly monospaced layout for all text, and so
    /// the `width` of this is equal to the advance width of the font.
    /// This may change in the future.
    pub(in crate::text) character_size: Size2D<u8, InGlyph>,

    /// Y position of the baseline in the glyph.
    ///
    /// The baseline, for the Latin alphabet, is the line which most characters’ bottoms touch,
    /// and which descenders extend past.
    ///
    /// Note that this coordinate identifies a vertical position between pixels, not a row of
    /// pixels.
    pub(in crate::text) baseline: Length<u8, InGlyph>,
}

impl Metrics {
    /// Returns the size of a character cell that should be used when this font is used with a
    /// strictly monospaced renderer.
    ///
    /// Currently, this will always agree with the spacing that [`FontDef::draw_str_monospaced()`]
    /// produces.
    /// Currently, All is Cubes uses strictly monospaced layout for all text, and so
    /// the `width` of this is equal to the advance width of the font.
    pub fn character_cell_size(&self) -> Size2D<u32, InGlyph> {
        let s = self.character_size;
        size2(u32::from(s.width), u32::from(s.height))
    }

    /// Y position of the baseline in the glyph.
    ///
    /// The baseline, for the Latin alphabet, is the line which most characters’ bottoms touch,
    /// and which descenders extend past.
    ///
    /// Note that this coordinate identifies a vertical position between pixels, not a row of
    /// pixels, in the [`InGlyph`] coordinate system.
    pub fn baseline(&self) -> Length<GridCoordinate, InGlyph> {
        Length::new(self.baseline.get().into())
    }
}

// -------------------------------------------------------------------------------------------------

/// An index into a font’s glyph table.
///
/// These are produced by [`FontDef::glyph_for_char()`].
///
/// TODO: optimize using smaller index type.
/// We could probably get away with using u16 for now.
pub(in crate::text) type GlyphIndex = usize;

/// The data of a font that may be used with [`Text`] blocks.
///
/// Currently, the only available fonts are obtained through [`Builtin`];
/// there is no way to construct a new [`FontDef`] value.
/// In the future, fonts will be editable or at least loadable.
#[derive(Clone, Debug, Eq, PartialEq, bevy_ecs::component::Component)]
pub struct FontDef {
    pub(crate) glyphs: Glyphs,
    pub(crate) metrics: Metrics,
}

impl FontDef {
    /// Returns the metrics of the font: measurements of dimensions which all characters
    /// in the font share.
    pub fn metrics(&self) -> &Metrics {
        &self.metrics
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
    ///   foreground pixels only. This is inefficient because it performs text layout twice.
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
        let layout = text::compute_layout(
            text,
            self,
            false,
            GridAab::ORIGIN_CUBE,
            text::Positioning {
                x: text::PositioningX::Left,
                line_y: text::PositioningY::BodyTop,
                z: text::PositioningZ::Back,
            },
        );
        for glyph in layout.glyphs() {
            // TODO: change the output convention so that we do not have to flip coords here?
            let translation: Translation2D<i32, InGlyph, euclid::UnknownUnit> = Translation2D::from(
                glyph.position.to_vector().cast_unit().component_mul(vec2(1, -1)),
            );
            self.glyphs.get(glyph.glyph_index).for_each(|(position, value)| {
                set_pixel(translation.transform_point(position), value);
            });
        }
    }

    /// Returns the [`GlyphIndex`] for the glyph in this font that should be used to represent the
    /// given character.
    ///
    /// TODO: Both the input and the output of this will have to change eventually:
    ///
    /// * Some characters could be more efficiently represented as combinations of glyphs, e.g.
    ///   accented characters.
    /// * Some single glyphs will be chosen from a grapheme cluster consisting of multiple `char`s.
    #[expect(clippy::unused_self, reason = "TODO")]
    pub(in crate::text) fn char_to_glyph_index(&self, c: char) -> GlyphIndex {
        // Currently, all fonts’ repertoire is exactly ISO-8859-1, plus some Unicode lookalikes.

        // Remap characters for which we use the same glyphs
        let c = match c {
            '\u{2018}' | '\u{2019}' => '\'', // curly single quotes/apostrophes
            '\u{201C}' | '\u{201D}' => '"',  // curly double quotes
            c => c,
        };
        match c {
            '\u{20}'..='\u{7F}' => (c as GlyphIndex) - 0x20,
            '\u{80}'..='\u{FF}' => (c as GlyphIndex) - 0x40,
            _ => 0x1f, // unavailable glyphs become question marks -- TODO: dedicate a glyph
        }
    }
}

impl universe::VisitHandles for FontDef {
    fn visit_handles(&self, _visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            // no handles yet
            glyphs: _,
            metrics: _,
        } = self;
    }
}

impl transaction::Transactional for FontDef {
    type Transaction = transaction::ValueTransaction<FontDef>;
}

universe::impl_universe_member_for_single_component_type!(FontDef);

// -------------------------------------------------------------------------------------------------

/// Number of glyphs which we place on each row of our .png and bitmap images.
///
/// TODO: Instead of this constant being used in many places, we should, when loading the font
/// definition, reorganize it into individual glyph images. (It will also need to go away
/// when we support non-monospaced fonts.)
const GLYPHS_PER_ROW: u32 = 16;

const GLYPHS_PER_ROW_USIZE: usize = u32size(GLYPHS_PER_ROW);

/// Data structure defining a font, that can go in a `static` even though the image needs decoding.
///
/// For convenience of the implementation, glyph sizes are not allowed to exceed 255, allowing
/// all dimensions to be `u8`.
pub(crate) struct FontDecl {
    png_data: &'static [u8],
    png_path: &'static str,

    metrics: Metrics,
}

impl FontDecl {
    pub(crate) fn load(&self) -> FontDef {
        let decoded_png = DecodedPng::decode_static(self.png_data, self.png_path);
        assert_eq!(
            decoded_png.size().width,
            u32::from(self.metrics.character_size.width) * GLYPHS_PER_ROW
        );
        FontDef {
            metrics: self.metrics.clone(),
            glyphs: Glyphs::new(&decoded_png, self.metrics.character_size),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Values a pixel of a glyph or piece of text can have.
///
/// These are not specific colors, but identify which color role (“foreground”, “outline”)
/// to use at a specific position, given the particular piece of text’s style.
///
/// This enum is produced by [`FontDef::draw_str_monospaced()`].
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
#[derive(Clone, Eq, PartialEq)]
pub(crate) struct Glyphs {
    /// Bit-packed glyph data.
    ///
    /// Each glyph starts on a whole byte.
    /// The bits of the glyph are interpred according to [`Value`].
    pixels: Vec<u8>,

    /// Indices of this vector are [`GlyphIndex`].
    /// Elements specify where in `self.pixels` the glyph data can be found.
    lookup: Vec<GlyphInfo>,
}

/// Information for locating a packed glyph in [`Glyphs`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GlyphInfo {
    /// Offset of the first byte of each glyph in `self.pixels`.
    data_offset: usize,

    /// Bounding box which this glyph occupies when drawn by [`Glyphs::get()`].
    ///
    /// This is stored in signed coordinates because outlines extend in the negative direction
    /// whenever they touch the low (top and left) edges of the `InGlyph` coordinate system.
    bounding_box_including_outline: Box2D<i16, InGlyph>,
}

/// [`euclid`] coordinate system type for coordinates within the glyph images stored in
/// [`Glyphs`].
///
/// This has the same scale and directionality as [`InGlyph`], but the origin of this coordinate
/// system is the top-left corner, counting the outline, of the actual set pixels of the glyph;
/// that is, the origin is equal to `glyph_info.bounding_box_including_outline.min`.
pub(crate) struct InStoredGlyph;

const BITS_PER_PIXEL: u32 = 2;
const PIXELS_PER_BYTE: u32 = u8::BITS / BITS_PER_PIXEL;
const PIXEL_MASK: u8 = 0b11;

/// Set bits in [`Glyphs`] bit-packed data.
fn set_glyph_bits(data: &mut [u8], pixel_index: usize, value: u8) {
    data[pixel_index / u32size(PIXELS_PER_BYTE)] |=
        value << ((pixel_index % u32size(PIXELS_PER_BYTE)) as u32 * BITS_PER_PIXEL);
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
        let glyph_size_32 = glyph_size.to_u32();
        let row_count = image.size().height / glyph_size_32.height;
        let glyph_count = row_count * GLYPHS_PER_ROW;

        assert_eq!(
            image.size(),
            size2(
                glyph_size_32.width * GLYPHS_PER_ROW,
                row_count * glyph_size_32.height
            ),
            "image not consistently sized"
        );

        let mut pixels = Vec::new();
        let mut lookup = Vec::new();

        // This is hardly a highly efficient image copying operation, but it's done only once per font.
        for glyph_index_u in 0..u32size(glyph_count) {
            let offset_of_glyph_in_output = pixels.len();

            let input_glyph_pixel_offset: Translation2D<u32, InGlyph, ImagePixel> =
                Translation2D::new(
                    (glyph_index_u % GLYPHS_PER_ROW_USIZE) as u32 * glyph_size_32.width,
                    (glyph_index_u / GLYPHS_PER_ROW_USIZE) as u32 * glyph_size_32.height,
                );

            // Returns an iterator over the set pixels of the glyph.
            let iter_glyph_image = || {
                iproduct!(0..glyph_size.height, 0..glyph_size.width)
                    .map(|(y, x)| <Point2D<u8, InGlyph>>::new(x, y))
                    .filter(|&position_in_glyph| {
                        let input_position =
                            input_glyph_pixel_offset.transform_point(position_in_glyph.to_u32());
                        rgba_to_bit(image.get_pixel(input_position.cast_unit()).unwrap())
                    })
            };

            // Find bounding box of set pixels (including outline).
            // This is the bounding box of the data that will be actually stored for later drawing.
            //
            // Note that while the coordinate system type of this box is `InGlyph`, it is actually
            // offset by the outline width from that; we’ll fix that when storing it.
            let storage_bounding_box: Box2D<u8, InGlyph> = {
                let mut nonempty = false;
                let base_box = Box2D::from_points(iter_glyph_image().inspect(|_| nonempty = true));
                if nonempty {
                    // Expanded to account for outlining (1 pixel each side)
                    // and for the width of the pixels themselves (1 pixel down-right).
                    base_box.outer_box(euclid::SideOffsets2D::new(0, 3, 3, 0))
                } else {
                    Box2D::zero()
                }
            };

            let byte_size_of_glyph =
                u32size(storage_bounding_box.to_u32().area().div_ceil(PIXELS_PER_BYTE));

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
                let row = usize::from(storage_bounding_box.width());
                [
                    (o, 0      ), (o,           1), (o,           2),
                    (o, row    ), (f, row     + 1), (o, row     + 2),
                    (o, row * 2), (o, row * 2 + 1), (o, row * 2 + 2),
                ]
            };

            let info = GlyphInfo {
                data_offset: offset_of_glyph_in_output,
                bounding_box_including_outline: storage_bounding_box
                    .cast::<i16>()
                    .translate(vec2(-1, -1)),
            };
            lookup.push(info);

            // Allocate zeroed space for the pixels of this glyph.
            pixels.resize(offset_of_glyph_in_output + byte_size_of_glyph, 0);
            let output_glyph_data = &mut pixels[offset_of_glyph_in_output..];

            // Copy RGBA input pixels to bit-masked output pixels, and write the outline bits in
            // neighboring pixels.
            for position_in_glyph in iter_glyph_image() {
                let position_in_stored_glyph: Point2D<u8, InStoredGlyph> =
                    (position_in_glyph - storage_bounding_box.min.to_vector()).cast_unit();

                let first_output_pixel = usize::from(position_in_stored_glyph.x)
                    + usize::from(position_in_stored_glyph.y)
                        * usize::from(storage_bounding_box.width());

                for (value, offset) in brush {
                    set_glyph_bits(output_glyph_data, first_output_pixel + offset, value);
                }
            }
        }

        Self { pixels, lookup }
    }

    /// Returns the bounding box of one glyph.
    ///
    /// This box covers exactly the pixels that would be drawn, and does not have any guaranteed
    /// relationship to the font’s metrics.
    ///
    /// Panics if `glyph_index` is out of range.
    pub(crate) fn rendering_bounding_box(
        &self,
        glyph_index: GlyphIndex,
        outline: bool,
    ) -> Box2D<GridCoordinate, InGlyph> {
        let info: &GlyphInfo = self.get_info(glyph_index);
        let bbox = info.bounding_box_including_outline.cast();
        if outline || bbox.is_empty() {
            bbox
        } else {
            // Subtract the outline.
            bbox.inner_box(euclid::SideOffsets2D::new_all_same(1))
        }
    }

    /// Returns an iterator over all the pixels making up one glyph.
    ///
    /// Panics if `glyph_index` is out of range.
    ///
    /// TODO: the results of this should be typed, not as points, but as unit squares like `Cube`
    /// is a unit cube (or literally `Cube` if we choose to denote voxels this early).
    pub(crate) fn get(
        &self,
        glyph_index: GlyphIndex,
    ) -> impl Iterator<Item = (Point2D<GridCoordinate, InGlyph>, Value)> {
        let info: &GlyphInfo = self.get_info(glyph_index);
        // this slice includes extra pixels beyond this glyph ones, but those are not used
        let this_glyph_pixels = &self.pixels[info.data_offset..];

        let translation_to_stored: Translation2D<i32, InGlyph, InStoredGlyph> = Translation2D::from(
            -info.bounding_box_including_outline.min.to_vector().cast::<i32>().cast_unit(),
        );
        let storage_row_width = info.bounding_box_including_outline.width() as usize;

        iproduct!(
            info.bounding_box_including_outline.y_range(),
            info.bounding_box_including_outline.x_range(),
        )
        .map(move |(y, x)| point2(x, y).map(GridCoordinate::from))
        .filter_map(move |position_in_glyph: Point2D<GridCoordinate, InGlyph>| {
            let position_in_stored_glyph: Point2D<usize, InStoredGlyph> =
                translation_to_stored.transform_point(position_in_glyph).cast::<usize>();

            let pixel_index =
                position_in_stored_glyph.y * storage_row_width + position_in_stored_glyph.x;

            let byte_index = pixel_index / u32size(PIXELS_PER_BYTE);
            let shift = (pixel_index % u32size(PIXELS_PER_BYTE)) * 2;

            let byte_value = *this_glyph_pixels.get(byte_index)?;
            let bit_value = (byte_value >> shift) & PIXEL_MASK;

            match bit_value {
                0b00 => None,
                0b01 => Some((position_in_glyph, Value::Outline)),
                0b11 => Some((position_in_glyph, Value::Foreground)),
                _ => unreachable!(),
            }
        })
    }

    fn get_info(&self, glyph_index: GlyphIndex) -> &GlyphInfo {
        self.lookup.get(glyph_index).expect("glyph index out of range")
    }
}

impl fmt::Debug for Glyphs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: print at least some basic info.
        f.debug_struct("Glyphs").finish_non_exhaustive()
    }
}

fn rgba_to_bit([r, _, _, a]: [u8; 4]) -> bool {
    r > 0 && a > 0
}
