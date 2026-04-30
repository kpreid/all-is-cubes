//! Text layout and glyph selection algorithm.

use alloc::sync::Arc;
use alloc::vec::Vec;

use euclid::{Point2D, point2, vec2, vec3};

use crate::block::text;
use crate::math::{GridAab, GridCoordinate};

// -------------------------------------------------------------------------------------------------

/// The result of text layout: a list of glyphs to draw in specific positions.
///
/// Must be used with the [`text::Font`] it was created with.
//---
// TODO: consider making this a slice-tailed `Arc<Layout>` to reduce the size of `Text` and
// `Primitive` (will need <https://docs.rs/slice-dst/>).
#[derive(Clone, Debug)]
pub(crate) struct Layout {
    /// Bounding box of all glyphs to be drawn according to this layout.
    ///
    /// This is not necessarily contained by the [`text::Text::layout_bounds`].
    /// It is also not necessarily equal to the logical size of the text;
    /// for example, if the text starts or ends with blank lines,
    /// this box will not contain the height of those blank lines.
    /// This box is intended solely to answer the *rendering* question of what region of space is
    /// affected by drawing this text.
    pub bounding_box: GridAab,

    /// Z-axis translation of the glyphs when they are drawn.
    /// Not part of `PositionedGlyph::position` since it is (currently) the same for all.
    pub z: GridCoordinate,

    /// Glyphs to draw.
    pub glyphs: Arc<[PositionedGlyph]>,
}

/// A single positioned glyph making up part of a text [`Layout`].
#[derive(Clone, Copy, Debug)]
pub(crate) struct PositionedGlyph {
    /// Index into the font's glyph table (ISO-8859-1 minus 0x20).
    ///
    /// TODO: optimize using smaller index type.
    /// We could probably get away with using u16.
    pub glyph_index: usize,

    /// Position, in X-right Y-up coordinates (same as `Evoxels` coordinates),
    /// relative to (TODO: explain), of the top-left corner of this glyph.
    ///
    /// TODO: once we have an explanation, define a proper coordinate system type.
    pub position: Point2D<GridCoordinate, ()>,
}

/// [`euclid`] coordinate system type for coordinates within a single glyph image.
///
/// X increases rightward and Y increases downward.
/// There is no Z.
/// The origin is the top-left corner of the glyph image.
pub(crate) struct InGlyph;

// -------------------------------------------------------------------------------------------------

/// For a given piece of text, computes what glyphs to actually draw at what positions.
pub(crate) fn compute_layout(
    string: &str,
    decl: &text::FontDecl,
    outline: bool,
    layout_bounds: GridAab,
    positioning: text::Positioning,
) -> Layout {
    let character_size_g = decl.character_size.to_i32();
    let z_expansion: GridCoordinate = outline.into();

    // Find the *point* within the layout_bounds the text is positioned relative to.
    //
    // TODO: The subtractions of 1 here are based on a bad foundation, namely that coordinates
    // identify pixels (not boundaries), so even leftward-extending text *includes* the
    // identified pixel. We should stop doing that and stick to boundaries.
    //
    // TODO: Given that both text width and layout bounds may be odd or even, it is not sufficient
    // for this to be an integer point; it needs to be possibly half-integer (0, 0.5, 1, 1.5, ...).
    let lb = layout_bounds;
    let layout_offset = vec3(
        match positioning.x {
            text::PositioningX::Left => lb.lower_bounds().x,
            // 0.75 is a fudge factor that empirically gets the *rounding* behavior we want.
            text::PositioningX::Center => libm::round(lb.center().x - 0.75) as GridCoordinate,
            text::PositioningX::Right => lb.upper_bounds().x - 1,
        },
        match positioning.line_y {
            text::PositioningY::BodyTop => lb.upper_bounds().y - 1,
            text::PositioningY::BodyMiddle => {
                // 0.75 is fudge factor to get rounding right — TODO: see if this cancels out another off by 1
                libm::round(lb.center().y - 0.75) as GridCoordinate
                    + (character_size_g.height - 1) / 2
            }
            text::PositioningY::Baseline => {
                lb.lower_bounds().y + GridCoordinate::from(decl.baseline)
            }
            text::PositioningY::BodyBottom => lb.lower_bounds().y + character_size_g.height - 1,
        },
        match positioning.z {
            text::PositioningZ::Back => lb.lower_bounds().z,
            text::PositioningZ::Front => lb.upper_bounds().z.saturating_sub(z_expansion + 1),
        },
    );

    let mut glyphs: Vec<PositionedGlyph> = Vec::new();
    let mut bounding_box: Option<GridAab> = None;
    // Y coordinate of the position of the glyphs of the next line, relative to the origin of the
    // glyphs of the first line. Accumulates the height of the text.
    let mut cursor_y: GridCoordinate = 0;

    // TODO: Eventually we will likely want to switch to treating line break characters more like
    // a command (inside a single loop over graphemes) than like a separator of lines.
    for line in string.split('\n') {
        // Index in `glyphs` of the first glyph of this line.
        // Once we know the line length, this will let us adjust the horizontal positioning
        let first_glyph_of_line = glyphs.len();

        // Accumulates the length of this line of text by summing glyph sizes.
        let mut cursor_x: GridCoordinate = 0;

        // TODO: chars() is not the correct tool; we should be splitting on grapheme clusters
        // and mapping those to glyphs.
        for c in line.chars() {
            let glyph_index = char_to_glyph_index(c);
            let position: Point2D<GridCoordinate, ()> =
                point2(cursor_x, cursor_y) + layout_offset.xy();

            // TODO: we should use tight bounding boxes around each glyph, so the text bounding box
            // is tight, but we currently don't have those to start with.

            // Advance cursor to the right edge of the glyph.
            // This will become more complex when we have proportional fonts and kerning.
            cursor_x += character_size_g.width;

            glyphs.push(PositionedGlyph {
                glyph_index,
                position,
            });
        }

        // Now that we know the width of the line, we can figure out where it should be positioned
        // horizontally.
        let line_width = cursor_x; // when fonts get fancier these may be different
        let line_start_x: GridCoordinate = match positioning.x {
            text::PositioningX::Left => 0,
            text::PositioningX::Center => -(line_width - 1) / 2,
            text::PositioningX::Right => -(line_width - 1),
        };

        // Move all glyphs in the line to where they should be,
        // and add them to the overall bounding box.
        for glyph in &mut glyphs[first_glyph_of_line..] {
            glyph.position.x = glyph.position.x.saturating_add(line_start_x);

            // TODO: neither this nor the consistency check accounts for outline expansion in x and y
            let glyph_bounding_box = GridAab::from_lower_upper(
                // accounting for Y flip -- TODO: load fonts Y-up instead
                (glyph.position + vec2(0, 1 - character_size_g.height))
                    .extend(layout_offset.z)
                    .cast_unit(),
                (glyph.position + vec2(character_size_g.width, 1))
                    .extend(layout_offset.z + 1 + z_expansion)
                    .cast_unit(),
            );
            bounding_box = Some(match bounding_box {
                None => glyph_bounding_box,
                Some(bb) => bb.union_cubes(glyph_bounding_box),
            });
        }

        // Move cursor to the next line.
        cursor_y -= character_size_g.height;
    }

    let result = Layout {
        bounding_box: bounding_box.unwrap_or(GridAab::ORIGIN_EMPTY),
        glyphs: Arc::from(glyphs),
        z: layout_offset.z,
    };

    #[cfg(debug_assertions)]
    result.consistency_check(decl);

    result
}

// -------------------------------------------------------------------------------------------------

impl Layout {
    #[cfg(debug_assertions)]
    pub(crate) fn consistency_check(&self, font: &text::FontDecl) {
        // TODO: neither this nor the original computation accounts for outlines
        let bounding_box_from_glyphs =
            euclid::Box2D::from_points(self.glyphs.iter().flat_map(|glyph| {
                [
                    glyph.position + vec2(0, 1), // account for Y flip -- TODO: load fonts Y-up instead
                    glyph.position
                        + vec2(
                            GridCoordinate::from(font.character_size.width),
                            1 - GridCoordinate::from(font.character_size.height),
                        ),
                ]
            }));

        if bounding_box_from_glyphs.is_empty() {
            assert!(self.bounding_box.is_empty());
        } else {
            // TODO: check Z axis too
            assert_eq!(
                euclid::Box2D::new(
                    self.bounding_box.lower_bounds().xy(),
                    self.bounding_box.upper_bounds().xy(),
                )
                .cast_unit(),
                bounding_box_from_glyphs
            );
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Convert a Unicode character to a glyph index.
///
/// This is a placeholder for fonts eventually having their own glyph tables.
fn char_to_glyph_index(c: char) -> usize {
    // Remap characters for which we use the same glyphs
    let c = match c {
        '\u{2018}' | '\u{2019}' => '\'',
        '\u{201C}' | '\u{201D}' => '"',
        c => c,
    };
    match c {
        '\u{20}'..='\u{7F}' => (c as usize) - 0x20,
        '\u{80}'..='\u{FF}' => (c as usize) - 0x40,
        _ => 0x1f, // unavailable glyphs become question marks -- TODO: dedicate a glyph
    }
}
