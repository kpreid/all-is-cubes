//! Text layout and glyph selection algorithm.

use core::fmt;

use alloc::sync::Arc;
use alloc::vec::Vec;

use euclid::{Point2D, Vector3D, point2, vec2, vec3};

use crate::block::text;
use crate::math::{GridAab, GridCoordinate};

// -------------------------------------------------------------------------------------------------

/// The result of text layout: a list of glyphs to draw in specific positions.
///
/// Must be used with the [`text::Font`] it was created with.
//---
// Using `slice_dst` lets the `Layout` be a single wide pointer, storing the metadata and
// the glyphs in a single slice.
#[derive(Clone)]
pub(in crate::block::text) struct Layout(
    erasable::Thin<Arc<slice_dst::SliceWithHeader<LayoutHeader, PositionedGlyph>>>,
);

pub(in crate::block::text) struct LayoutHeader {
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
}

/// A single positioned glyph making up part of a text [`Layout`].
#[derive(Clone, Copy, Debug)]
pub(in crate::block::text) struct PositionedGlyph {
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
/// The origin is the top-left corner of the abstract character cell,
/// not counting the effect of outlining or any other transformation or combination of glyphs
/// that may spill out of the cell.
pub(crate) struct InGlyph;

// -------------------------------------------------------------------------------------------------

/// Glyphs whose [`PositionedGlyph::position()`] would be outside this range are discarded instead
/// of being entered into the [`Layout`].
///
/// This is a mechanism to robustly avoid numeric overflow occurring, at the cost of missing text
/// when we are near the area where that text would have to be clipped regardless.
const POSITION_CLIP_RANGE: euclid::Box2D<GridCoordinate, ()> = {
    // glyphs are constrained to be no more than 255 × 255,
    // so this prevents any individual glyph from overflowing
    let min = GridCoordinate::MIN + 256;
    let max = GridCoordinate::MAX - 256;
    euclid::Box2D {
        min: Point2D::new(min, min),
        max: Point2D::new(max, max),
    }
};

/// Internally used temporarily to mark that a glyph should be discarded from the layout.
///
/// This position must be outside of [`POSITION_CLIP_RANGE`] to ensure that it cannot ever be a
/// real position.
const DELETE_ME_POSITION: Point2D<GridCoordinate, ()> =
    Point2D::new(GridCoordinate::MAX, GridCoordinate::MAX);

/// For a given piece of text, computes what glyphs to actually draw at what positions.
pub(crate) fn compute_layout(
    mut string: &str,
    decl: &text::FontDecl,
    outline: bool,
    layout_bounds: GridAab,
    positioning: text::Positioning,
) -> Layout {
    let character_size_g = decl.character_size.to_i32();
    let outline_expansion: GridCoordinate = outline.into();
    let thickness = 1 + outline_expansion;

    // Find the *point* within the layout_bounds the text is positioned relative to.
    //
    // TODO: The subtractions of 1 here are based on a bad foundation, namely that coordinates
    // identify pixels (not boundaries), so even leftward-extending text *includes* the
    // identified pixel. We should stop doing that and stick to boundaries.
    //
    // TODO: Given that both text width and layout bounds may be odd or even, it is not sufficient
    // for this to be an integer point; it needs to be possibly half-integer (0, 0.5, 1, 1.5, ...).
    let lb = layout_bounds;
    let layout_offset: Vector3D<GridCoordinate, ()> = vec3(
        // Horizontal positioning is done after each line’s width is known.
        0,
        match positioning.line_y {
            text::PositioningY::BodyTop => lb.upper_bounds().y.saturating_sub(1),
            text::PositioningY::BodyMiddle => {
                // 0.75 is fudge factor to get rounding right — TODO: see if this cancels out another off by 1
                (libm::round(lb.center().y - 0.75) as GridCoordinate)
                    .saturating_add((character_size_g.height - 1) / 2)
            }
            text::PositioningY::Baseline => {
                lb.lower_bounds().y.saturating_add(GridCoordinate::from(decl.baseline))
            }
            text::PositioningY::BodyBottom => {
                lb.lower_bounds().y.saturating_add(character_size_g.height).saturating_sub(1)
            }
        },
        match positioning.z {
            text::PositioningZ::Back => lb.lower_bounds().z,
            text::PositioningZ::Front => lb.upper_bounds().z.saturating_sub(thickness),
        },
    );

    if layout_offset.z.checked_add(thickness).is_none() {
        // Z would overflow, so the whole string is clipped instead.
        string = "";
    }

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
            let position: Point2D<GridCoordinate, ()> = point2(
                cursor_x.saturating_add(layout_offset.x),
                cursor_y.saturating_add(layout_offset.y),
            );
            if !POSITION_CLIP_RANGE.contains(position) {
                continue;
            }

            // TODO: we should use tight bounding boxes around each glyph, so the text bounding box
            // is tight, but we currently don't have those to start with.

            // Advance cursor to the right edge of the glyph.
            // This will become more complex when we have proportional fonts and kerning.
            cursor_x = cursor_x.saturating_add(character_size_g.width);

            glyphs.push(PositionedGlyph {
                glyph_index,
                position,
            });
        }

        // Now that we know the width of the line, we can figure out where it should be positioned
        // horizontally.
        //
        // Note: Saturating is not strictly the right overflow behavior here, but it's cheap, and
        // our goal is to avoid long-distance misbehavior resulting from wrapping arithmetic,
        // not to get perfect rendering of extreme cases.
        let line_width = cursor_x; // when fonts get fancier these may be different
        let line_start_x: GridCoordinate = match positioning.x {
            text::PositioningX::Left => lb.lower_bounds().x.saturating_add(outline_expansion),
            // By adding up everything before we divide by 2, we get perfectly centered
            // odd-width texts in odd-with bounds, and even-width texts in even-width bounds,
            // and when the parity does not match, we round down.
            text::PositioningX::Center => {
                (lb.lower_bounds()
                    .x
                    .saturating_add(lb.upper_bounds().x)
                    .saturating_sub(line_width))
                    / 2
            }
            text::PositioningX::Right => {
                lb.upper_bounds().x.saturating_sub(line_width).saturating_sub(outline_expansion)
            }
        };

        // Move all glyphs in the line to where they should be,
        // and add them to the overall bounding box.
        //
        // Some glyphs’ positions could overflow, and we mark those for later removal instead of
        // adding them into the bounding box.
        for glyph in &mut glyphs[first_glyph_of_line..] {
            let mut new_position = glyph.position;
            new_position.x = new_position.x.saturating_add(line_start_x);

            if !POSITION_CLIP_RANGE.contains(new_position) {
                glyph.position = DELETE_ME_POSITION;
                continue;
            }

            glyph.position = new_position;

            let glyph_bounding_box = GridAab::from_lower_upper(
                // accounting for Y flip -- TODO: load fonts Y-up instead
                (glyph.position
                    + vec2(
                        -outline_expansion,
                        1 - character_size_g.height - outline_expansion,
                    ))
                .extend(layout_offset.z)
                .cast_unit(),
                (glyph.position
                    + vec2(
                        character_size_g.width + outline_expansion,
                        1 + outline_expansion,
                    ))
                .extend(layout_offset.z + thickness)
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

    glyphs.retain(|g| g.position != DELETE_ME_POSITION);

    let arc_with_length: Arc<slice_dst::SliceWithHeader<LayoutHeader, PositionedGlyph>> =
        slice_dst::SliceWithHeader::new(
            LayoutHeader {
                bounding_box: bounding_box.unwrap_or(GridAab::ORIGIN_EMPTY),
                z: layout_offset.z,
            },
            glyphs,
        );
    let result = Layout(arc_with_length.into());

    #[cfg(debug_assertions)]
    result.consistency_check(decl, outline);

    result
}

// -------------------------------------------------------------------------------------------------

impl Layout {
    /// Use this for destructuring the whole layout.
    pub(in crate::block::text) fn parts(&self) -> (&LayoutHeader, &[PositionedGlyph]) {
        (&self.0.header, &self.0.slice)
    }

    pub(in crate::block::text) fn header(&self) -> &LayoutHeader {
        &self.0.header
    }

    pub(crate) fn glyphs(&self) -> &[PositionedGlyph] {
        &self.0.slice
    }

    #[cfg(debug_assertions)]
    pub(crate) fn consistency_check(&self, font: &text::FontDecl, outline: bool) {
        let (LayoutHeader { bounding_box, z: _ }, glyphs) = self.parts();

        let outline_expansion: GridCoordinate = outline.into();

        let bounding_box_from_glyphs =
            euclid::Box2D::from_points(glyphs.iter().flat_map(|glyph| {
                [
                    glyph.position + vec2(-outline_expansion, 1 + outline_expansion), // account for Y flip -- TODO: load fonts Y-up instead
                    glyph.position
                        + vec2(
                            GridCoordinate::from(font.character_size.width) + outline_expansion,
                            1 - GridCoordinate::from(font.character_size.height)
                                - outline_expansion,
                        ),
                ]
            }));

        if bounding_box_from_glyphs.is_empty() {
            assert!(bounding_box.is_empty());
        } else {
            // TODO: check Z axis too
            assert_eq!(
                euclid::Box2D::new(
                    bounding_box.lower_bounds().xy(),
                    bounding_box.upper_bounds().xy(),
                )
                .cast_unit(),
                bounding_box_from_glyphs
            );
        }
    }
}

impl fmt::Debug for Layout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let LayoutHeader { bounding_box, z } = &self.0.header;
        f.debug_struct("Layout")
            .field("bounding_box", bounding_box)
            .field("z", z)
            .finish_non_exhaustive()
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

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::R32;
    use crate::block::text::Font;

    #[track_caller]
    #[inline(never)]
    #[allow(clippy::needless_pass_by_value)]
    fn assert_bb(
        layout: Layout,
        lower_bounds: [GridCoordinate; 3],
        upper_bounds: [GridCoordinate; 3],
    ) {
        assert_eq!(
            layout.parts().0.bounding_box,
            GridAab::from_lower_upper(lower_bounds, upper_bounds)
        )
    }

    // TODO: think about how to get syntax this concise for normal usage of `Positioning`
    macro_rules! positioning {
        ($x:ident, $y:ident, $z:ident) => {
            text::Positioning {
                x: text::PositioningX::$x,
                line_y: text::PositioningY::$y,
                z: text::PositioningZ::$z,
            }
        };
    }

    fn one_letter_for_positioning(p: text::Positioning) -> Layout {
        compute_layout(
            "A",
            Font::System16.font_decl(),
            false,
            GridAab::for_block(R32),
            p,
        )
    }
    fn one_outlined_letter_for_positioning(p: text::Positioning) -> Layout {
        compute_layout(
            "A",
            Font::System16.font_decl(),
            true,
            GridAab::for_block(R32),
            p,
        )
    }

    // TODO: these bounding box tests overlap with test-aic/block_text.rs positioning_x() --
    // we should keep only one of them, probably.

    #[test]
    fn bb_x_left() {
        assert_bb(
            one_letter_for_positioning(positioning!(Left, BodyTop, Back)),
            [0, 16, 0],
            [7, 32, 1],
        )
    }

    #[test]
    fn bb_x_left_and_outline() {
        assert_bb(
            one_outlined_letter_for_positioning(positioning!(Left, BodyTop, Back)),
            // TODO: Y positioning is wrong (not what this test is about)
            [0, 15, 0],
            [9, 33, 2],
        )
    }

    #[test]
    fn bb_x_center_odd_text_in_even_box() {
        assert_bb(
            one_letter_for_positioning(positioning!(Center, BodyTop, Back)),
            // when we must round, we round down (leftward)
            [16 - 4, 16, 0],
            [16 + 3, 32, 1],
        )
    }

    #[test]
    fn bb_x_center_even_text_in_even_box() {
        assert_bb(
            compute_layout(
                "AB",
                Font::System16.font_decl(),
                false,
                GridAab::for_block(R32),
                positioning!(Center, BodyTop, Back),
            ),
            [16 - 7, 16, 0],
            [16 + 7, 32, 1],
        )
    }

    #[test]
    fn bb_x_center_odd_text_in_odd_box() {
        assert_bb(
            compute_layout(
                "A",
                Font::System16.font_decl(),
                false,
                GridAab::from_lower_upper([0, 0, 0], [31, 32, 32]),
                positioning!(Center, BodyTop, Back),
            ),
            [12, 16, 0],
            [19, 32, 1],
        )
    }

    #[test]
    fn bb_x_center_even_text_in_odd_box() {
        assert_bb(
            compute_layout(
                "AB",
                Font::System16.font_decl(),
                false,
                GridAab::from_lower_upper([0, 0, 0], [31, 32, 32]),
                positioning!(Center, BodyTop, Back),
            ),
            // when we must round, we round down (leftward)
            [15 - 7, 16, 0],
            [15 + 7, 32, 1],
        )
    }

    #[test]
    fn bb_x_right() {
        assert_bb(
            one_letter_for_positioning(positioning!(Right, BodyTop, Back)),
            [25, 16, 0],
            [32, 32, 1],
        )
    }

    #[test]
    fn bb_y_body_top() {
        assert_bb(
            one_letter_for_positioning(positioning!(Left, BodyTop, Back)),
            [0, 16, 0],
            [7, 32, 1],
        )
    }

    #[test]
    fn bb_y_body_middle() {
        assert_bb(
            one_letter_for_positioning(positioning!(Left, BodyMiddle, Back)),
            // TODO: this is an incorrect rounding; the Y range should be 8..24
            [0, 7, 0],
            [7, 23, 1],
        )
    }

    #[test]
    fn bb_y_baseline() {
        assert_bb(
            one_letter_for_positioning(positioning!(Left, Baseline, Back)),
            [0, -3, 0],
            [7, 13, 1],
        )
    }

    #[test]
    fn bb_y_body_bottom() {
        assert_bb(
            one_letter_for_positioning(positioning!(Left, BodyBottom, Back)),
            [0, 0, 0],
            [7, 16, 1],
        )
    }

    // There is no corresponding bb_z_back test because all other tests are `Back`.
    #[test]
    fn bb_z_front() {
        assert_bb(
            one_letter_for_positioning(positioning!(Left, BodyBottom, Front)),
            [0, 0, 31],
            [7, 16, 32],
        )
    }
}
