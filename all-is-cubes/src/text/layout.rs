//! Text layout and glyph selection algorithm.

use core::fmt;

use all_is_cubes_base::resolution::Resolution;
use alloc::sync::Arc;
use alloc::vec::Vec;

use euclid::{Box2D, Point2D, SideOffsets2D, Vector3D, point2, vec3};

use crate::math::{Cube, GridAab, GridCoordinate, GridPoint};
use crate::text;

#[cfg(doc)]
use crate::block::{Primitive, Text};

// -------------------------------------------------------------------------------------------------

/// The result of text layout: a list of glyphs to draw in specific positions.
///
/// Must be used with the [`text::Font`] it was created with.
//---
// Using `slice_dst` lets the `Layout` be a single wide pointer, storing the metadata and
// the glyphs in a single allocation.
#[derive(Clone)]
pub(crate) struct Layout(
    erasable::Thin<Arc<slice_dst::SliceWithHeader<LayoutHeader, PositionedGlyph>>>,
);

// TODO: refactor so visibility can be crate::text only?
#[derive(Clone, Copy)]
pub(crate) struct LayoutHeader {
    /// Logical bounding box of the text as determined by font metrics and outline style.
    ///
    /// This box may be significantly larger than the actual visual space taken up; it measures
    /// whole line heights without considering the presence or absence of ascenders, descenders, or
    /// accents.
    ///
    /// This is not necessarily contained by the [`Text::layout_bounds`], in case the text
    /// is larger than fits in those bounds.
    ///
    /// TODO: Currently, leading and trailing newlines are not counted, but they should be.
    pub logical_bounding_box: GridAab,

    /// Bounding box of all glyphs to be drawn according to this layout.
    ///
    /// This is not necessarily contained by the [`Text::layout_bounds`].
    /// It is also not necessarily equal to the logical size of the text;
    /// for example, if the text starts or ends with blank lines,
    /// this box will not contain the height of those blank lines.
    /// This box is intended solely to answer the *rendering* question of what region of space is
    /// affected by drawing this text.
    pub rendering_bounding_box: GridAab,

    /// Z-axis translation of the glyphs when they are drawn.
    /// Not part of `PositionedGlyph::position` since it is (currently) the same for all.
    pub z: GridCoordinate,
}

/// A single positioned glyph making up part of a text [`Layout`].
//---
// TODO: refactor so visibility can be crate::text only?
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
const POSITION_CLIP_RANGE: Box2D<GridCoordinate, ()> = {
    // glyphs are constrained to be no more than 255 × 255,
    // so this prevents any individual glyph from overflowing
    let min = GridCoordinate::MIN + 256;
    let max = GridCoordinate::MAX - 256;
    Box2D {
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
    font: &text::FontDef,
    outline: bool,
    layout_bounds: GridAab,
    positioning: text::Positioning,
) -> Layout {
    let metrics = &font.metrics;
    let font_glyphs = &font.glyphs;
    let character_size_g = metrics.character_size.to_i32();
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
                lb.lower_bounds().y.saturating_add(GridCoordinate::from(metrics.baseline))
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
    // Accumulated bounding box of the character cells.
    // TODO: This is an `Option` because we want it to be able to be meaningful even when zero
    // width or zero height (e.g. the string "\n" should be two line heights and zero width).
    // However, this is not yet implemented correctly.
    let mut logical_bounding_box: Option<GridAab> = None;
    // Accumulated bounding box of the rendered voxels; usually smaller than the logical box.
    let mut rendering_bounding_box: GridAab = GridAab::ORIGIN_EMPTY;
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

            // Advance cursor to the right edge of the glyph.
            // This will become more complex when we have proportional fonts and kerning.
            cursor_x = cursor_x.saturating_add(character_size_g.width);

            if font_glyphs.rendering_bounding_box(glyph_index, outline).is_empty() {
                // If the glyph doesn’t actually draw anything, then it does not need to be stored.
                continue;
            }

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
        //
        // TODO: the logical bounding box should not be based solely on glyphs but also the start
        // and end positions, so that it includes leading and trailing whitespace.
        for glyph in &mut glyphs[first_glyph_of_line..] {
            let mut new_position = glyph.position;
            new_position.x = new_position.x.saturating_add(line_start_x);

            if !POSITION_CLIP_RANGE.contains(new_position) {
                glyph.position = DELETE_ME_POSITION;
                continue;
            }

            glyph.position = new_position;

            union_opt_aab(
                &mut logical_bounding_box,
                glyph_bounding_box_to_3d(
                    Box2D {
                        min: Point2D::zero(),
                        max: character_size_g.to_vector().to_point(),
                    }
                    .outer_box(SideOffsets2D::new_all_same(outline_expansion)),
                    glyph.position.cast_unit().extend(layout_offset.z),
                    outline,
                ),
            );

            rendering_bounding_box = rendering_bounding_box.union_cubes(glyph_bounding_box_to_3d(
                font_glyphs.rendering_bounding_box(glyph.glyph_index, outline),
                glyph.position.cast_unit().extend(layout_offset.z),
                outline,
            ));
        }

        // Move cursor to the next line.
        cursor_y -= character_size_g.height;
    }

    glyphs.retain(|g| g.position != DELETE_ME_POSITION);

    let arc_with_length: Arc<slice_dst::SliceWithHeader<LayoutHeader, PositionedGlyph>> =
        slice_dst::SliceWithHeader::new(
            LayoutHeader {
                logical_bounding_box: logical_bounding_box.unwrap_or(GridAab::ORIGIN_EMPTY),
                rendering_bounding_box,
                z: layout_offset.z,
            },
            glyphs,
        );
    let result = Layout(arc_with_length.into());

    #[cfg(debug_assertions)]
    result.consistency_check(font, outline);

    result
}

/// Y flip and apply translation
pub(crate) fn glyph_bounding_box_to_3d(
    bbox: Box2D<GridCoordinate, InGlyph>,
    position_of_glyph_origin: GridPoint,
    outline: bool,
) -> GridAab {
    let glyph_bounding_box_2d = bbox.cast_unit::<Cube>().to_i32();

    // Y flip and change origin from top left to bottom left
    let mut flipped_glyph_bounding_box_2d = glyph_bounding_box_2d;
    flipped_glyph_bounding_box_2d.min.y = 1 - glyph_bounding_box_2d.max.y;
    flipped_glyph_bounding_box_2d.max.y = 1 - glyph_bounding_box_2d.min.y;

    GridAab::from_lower_upper(
        flipped_glyph_bounding_box_2d.min.extend(0).cast_unit(),
        flipped_glyph_bounding_box_2d
            .max
            .extend(1 + GridCoordinate::from(outline))
            .cast_unit(),
    )
    .translate(position_of_glyph_origin.to_vector())
}

fn union_opt_aab(accumulator: &mut Option<GridAab>, addition: GridAab) {
    if addition.is_empty() {
        return;
    }
    *accumulator = Some(match *accumulator {
        None => addition,
        Some(bb) => bb.union_cubes(addition),
    });
}

// -------------------------------------------------------------------------------------------------

impl Layout {
    /// Use this for destructuring the whole layout.
    pub(crate) fn parts(&self) -> (&LayoutHeader, &[PositionedGlyph]) {
        (&self.0.header, &self.0.slice)
    }

    pub(crate) fn header(&self) -> &LayoutHeader {
        &self.0.header
    }

    pub(crate) fn glyphs(&self) -> &[PositionedGlyph] {
        &self.0.slice
    }

    #[cfg(debug_assertions)]
    fn consistency_check(&self, font: &text::FontDef, outline: bool) {
        let (
            &LayoutHeader {
                // Cannot truly validate the logical bounding box because it is determined in part
                // by whitespace that doesn’t actually render as glyphs.
                logical_bounding_box: _,
                rendering_bounding_box,
                z,
            },
            glyphs,
        ) = self.parts();

        let rendering_bounding_box_from_glyphs = glyphs
            .iter()
            .map(|glyph| {
                glyph_bounding_box_to_3d(
                    font.glyphs.rendering_bounding_box(glyph.glyph_index, outline),
                    glyph.position.extend(z).cast_unit(),
                    outline,
                )
            })
            .reduce(GridAab::union_cubes);

        if let Some(bb) = rendering_bounding_box_from_glyphs {
            assert_eq!(
                rendering_bounding_box, bb,
                "stored bounding box != expected bounding box"
            );
        } else {
            assert!(
                rendering_bounding_box.is_empty(),
                "when there are no glyphs, stored bounding box should be empty"
            );
        }
    }
}

impl fmt::Debug for Layout {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let LayoutHeader {
            logical_bounding_box,
            rendering_bounding_box,
            z,
        } = &self.0.header;
        f.debug_struct("Layout")
            .field("logical_bounding_box", logical_bounding_box)
            .field("rendering_bounding_box", rendering_bounding_box)
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

/// Information about the space a [`Text`] takes up.
pub struct Measurement {
    pub(crate) resolution: Resolution,
    pub(crate) layout_bounds: GridAab,
    /// Note: Using `LayoutHeader` here is a shortcut. If `LayoutHeader` starts having lots of
    /// data irrelevant to measurement, we should stop using it.
    pub(crate) layout_header: LayoutHeader,
}

impl Measurement {
    /// Returns the voxel resolution which text blocks made from this [`Text`] will have.
    /// This is the scale factor between the sizes the `*_blocks` and `*_voxels` methods report.
    pub fn resolution(&self) -> Resolution {
        self.resolution
    }

    /// Returns the bounding box relative to which the text was laid out.
    ///
    /// This does not reflect the actual size of the text, but the configuration with which the
    /// [`Text`] value was constructed.
    /// The text may overflow this bounding box depending on its length and the
    /// [`positioning()`](Text::positioning).
    pub fn layout_bounds(&self) -> GridAab {
        self.layout_bounds
    }

    /// Returns the bounding box that the text logically occupies,
    /// in voxels at the [`resolution()`](Self::resolution).
    ///
    /// This box may be significantly larger than the actual visual space taken up; it measures
    /// whole line heights without considering the presence or absence of ascenders, descenders, or
    /// accents.
    /// It is most appropriate for arranging pieces of text next to other pieces of text,
    /// such as in UI layouts.
    ///
    /// This box is in the same units as [`Self::layout_bounds()`] but reflects the actual text
    /// layout rather than the configuration.
    pub fn logical_bounding_voxels(&self) -> GridAab {
        self.layout_header.logical_bounding_box
    }

    /// Returns the bounding box that the text as drawn actually covers,
    /// in voxels at the [`resolution()`](Self::resolution).
    ///
    /// This box is in the same units as [`Self::layout_bounds()`] but reflects the actual text
    /// layout rather than the configuration.
    pub fn rendering_bounding_voxels(&self) -> GridAab {
        self.layout_header.rendering_bounding_box
    }

    /// Returns the bounding box that the text logically occupies,
    /// in whole blocks — the set of [`Primitive::Text`] offsets that will fit all of it.
    ///
    /// This is identical to [`Self::logical_bounding_voxels()`] scaled down by [`Self::resolution()`].
    pub fn logical_bounding_blocks(&self) -> GridAab {
        self.logical_bounding_voxels().divide(self.resolution.into())
    }

    /// Returns the bounding box that the text as drawn actually covers,
    /// in whole blocks — the set of [`Primitive::Text`] offsets that will fit all of it.
    ///
    /// This is identical to [`Self::rendering_bounding_voxels()`] scaled down by [`Self::resolution()`].
    pub fn rendering_bounding_blocks(&self) -> GridAab {
        self.rendering_bounding_voxels().divide(self.resolution.into())
    }
}

impl fmt::Debug for Measurement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            resolution,
            layout_bounds,
            layout_header:
                LayoutHeader {
                    logical_bounding_box,
                    rendering_bounding_box,
                    z,
                },
        } = self;
        f.debug_struct("Measurement")
            .field("resolution", resolution)
            .field("layout_bounds", layout_bounds)
            .field("logical_bounding_box", logical_bounding_box)
            .field("rendering_bounding_box", rendering_bounding_box)
            .field("z", z)
            .finish()
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::R32;
    use crate::text::Font;

    #[track_caller]
    #[inline(never)]
    #[allow(clippy::needless_pass_by_value)]
    fn assert_bb(
        layout: Layout,
        lower_bounds: [GridCoordinate; 3],
        upper_bounds: [GridCoordinate; 3],
    ) {
        // TODO: also test rendering bb
        assert_eq!(
            layout.parts().0.logical_bounding_box,
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
            Font::System16.font_def(),
            false,
            GridAab::for_block(R32),
            p,
        )
    }
    fn one_outlined_letter_for_positioning(p: text::Positioning) -> Layout {
        compute_layout(
            "A",
            Font::System16.font_def(),
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
                Font::System16.font_def(),
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
                Font::System16.font_def(),
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
                Font::System16.font_def(),
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
