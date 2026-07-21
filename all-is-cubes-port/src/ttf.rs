//! Export to TrueType font format (`.ttf`).

use std::io::Write;
use std::mem;
use std::path::Path;

use descriptive_unwrap::ResultExt as _;
use futures_core::future::BoxFuture;
use write_fonts::tables::{self, head::Flags};
use write_fonts::types::{FWord, Fixed, NameId, Tag, UfWord};

use all_is_cubes::euclid::{Box2D, Point2D, Size2D, Transform2D, Translation2D};
use all_is_cubes::text::{self, FontDef};
use all_is_cubes::universe;
use all_is_cubes::util::YieldProgress;
use all_is_cubes_mesh::planar;

use crate::ExportError;

// -------------------------------------------------------------------------------------------------

/// The funny return type is to work with [`crate::export_to_path`].
pub(crate) fn export_ttf(
    progress: YieldProgress,
    read_ticket: universe::ReadTicket<'_>,
    mut source: crate::ExportSet,
    destination: &Path,
) -> Result<BoxFuture<'static, Result<(), ExportError>>, ExportError> {
    let fonts = source.contents.extract_type::<FontDef>();
    source.reject_unsupported(crate::Format::Ttf)?;

    let items = fonts
        .into_iter()
        .map(|handle| {
            // TODO: revise `export_separate_files` to not have to use the name twice
            Ok((
                source.member_export_path(destination, &handle),
                (
                    handle.name(),
                    (handle.name(), handle.read(read_ticket)?.clone()),
                ),
            ))
        })
        .collect::<Result<crate::MultiFileData<(universe::Name, FontDef)>, ExportError>>()?;

    Ok(crate::export_separate_files(
        progress,
        items,
        |progress, file, (handle_name, font_def)| -> Result<(), ExportError> {
            // TODO: a good handle name isn’t similar to a good font name; add font metadata
            // to FontDef so we have a good name to use here.
            let font_name: String = match handle_name {
                universe::Name::Specific(s) => s.to_string(),
                universe::Name::Builtin(builtin) => builtin.to_string(),
                universe::Name::Anonym(_) | universe::Name::Pending => "font".to_string(),
            };
            let ttf_bytes = build_ttf(&font_def, &font_name)?;
            progress.progress_without_yield(0.9);
            file.write_all(&ttf_bytes)?;
            progress.progress_without_yield(1.0);
            Ok(())
        },
    ))
}

// -------------------------------------------------------------------------------------------------

#[expect(clippy::doc_markdown)]
/// `euclid` coordinate system type for FUnits / font units / font design units.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/ttch01#funits-and-the-grid>
struct FUnit;

#[expect(clippy::doc_markdown)]
/// Arbitrary choice of scale factor, converting from 1 [`FontDef`] pixel/voxel to
/// 1 “FUnit” or “font design unit”.
///
/// Setting this to greater than 1 was recommended, but the effects of choosing different values
/// have not been empirically tested.
const SCALE: i16 = 16;

// -------------------------------------------------------------------------------------------------

/// Converts the given [`FontDef`] to the bytes of a TrueType file.
fn build_ttf(font: &FontDef, font_name: &str) -> Result<Vec<u8>, ExportError> {
    let metrics = font.metrics();
    let cell: Size2D<u32, text::InGlyph> = metrics.character_cell_size();

    // Glyph dimensions are bounded by u8 per the font format, so i16 can hold all the transformed
    // coordinates we work with here.
    // TODO: make u8 sizes obtainable from the FontDef so we don't have to fallibly cast from u32.
    let char_height = i16::try_from(cell.height).err_is_unreachable();
    let char_width = i16::try_from(cell.width).err_is_unreachable();
    let baseline_px = i16::try_from(metrics.baseline().get()).err_is_unreachable();

    // Coordinate transform from `FontDef` glyph coordinates (Y-down, origin at top left)
    // to TrueType font units (Y-up, scaled, origin on baseline).
    //
    // TrueType does not strictly require that the baseline is at zero, but it is preferred
    // (and signaled by a flag in the `head` table), so we translate to do so.
    let transform: Transform2D<i16, text::InGlyph, FUnit> =
        Transform2D::translation(0, -baseline_px).then_scale(SCALE, -SCALE);

    // Full monospace character cell in the output coordinate system.
    let character_cell_bounds: Box2D<i16, FUnit> = transform.outer_transformed_box(&Box2D {
        min: Point2D::new(0, 0),
        max: Point2D::new(char_width, char_height),
    });

    // units_per_em determines the relationship of the `FUnit` integer coordinates in the glyph data
    // to the size (in ems) of the glyphs as rendered.
    let units_per_em: u16 = character_cell_bounds.size().height.cast_unsigned();

    // TODO: ascender and descender make up the definition of the line spacing (i.e. they do not
    // refer to the characters’ ascenders and descenders per se), but eventually we shouldn’t be
    // setting them equal to the overall box but to what we want the line height to be.
    // For now, we don’t have any font metrics to use in that way.
    // <https://learn.microsoft.com/en-us/typography/opentype/spec/recom#stypoascender-stypodescender-and-stypolinegap>
    let ascender: FWord = FWord::new(character_cell_bounds.max.y);
    let descender: FWord = FWord::new(character_cell_bounds.min.y);
    let advance_width: UfWord = UfWord::new(character_cell_bounds.size().width.cast_unsigned());

    // Build glyph contours.
    // TTF glyph 0 = .notdef (empty).
    // TTF glyph (n + 1) = font's glyph at internal index n.
    let mut glyph_contours: Vec<kurbo::BezPath> = vec![kurbo::BezPath::default()];
    for glyph in font.iter_glyphs() {
        glyph_contours.push(glyph_to_bez_path(glyph, transform.cast::<f64>()));
    }
    assert_eq!(
        glyph_contours.len(),
        // All ISO-8859-1 characters except controls but including DEL, plus the TrueType
        // .notdef glyph, which is always present.
        0xC1,
        "currently fonts must exactly match ISO-8859-1"
    );

    let num_glyphs = u16::try_from(glyph_contours.len()).map_err(|_| ExportError {
        source: None,
        destination: None,
        detail: crate::ExportErrorKind::NotRepresentable {
            format: crate::Format::Ttf,
            reason: format!("font has too many glyphs, {}", glyph_contours.len()),
        },
    })?;
    let max_points: u16 = u16::try_from(
        glyph_contours
            .iter()
            .map(|g| {
                g.elements()
                    .iter()
                    .map(|el| -> usize {
                        match el {
                            kurbo::PathEl::MoveTo(_) => 1,
                            kurbo::PathEl::LineTo(_) => 1,
                            kurbo::PathEl::QuadTo(_, _) => 2,
                            kurbo::PathEl::CurveTo(_, _, _) => 3,
                            kurbo::PathEl::ClosePath => 0,
                        }
                    })
                    .sum::<usize>()
            })
            .max()
            .unwrap_or(0),
    )
    .map_err(count_overflow_to_export_error)?;
    let max_contours: u16 =
        u16::try_from(glyph_contours.iter().map(|g| g.subpaths().count()).max().unwrap_or(0))
            .map_err(count_overflow_to_export_error)?;
    let max_glyph_bounding_box = glyph_contours
        .iter()
        .filter(|g| !g.is_empty())
        .map(|g| g.control_box())
        .reduce(|a, b| a.union(b))
        .unwrap_or(kurbo::Rect::default());

    let mut builder = write_fonts::FontBuilder::new();

    // Glyph data tables.
    let (glyf_table, loca_table, loca_format) = build_glyf_loca(&glyph_contours);
    let (hmtx_table, min_left_side_bearing, min_right_side_bearing) =
        build_hmtx(&glyph_contours, advance_width);
    builder.add_table(&loca_table)?;
    builder.add_table(&glyf_table)?;
    builder.add_table(&hmtx_table)?;

    // TODO: We should generate bitmap glyphs, but `write-fonts` doesn’t offer assistance with
    // constructing it.

    // Character map (currently fixed, but this will change when `FontDef` goes proper Unicode).
    builder.add_table(&build_cmap())?;

    // Font metadata tables.
    builder.add_table(&tables::head::Head {
        // Facts about the font we’re generating.
        flags: Flags::BASELINE_AT_Y_0 | Flags::LSB_AT_X_0 | Flags::FORCE_INTEGER_PPEM,
        units_per_em,
        x_min: max_glyph_bounding_box.x0.round() as i16,
        x_max: max_glyph_bounding_box.x1.round() as i16,
        y_min: max_glyph_bounding_box.y0.round() as i16,
        y_max: max_glyph_bounding_box.y1.round() as i16,
        lowest_rec_ppem: metrics.character_cell_size().height.try_into().err_is_unreachable(),
        index_to_loc_format: loca_format as i16,

        // Dummy values for metadata we do not have available.
        font_revision: Fixed::ONE,
        mac_style: tables::head::MacStyle::empty(), // TODO: add bold/italic metadata to FontDef
        created: write_fonts::types::LongDateTime::new(0),
        modified: write_fonts::types::LongDateTime::new(0),

        // Fields that are required to have specific values.
        checksum_adjustment: 0,
        magic_number: 0x5F0F3CF5,
        font_direction_hint: 2, // "deprecated"
    })?;
    builder.add_table(&tables::hhea::Hhea {
        // Facts derived from the input font.
        ascender,
        descender,
        advance_width_max: advance_width,
        x_max_extent: FWord::new(max_glyph_bounding_box.x1.round() as i16),

        min_left_side_bearing,
        min_right_side_bearing,

        // Facts that are true for all fonts currently supported.
        caret_slope_rise: 1,
        caret_slope_run: 0,
        caret_offset: 0,
        number_of_h_metrics: num_glyphs,

        // Additional space between lines. We do not use this.
        // If we ever decide to, it must also be set in the `OS/2` table.
        line_gap: FWord::new(0),
    })?;
    builder.add_table(&tables::os2::Os2 {
        // Facts derived from the input font.
        x_avg_char_width: fword_cast_signed(advance_width).to_i16(),
        s_cap_height: Some(ascender.to_i16()),
        s_typo_ascender: ascender.to_i16(),
        s_typo_descender: descender.to_i16(),
        us_win_ascent: fword_cast_unsigned(ascender).to_u16(),
        us_win_descent: (-descender.to_i16()).cast_unsigned(),

        // TODO: FontDef metrics should include these.
        y_subscript_x_size: 0,
        y_subscript_y_size: 0,
        y_subscript_x_offset: 0,
        y_subscript_y_offset: 0,
        y_superscript_x_size: 0,
        y_superscript_y_size: 0,
        y_superscript_x_offset: 0,
        y_superscript_y_offset: 0,
        y_strikeout_size: 0,
        y_strikeout_position: 0,

        // Facts that are true for all fonts currently supported, or unused.
        us_default_char: Some(0),
        us_break_char: Some(0x0020),
        us_max_context: Some(0),
        us_lower_optical_point_size: None,
        us_upper_optical_point_size: None,
        ul_unicode_range_1: 0x00000003,
        ul_unicode_range_2: 0,
        ul_unicode_range_3: 0,
        ul_unicode_range_4: 0,
        us_first_char_index: 0x0020,
        us_last_char_index: 0x00FF,
        ul_code_page_range_1: Some(0x00000001), // Latin-1
        ul_code_page_range_2: Some(0),

        // Dummy values for metadata or metrics we do not have available.
        panose_10: [0; 10],
        s_family_class: 0,
        // TODO: FontDef metrics should include x-height, weight, and width.
        sx_height: Some((baseline_px * 5 / 8) * SCALE),
        us_weight_class: 400, // medium/normal weight
        us_width_class: 5,    // medium/normal width
        fs_type: 0,
        ach_vend_id: Tag::new(b"    "),
        fs_selection: tables::os2::SelectionFlags::REGULAR,

        // Additional space between lines. We do not use this.
        // If we ever decide to, it must also be set in the `OS/2` table.
        s_typo_line_gap: 0,
    })?;
    builder.add_table(&tables::maxp::Maxp {
        num_glyphs,
        max_points: Some(max_points),
        max_contours: Some(max_contours),
        max_composite_points: Some(0),
        max_composite_contours: Some(0),
        max_zones: Some(1),
        max_twilight_points: Some(0),
        max_storage: Some(0),
        max_function_defs: Some(0),
        max_instruction_defs: Some(0),
        max_stack_elements: Some(0),
        max_size_of_instructions: Some(0),
        max_component_elements: Some(0),
        max_component_depth: Some(0),
    })?;
    builder.add_table(&{
        fn name_record(name_id: NameId, string: impl Into<String>) -> tables::name::NameRecord {
            tables::name::NameRecord {
                platform_id: 0, // Unicode
                encoding_id: 4, // full Unicode
                language_id: 0, // no specific language
                name_id,
                string: write_fonts::OffsetMarker::new(string.into()),
            }
        }
        tables::name::Name::new(vec![
            name_record(NameId::FAMILY_NAME, font_name),
            // TODO: FontDef metrics should include subfamily.
            name_record(NameId::SUBFAMILY_NAME, "Regular"),
            name_record(NameId::FULL_NAME, format!("{font_name} Regular")),
            name_record(NameId::POSTSCRIPT_NAME, format!("{font_name}-Regular")),
        ])
    })?;
    builder.add_table(&tables::post::Post {
        version: write_fonts::types::Version16Dot16::new(3, 0),

        // Facts that are true for all fonts currently supported.
        is_fixed_pitch: 1,

        // Dummy values for metadata or metrics we do not have available.
        italic_angle: Fixed::ZERO,
        underline_position: FWord::new(-SCALE * 2), // below baseline, not touching it
        underline_thickness: FWord::new(SCALE),
        min_mem_type42: 0, // zero = unknown
        max_mem_type42: 0, // zero = unknown
        min_mem_type1: 0,  // zero = unknown
        max_mem_type1: 0,  // zero = unknown
        num_glyphs: None,
        glyph_name_index: None,
        string_data: None,
    })?;

    Ok(builder.build())
}

// -------------------------------------------------------------------------------------------------

/// Convert a [`text::ReadGlyph`] bitmap glyph to a [`BezPath`] outline.
fn glyph_to_bez_path(
    glyph: text::ReadGlyph<'_>,
    transform: Transform2D<f64, text::InGlyph, FUnit>,
) -> kurbo::BezPath {
    let bb = glyph.bounding_box();

    let mut image = imgref::ImgVec::new(
        vec![false; usize::try_from(bb.inflate(1, 1).area()).err_is_unreachable()],
        usize::try_from(bb.width() + 2).err_is_unreachable(),
        usize::try_from(bb.height() + 2).err_is_unreachable(),
    );
    let translation_to_image: Translation2D<i32, text::InGlyph, ()> =
        Translation2D::new(1 - bb.min.x, 1 - bb.min.y);
    glyph.pixels().for_each(|position| {
        image[translation_to_image.transform_point(position).to_usize().to_tuple()] = true;
    });

    let (basis, vertices) = planar::analyze_2d(bb, |point| {
        image[translation_to_image.transform_point(point).to_usize().to_tuple()]
    });

    let mut path = kurbo::BezPath::new();
    planar::Outliner::new()
        .outline(basis, vertices.into_iter(), |loop_| {
            let mut first = true;
            path.extend(loop_.iter().map(|vertex| {
                let point = kurbo::Point::from(
                    transform
                        .transform_point(vertex.position.cast_unit().xy().to_f64())
                        .to_f32()
                        .to_tuple(),
                );
                if mem::take(&mut first) {
                    kurbo::PathEl::MoveTo(point)
                } else {
                    kurbo::PathEl::LineTo(point)
                }
            }));
            Ok(())
        })
        .expect("not handling OOM");

    path
}

// -------------------------------------------------------------------------------------------------
// Nontrivial table constructions.

/// `glyf` and `loca` tables, which store glyph data.
fn build_glyf_loca(
    glyphs: &[kurbo::BezPath],
) -> (
    tables::glyf::Glyf,
    tables::loca::Loca,
    tables::loca::LocaFormat,
) {
    let mut builder = tables::glyf::GlyfLocaBuilder::new();
    for glyph in glyphs {
        builder
            .add_glyph(
                &tables::glyf::SimpleGlyph::from_bezpath(glyph)
                    // error type doesn't implement Error or we would err_is_unreachable()
                    .expect("generated path should be valid"),
            )
            .err_is_unreachable();
    }
    builder.build()
}

/// Character map table.
fn build_cmap() -> tables::cmap::Cmap {
    tables::cmap::Cmap::new(vec![tables::cmap::EncodingRecord {
        platform_id: tables::cmap::PlatformId::Unicode,
        // “Encoding ID 3 should be used in conjunction with 'cmap' subtable formats 4 or 6.”
        // — <https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#encoding-records-and-encodings>
        encoding_id: 3,
        // Format 4 with 2 segments for ISO-8859-1 non-controls
        subtable: write_fonts::OffsetMarker::new(tables::cmap::CmapSubtable::Format4(
            // <https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#format-4-segment-mapping-to-delta-values>
            tables::cmap::Cmap4 {
                language: 0,
                start_code: vec![0x0020, 0x00A0, 0xFFFF],
                end_code: vec![0x007F, 0x00FF, 0xFFFF],
                // Deltas chosen remove the control characters (which have no glyphs) to make a
                // contiguous sequence of glyphs. However, index 0 is reserved for the “missing
                // glyph” glyph, so the deltas are offset by 1. The last segment is not used,
                // but its delta is chosen to map to index 0 too (by wrapping arithmetic).
                id_delta: vec![-0x1F, -0x3F, 1],
                id_range_offsets: vec![0, 0, 0],
                glyph_id_array: Vec::new(), // not used because id_range_offsets are all 0
            },
        )),
    }])
}

/// Create the horizontal metrics table.
///
/// Also returns the min side bearings, which must be computed using the same information.
fn build_hmtx(glyphs: &[kurbo::BezPath], advance: UfWord) -> (tables::hmtx::Hmtx, FWord, FWord) {
    // TODO: When the advance is always the same it may be more efficiently representable — try this
    //  later. <https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx>
    let advance = advance.to_u16();
    let metrics = glyphs
        .iter()
        .map(|glyph| tables::hmtx::LongMetric {
            advance,
            // Note: When we change this, `Hhea::min_{left,right}_side_bearing` must change too.
            side_bearing: glyph.control_box().x0.round() as i16,
        })
        .collect();

    let min_left_side_bearing = glyphs
        .iter()
        // "empty glyphs should be ignored" — <https://learn.microsoft.com/en-us/typography/opentype/spec/hhea>
        .filter(|glyph| !glyph.is_empty())
        .map(|glyph| FWord::new(glyph.control_box().x0.round() as i16))
        .min()
        .unwrap_or(FWord::new(0));

    let min_right_side_bearing = glyphs
        .iter()
        .filter(|glyph| !glyph.is_empty())
        .map(|glyph| FWord::new(advance.cast_signed() - glyph.control_box().x1.round() as i16))
        .min()
        .unwrap_or(FWord::new(0));

    let hmtx = tables::hmtx::Hmtx::new(metrics, vec![]);

    (hmtx, min_left_side_bearing, min_right_side_bearing)
}

// -------------------------------------------------------------------------------------------------
// Utilities

fn fword_cast_unsigned(value: FWord) -> UfWord {
    UfWord::new(value.to_i16().try_into().expect("all coordinates should fit in u16/i16"))
}

fn fword_cast_signed(value: UfWord) -> FWord {
    FWord::new(value.to_u16().try_into().expect("all coordinates should fit in u16/i16"))
}

impl From<write_fonts::BuilderError> for ExportError {
    fn from(error: write_fonts::BuilderError) -> Self {
        ExportError {
            source: None,      // will be filled in later
            destination: None, // will be filled in later

            // This error may be a logic error in the exporter, or may be due to the font not
            // being validly representable in TrueType. Assume the latter.
            detail: crate::ExportErrorKind::NotRepresentable {
                format: crate::Format::Ttf,
                reason: format!("font export produced invalid table: {error}"),
            },
        }
    }
}

fn count_overflow_to_export_error(overflow_error: core::num::TryFromIntError) -> ExportError {
    ExportError {
        source: None,      // will be filled in later
        destination: None, // will be filled in later
        detail: crate::ExportErrorKind::NotRepresentable {
            format: crate::Format::Ttf,
            reason: format!("font export produced too many glyphs or points: {overflow_error}"),
        },
    }
}
