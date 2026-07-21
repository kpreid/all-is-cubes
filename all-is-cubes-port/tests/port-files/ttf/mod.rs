use std::path::{Path, PathBuf};

use read_fonts::TableProvider as _;

use all_is_cubes::universe;
use all_is_cubes::util::yield_progress_for_testing;

#[cfg(feature = "export")]
#[macro_rules_attribute::apply(smol_macros::test)]
async fn export_ttf() {
    // We're exporting a builtin, so this universe isn’t used, but we still need to have one.
    let universe = universe::Universe::new();
    let original_font = universe::Builtin::font_system16();

    let destination_dir = tempfile::tempdir().unwrap();
    let destination: PathBuf = destination_dir.path().join("system16.ttf");
    port::export_to_path(
        yield_progress_for_testing(),
        universe.read_ticket(),
        port::Format::Ttf,
        &port::ExportOptions::default(),
        port::ExportSet::from_iter([original_font]),
        destination.clone(),
    )
    .await
    .unwrap();

    // Test font is byte-identical to what we expect.
    snapbox::Assert::new().action_env("AICSNAP").subset_eq(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/port-files/ttf/expected"),
        destination_dir.path(),
    );

    // Also test that the font is parseable, regardless of what the snapshot says.
    let bytes = std::fs::read(&destination).expect("read file");
    let font = read_fonts::FontRef::new(&bytes).expect("exported TTF should be parseable");

    // Compare some properties to trhe original font.
    let original_font = original_font.read(universe.read_ticket()).unwrap();
    assert_eq!(
        u32::from(font.hhea().unwrap().advance_width_max().to_u16()),
        // TODO: stop testing against this arbitrary scale factor
        original_font.metrics().character_cell_size().width * 16
    );

    // Check for some characters’ presence
    let charmap = font.cmap().unwrap();
    assert!(charmap.map_codepoint('A').is_some());
    assert!(charmap.map_codepoint(' ').is_some());
    assert!(charmap.map_codepoint('é').is_some());
}
