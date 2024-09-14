use std::sync::Arc;
use std::assert_matches;

use either::Either;
use pretty_assertions::assert_eq;

use all_is_cubes::block::{self, Block, BlockDef};
use all_is_cubes::math::{GridAab, Rgba};
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, Universe};
use all_is_cubes::util::yield_progress_for_testing;
use all_is_cubes_render::raytracer::print_space;

use crate::mv::{self, coord};

// -------------------------------------------------------------------------------------------------

#[test]
#[ignore = "debug tool, not a test"]
fn print_many_transforms() {
    let gbox = GridAab::from_lower_size([0, 0, 0], [2, 2, 2]);
    let transform =
        coord::mv_to_aic_coordinate_transform(dot_vox::Size { x: 2, y: 2, z: 2 }).unwrap();
    for point in gbox.interior_iter() {
        let tmat = transform.transform_cube(point);

        println!("{point:?} -> {tmat:?}");
    }
    panic!(); // cause output to be displayed
}

// -------------------------------------------------------------------------------------------------

#[cfg(all(feature = "export", feature = "import"))]
async fn roundtrip(
    export_universe: &Universe,
) -> Result<Box<Universe>, Either<crate::ExportError, mv::DotVoxConversionError>> {
    let data_export = mv::export_to_dot_vox_data(
        yield_progress_for_testing(),
        export_universe.read_ticket(),
        crate::ExportSet::all_of_universe(export_universe),
    )
    .await
    .map_err(Either::Left)?;
    let mut bytes = Vec::new();
    data_export.write_vox(&mut bytes).unwrap();
    let data_import = dot_vox::load_bytes(&bytes).unwrap();
    mv::import::dot_vox_data_to_universe(yield_progress_for_testing(), Arc::new(data_import))
        .await
        .map_err(Either::Right)
}

#[cfg(all(feature = "export", feature = "import"))]
#[macro_rules_attribute::apply(smol_macros::test)]
async fn export_import_space() {
    // Data expected to be preserved:
    // * Voxel reflectance
    // Data expected not to be preserved (for now):
    // * Voxel emission (but with discarding reflectance)
    // * Space's coordinate origin

    // Irregular and negative dimensions to check that the coordinate transform worked.
    let bounds = GridAab::from_lower_size([-20, -30, -40], [1, 2, 3]);
    let block1 = Block::builder().color(Rgba::BLACK).display_name("a").build();
    let block2 = Block::builder().color(Rgba::WHITE).display_name("b").build();

    // Construct universe for export.
    let mut export_universe = Universe::new();
    let export_space = Space::builder(bounds)
        .build_and_mutate(|m| {
            m.set([-20, -30 + 1, -40 + 2], &block1)?;
            m.set([-20, -30 + 1, -40], &block2)?;
            Ok(())
        })
        .unwrap();
    let export_space = export_universe.insert_anonymous(export_space);
    print_space(
        &export_space.read(export_universe.read_ticket()).unwrap(),
        [1., 1., 1.],
    );

    // Export and import.
    let import_universe = roundtrip(&export_universe).await.expect("roundtrip failed");
    // then find the one space in it.
    let import_space: Handle<Space> = import_universe.iter_by_type().next().unwrap().1;

    print_space(
        &import_space.read(import_universe.read_ticket()).unwrap(),
        [1., 1., 1.],
    );

    // Compare.
    // (The lower bounds will be zero.)
    let s = import_space.read(import_universe.read_ticket()).unwrap();
    assert_eq!(
        s.bounds(),
        GridAab::from_lower_size([0, 0, 0], bounds.size())
    );
    // Block metadata is not preserved. The display name is filled with the palette index.
    assert_eq!(
        s[[0, 1, 2]],
        Block::builder().color(block1.color()).display_name("0").build()
    );
    assert_eq!(
        s[[0, 1, 0]],
        Block::builder().color(block2.color()).display_name("1").build()
    );
    // TODO: make more assertions about the data?
}

// -------------------------------------------------------------------------------------------------
// Import-only tests

#[cfg(feature = "import")]
#[macro_rules_attribute::apply(smol_macros::test)]
async fn invalid_file_error() {
    let error = mv::load_dot_vox(yield_progress_for_testing(), &[]).await.unwrap_err();
    assert!(
        matches!(
            error,
            mv::DotVoxConversionError::Parse("Not a valid MagicaVoxel .vox file")
        ),
        "{error:?}"
    );
}

// -------------------------------------------------------------------------------------------------
// Export-only tests

/// [`dot_vox`] only supports coordinates from 0-255
#[cfg(feature = "export")]
#[macro_rules_attribute::apply(smol_macros::test)]
async fn export_too_large_space() {
    let mut universe = Universe::new();
    let space = universe
        .insert_anonymous(Space::builder(GridAab::from_lower_size([0, 0, 0], [257, 1, 1])).build());

    let error = mv::export_to_dot_vox_data(
        yield_progress_for_testing(),
        universe.read_ticket(),
        crate::ExportSet::from_spaces(vec![space]),
    )
    .await
    .unwrap_err();
    assert_matches!(error, crate::ExportError::NotRepresentable { .. });
}

/// Test that `export_to_dot_vox_data` accepts BlockDefs.
///
/// [`exported_space_equals_exported_block`] below is a more thorough test of the actual content.
/// TODO: be more tidy about what we're testing, and also have better behavior for
/// exporting the whole universe (don't export redundant models).
#[cfg(feature = "export")]
#[macro_rules_attribute::apply(smol_macros::test)]
async fn export_block_def() {
    let mut universe = Universe::new();
    let [block] = all_is_cubes::content::make_some_voxel_blocks(&mut universe);
    let block_def = universe
        .insert("x".into(), BlockDef::new(universe.read_ticket(), block))
        .unwrap();

    let data = mv::export_to_dot_vox_data(
        yield_progress_for_testing(),
        universe.read_ticket(),
        crate::ExportSet::from_block_defs(vec![block_def]),
    )
    .await
    .unwrap();

    assert!(
        matches!(
            data,
            dot_vox::DotVoxData { ref models, .. } if models.len() == 1
        ),
        "{data:#?}",
    );
}

#[cfg(feature = "export")]
#[test]
fn exported_space_equals_exported_block() {
    let mut universe = Universe::new();
    let [block] = all_is_cubes::content::make_some_voxel_blocks(&mut universe);
    let block::Primitive::Recur {
        space: space_handle,
        ..
    } = block.primitive()
    else {
        unreachable!();
    };

    let mut palette_from_space = mv::palette::ExportPalette::new();
    let mut palette_from_block = mv::palette::ExportPalette::new();
    let model_from_space = mv::model::from_space(
        universe.read_ticket(),
        space_handle,
        &mut palette_from_space,
    )
    .unwrap();
    let model_from_block = mv::model::from_block(
        "msvb[0]".into(),
        &block.evaluate(universe.read_ticket()).unwrap(),
        &mut palette_from_block,
    )
    .unwrap();

    // TODO: write a palette normalizer and compare the actual voxel data.
    assert_eq!(
        (
            model_from_space.size,
            // palette_from_space.len(),
            model_from_space.voxels.len()
        ),
        (
            model_from_block.size,
            // palette_from_block.len(),
            model_from_space.voxels.len()
        )
    );
}
