use std::sync::Arc;

use either::Either;
use pretty_assertions::assert_eq;

use all_is_cubes::block::{self, Block, BlockDef};
use all_is_cubes::math::{GridAab, Rgba, rgb_const, rgba_const};
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, Universe};
use all_is_cubes::util::yield_progress_for_testing;
use all_is_cubes_render::raytracer::print_space;

use crate::mv::{self, coord};
use crate::{ExportError, ExportSet};

// -------------------------------------------------------------------------------------------------

#[test]
#[ignore = "debug tool, not a test"]
fn print_many_transforms() {
    let gbox = GridAab::from_lower_size([0, 0, 0], [2, 2, 2]);
    let transform = coord::mv_to_aic_coordinate_transform(dot_vox::Size { x: 2, y: 2, z: 2 });
    for point in gbox.interior_iter() {
        let tmat = transform.transform_cube(point);

        println!("{point:?} -> {tmat:?}");
    }
    panic!(); // cause output to be displayed
}

#[cfg(all(feature = "export", feature = "import"))]
async fn roundtrip(
    export_universe: &Universe,
) -> Result<Box<Universe>, Either<ExportError, mv::DotVoxConversionError>> {
    // TODO: also roundtrip through bytes, for maximum rigor
    let data = mv::export_to_dot_vox_data(
        yield_progress_for_testing(),
        export_universe.read_ticket(),
        ExportSet::all_of_universe(export_universe),
    )
    .await
    .map_err(Either::Left)?;
    mv::import::dot_vox_data_to_universe(yield_progress_for_testing(), Arc::new(data))
        .await
        .map_err(Either::Right)
}

#[cfg(all(feature = "export", feature = "import"))]
#[macro_rules_attribute::apply(smol_macros::test)]
async fn export_import_space() {
    // Irregular and negative dimensions to check that the coordinate transform worked.
    let bounds = GridAab::from_lower_size([-20, -30, -40], [1, 2, 3]);
    let block1 = Block::builder()
        .color(Rgba::WHITE)
        .display_name("a")
        .build();
    let block2 = Block::builder()
        .color(Rgba::WHITE)
        .display_name("b")
        .build();

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
        Block::builder()
            .color(block1.color())
            .display_name("0")
            .build()
    );
    assert_eq!(
        s[[0, 1, 0]],
        Block::builder()
            .color(block1.color())
            .display_name("1")
            .build()
    );
    // TODO: make more assertions about the data?
}

// -------------------------------------------------------------------------------------------------
// Import-only tests

#[cfg(feature = "import")]
#[macro_rules_attribute::apply(smol_macros::test)]
async fn invalid_file_error() {
    let error = mv::load_dot_vox(yield_progress_for_testing(), &[])
        .await
        .unwrap_err();
    assert!(
        matches!(
            error,
            mv::DotVoxConversionError::Parse("Not a valid MagicaVoxel .vox file")
        ),
        "{error:?}"
    );
}

#[cfg(feature = "import")]
#[macro_rules_attribute::apply(smol_macros::test)]
async fn import_materials() {
    // This file contains one each of the six material types MagicaVoxel creates, in order.
    // (It does not contain each of the "media" types.)
    let universe = mv::load_dot_vox(
        yield_progress_for_testing(),
        include_bytes!("tests/materials.vox"),
    )
    .await
    .unwrap();
    let space: Handle<Space> = universe.get(&"model_0".into()).unwrap();
    let space: &Space = &space.read(universe.read_ticket()).unwrap();
    let bounds = GridAab::from_lower_size([0, 0, 0], [6, 1, 1]);
    assert_eq!(space.bounds(), bounds);
    let blocks = space
        .extract::<Vec<Block>, _>(bounds, |e| e.block_data().block().clone())
        .into_elements();

    assert_eq!(
        blocks,
        [
            // Diffuse
            Block::builder()
                .display_name("0")
                .color(rgba_const!(1.0, 0.0, 0.0, 1.0))
                .build(),
            // Metal
            Block::builder()
                .display_name("1")
                .color(rgba_const!(0.0, 1.0, 0.0, 1.0))
                .build(),
            // Emit
            Block::builder()
                .display_name("2")
                .color(Rgba::BLACK)
                .light_emission(rgb_const!(0.0, 0.0, 10.0))
                .build(),
            // Glass
            Block::builder()
                .display_name("3")
                .color(rgba_const!(0.0, 1.0, 1.0, 0.5))
                .build(),
            // Blend
            Block::builder()
                .display_name("4")
                .color(rgba_const!(1.0, 0.0, 1.0, 0.5))
                .build(),
            // Cloud
            Block::builder()
                .display_name("5")
                // TODO: The alpha should be much lower than this, but it is not clear what
                // the appropriate scaling actually is.
                .color(rgba_const!(1.0, 1.0, 0.0, 1.0))
                .build(),
        ]
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
        ExportSet::from_spaces(vec![space]),
    )
    .await
    .unwrap_err();
    assert!(matches!(error, ExportError::NotRepresentable { .. }));
}

#[cfg(feature = "export")]
#[macro_rules_attribute::apply(smol_macros::test)]
async fn export_block_def() {
    let mut universe = Universe::new();
    universe
        .insert(
            "x".into(),
            BlockDef::new(universe.read_ticket(), block::AIR),
        )
        .unwrap();

    let error = mv::export_to_dot_vox_data(
        yield_progress_for_testing(),
        universe.read_ticket(),
        ExportSet::all_of_universe(&universe),
    )
    .await
    .unwrap_err();
    assert!(matches!(
        error,
        ExportError::MemberTypeNotRepresentable {
            name,
            ..
        }
     if name == "x".into()));
}
