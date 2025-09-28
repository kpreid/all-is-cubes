use std::sync::Arc;

use pretty_assertions::assert_eq;

use all_is_cubes::block::Block;
use all_is_cubes::math::{GridAab, Rgba, rgb_const, rgba_const};
use all_is_cubes::space::{self, Space};
use all_is_cubes::universe::Handle;
use all_is_cubes::util::{async_test, yield_progress_for_testing};

// -------------------------------------------------------------------------------------------------
// Import-only tests

#[cfg(feature = "import")]
#[async_test]
async fn import_materials() {
    // This file contains one each of the six material types MagicaVoxel creates, in order.
    // (It does not contain each of the "media" types.)
    let universe = port::load_universe_from_file(
        yield_progress_for_testing(),
        Arc::new(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/port-files/mv/materials.vox"),
        ),
    )
    .await
    .unwrap();
    let space: Handle<Space> = universe.get(&"model_0".into()).unwrap();
    let space: &space::Read<'_> = &space.read(universe.read_ticket()).unwrap();
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
