//! Visualize the progression of the block mesh building algorithm using Rerun.
//!
//! Note: This is *not* a code sample to be imitated, as it uses unstable/pseudo-private APIs.
//! It is listed as an “example” because it is a program that only makes sense to run manually.

use pollster::block_on;

use all_is_cubes::block::{self, Block, EvaluatedBlock, Resolution};
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::math::{Face6, FaceMap, GridAab, Rgb, Rgba};
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;
use all_is_cubes_content as content;
use all_is_cubes_mesh as mesh;
use all_is_cubes_render::camera::GraphicsOptions;
use content::DemoBlocks;

/// Private — do not use.
use all_is_cubes::rerun_glue as rg;

fn main() {
    let destination = rg::Destination {
        stream: rg::RecordingStreamBuilder::new("all-is-cubes/visualize-block-mesh")
            .default_enabled(true)
            .connect_grpc()
            .unwrap(),
        path: rg::entity_path![],
    };
    destination.log_initialization();

    let mut universe = Universe::new();
    let blocks = make_example_blocks(&mut universe);

    std::thread::scope(|scope| {
        let mut next_x = 0.;
        for (i, block) in blocks.iter().enumerate() {
            let evaluated = block.evaluate(universe.read_ticket()).unwrap();

            let x = next_x;
            next_x += f32::from(evaluated.resolution()) + 4.0;

            let destination = destination.child(&rg::entity_path![format!("{i}")]);
            scope.spawn(move || show(destination, x, &evaluated));
        }
    });

    destination.stream.flush_blocking();
}

fn show(destination: rg::Destination, x: f32, evaluated: &EvaluatedBlock) {
    destination.log(
        &rg::entity_path![],
        &rg::archetypes::Transform3D::from_translation(rg::datatypes::Vec3D::new(x, 1., 1.)),
    );

    let mut mesh = mesh::BlockMesh::<mesh::testing::NoTextureMt>::default();
    mesh.compute_with_viz(
        evaluated,
        &mesh::texture::NoTextures,
        &mesh::MeshOptions::new(&GraphicsOptions::default()),
        mesh::Viz::new(destination),
    );
}

fn make_example_blocks(universe: &mut Universe) -> Vec<Block> {
    {
        let mut install_txn = UniverseTransaction::default();
        block_on(content::install_demo_blocks(
            &mut install_txn,
            YieldProgress::noop(),
        ))
        .unwrap();
        install_txn
            .execute(universe, &mut transaction::no_outputs)
            .unwrap();
    }
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe).unwrap();

    let mut blocks = vec![
        content::make_slab(universe, 8, Resolution::R16),
        demo_blocks[DemoBlocks::Signboard].clone(),
        demo_blocks[DemoBlocks::LamppostBase].clone(),
        demo_blocks[DemoBlocks::Arrow].clone(),
        demo_blocks[DemoBlocks::Pedestal].clone(),
        make_transparent_boxes(universe),
        make_transparent_window(universe),
    ];

    // Generate a set of blocks which exercise different types of corner.
    let indexing = GridAab::for_block(Resolution::R2).to_vol().unwrap();
    for i in [0b111, 0b10111, 0b01010011, 0b01111111, 0b01011111] {
        blocks.push(
            Block::builder()
                .voxels_fn(Resolution::R2, |cube| {
                    if i & (1 << (indexing.index(cube).unwrap() as u8)) != 0 {
                        block::from_color!(0.5, 0.5, 0.5)
                    } else {
                        block::AIR
                    }
                })
                .unwrap()
                .build_into(universe),
        );
    }

    blocks
}

/// A block containing transparent and emissive surfaces, that also meet opaque ones
/// perpendicularly.
fn make_transparent_boxes(universe: &mut Universe) -> Block {
    let opaque_voxel = block::from_color!(content::palette::LOGO_FILL);
    let transparent_voxel = block::from_color!(0.7, 0.7, 0.2, 0.25);
    let emissive_voxel = Block::builder()
        .color(Rgba::TRANSPARENT)
        .light_emission(Rgb::new(0.0, 1.0, 1.0))
        .build();
    let resolution = Resolution::R16;
    let solid_box = GridAab::for_block(resolution)
        .shrink(FaceMap::splat(2))
        .unwrap();
    let transparent_box = GridAab::for_block(resolution).abut(Face6::PX, -4).unwrap();
    let emissive_box = GridAab::for_block(resolution).abut(Face6::NX, -4).unwrap();
    Block::builder()
        .voxels_fn(resolution, |cube| {
            if solid_box.contains_cube(cube) {
                &opaque_voxel
            } else if transparent_box.contains_cube(cube) {
                &transparent_voxel
            } else if emissive_box.contains_cube(cube) {
                &emissive_voxel
            } else {
                &block::AIR
            }
        })
        .unwrap()
        .build_into(universe)
}

/// A block containing transparent surfaces that abut opaque ones.
fn make_transparent_window(universe: &mut Universe) -> Block {
    let opaque_voxel = block::from_color!(content::palette::LOGO_FILL);
    let transparent_voxel = block::from_color!(0.7, 0.7, 0.2, 0.25);

    let resolution = Resolution::R16;
    let solid_box = GridAab::for_block(resolution)
        .shrink(FaceMap::symmetric([0, 0, 2]))
        .unwrap();
    let transparent_box = GridAab::for_block(resolution)
        .shrink(FaceMap::symmetric([2, 4, 2]))
        .unwrap();
    Block::builder()
        .voxels_fn(resolution, |cube| {
            if transparent_box.contains_cube(cube) {
                &transparent_voxel
            } else if solid_box.contains_cube(cube) {
                &opaque_voxel
            } else {
                &block::AIR
            }
        })
        .unwrap()
        .build_into(universe)
}
