//! Visualize the progression of the block mesh building algorithm using Rerun.
//!
//! Note: This is *not* a code sample to be imitated, as it uses unstable/pseudo-private APIs.
//! It is listed as an “example” because it is a program that only makes sense to run manually.

use all_is_cubes::math::{Face6, FaceMap, GridAab, Rgb, Rgba};
use pollster::block_on;

use all_is_cubes::block::{self, Block, EvaluatedBlock, Resolution};
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;
use all_is_cubes_content as content;
use all_is_cubes_mesh as mesh;
use all_is_cubes_render::camera::GraphicsOptions;
use content::DemoBlocks;

/// Private — do not use.
use all_is_cubes::{color_block, rerun_glue as rg};

fn main() {
    let destination = rg::Destination {
        stream: rg::RecordingStreamBuilder::new("all-is-cubes/visualize-block-mesh")
            .default_enabled(true)
            .connect_tcp()
            .unwrap(),
        path: rg::entity_path![],
    };
    destination.log_initialization();

    let mut universe = Universe::new();
    {
        let mut install_txn = UniverseTransaction::default();
        block_on(content::install_demo_blocks(
            &mut install_txn,
            YieldProgress::noop(),
        ))
        .unwrap();
        install_txn
            .execute(&mut universe, &mut transaction::no_outputs)
            .unwrap();
    }
    let demo_blocks = BlockProvider::<DemoBlocks>::using(&universe).unwrap();

    let blocks = [
        content::make_slab(&mut universe, 8, Resolution::R16),
        demo_blocks[DemoBlocks::Signboard].clone(),
        demo_blocks[DemoBlocks::LamppostBase].clone(),
        demo_blocks[DemoBlocks::Arrow].clone(),
        demo_blocks[DemoBlocks::Pedestal].clone(),
        make_transparent_block(&mut universe),
    ];

    std::thread::scope(|scope| {
        let mut x = 0.;
        for (i, block) in blocks.iter().enumerate() {
            let evaluated = block.evaluate().unwrap();

            x += f32::from(evaluated.resolution()) + 4.0;

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

/// A block containing transparent and emissive surfaces, that also meet opaque ones.
fn make_transparent_block(universe: &mut Universe) -> Block {
    let opaque_voxel = color_block!(content::palette::LOGO_FILL);
    let transparent_voxel = color_block!(0.7, 0.7, 0.2, 0.25);
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
