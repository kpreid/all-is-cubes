#![no_main]

use all_is_cubes::block::{self, Block};
use all_is_cubes::math::{Aab, GridAab, Vol};
use all_is_cubes::space::{BlockIndex, Space};
use all_is_cubes::universe::ArbitraryWithUniverse;
use all_is_cubes_mesh as mesh;

#[derive(Debug, arbitrary::Arbitrary)]
struct SpaceIngredients {
    blocks: [Block; 3],
    pattern: [u8; 27],
}

libfuzzer_sys::fuzz_target!(|input: ArbitraryWithUniverse<(
    SpaceIngredients,
    mesh::MeshOptions<mesh::testing::TextureMt>,
    [mesh::Position; 2],
)>| {
    let (sing, options, view_positions) = input.contents;
    let space = construct_space(sing);

    let tex = mesh::testing::Allocator::new();
    let block_meshes = mesh::block_meshes_for_space(&space.read(), &tex, &options);
    let mut space_mesh: mesh::SpaceMesh<mesh::testing::TextureMt> =
        mesh::SpaceMesh::new(&space.read(), space.bounds(), &options, &*block_meshes);

    // We have implicitly exercised the consistency_check()s of BlockMesh and SpaceMesh, which run
    // automatically when debug assertions are set.

    // Run the depth sort, twice in case of quirks in the lazy initialization.
    for view_position in view_positions {
        _ = space_mesh.depth_sort_for_view(
            mesh::DepthOrdering::from_view_of_aabb(
                view_position.to_f64().cast_unit(),
                space_mesh.bounding_box().transparent().unwrap_or(Aab::ZERO),
            ),
            view_position,
        );
    }
});

// TODO: coverage for other mesh operations

fn construct_space(sing: SpaceIngredients) -> Space {
    let bounds = GridAab::from_lower_size([0, 0, 0], [3, 3, 3]);

    // Include AIR in the palette always, so that we more reliably exercise different mesh shapes.
    const PALETTE_LEN: u8 = 4;
    let [b1, b2, b3] = sing.blocks;
    let palette: [Block; PALETTE_LEN as usize] = [b1, b2, b3, block::AIR];

    Space::builder(bounds)
        .palette_and_contents(
            palette,
            Vol::from_elements(
                bounds,
                // TODO: this is wasting a lot of bits; unpack bits from the bytes
                Box::<[BlockIndex]>::from_iter(
                    sing.pattern.into_iter().map(|i| BlockIndex::from(i % PALETTE_LEN)),
                ),
            )
            .unwrap(),
            None,
        )
        .unwrap()
        .build()
}
