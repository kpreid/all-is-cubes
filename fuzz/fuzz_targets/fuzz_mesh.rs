#![no_main]

use all_is_cubes::block::Block;
use all_is_cubes::math::{Aab, GridAab};
use all_is_cubes::space::Space;
use all_is_cubes::universe::ArbitraryWithUniverse;
use all_is_cubes_mesh as mesh;

#[derive(Debug, arbitrary::Arbitrary)]
struct SpaceIngredients {
    blocks: [Block; 3],
    pattern: [u8; 27],
}

libfuzzer_sys::fuzz_target!(|input: ArbitraryWithUniverse<(
    SpaceIngredients,
    mesh::MeshOptions,
    [mesh::Position; 2],
)>| {
    let (sing, options, view_positions) = input.contents;
    let space = construct_space(sing);

    let tex = mesh::testing::Allocator::new();
    let block_meshes = mesh::block_meshes_for_space(&space, &tex, &options);
    let mut space_mesh: mesh::SpaceMesh<mesh::testing::TextureMt> =
        mesh::SpaceMesh::new(&space, space.bounds(), &options, &*block_meshes);

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
    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [3, 3, 3]))
        .build_and_mutate(|m| {
            let mut pattern_iter = sing.pattern.into_iter();
            m.fill_all(|_cube| {
                // The len + 1 causes some blocks to be left empty AIR.
                sing.blocks.get(
                    usize::from(pattern_iter.next().unwrap()).rem_euclid(sing.blocks.len() + 1),
                )
            })
        })
        .unwrap();
    space
}
