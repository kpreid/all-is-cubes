// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::math::Rgba;
use all_is_cubes::rgba_const;
use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

use all_is_cubes::block::{Block, AIR};

use all_is_cubes::mesh::{
    triangulate_blocks, triangulate_space, BlockMeshes, BlockVertex, MeshOptions, SpaceMesh,
    TestTextureAllocator, TestTextureTile,
};
use all_is_cubes::space::{Grid, Space};

pub fn triangulator_bench(c: &mut Criterion) {
    let options = &MeshOptions::new(&GraphicsOptions::default());

    c.bench_function("triangulate_space: checkerboard, new buffer", |b| {
        b.iter_batched(
            || checkerboard_setup(options, false),
            |(space, block_meshes)| {
                triangulate_space(&space, space.grid(), options, &*block_meshes)
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("triangulate_space: checkerboard, reused buffer", |b| {
        b.iter_batched(
            || {
                let (space, block_meshes) = checkerboard_setup(options, false);
                let mut buffer = SpaceMesh::new();
                buffer.compute(&space, space.grid(), options, &*block_meshes);
                // Sanity check that we're actually rendering as much as we expect.
                assert_eq!(buffer.vertices().len(), 6 * 4 * (16 * 16 * 16) / 2);
                (space, block_meshes, buffer)
            },
            |(space, block_meshes, mut buffer)| {
                // As of this writing, this benchmark is really just "what if we don't allocate
                // a new Vec". Later, the buffers will hopefully become cleverer and we'll be
                // able to reuse some work (or at least send only part of the buffer to the GPU),
                // and so this will become a meaningful benchmark of how much CPU time we're
                // spending or saving on that.
                buffer.compute(&space, space.grid(), options, &*block_meshes)
            },
            BatchSize::SmallInput,
        );
    });

    let mut slow_group = c.benchmark_group("slow");
    slow_group.sample_size(10);

    slow_group.bench_function("triangulate_space: transparent, new buffer", |b| {
        b.iter_batched(
            || checkerboard_setup(options, true),
            |(space, block_meshes)| {
                triangulate_space(&space, space.grid(), options, &*block_meshes)
            },
            BatchSize::SmallInput,
        );
    });
}

fn checkerboard_setup(
    options: &MeshOptions,
    transparent: bool,
) -> (Space, BlockMeshes<BlockVertex, TestTextureTile>) {
    let grid = Grid::new((0, 0, 0), (16, 16, 16));
    let mut space = Space::empty(grid);
    let blocks = [
        AIR,
        Block::from(if transparent {
            rgba_const!(0.5, 0.5, 0.5, 0.5)
        } else {
            Rgba::WHITE
        }),
    ];

    // Generate checkerboard
    space
        .fill(grid, |p| {
            Some(&blocks[((p.x + p.y + p.z) as usize).rem_euclid(blocks.len())])
        })
        .unwrap();

    let block_meshes = triangulate_blocks(&space, &mut TestTextureAllocator::new(), options);

    (space, block_meshes)
}

criterion_group!(benches, triangulator_bench);
criterion_main!(benches);
