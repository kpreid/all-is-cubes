use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

use all_is_cubes::block::AIR;
use all_is_cubes::blockgen::make_some_blocks;
use all_is_cubes::math::Face;
use all_is_cubes::space::{Grid, Space};
use all_is_cubes::triangulator::{
    new_space_buffer, triangulate_blocks, triangulate_space, BlockVertex, BlocksRenderData,
    NullTextureAllocator,
};

pub fn triangulator_bench(c: &mut Criterion) {
    c.bench_function("triangulate_space: checkerboard, new buffer", |b| {
        b.iter_batched(
            checkerboard_setup,
            |(space, blocks_render_data)| {
                triangulate_space(
                    &space,
                    space.grid(),
                    &blocks_render_data,
                    &mut new_space_buffer(),
                )
            },
            BatchSize::SmallInput,
        );
    });

    c.bench_function("triangulate_space: checkerboard, reused buffer", |b| {
        b.iter_batched(
            || {
                let (space, blocks_render_data) = checkerboard_setup();
                let mut buffer = new_space_buffer();
                triangulate_space(&space, space.grid(), &blocks_render_data, &mut buffer);
                // Sanity check that we're actually rendering as much as we expect.
                assert_eq!(buffer[Face::PX].len(), 6 * (16 * 16 * 16) / 2);
                (space, blocks_render_data, buffer)
            },
            |(space, blocks_render_data, mut buffer)| {
                // As of this writing, this benchmark is really just "what if we don't allocate
                // a new Vec". Later, the buffers will hopefully become cleverer and we'll be
                // able to reuse some work (or at least send only part of the buffer to the GPU),
                // and so this will become a meaningful benchmark of how much CPU time we're
                // spending or saving on that.
                triangulate_space(&space, space.grid(), &blocks_render_data, &mut buffer)
            },
            BatchSize::SmallInput,
        );
    });
}

fn checkerboard_setup() -> (Space, BlocksRenderData<BlockVertex, NullTextureAllocator>) {
    let grid = Grid::new((0, 0, 0), (16, 16, 16));
    let mut space = Space::empty(grid);
    let mut blocks = make_some_blocks(2);
    blocks[0] = AIR.clone();

    // Generate checkerboard
    space
        .fill(grid, |p| {
            Some(&blocks[((p.x + p.y + p.z) as usize).rem_euclid(blocks.len())])
        })
        .unwrap();

    let blocks_render_data = triangulate_blocks(&space, &mut NullTextureAllocator);

    (space, blocks_render_data)
}

criterion_group!(benches, triangulator_bench);
criterion_main!(benches);
