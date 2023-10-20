use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

use all_is_cubes::block::{Block, Resolution::R16, AIR};
use all_is_cubes::camera::{Camera, Flaws, GraphicsOptions, Viewport};
use all_is_cubes::content::make_some_voxel_blocks;
use all_is_cubes::math::{GridAab, Rgba};
use all_is_cubes::rgba_const;
use all_is_cubes::space::Space;
use all_is_cubes::time;
use all_is_cubes::universe::{Name, URef, Universe};

use all_is_cubes_mesh::texture::{TestAllocator, TestPoint, TestTile};
use all_is_cubes_mesh::{
    block_meshes_for_space, dynamic, BlockMesh, BlockMeshes, BlockVertex, MeshOptions, SpaceMesh,
};

criterion_group!(
    benches,
    block_mesh_benches,
    space_mesh_benches,
    slow_mesh_benches,
    dynamic_benches,
);
criterion_main!(benches);

fn block_mesh_benches(c: &mut Criterion) {
    let mut g = c.benchmark_group("block");
    let options = &MeshOptions::new(&GraphicsOptions::default());

    g.bench_function("checker-new", |b| {
        let mut universe = Universe::new();
        let block = checkerboard_block(&mut universe, [AIR, Block::from(Rgba::WHITE)]);
        let ev = block.evaluate().unwrap();

        b.iter_batched_ref(
            || (),
            |()| BlockMesh::<BlockVertex<TestPoint>, _>::new(&ev, &TestAllocator::new(), options),
            BatchSize::SmallInput,
        );
    });

    g.bench_function("checker-reused", |b| {
        let mut universe = Universe::new();
        let block = checkerboard_block(&mut universe, [AIR, Block::from(Rgba::WHITE)]);
        let ev = block.evaluate().unwrap();

        let mut shared_mesh = BlockMesh::<BlockVertex<TestPoint>, _>::default();

        b.iter_batched_ref(
            || (),
            |()| shared_mesh.compute(&ev, &TestAllocator::new(), options),
            BatchSize::SmallInput,
        );
    });

    g.bench_function("opaque", |b| {
        let mut universe = Universe::new();
        let block = checkerboard_block(
            &mut universe,
            [Block::from(Rgba::BLACK), Block::from(Rgba::WHITE)],
        );
        let ev = block.evaluate().unwrap();

        b.iter_batched_ref(
            || (),
            |()| BlockMesh::<BlockVertex<TestPoint>, _>::new(&ev, &TestAllocator::new(), options),
            BatchSize::SmallInput,
        );
    });

    g.bench_function("msvb", |b| {
        let mut universe = Universe::new();
        let [block] = make_some_voxel_blocks(&mut universe);
        let ev = block.evaluate().unwrap();

        b.iter_batched_ref(
            || (),
            |()| BlockMesh::<BlockVertex<TestPoint>, _>::new(&ev, &TestAllocator::new(), options),
            BatchSize::SmallInput,
        );
    });

    // TODO: Add meshing a block that has a complex but not worst-case shape.
}

fn space_mesh_benches(c: &mut Criterion) {
    let mut g = c.benchmark_group("space");
    let options = MeshOptions::new(&GraphicsOptions::default());
    // The Space and block meshes are not mutated, so we can construct them once.
    // This also ensures we're not timing dropping them.
    let checker_ing = checkerboard_space_bench_setup(options.clone(), false);

    g.bench_function("checker-new", |b| {
        // This benchmark actually has no use for iter_batched_ref -- it could be
        // iter_with_large_drop -- but I want to make sure any quirks of the measurement
        // are shared between all cases.
        b.iter_batched_ref(|| (), |()| checker_ing.do_new(), BatchSize::SmallInput);
    });

    g.bench_function("checker-reused", |b| {
        b.iter_batched_ref(
            || {
                let mut buffer = SpaceMesh::default();
                checker_ing.do_compute(&mut buffer);
                // Sanity check that we're actually rendering as much as we expect.
                assert_eq!(buffer.vertices().len(), 6 * 4 * (16 * 16 * 16) / 2);
                buffer
            },
            |buffer: &mut SpaceMesh<BlockVertex<TestPoint>, TestTile>| {
                // As of this writing, this benchmark is really just "what if we don't allocate
                // a new Vec". Later, the buffers will hopefully become cleverer and we'll be
                // able to reuse some work (or at least send only part of the buffer to the GPU),
                // and so this will become a meaningful benchmark of how much CPU time we're
                // spending or saving on that.
                checker_ing.do_compute(buffer)
            },
            BatchSize::SmallInput,
        );
    });

    let half_ing = SpaceMeshIngredients::new(options, half_space(Block::from(Rgba::WHITE)));
    g.bench_function("half-new", |b| {
        b.iter_batched_ref(|| (), |()| half_ing.do_new(), BatchSize::SmallInput);
    });

    g.bench_function("half-reused", |b| {
        b.iter_batched_ref(
            || {
                let mut buffer = SpaceMesh::default();
                half_ing.do_compute(&mut buffer);
                buffer
            },
            |buffer: &mut SpaceMesh<BlockVertex<TestPoint>, TestTile>| half_ing.do_compute(buffer),
            BatchSize::SmallInput,
        );
    });
}

fn slow_mesh_benches(c: &mut Criterion) {
    let mut g = c.benchmark_group("slow");
    g.sample_size(10);
    let options = MeshOptions::new(&GraphicsOptions::default());

    g.bench_function("transparent", |b| {
        let ing = checkerboard_space_bench_setup(options.clone(), true);
        b.iter_batched_ref(|| (), |()| ing.do_new(), BatchSize::SmallInput);
    });
}

fn dynamic_benches(c: &mut Criterion) {
    let mut g = c.benchmark_group("dynamic");
    let graphics_options = GraphicsOptions::default();
    let camera = Camera::new(graphics_options, Viewport::with_scale(1.0, [100, 100]));

    g.bench_function("initial-update", |b| {
        let space_ref = URef::new_pending(Name::Pending, half_space(Block::from(Rgba::WHITE)));
        b.iter_batched_ref(
            || {
                let csm: dynamic::ChunkedSpaceMesh<
                    (),
                    BlockVertex<TestPoint>,
                    TestAllocator,
                    std::time::Instant,
                    16,
                > = dynamic::ChunkedSpaceMesh::new(space_ref.clone(), true);
                let tex = TestAllocator::new();
                (tex, csm)
            },
            |(tex, csm)| {
                let info = csm.update_blocks_and_some_chunks(
                    &camera,
                    tex,
                    time::DeadlineStd::Whenever,
                    |_| {},
                );
                assert_eq!(info.flaws, Flaws::empty()); // should not be unfinished
            },
            BatchSize::LargeInput,
        );
    });

    // TODO: Add a test for updates past the initial one
}

// --- End of benches, beginning helpers ---

fn checkerboard_space_bench_setup(options: MeshOptions, transparent: bool) -> SpaceMeshIngredients {
    SpaceMeshIngredients::new(
        options,
        checkerboard_space([
            AIR,
            Block::from(if transparent {
                rgba_const!(0.5, 0.5, 0.5, 0.5)
            } else {
                Rgba::WHITE
            }),
        ]),
    )
}

fn checkerboard_block(universe: &mut Universe, voxels: [Block; 2]) -> Block {
    Block::builder()
        .voxels_ref(R16, universe.insert_anonymous(checkerboard_space(voxels)))
        .build()
}

fn checkerboard_space(blocks: [Block; 2]) -> Space {
    let bounds = GridAab::from_lower_size([0, 0, 0], [16, 16, 16]);
    let mut space = Space::empty(bounds);
    space
        .fill(bounds, |p| {
            Some(&blocks[((p.x + p.y + p.z) as usize).rem_euclid(blocks.len())])
        })
        .unwrap();
    space
}

/// Space whose lower half is filled with the block.
fn half_space(block: Block) -> Space {
    let bounds = GridAab::from_lower_size([0, 0, 0], [16, 16, 16]);
    let mut space = Space::empty(bounds);
    space
        .fill_uniform(GridAab::from_lower_size([0, 0, 0], [16, 8, 16]), &block)
        .unwrap();
    space
}

/// Data prepared for a benchmark of [`SpaceMesh::new`] or [`SpaceMesh::compute`].
struct SpaceMeshIngredients {
    space: Space,
    block_meshes: BlockMeshes<BlockVertex<TestPoint>, TestTile>,
    options: MeshOptions,
}

impl SpaceMeshIngredients {
    fn new(options: MeshOptions, space: Space) -> Self {
        let block_meshes = block_meshes_for_space(&space, &TestAllocator::new(), &options);

        SpaceMeshIngredients {
            space,
            block_meshes,
            options,
        }
    }

    fn do_new(&self) -> SpaceMesh<BlockVertex<TestPoint>, TestTile> {
        SpaceMesh::new(
            &self.space,
            self.space.bounds(),
            &self.options,
            &*self.block_meshes,
        )
    }

    fn do_compute(&self, mesh: &mut SpaceMesh<BlockVertex<TestPoint>, TestTile>) {
        SpaceMesh::compute(
            mesh,
            &self.space,
            self.space.bounds(),
            &self.options,
            &*self.block_meshes,
        )
    }
}
