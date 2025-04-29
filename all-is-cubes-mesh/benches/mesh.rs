#![allow(missing_docs)]

use criterion::{BatchSize, Criterion, criterion_main};

use all_is_cubes::block::{self, AIR, Block, Resolution::R16};
use all_is_cubes::content::make_some_voxel_blocks;
use all_is_cubes::math::{GridAab, Rgba};
use all_is_cubes::space::Space;
use all_is_cubes::universe::{ReadTicket, Universe};
use all_is_cubes_render::camera::GraphicsOptions;

use all_is_cubes_mesh::testing::{Allocator, TextureMt as Mt};
use all_is_cubes_mesh::{BlockMesh, BlockMeshes, MeshOptions, SpaceMesh, block_meshes_for_space};

criterion_main!(benches);
fn benches() {
    let mut c = Criterion::default().configure_from_args();
    block_mesh_benches(&mut c);
    space_mesh_benches(&mut c);
    slow_mesh_benches(&mut c);
    #[cfg(feature = "dynamic")]
    dynamic_benches(&mut c);
}

fn block_mesh_benches(c: &mut Criterion) {
    let mut g = c.benchmark_group("block");
    let options = &MeshOptions::new(&GraphicsOptions::default());

    // Exercise the code paths that only apply to resolution 1;
    // all the other benchmarks use larger resolutions.
    g.bench_function("r1-new", |b| {
        let universe = Universe::new();
        iter_new_block_mesh(
            b,
            &universe,
            options,
            &Block::builder().color(Rgba::WHITE).build(),
        );
    });
    g.bench_function("r1-reused", |b| {
        let universe = Universe::new();
        iter_reused_block_mesh(
            b,
            &universe,
            options,
            &Block::builder().color(Rgba::WHITE).build(),
        );
    });

    g.bench_function("checker-new", |b| {
        let mut universe = Universe::new();
        let block = checkerboard_block(&mut universe, &[AIR, block::from_color!(Rgba::WHITE)]);
        iter_new_block_mesh(b, &universe, options, &block);
    });
    g.bench_function("checker-reused", |b| {
        let mut universe = Universe::new();
        let block = checkerboard_block(&mut universe, &[AIR, block::from_color!(Rgba::WHITE)]);
        iter_reused_block_mesh(b, &universe, options, &block);
    });

    g.bench_function("half", |b| {
        let mut universe = Universe::new();
        // Elsewhere this kind of block is called a slab, but “half” is consistent with the
        // terminology of the space mesh benches in this file.
        // Note also that unlike `make_slab()`, `half_space()` is not shrinkwrapping the space
        // — there are empty voxels.
        let block = Block::builder()
            .voxels_handle(
                R16,
                universe.insert_anonymous(half_space(&block::from_color!(Rgba::WHITE))),
            )
            .build();
        iter_new_block_mesh(b, &universe, options, &block);
    });

    g.bench_function("opaque", |b| {
        let mut universe = Universe::new();
        let block = checkerboard_block(
            &mut universe,
            &[
                block::from_color!(Rgba::BLACK),
                block::from_color!(Rgba::WHITE),
            ],
        );
        iter_new_block_mesh(b, &universe, options, &block);
    });

    g.bench_function("msvb", |b| {
        let mut universe = Universe::new();
        let [block] = make_some_voxel_blocks(&mut universe);
        iter_new_block_mesh(b, &universe, options, &block);
    });

    // TODO: Add meshing a block that has a complex but not worst-case shape.
}

fn iter_reused_block_mesh(
    b: &mut criterion::Bencher<'_>,
    universe: &Universe,
    options: &MeshOptions,
    block: &Block,
) {
    let evaluated = block.evaluate(universe.read_ticket()).unwrap();
    let mut shared_mesh = BlockMesh::<Mt>::default();
    b.iter_batched_ref(
        || (),
        |()| shared_mesh.compute(&evaluated, &Allocator::new(), options),
        BatchSize::SmallInput,
    );
}

fn iter_new_block_mesh(
    b: &mut criterion::Bencher<'_>,
    universe: &Universe,
    options: &MeshOptions,
    block: &Block,
) {
    let evaluated = block.evaluate(universe.read_ticket()).unwrap();
    b.iter_batched_ref(
        || (),
        |()| BlockMesh::<Mt>::new(&evaluated, &Allocator::new(), options),
        BatchSize::SmallInput,
    );
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
            |buffer: &mut SpaceMesh<Mt>| {
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

    let half_ing = SpaceMeshIngredients::new(options, half_space(&block::from_color!(Rgba::WHITE)));
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
            |buffer: &mut SpaceMesh<Mt>| half_ing.do_compute(buffer),
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

#[cfg(feature = "dynamic")]
fn dynamic_benches(c: &mut Criterion) {
    use all_is_cubes::time;
    use all_is_cubes::universe::{Handle, Name};
    use all_is_cubes_mesh::dynamic;
    use all_is_cubes_render::Flaws;
    use all_is_cubes_render::camera::{Camera, Viewport};

    let mut g = c.benchmark_group("dynamic");
    let graphics_options = GraphicsOptions::default();
    let camera = Camera::new(graphics_options, Viewport::with_scale(1.0, [100, 100]));

    g.bench_function("initial-update", |b| {
        let space_handle =
            Handle::new_pending(Name::Pending, half_space(&block::from_color!(Rgba::WHITE)));
        b.iter_batched_ref(
            || {
                let csm: dynamic::ChunkedSpaceMesh<Mt, 16> =
                    dynamic::ChunkedSpaceMesh::new(space_handle.clone(), Allocator::new(), true);
                csm
            },
            |csm| {
                let info = csm.update(
                    ReadTicket::stub(),
                    &camera,
                    time::DeadlineNt::Whenever,
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
        checkerboard_space(&[
            AIR,
            if transparent {
                block::from_color!(0.5, 0.5, 0.5, 0.5)
            } else {
                block::from_color!(Rgba::WHITE)
            },
        ]),
    )
}

fn checkerboard_block(universe: &mut Universe, voxels: &[Block; 2]) -> Block {
    Block::builder()
        .voxels_handle(R16, universe.insert_anonymous(checkerboard_space(voxels)))
        .build()
}

fn checkerboard_space(blocks: &[Block; 2]) -> Space {
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
fn half_space(block: &Block) -> Space {
    let bounds = GridAab::from_lower_size([0, 0, 0], [16, 16, 16]);
    let mut space = Space::empty(bounds);
    space
        .fill_uniform(GridAab::from_lower_size([0, 0, 0], [16, 8, 16]), block)
        .unwrap();
    space
}

/// Data prepared for a benchmark of [`SpaceMesh::new`] or [`SpaceMesh::compute`].
struct SpaceMeshIngredients {
    space: Space,
    block_meshes: BlockMeshes<Mt>,
    options: MeshOptions,
}

impl SpaceMeshIngredients {
    fn new(options: MeshOptions, space: Space) -> Self {
        let block_meshes = block_meshes_for_space(&space, &Allocator::new(), &options);

        SpaceMeshIngredients {
            space,
            block_meshes,
            options,
        }
    }

    fn do_new(&self) -> SpaceMesh<Mt> {
        SpaceMesh::new(
            &self.space,
            self.space.bounds(),
            &self.options,
            &*self.block_meshes,
        )
    }

    fn do_compute(&self, mesh: &mut SpaceMesh<Mt>) {
        SpaceMesh::compute(
            mesh,
            &self.space,
            self.space.bounds(),
            &self.options,
            &*self.block_meshes,
        )
    }
}
