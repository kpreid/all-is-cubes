#![allow(missing_docs)]

use criterion::{BatchSize, Criterion, criterion_main};

use all_is_cubes::block::{self, AIR, Block, Resolution::R16};
use all_is_cubes::content::make_some_voxel_blocks;
use all_is_cubes::math::{GridAab, Rgba};
use all_is_cubes::space::Space;
use all_is_cubes::universe::Universe;
use all_is_cubes_render::camera::GraphicsOptions;

use all_is_cubes_mesh as mesh;
use all_is_cubes_mesh::testing::{Allocator, TextureMt as Mt};
use all_is_cubes_mesh::{BlockMesh, BlockMeshes, MeshOptions, SpaceMesh};

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
    let default_options = &MeshOptions::new(&GraphicsOptions::default());

    // Exercise the code paths that only apply to resolution 1;
    // all the other benchmarks use larger resolutions.
    g.bench_function("r1-fresh", |b| {
        let universe = Universe::new();
        iter_new_block_mesh(
            b,
            &universe,
            default_options,
            &Block::builder().color(Rgba::WHITE).build(),
        );
    });
    g.bench_function("r1-reused", |b| {
        let universe = Universe::new();
        iter_reused_block_mesh(
            b,
            &universe,
            default_options,
            &Block::builder().color(Rgba::WHITE).build(),
        );
    });

    for (use_new_block_triangulator, suffix) in [(false, "greedy"), (true, "newtri")] {
        let mut options = MeshOptions::new(&GraphicsOptions::default());
        options.use_new_block_triangulator = use_new_block_triangulator;
        let options = &options;

        let namer = |str| format!("{str}/{suffix}");

        g.bench_function(namer("checker-fresh"), |b| {
            let mut universe = Universe::new();
            let block = checkerboard_block(&mut universe, &[AIR, block::from_color!(Rgba::WHITE)]);
            iter_new_block_mesh(b, &universe, options, &block);
        });
        g.bench_function(namer("checker-reused"), |b| {
            let mut universe = Universe::new();
            let block = checkerboard_block(&mut universe, &[AIR, block::from_color!(Rgba::WHITE)]);
            iter_reused_block_mesh(b, &universe, options, &block);
        });

        g.bench_function(namer("half"), |b| {
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

        g.bench_function(namer("opaque"), |b| {
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

        g.bench_function(namer("msvb"), |b| {
            let mut universe = Universe::new();
            let [block] = make_some_voxel_blocks(&mut universe);
            iter_new_block_mesh(b, &universe, options, &block);
        });
    }

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

    g.bench_function("checker-fresh", |b| {
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
                assert_eq!(buffer.vertices().0.len(), 6 * 4 * (16 * 16 * 16) / 2);
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
    g.bench_function("half-fresh", |b| {
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
    use all_is_cubes::chunking::ChunkPos;
    use all_is_cubes::euclid::{self, vec3};
    use all_is_cubes::math::Cube;
    use all_is_cubes::time;
    use all_is_cubes::universe::Handle;
    use all_is_cubes_mesh::dynamic;
    use all_is_cubes_render::Flaws;
    use all_is_cubes_render::camera::{Camera, ViewTransform, Viewport};

    let mut g = c.benchmark_group("dynamic");
    let graphics_options = GraphicsOptions::default();
    let default_camera = Camera::new(graphics_options, Viewport::with_scale(1.0, [100, 100]));

    g.bench_function("initial-update", |b| {
        let mut universe = Universe::new();
        let space_handle = universe.insert_anonymous(half_space(&block::from_color!(Rgba::WHITE)));
        b.iter_batched_ref(
            || {
                let csm: dynamic::ChunkedSpaceMesh<Mt, 16> =
                    dynamic::ChunkedSpaceMesh::new(space_handle.clone(), Allocator::new(), true);
                csm
            },
            |csm| {
                let info = csm.update(
                    universe.read_ticket(),
                    &default_camera,
                    time::Deadline::Whenever,
                    dynamic::noop_render_data_updater,
                );
                assert_eq!(info.flaws, Flaws::empty()); // should not be unfinished
            },
            BatchSize::LargeInput,
        );
    });

    // Depth sorting performance benches, designed to detect the difference between
    // split and single-struct vertices.
    {
        /// A vertex type that, unlike `BlockVertex`, has less data,
        /// so we can measure the performance impact of reducing the amount of data depth
        /// sorting applies to.
        #[derive(Clone, Copy, Debug, PartialEq)]
        struct PositionOnlyVertex {
            position: mesh::Position,
        }
        impl mesh::Vertex for PositionOnlyVertex {
            // These choices match `BlockVertex`.
            const WANTS_DEPTH_SORTING: bool = true;
            type SecondaryData = mesh::Coloring<mesh::texture::NoTexture>;
            type TexPoint = mesh::texture::NoTexture;
            type BlockInst = Cube;

            fn from_block_vertex(
                vertex: mesh::BlockVertex<Self::TexPoint>,
            ) -> (Self, Self::SecondaryData) {
                (
                    Self {
                        position: vertex.position,
                    },
                    vertex.coloring,
                )
            }

            fn instantiate_block(cube: Cube) -> Self::BlockInst {
                cube
            }

            fn instantiate_vertex(&mut self, block: Self::BlockInst) {
                self.position += block.lower_bounds().to_f32().to_vector().cast_unit();
            }

            fn position(&self) -> mesh::Position {
                self.position
            }
        }
        struct PovMt;
        impl mesh::MeshTypes for PovMt {
            type Vertex = PositionOnlyVertex;
            type Alloc = mesh::texture::NoTextures;
            type Tile = mesh::texture::NoTexture;
        }
        impl dynamic::DynamicMeshTypes for PovMt {
            type RenderData = ();
            const MAXIMUM_MERGED_BLOCK_MESH_SIZE: usize = usize::MAX;
        }

        struct State<'a, Mt: dynamic::DynamicMeshTypes> {
            universe: &'a Universe,
            csm: dynamic::ChunkedSpaceMesh<Mt, 16>,
            camera: Camera,
            pos_step: f64,
        }

        impl<'a, Mt: dynamic::DynamicMeshTypes<Alloc: Send + Sync, Tile: Send + Sync>> State<'a, Mt> {
            fn new(
                universe: &'a Universe,
                space_handle: Handle<Space>,
                camera: Camera,
                allocator: Mt::Alloc,
            ) -> Self {
                let mut csm: dynamic::ChunkedSpaceMesh<Mt, 16> =
                    dynamic::ChunkedSpaceMesh::new(space_handle, allocator, true);
                csm.update(
                    universe.read_ticket(),
                    &camera,
                    time::Deadline::Whenever,
                    dynamic::noop_render_data_updater,
                );
                // Note: this number will change if the number of `DepthOrdering`s changes.
                assert_eq!(
                    1327104,
                    csm.chunk(ChunkPos::new(0, 0, 0))
                        .unwrap()
                        .mesh()
                        .indices()
                        .len(),
                    "mesh is not of the expected complexity"
                );

                State {
                    csm,
                    universe,
                    camera,
                    pos_step: 0.5,
                }
            }

            // Inner loop of both benchmarks
            fn inner(&mut self) {
                self.pos_step = (self.pos_step + 1.).rem_euclid(16.);
                self.camera.set_view_transform(ViewTransform {
                    rotation: euclid::Rotation3D::around_y(euclid::Angle::frac_pi_2()),
                    // This position must be within the chunk or full depth sorting won't happen.
                    translation: vec3(self.pos_step, 0.5, 0.5),
                });

                let info = self.csm.update(
                    self.universe.read_ticket(),
                    &self.camera,
                    time::Deadline::Whenever,
                    dynamic::noop_render_data_updater,
                );
                assert_ne!(info.depth_sort_info.quads_sorted, 0);
            }
        }

        let mut universe = Universe::new();
        // transparent block so we cause depth sorting
        let space_handle = universe.insert_anonymous(half_space(&block::from_color!(Rgba::new(
            0.0, 1.0, 0.0, 0.5
        ))));

        // Uses non-split vertices (no secondary data)
        g.bench_function("depth-sort-whole", |b| {
            b.iter_batched_ref(
                || {
                    State::<Mt>::new(
                        &universe,
                        space_handle.clone(),
                        default_camera.clone(),
                        Allocator::new(),
                    )
                },
                State::inner,
                BatchSize::LargeInput,
            );
        });

        // Uses secondary data
        g.bench_function("depth-sort-split", |b| {
            b.iter_batched_ref(
                || {
                    State::<PovMt>::new(
                        &universe,
                        space_handle.clone(),
                        default_camera.clone(),
                        mesh::texture::NoTextures,
                    )
                },
                State::inner,
                BatchSize::LargeInput,
            );
        });
    }
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
    Space::builder(bounds)
        .build_and_mutate(|m| {
            m.fill(bounds, |p| {
                Some(&blocks[((p.x + p.y + p.z) as usize).rem_euclid(blocks.len())])
            })
        })
        .unwrap()
}

/// Space whose lower half is filled with the block.
fn half_space(block: &Block) -> Space {
    let bounds = GridAab::from_lower_size([0, 0, 0], [16, 16, 16]);
    Space::builder(bounds)
        .build_and_mutate(|m| {
            m.fill_uniform(GridAab::from_lower_size([0, 0, 0], [16, 8, 16]), block)
        })
        .unwrap()
}

/// Data prepared for a benchmark of [`SpaceMesh::new`] or [`SpaceMesh::compute`].
struct SpaceMeshIngredients {
    space: Space,
    block_meshes: BlockMeshes<Mt>,
    options: MeshOptions,
}

impl SpaceMeshIngredients {
    fn new(options: MeshOptions, space: Space) -> Self {
        let block_meshes = mesh::block_meshes_for_space(&space, &Allocator::new(), &options);

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
