#![expect(clippy::identity_op)]

use alloc::boxed::Box;
use alloc::vec::Vec;
use std::sync::Mutex;

use all_is_cubes::arcstr;
use all_is_cubes::block::{self, AIR, Block, BlockDef, BlockDefTransaction};
use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::content::make_some_blocks;
use all_is_cubes::euclid::{Point3D, Scale, point3};
use all_is_cubes::listen::Store as _;
use all_is_cubes::math::{Cube, GridAab, GridCoordinate, GridPoint, Rgba, Vol, zo32};
use all_is_cubes::space::{BlockIndex, Space, SpaceChange, SpaceTransaction};
use all_is_cubes::time;
use all_is_cubes::universe::{Handle, Universe};
use all_is_cubes_render::Flaws;
use all_is_cubes_render::camera::{Camera, GraphicsOptions, TransparencyOption, Viewport};

use crate::texture::NoTextures;
use crate::{dynamic, testing};

use super::{ChunkTodo, ChunkedSpaceMesh, CsmTodo, CsmUpdateInfo};

type Mt<const MBM: usize> = testing::Mt<NoTextures, MBM>;

const CHUNK_SIZE: GridCoordinate = 16;
const LARGE_VIEW_DISTANCE: f64 = 200.0;
const NO_INSTANCES: usize = usize::MAX;
const ALL_INSTANCES: usize = 0;

fn read_todo_chunks(todo: &CsmTodo<CHUNK_SIZE>) -> Vec<(ChunkPos<CHUNK_SIZE>, ChunkTodo)> {
    let mut v = todo
        .chunks
        .iter()
        .map(|(&p, ct)| (p, ct.clone()))
        .collect::<Vec<_>>();
    v.sort_by_key(|(p, _): &(ChunkPos<CHUNK_SIZE>, _)| <_ as Into<[GridCoordinate; 3]>>::into(p.0));
    v
}

#[test]
fn update_adjacent_chunk_positive() {
    let mut todo: CsmTodo<CHUNK_SIZE> = Default::default();
    todo.chunks.extend(vec![
        (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
        (ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN),
        (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
    ]);
    todo.receive(&[SpaceChange::CubeBlock {
        cube: Cube::new(CHUNK_SIZE - 1, CHUNK_SIZE / 2, CHUNK_SIZE / 2),
        old_block_index: 123,
        new_block_index: 456,
    }]);
    assert_eq!(
        read_todo_chunks(&todo),
        vec![
            (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
            (
                ChunkPos::new(0, 0, 0),
                ChunkTodo {
                    state: dynamic::chunk::ChunkTodoState::DirtyMeshAndInstances,
                    ..ChunkTodo::CLEAN
                }
            ),
            (
                ChunkPos::new(1, 0, 0),
                ChunkTodo {
                    state: dynamic::chunk::ChunkTodoState::DirtyMeshAndInstances,
                    ..ChunkTodo::CLEAN
                }
            ),
        ],
    );
}

#[test]
fn update_adjacent_chunk_negative() {
    let mut todo: CsmTodo<CHUNK_SIZE> = Default::default();
    todo.chunks.extend(vec![
        (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
        (ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN),
        (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
    ]);
    todo.receive(&[SpaceChange::CubeBlock {
        cube: Cube::new(0, CHUNK_SIZE / 2, CHUNK_SIZE / 2),
        old_block_index: 123,
        new_block_index: 456,
    }]);
    assert_eq!(
        read_todo_chunks(&todo),
        vec![
            (
                ChunkPos::new(-1, 0, 0),
                ChunkTodo {
                    state: dynamic::chunk::ChunkTodoState::DirtyMeshAndInstances,
                    ..ChunkTodo::CLEAN
                }
            ),
            (
                ChunkPos::new(0, 0, 0),
                ChunkTodo {
                    state: dynamic::chunk::ChunkTodoState::DirtyMeshAndInstances,
                    ..ChunkTodo::CLEAN
                }
            ),
            (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
        ],
    );
}

#[test]
fn todo_ignores_absent_chunks() {
    let mut todo: CsmTodo<CHUNK_SIZE> = Default::default();

    let cube = Cube::from(GridPoint::new(1, 1, 1) * (CHUNK_SIZE / 2));
    // Nothing happens...
    todo.receive(&[SpaceChange::CubeBlock {
        cube,
        old_block_index: 0,
        new_block_index: 0,
    }]);
    assert_eq!(read_todo_chunks(&todo), vec![]);
    // ...until the chunk exists in the table already.
    todo.chunks.insert(ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN);
    todo.receive(&[SpaceChange::CubeBlock {
        cube,
        old_block_index: 0,
        new_block_index: 0,
    }]);
    assert_eq!(
        read_todo_chunks(&todo),
        vec![(
            ChunkPos::new(0, 0, 0),
            ChunkTodo {
                state: dynamic::chunk::ChunkTodoState::DirtyMeshAndInstances,
                ..ChunkTodo::CLEAN
            }
        ),],
    );
}

#[derive(Debug)]
struct CsmTester<const MBM: usize> {
    universe: Universe,
    space: Handle<Space>,
    camera: Camera,
    csm: ChunkedSpaceMesh<Mt<MBM>, CHUNK_SIZE>,
}

impl<const MBM: usize> CsmTester<MBM> {
    fn new(mut universe: Universe, space: Space, view_distance: f64) -> Self {
        let space_handle = universe.insert_anonymous(space);
        let csm = ChunkedSpaceMesh::new(space_handle.clone(), NoTextures, true);
        let camera = Camera::new(
            {
                let mut o = GraphicsOptions::default();
                o.view_distance = view_distance.try_into().unwrap();
                o
            },
            Viewport::ARBITRARY,
        );
        Self {
            universe,
            space: space_handle,
            camera,
            csm,
        }
    }

    /// Call [`ChunkedSpaceMesh::update()`] with the tester's placeholders.
    ///
    /// For convenience, the `render_data_updater` may be a [`FnMut`] instead of a [`Fn`].
    fn update<F>(&mut self, render_data_updater: F) -> CsmUpdateInfo
    where
        F: FnMut(dynamic::RenderDataUpdate<'_, Mt<MBM>>) + Send,
    {
        let updater_cell = Mutex::new(render_data_updater);
        self.csm.update(
            self.universe.read_ticket(),
            &self.camera,
            time::Deadline::Whenever,
            |u| updater_cell.lock().unwrap()(u),
        )
    }

    /// Move camera to a position measured in chunks.
    fn move_camera_to(&mut self, position: impl Into<Point3D<f64, ()>>) {
        const SCALE: Scale<f64, (), Cube> = Scale::new(CHUNK_SIZE as f64);

        let mut view_transform = self.camera.view_transform();
        view_transform.translation = SCALE.transform_point3d(position.into()).to_vector();
        self.camera.set_view_transform(view_transform);
    }

    /// Fetch instances and convert them into simpler data types
    fn instances(&self) -> Vec<(BlockIndex, Vec<[GridCoordinate; 3]>)> {
        let mut by_block: Vec<_> = self
            .csm
            .iter_chunks()
            .flat_map(|chunk| chunk.block_instances())
            .collect::<dynamic::InstanceCollector>()
            .iter()
            .map(|(block_index, cubes)| {
                let mut cubes: Vec<_> = cubes.map(<[GridCoordinate; 3]>::from).collect();
                cubes.sort_unstable();
                (block_index, cubes)
            })
            .collect();
        by_block.sort();
        by_block
    }
}

#[test]
fn basic_chunk_presence() {
    let mut tester: CsmTester<NO_INSTANCES> = CsmTester::new(
        Universe::new(),
        Space::empty_positive(1, 1, 1),
        LARGE_VIEW_DISTANCE,
    );
    tester.update(dynamic::noop_render_data_updater);
    assert_ne!(None, tester.csm.chunk(ChunkPos::new(0, 0, 0)));
    // There should not be a chunk where there's no Space
    assert_eq!(None, tester.csm.chunk(ChunkPos::new(1, 0, 0)));
    // TODO: Check that chunks end at the view distance.
}

/// Tests that we don’t spuriously call the render data updater.
///
/// (This used to be a test expecting ongoing `indices_only` updates, but we added caching that
/// skips that too. TODO: Expand this test to trigger a depth sort and look for that.)
#[test]
fn no_render_data_updates_after_completion() {
    let mut tester: CsmTester<NO_INSTANCES> = CsmTester::new(
        Universe::new(),
        Space::empty_positive(1, 1, 1),
        LARGE_VIEW_DISTANCE,
    );
    tester.update(|u| {
        assert_eq!(u.indices_only, None);
    });

    tester
        .universe
        .execute_1(
            &tester.space,
            SpaceTransaction::set_cube(
                [0, 0, 0],
                None,
                Some(block::from_color!(1.0, 1.0, 1.0, 0.5)),
            ),
        )
        .unwrap();
    tester.update(|u| {
        assert_eq!(u.indices_only, None);
    });

    // In the current implementation there is one extra indices-only update because we don't
    // properly depth sort just-created chunks, but ideally there wouldn't be this one.
    tester.update(|u| {
        assert_eq!(u.indices_only, Some(0..36));
    });

    // Now, expect quiescence.
    let mut did_call = None;
    tester.update(|u| {
        did_call = Some((u.mesh_id, u.indices_only));
    });
    assert_eq!(did_call, None, "Expected no update when nothing changed");
}

#[test]
fn graphics_options_change() {
    // TODO: This test is fragile because it doesn't think about multiple chunks.
    let mut options = GraphicsOptions::default();
    options.view_distance = 1u8.into();
    options.transparency = TransparencyOption::Volumetric;

    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
        .filled_with(block::from_color!(1., 1., 1., 0.25))
        .build();

    let mut tester: CsmTester<NO_INSTANCES> = CsmTester::new(Universe::new(), space, 200.0);
    tester.camera.set_options(options.clone());

    let mut vertices = None;
    tester.update(|u| vertices = Some(u.mesh.vertices().0.len()));
    assert_eq!(vertices, Some(24));

    // Change options so that the mesh should disappear
    options.transparency = TransparencyOption::Threshold(zo32(0.5));
    tester.camera.set_options(options);

    vertices = None;
    tester.update(|u| vertices = Some(u.mesh.vertices().0.len()));
    assert_eq!(vertices, Some(0));
}

/// Check that chunks out of view are eventually dropped.
#[test]
fn drop_chunks_when_moving() {
    // use small view distance in a large space (especially large in x)
    let mut tester: CsmTester<NO_INSTANCES> = CsmTester::new(
        Universe::new(),
        Space::builder(GridAab::from_lower_upper(
            [-1000, -100, -100],
            [1000, 100, 100],
        ))
        .build(),
        f64::from(CHUNK_SIZE) / 2.0,
    );
    // middle of the first chunk
    tester.move_camera_to([0.5, 0.5, 0.5]);

    tester.update(dynamic::noop_render_data_updater);
    // Save the chunk count we got after initial update.
    // Note: We don't *actually* care what this is exactly, but we want to compare
    // it to future counts.
    let initial_chunk_count = tester.csm.iter_chunks().count();
    assert_eq!(initial_chunk_count, 3usize.pow(3));

    // Move over one chunk at a time repeatedly.
    for i in 1..30 {
        let position = point3(1.5 + f64::from(i), 0.5, 0.5);
        tester.move_camera_to(position);
        tester.update(dynamic::noop_render_data_updater);
        let count = tester.csm.iter_chunks().count();
        println!("{i}: {position:?}, {count} chunks");
    }
    // Check that there is not indefinite accumulation of chunks.
    // (But allow some slop for a caching policy.)
    assert!(tester.csm.iter_chunks().count() < initial_chunk_count * 3);
}

/// Test the logic which decides whether `ChunkedSpaceMesh` managed to completely
/// update itself.
#[test]
fn did_not_finish_detection() {
    let mut tester: CsmTester<NO_INSTANCES> = CsmTester::new(
        Universe::new(),
        Space::empty_positive(1000, 1, 1),
        LARGE_VIEW_DISTANCE,
    );

    {
        eprintln!("--- timing out update");
        let info = tester.csm.update(
            tester.universe.read_ticket(),
            &tester.camera,
            time::Deadline::Asap,
            dynamic::noop_render_data_updater,
        );

        // This is the state that should(n't) be affected.
        // (If we stop having `complete_time` then it's okay to just delete that part of
        // the assertion.)
        assert_eq!(
            (
                info.flaws,
                tester.csm.did_not_finish_chunks,
                tester.csm.complete_time
            ),
            (Flaws::UNFINISHED, true, None)
        );
    }

    {
        eprintln!("--- normal update");
        // Now while we're at it, try finishing things and check that state too.
        let info = tester.update(dynamic::noop_render_data_updater);
        assert_eq!(
            (
                info.flaws,
                tester.csm.did_not_finish_chunks,
                tester.csm.complete_time.is_some(),
            ),
            (Flaws::empty(), false, true)
        );
    }
}

/// Instances are grouped by block index, even if they are in different chunks.
///
/// TODO(instancing): This test used to be meaningful but then we refactored so it is trivial,
/// just testing `CsmTester::instances()` and `InstanceCollector`...
#[test]
fn instances_grouped_by_block() {
    let [block1, block2] = make_some_blocks();
    let space = Space::builder(GridAab::from_lower_size(
        [0, 0, 0],
        [CHUNK_SIZE.cast_unsigned() * 2, 1, 1],
    ))
    .build_and_mutate(|m| {
        m.set([0, 0, 0], &block1).unwrap();
        m.set([1, 0, 0], &block2).unwrap();
        m.set([CHUNK_SIZE + 0, 0, 0], &block1).unwrap();
        m.set([CHUNK_SIZE + 1, 0, 0], &block2).unwrap();
        Ok(())
    })
    .unwrap();

    let mut tester: CsmTester<ALL_INSTANCES> =
        CsmTester::new(Universe::new(), space, LARGE_VIEW_DISTANCE);
    tester.update(dynamic::noop_render_data_updater);

    assert_eq!(
        tester.instances(),
        vec![
            // index 0 is air
            (1, vec![[0, 0, 0], [CHUNK_SIZE + 0, 0, 0]]),
            (2, vec![[1, 0, 0], [CHUNK_SIZE + 1, 0, 0]]),
        ]
    );
}

/// Instances are used for blocks with relevant animation hints.
#[test]
fn instances_for_animated() {
    let [not_anim, will_be_anim] = make_some_blocks();
    let anim = Block::builder()
        .display_name("animated")
        .color(will_be_anim.color())
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .build();
    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
        .build_and_mutate(|m| {
            m.set([0, 0, 0], &not_anim).unwrap();
            m.set([1, 0, 0], &anim).unwrap();
            Ok(())
        })
        .unwrap();
    let index_of_anim = space.get_block_index([1, 0, 0]).unwrap();

    // TODO(instancing): Kludge: the question "should we use instances *at all*" has not yet been
    // separated from the question of "should we use instances always". We care about the former
    // and not the latter here.
    let mut tester: CsmTester<1000> = CsmTester::new(Universe::new(), space, LARGE_VIEW_DISTANCE);
    tester.update(dynamic::noop_render_data_updater);

    assert_eq!(tester.instances(), vec![(index_of_anim, vec![[1, 0, 0]])]);
}

/// When block meshes are merged into chunk meshes, the chunk meshes need to be updated when the
/// block meshes change -- but when not merged and rendered as instances, the chunk meshes should
/// *not* be updated.
///
/// Note that this doesn't cover changes to the `Space`, only changes to the block definition.
/// There's a separate test for that.
#[test]
fn instances_dont_dirty_mesh_when_block_changes() {
    let mut universe = Universe::new();
    let [not_anim, will_be_anim] = make_some_blocks();
    let anim_def = universe.insert_anonymous(BlockDef::new(
        universe.read_ticket(),
        Block::builder()
            .display_name("animated")
            .color(will_be_anim.color())
            .animation_hint(block::AnimationHint::redefinition(
                block::AnimationChange::Shape,
            ))
            .build(),
    ));
    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
        .read_ticket(universe.read_ticket())
        .build_and_mutate(|m| {
            m.set([0, 0, 0], &not_anim).unwrap();
            m.set([1, 0, 0], Block::from(anim_def.clone())).unwrap();
            Ok(())
        })
        .unwrap();

    let mut tester: CsmTester<1000> = CsmTester::new(universe, space, LARGE_VIEW_DISTANCE);
    tester.update(dynamic::noop_render_data_updater);

    // Make a change to the block defunition...
    tester
        .universe
        .execute_1(
            &anim_def,
            BlockDefTransaction::overwrite(
                Block::builder()
                    .display_name("replaced")
                    .color(will_be_anim.color())
                    .build(),
            ),
        )
        .unwrap();

    // ...and an update should not include updating any meshes.
    tester.update(|rdu| {
        panic!("unwanted render data update: {rdu:#?}");
    });
}

/// Modifying a cube of the `Space`, when the old and new blocks are both instanced, should not
/// cause a mesh rebuild.
///
/// Note that this doesn't cover changes to the block definition, only the `Space`.
/// There's a separate test for that.
#[test]
fn instances_dont_dirty_mesh_when_space_changes() {
    let [not_anim, will_be_anim] = make_some_blocks();
    let anim = Block::builder()
        .display_name("animated")
        .color(will_be_anim.color())
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .build();
    let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [3, 1, 1]))
        .build_and_mutate(|m| {
            m.set([0, 0, 0], &not_anim).unwrap();
            m.set([1, 0, 0], &anim).unwrap();
            m.set([2, 0, 0], &anim).unwrap(); // unchanged copy of the block to confirm the right change is made
            Ok(())
        })
        .unwrap();
    let index_of_anim = space.get_block_index([1, 0, 0]).unwrap();

    // Initialize
    let mut tester: CsmTester<1000> = CsmTester::new(Universe::new(), space, LARGE_VIEW_DISTANCE);
    tester.update(dynamic::noop_render_data_updater);

    // Check initial state
    assert_eq!(
        tester.instances(),
        vec![(index_of_anim, vec![[1, 0, 0], [2, 0, 0]])]
    );

    // Make a change to the space...
    tester
        .universe
        .execute_1(
            &tester.space,
            SpaceTransaction::set_cube([1, 0, 0], Some(anim.clone()), Some(AIR)),
        )
        .unwrap();

    // ...and an update should not include updating any meshes.
    tester.update(|rdu| {
        panic!("unwanted render data update: {rdu:#?}");
    });

    assert_eq!(tester.instances(), vec![(index_of_anim, vec![[2, 0, 0]]),]);
}

#[test]
fn at_maximum_number_of_blocks() {
    assert_eq!(
        BlockIndex::MAX,
        65535,
        "this test may need redesign to be feasible"
    );

    let blocks: Vec<Block> = (0..=BlockIndex::MAX)
        .map(|i| {
            // These blocks must all be distinct.
            // We are not using `make_some_blocks()` because that would construct a large array
            // on the stack.
            Block::builder()
                .color(Rgba::WHITE)
                .display_name(arcstr::format!("#{i}"))
                .build()
        })
        .collect();
    let bounds = GridAab::from_lower_size([0, 0, 0], [256, 256, 1]);
    let space = Space::builder(bounds)
        .palette_and_contents(
            blocks,
            Vol::from_elements(bounds, (0..=BlockIndex::MAX).collect::<Box<[BlockIndex]>>())
                .unwrap(),
            None,
        )
        .unwrap()
        .build();

    // All we're checking for, for now, is that this doesn't panic.
    let mut tester: CsmTester<NO_INSTANCES> =
        CsmTester::new(Universe::new(), space, LARGE_VIEW_DISTANCE);
    tester.update(dynamic::noop_render_data_updater);
}
