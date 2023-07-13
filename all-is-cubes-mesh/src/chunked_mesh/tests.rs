use std::sync::{Arc, Mutex};

use instant::{Duration, Instant};

use all_is_cubes::block::Block;
use all_is_cubes::camera::{Camera, Flaws, GraphicsOptions, TransparencyOption, Viewport};
use all_is_cubes::cgmath::{EuclideanSpace as _, Point3};
use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::listen::Listener as _;
use all_is_cubes::math::{FreeCoordinate, GridAab, GridCoordinate};
use all_is_cubes::math::{GridPoint, NotNan};
use all_is_cubes::space::{Space, SpaceChange, SpaceTransaction};
use all_is_cubes::universe::{URef, Universe};
use all_is_cubes::{notnan, rgba_const, transaction};

use crate::{BlockVertex, NoTexture, NoTextures};

use super::{ChunkMeshUpdate, ChunkTodo, ChunkedSpaceMesh, CsmTodo, CsmUpdateInfo, TodoListener};

const CHUNK_SIZE: GridCoordinate = 16;
const LARGE_VIEW_DISTANCE: f64 = 200.0;

fn read_todo_chunks(todo: &Mutex<CsmTodo<CHUNK_SIZE>>) -> Vec<(ChunkPos<CHUNK_SIZE>, ChunkTodo)> {
    let mut v = todo
        .lock()
        .unwrap()
        .chunks
        .iter()
        .map(|(&p, &ct)| (p, ct))
        .collect::<Vec<_>>();
    v.sort_by_key(|(p, _): &(ChunkPos<CHUNK_SIZE>, _)| <_ as Into<[GridCoordinate; 3]>>::into(p.0));
    v
}

#[test]
fn update_adjacent_chunk_positive() {
    let todo: Arc<Mutex<CsmTodo<CHUNK_SIZE>>> = Default::default();
    let listener = TodoListener(Arc::downgrade(&todo));
    todo.lock().unwrap().chunks.extend(vec![
        (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
        (ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN),
        (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
    ]);
    listener.receive(SpaceChange::Block(GridPoint::new(
        CHUNK_SIZE - 1,
        CHUNK_SIZE / 2,
        CHUNK_SIZE / 2,
    )));
    assert_eq!(
        read_todo_chunks(&todo),
        vec![
            (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
            (
                ChunkPos::new(0, 0, 0),
                ChunkTodo {
                    recompute_mesh: true,
                    ..ChunkTodo::CLEAN
                }
            ),
            (
                ChunkPos::new(1, 0, 0),
                ChunkTodo {
                    recompute_mesh: true,
                    ..ChunkTodo::CLEAN
                }
            ),
        ],
    );
}

#[test]
fn update_adjacent_chunk_negative() {
    let todo: Arc<Mutex<CsmTodo<CHUNK_SIZE>>> = Default::default();
    let listener = TodoListener(Arc::downgrade(&todo));
    todo.lock().unwrap().chunks.extend(vec![
        (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
        (ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN),
        (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
    ]);
    listener.receive(SpaceChange::Block(GridPoint::new(
        0,
        CHUNK_SIZE / 2,
        CHUNK_SIZE / 2,
    )));
    assert_eq!(
        read_todo_chunks(&todo),
        vec![
            (
                ChunkPos::new(-1, 0, 0),
                ChunkTodo {
                    recompute_mesh: true,
                    ..ChunkTodo::CLEAN
                }
            ),
            (
                ChunkPos::new(0, 0, 0),
                ChunkTodo {
                    recompute_mesh: true,
                    ..ChunkTodo::CLEAN
                }
            ),
            (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
        ],
    );
}

#[test]
fn todo_ignores_absent_chunks() {
    let todo: Arc<Mutex<CsmTodo<CHUNK_SIZE>>> = Default::default();
    let listener = TodoListener(Arc::downgrade(&todo));

    let p = GridPoint::new(1, 1, 1) * (CHUNK_SIZE / 2);
    // Nothing happens...
    listener.receive(SpaceChange::Block(p));
    assert_eq!(read_todo_chunks(&todo), vec![]);
    // until the chunk exists in the table already.
    todo.lock()
        .unwrap()
        .chunks
        .insert(ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN);
    listener.receive(SpaceChange::Block(p));
    assert_eq!(
        read_todo_chunks(&todo),
        vec![(
            ChunkPos::new(0, 0, 0),
            ChunkTodo {
                recompute_mesh: true,
                ..ChunkTodo::CLEAN
            }
        ),],
    );
}

#[derive(Debug)]
struct CsmTester {
    #[allow(dead_code)] // Universe must be kept alive but is not read after construction
    universe: Universe,
    space: URef<Space>,
    camera: Camera,
    csm: ChunkedSpaceMesh<(), BlockVertex<NoTexture>, NoTextures, 16>,
}

impl CsmTester {
    fn new(space: Space, view_distance: f64) -> Self {
        let mut universe = Universe::new();
        let space_ref = universe.insert_anonymous(space);
        let csm = ChunkedSpaceMesh::<(), BlockVertex<NoTexture>, NoTextures, CHUNK_SIZE>::new(
            space_ref.clone(),
        );
        let camera = Camera::new(
            {
                let mut o = GraphicsOptions::default();
                o.view_distance = NotNan::new(view_distance).unwrap();
                o
            },
            Viewport::ARBITRARY,
        );
        Self {
            universe,
            space: space_ref,
            camera,
            csm,
        }
    }

    /// Call `csm.update_blocks_and_some_chunks()` with the tester's placeholders
    fn update<F>(&mut self, render_data_updater: F) -> CsmUpdateInfo
    where
        F: FnMut(ChunkMeshUpdate<'_, (), BlockVertex<NoTexture>, NoTexture>),
    {
        self.csm.update_blocks_and_some_chunks(
            &self.camera,
            &NoTextures,
            // In theory we should have a “fake time source” for testing purposes,
            // but this will do until we have tests of the actual timing logic.
            Instant::now() + Duration::from_secs(1_000_000),
            render_data_updater,
        )
    }

    /// Move camera to a position measured in chunks.
    fn move_camera_to(&mut self, position: impl Into<Point3<FreeCoordinate>>) {
        let mut view_transform = self.camera.get_view_transform();
        view_transform.disp = position.into().to_vec() * f64::from(CHUNK_SIZE);
        self.camera.set_view_transform(view_transform);
    }
}

#[test]
fn basic_chunk_presence() {
    let mut tester = CsmTester::new(Space::empty_positive(1, 1, 1), LARGE_VIEW_DISTANCE);
    tester.update(|_| {});
    assert_ne!(None, tester.csm.chunk(ChunkPos::new(0, 0, 0)));
    // There should not be a chunk where there's no Space
    assert_eq!(None, tester.csm.chunk(ChunkPos::new(1, 0, 0)));
    // TODO: Check that chunks end at the view distance.
}

#[test]
fn sort_view_every_frame_only_if_transparent() {
    let mut tester = CsmTester::new(Space::empty_positive(1, 1, 1), LARGE_VIEW_DISTANCE);
    tester.update(|u| {
        assert!(!u.indices_only);
    });
    tester
        .space
        .execute(
            &SpaceTransaction::set_cube(
                [0, 0, 0],
                None,
                Some(Block::from(rgba_const!(1.0, 1.0, 1.0, 0.5))),
            ),
            &mut transaction::no_outputs,
        )
        .unwrap();
    let mut did_call = false;
    tester.update(|u| {
        if u.indices_only {
            did_call = true;
        }
    });
    assert!(did_call, "Expected indices_only_updater");
    did_call = false;
    tester.update(|u| {
        if u.indices_only {
            did_call = true;
        }
    });
    assert!(did_call, "Expected indices_only_updater #2");
    // TODO: Change the behavior so additional frames *don't* depth sort if the view is unchanged.
}

#[test]
fn graphics_options_change() {
    // TODO: This test is fragile because it doesn't think about multiple chunks.
    let mut options = GraphicsOptions::default();
    options.view_distance = NotNan::from(1);
    options.transparency = TransparencyOption::Volumetric;

    let mut space = Space::empty_positive(1, 1, 1);
    space
        .set([0, 0, 0], Block::from(rgba_const!(1., 1., 1., 0.25)))
        .unwrap();

    let mut tester = CsmTester::new(space, 200.0);
    tester.camera.set_options(options.clone());

    let mut vertices = None;
    tester.update(|u| vertices = Some(u.mesh.vertices().len()));
    assert_eq!(vertices, Some(24));

    // Change options so that the mesh should disappear
    options.transparency = TransparencyOption::Threshold(notnan!(0.5));
    tester.camera.set_options(options.clone());

    vertices = None;
    tester.update(|u| vertices = Some(u.mesh.vertices().len()));
    assert_eq!(vertices, Some(0));
}

/// Check that chunks out of view are eventually dropped.
#[test]
fn drop_chunks_when_moving() {
    // use small view distance in a large space (especially large in x)
    let mut tester = CsmTester::new(
        Space::builder(GridAab::from_lower_upper(
            [-1000, -100, -100],
            [1000, 100, 100],
        ))
        .build(),
        f64::from(CHUNK_SIZE) / 2.0,
    );
    // middle of the first chunk
    tester.move_camera_to([0.5, 0.5, 0.5]);

    tester.update(|_| {});
    // Save the chunk count we got after initial update.
    // Note: We don't *actually* care what this is exactly, but we want to compare
    // it to future counts.
    let initial_chunk_count = tester.csm.iter_chunks().count();
    assert_eq!(initial_chunk_count, 3usize.pow(3));

    // Move over one chunk at a time repeatedly.
    for i in 1..30 {
        let position = Point3::new(1.5 + f64::from(i), 0.5, 0.5);
        tester.move_camera_to(position);
        tester.update(|_| {});
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
    let mut tester = CsmTester::new(Space::empty_positive(1000, 1, 1), LARGE_VIEW_DISTANCE);

    eprintln!("--- timing out update");
    // Perform an update with no time available so it will always time out and not
    // update anything.
    let info = tester.csm.update_blocks_and_some_chunks(
        &tester.camera,
        &NoTextures,
        Instant::now().checked_sub(Duration::from_secs(1)).unwrap(),
        |_| {},
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

    eprintln!("--- normal update");
    // Now while we're at it, try finishing things and check that state too.
    let info = tester.update(|_| {});
    assert_eq!(
        (
            info.flaws,
            tester.csm.did_not_finish_chunks,
            tester.csm.complete_time.is_some(),
        ),
        (Flaws::empty(), false, true)
    );
}
