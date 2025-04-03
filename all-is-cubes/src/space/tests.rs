//! Tests for [`crate::space`].
//!
//! Note that some sub-modules have their own test modules.

use alloc::boxed::Box;
use alloc::string::ToString;
use alloc::vec::Vec;
use core::num::NonZeroU16;

use euclid::Vector3D;
use indoc::indoc;

use crate::block::{self, AIR, Block, BlockDef, BlockDefTransaction, Resolution::*, TickAction};
use crate::content::make_some_blocks;
use crate::fluff::{self, Fluff};
use crate::listen::{Listen as _, Sink};
use crate::math::{Cube, Face6, GridAab, GridCoordinate, GridPoint, Rgba, Vol};
use crate::op::Operation;
use crate::space::{
    CubeTransaction, LightPhysics, PackedLight, SetCubeError, Space, SpaceChange, SpaceFluff,
    SpacePhysics,
};
use crate::time::{self, Tick};
use crate::transaction::{self, Transaction as _};
use crate::universe::{Handle, HandleError, Name, Universe, UniverseTransaction};

// TODO: test consistency between the index and get_* methods
// TODO: test fill() equivalence and error handling

#[test]
fn initial_state_consistency() {
    Space::empty_positive(0, 0, 0).consistency_check();
    Space::empty_positive(1, 0, 0).consistency_check();
    Space::empty_positive(1, 1, 1).consistency_check();
    Space::empty_positive(10, 20, 30).consistency_check();
    Space::empty(GridAab::from_lower_size([1, 2, 3], [10, 20, 30])).consistency_check();
    Space::builder(GridAab::from_lower_size([1, 2, 3], [10, 20, 30]))
        .build()
        .consistency_check();
    Space::builder(GridAab::from_lower_size([1, 2, 3], [10, 20, 30]))
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build()
        .consistency_check();
}

/// `set()` returns Ok when the cube was changed or already equal.
#[test]
fn set_success() {
    let [first, second] = make_some_blocks();
    let mut space = Space::empty_positive(1, 1, 1);
    let pt = GridPoint::origin();
    assert_eq!(Ok(true), space.set(pt, &first));
    assert_eq!(&space[pt], &first);
    assert_eq!(Ok(false), space.set(pt, &first));
    assert_eq!(&space[pt], &first);
    assert_eq!(Ok(true), space.set(pt, &second));
    assert_eq!(&space[pt], &second);

    space.consistency_check(); // bonus testing
}

/// Test `set()` with a block that fails evaluation.
/// This should succeed but leave a placeholder.
///
/// This test case should also cover [`HandleError::InUse`] and other evaluation errors.
#[test]
fn set_success_despite_eval_error_gone() {
    let block = Block::builder()
        .voxels_handle(R1, Handle::<Space>::new_gone("bs".into()))
        .build();
    let mut space = Space::empty_positive(1, 1, 1);

    assert_eq!(Ok(true), space.set([0, 0, 0], &block));

    // Check for the expected placeholder.
    assert_eq!(
        space.get_evaluated([0, 0, 0]),
        &block::EvalBlockError {
            block,
            budget: block::Budget::default().to_cost(),
            used: block::Cost {
                components: 1,
                voxels: 0,
                recursion: 0
            },
            kind: block::ErrorKind::Handle(HandleError::Gone("bs".into()))
        }
        .to_placeholder()
    );

    // TODO: Test that evaluation works the next time around. (It will be tricky to
    // set up a Handle that cooperates with that.)
    // TODO: The placeholder should be subject to special stepping rules that prevent it
    // from being interacted with, but that's not implemented yet.

    space.consistency_check(); // bonus testing
}

#[test]
fn set_failure_out_of_bounds() {
    let [block] = make_some_blocks();
    let cube = Cube::new(1, 0, 0);
    let space_bounds = GridAab::from_lower_size([0, 0, 0], [1, 1, 1]);
    let mut space = Space::empty(space_bounds);

    let error = Err(SetCubeError::OutOfBounds {
        modification: cube.grid_aab(),
        space_bounds,
    });
    assert_eq!(space.set(cube, &block), error);
    assert_eq!(space.set(cube, &AIR), error);

    space.consistency_check(); // bonus testing
}

#[test]
fn set_failure_too_many() {
    const N: u16 = 300_u16;
    let blocks = make_some_blocks::<{ N as usize }>();
    let mut space = Space::empty_positive(N.into(), 1, 1);
    for i in 0..N {
        match space.set([i.into(), 0, 0], &blocks[usize::from(i)]) {
            Ok(true) => {}
            Err(SetCubeError::TooManyBlocks()) => break,
            unexpected => panic!("unexpected result: {unexpected:?}"),
        }
    }
    space.consistency_check(); // bonus testing
}

#[test]
fn set_error_format() {
    assert_eq!(
        SetCubeError::OutOfBounds {
            modification: Cube::new(1, 2, 3).grid_aab(),
            space_bounds: GridAab::from_lower_size([0, 0, 0], [2, 2, 2])
        }
        .to_string(),
        // TODO: simplify the single cube case
        "GridAab(1..2, 2..3, 3..4) is outside of the Space bounds GridAab(0..2, 0..2, 0..2)"
    );
    assert_eq!(
        SetCubeError::TooManyBlocks().to_string(),
        "more than 65536 unique blocks in a Space is not yet supported"
    );
}

/// `EvaluatedBlock` data is updated when a new block index is allocated.
#[test]
fn set_updates_evaluated_on_added_block() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(2, 1, 1);
    space.set([0, 0, 0], &block).unwrap();
    // Confirm the expected indices
    assert_eq!(Some(1), space.get_block_index([0, 0, 0]));
    assert_eq!(Some(0), space.get_block_index([1, 0, 0]));
    // Confirm the data is correct
    assert_eq!(space.get_evaluated([0, 0, 0]), &block.evaluate().unwrap());
    space.consistency_check(); // bonus testing
}

/// `EvaluatedBlock` data is updated when a block index is reused because the block was unique.
#[test]
fn set_updates_evaluated_and_notifies_on_replaced_block() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(1, 1, 1);
    let sink = Sink::new();
    space.listen(sink.listener());

    let cube = Cube::ORIGIN;
    space.set(cube, &block).unwrap();

    // Confirm the expected indices
    assert_eq!(Some(0), space.get_block_index(cube));
    // Confirm the data is correct
    assert_eq!(space.get_evaluated(cube), &block.evaluate().unwrap());
    // Confirm expected notifications
    assert_eq!(
        sink.drain(),
        vec![
            SpaceChange::BlockIndex(0),
            SpaceChange::CubeLight { cube },
            SpaceChange::CubeBlock {
                cube,
                old_block_index: 0,
                new_block_index: 0,
            }
        ]
    );

    space.consistency_check(); // bonus testing
}

/// No arithmetic overflow when modifying a block at the numeric range upper bound.
#[test]
fn set_no_neighbor_overflow_high() {
    let [block] = make_some_blocks();
    let one_less = GridCoordinate::MAX - 1;
    let high_corner = GridPoint::new(one_less, one_less, one_less);
    let mut space = Space::empty(GridAab::from_lower_size(high_corner, [1, 1, 1]));
    space.set(high_corner, block.clone()).unwrap();
}
/// No arithmetic overflow when modifying a block at the numeric range lower bound.
#[test]
fn set_no_neighbor_overflow_low() {
    let [block] = make_some_blocks();
    let low_corner = GridPoint::new(
        GridCoordinate::MIN,
        GridCoordinate::MIN,
        GridCoordinate::MIN,
    );
    let mut space = Space::empty(GridAab::from_lower_size(low_corner, [1, 1, 1]));
    space.set(low_corner, block.clone()).unwrap();
}

#[test]
fn removed_blocks_are_forgotten() {
    let [block_0, block_1, block_2] = make_some_blocks();
    let mut space = Space::empty_positive(2, 1, 1);
    let pt1 = GridPoint::new(0, 0, 0);
    let pt2 = GridPoint::new(1, 0, 0);
    // TODO: This test depends on block allocation order. distinct_blocks() ought to be stable or explicitly return a HashSet or something.
    assert_eq!(space.distinct_blocks(), vec![AIR.clone()], "step 1");
    space.set(pt1, &block_0).unwrap();
    space.consistency_check();
    assert_eq!(
        space.distinct_blocks(),
        vec![AIR.clone(), block_0.clone()],
        "step 2"
    );
    space.set(pt2, &block_1).unwrap();
    space.consistency_check();
    assert_eq!(
        space.distinct_blocks(),
        vec![block_1.clone(), block_0.clone()],
        "step 3"
    );
    space.set(pt1, &block_2).unwrap();
    space.consistency_check();
    assert_eq!(
        space.distinct_blocks(),
        vec![block_1.clone(), block_2.clone()],
        "step 4"
    );

    // Make sure that reinserting an old block correctly allocates an index rather than using the old one.
    space.set(pt2, &block_0).unwrap();
    space.consistency_check();
    assert_eq!(
        space.distinct_blocks(),
        vec![block_0.clone(), block_2.clone()],
        "step 4"
    );
}

#[test]
fn change_listener_simple() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(2, 1, 1);
    let sink = Sink::new();
    space.listen(sink.listener());

    assert_eq!(Ok(true), space.set([0, 0, 0], &block));
    assert_eq!(
        sink.drain(),
        vec![
            SpaceChange::BlockIndex(1),
            SpaceChange::CubeLight {
                cube: Cube::new(0, 0, 0)
            },
            SpaceChange::CubeBlock {
                cube: Cube::new(0, 0, 0),
                old_block_index: 0,
                new_block_index: 1
            },
        ],
    );

    // No change, no notification
    assert_eq!(Ok(false), space.set([0, 0, 0], &block));
    assert_eq!(sink.drain(), vec![]);
}

#[test]
fn fluff_listener() {
    let mut space = Space::empty_positive(1, 1, 1);
    let txn = CubeTransaction::fluff(Fluff::Happened).at(Cube::ORIGIN);
    let sink = Sink::new();
    space.fluff().listen(sink.listener());

    txn.execute(&mut space, &mut transaction::no_outputs)
        .unwrap();

    assert_eq!(
        sink.drain(),
        vec![SpaceFluff {
            position: Cube::ORIGIN,
            fluff: Fluff::Happened,
        }]
    );
}

#[test]
fn extract() {
    let [block_0, block_1] = make_some_blocks();
    let mut space = Space::empty_positive(2, 1, 1);
    space.set([0, 0, 0], &block_0).unwrap();
    space.set([1, 0, 0], &block_1).unwrap();

    let extract_bounds = GridAab::from_lower_size([1, 0, 0], [1, 1, 1]);
    let extracted: Vol<Box<[Block]>> = space.extract(extract_bounds, |e| {
        // TODO: arrange to sanity check index and lighting
        let block = e.block_data().block().clone();
        assert_eq!(block.evaluate().unwrap(), e.block_data().evaluated);
        block
    });

    assert_eq!(extracted.bounds(), extract_bounds);
    assert_eq!(&extracted[[1, 0, 0]], &block_1);
}

#[test]
#[should_panic = "assertion failed: self.bounds().contains_box(bounds)"]
fn extract_out_of_bounds() {
    let space = Space::empty_positive(2, 1, 1);
    let extract_bounds = GridAab::from_lower_size([1, 0, 0], [1, 2, 1]);
    let _: Vol<Box<[()]>> = space.extract(extract_bounds, |_| ());
}

#[test]
fn fill_out_of_bounds() {
    let mut space = Space::empty_positive(2, 1, 1);
    let fill_bounds = GridAab::from_lower_size([1, 0, 0], [1, 2, 1]);
    let result = space.fill(fill_bounds, |_| None::<Block>);
    assert_eq!(
        result,
        Err(SetCubeError::OutOfBounds {
            modification: fill_bounds,
            space_bounds: GridAab::from_lower_size([0, 0, 0], [2, 1, 1])
        })
    );
}

/// Test filling an entire space with one block using [`Space::fill`].
#[test]
fn fill_entire_space() {
    let block = block::from_color!(0., 0., 0., 0.5); // transparent so light gets involved
    let bounds = GridAab::from_lower_size([0, 3, 0], [25 * 16, 16, 2]);
    let mut space = Space::empty(bounds);

    space.fill(bounds, |_| Some(&block)).unwrap();

    space.consistency_check();
    for cube in bounds.interior_iter() {
        assert_eq!(&space[cube], &block);
        assert!(
            space.in_light_update_queue(cube),
            "{cube:?} in light update queue"
        );
    }
}

/// Test filling an entire space with one block using [`Space::fill_uniform`].
#[test]
fn fill_uniform_entire_space() {
    let [block] = make_some_blocks();
    let bounds = GridAab::from_lower_size([0, 3, 0], [25 * 16, 16, 2]);
    let mut space = Space::empty(bounds);
    let sink = Sink::new();
    space.listen(sink.listener());

    space.fill_uniform(bounds, &block).unwrap();

    assert_eq!(sink.drain(), vec![SpaceChange::EveryBlock]);

    space.consistency_check();
    for cube in bounds.interior_iter() {
        assert_eq!(&space[cube], &block);
        assert!(
            space.in_light_update_queue(cube),
            "{cube:?} in light update queue"
        );
    }
}

/// There was a bug triggered when the last instance of a block was replaced with
/// a block already in the space. This specifically runs a consistency check in that
/// case.
#[test]
fn replace_last_block_regression() {
    let [block] = make_some_blocks();
    let bounds = GridAab::from_lower_size([0, 0, 0], [3, 1, 1]);
    let mut space = Space::empty(bounds);
    for i in 0..3 {
        space.set([i, 0, 0], &block).unwrap();
        space.consistency_check();
    }
}

#[test]
fn listens_to_block_changes() {
    // Set up indirect block
    let mut universe = Universe::new();
    let block_def_handle =
        universe.insert_anonymous(BlockDef::new(block::from_color!(Rgba::WHITE)));
    let indirect = Block::from(block_def_handle.clone());

    // Set up space and listener
    let mut space = Space::builder(GridAab::ORIGIN_CUBE)
        .filled_with(indirect)
        .build();
    let sink = Sink::new();
    space.listen(sink.listener());
    assert_eq!(sink.drain(), vec![]);

    // Now mutate the block def .
    let new_block = block::from_color!(Rgba::BLACK);
    block_def_handle
        .execute(&BlockDefTransaction::overwrite(new_block))
        .unwrap();
    let new_evaluated = Block::from(block_def_handle).evaluate().unwrap();

    // This does not result in an outgoing notification, because we don't want
    // computations like reevaluation to happen during the notification process.
    assert_eq!(sink.drain(), vec![]);
    // Instead, it only happens the next time the space is stepped.
    let (_, _) = space.step(None, Tick::arbitrary(), time::DeadlineNt::Whenever);
    // Now we should see a notification and the evaluated block data having changed.
    assert_eq!(sink.drain(), vec![SpaceChange::BlockEvaluation(0)]);
    assert_eq!(space.get_evaluated([0, 0, 0]), &new_evaluated);
}

#[test]
fn indirect_becomes_evaluation_error() {
    let block_name = Name::from("block");

    // Set up 2 levels of indirect block
    // (because right now, a Handle going away is silent...)
    let mut universe = Universe::new();
    let block_def_ref = universe
        .insert(
            block_name.clone(),
            BlockDef::new(block::from_color!(Rgba::WHITE)),
        )
        .unwrap();
    let block = Block::from(block_def_ref.clone());

    // Set up space and listener
    let mut space = Space::empty_positive(1, 1, 1);
    space.set([0, 0, 0], &block).unwrap();
    let sink = Sink::new();
    space.listen(sink.listener());

    // Make the block def refer to itself, guaranteeing an evaluation error
    block_def_ref
        .execute(&BlockDefTransaction::overwrite(block.clone()))
        .unwrap();

    // Step the space to let it notice.
    let (_, _) = space.step(None, Tick::arbitrary(), time::DeadlineNt::Whenever);

    // Now we should see a notification and the evaluated block data having changed.
    assert_eq!(sink.drain(), vec![SpaceChange::BlockEvaluation(0)]);
    assert_eq!(
        space.get_evaluated([0, 0, 0]),
        &block.evaluate().unwrap_err().to_placeholder()
    );
}

#[test]
fn space_debug() {
    let mut space = Space::empty_positive(1, 1, 1);
    space.set_physics(SpacePhysics {
        light: LightPhysics::None,
        ..SpacePhysics::default()
    });
    println!("{space:#?}");
    pretty_assertions::assert_str_eq!(
        format!("{space:#?}\n"),
        indoc! {"
            Space {
                bounds: GridAab(
                    0..1 (1),
                    0..1 (1),
                    0..1 (1),
                ),
                palette: Palette([
                    SpaceBlockData {
                        count: 1,
                        block: Block {
                            primitive: Air,
                        },
                        ..
                    },
                ]),
                physics: SpacePhysics {
                    gravity: (+0.000, -20.000, +0.000),
                    sky: Uniform(
                        Rgb(0.89626944, 0.89626944, 1.0),
                    ),
                    light: None,
                },
                behaviors: BehaviorSet({}),
                cubes_wanting_ticks: {},
                ..
            }
        "}
    );
}

#[test]
fn set_physics_light_none() {
    let mut space = Space::empty_positive(1, 1, 1);
    space
        .set([0, 0, 0], block::from_color!(1.0, 1.0, 1.0, 0.5))
        .unwrap();
    assert_eq!(space.light.light_update_queue.len(), 1);
    // Check that a no-op update doesn't clear
    space.set_physics(SpacePhysics::default());
    assert_eq!(space.light.light_update_queue.len(), 1);

    space.set_physics(SpacePhysics {
        light: LightPhysics::None,
        ..SpacePhysics::default()
    });

    // No light data and no queue
    assert_eq!(space.light.light_update_queue.len(), 0);
    assert_eq!(space.light.contents.volume(), 0);
    // TODO: test what change notifications are sent
}

#[test]
fn set_physics_light_rays() {
    let mut space = Space::empty_positive(2, 1, 1);
    space
        .set([0, 0, 0], block::from_color!(1.0, 1.0, 1.0, 0.5))
        .unwrap();
    space
        .set([1, 0, 0], block::from_color!(1.0, 1.0, 1.0, 1.0))
        .unwrap();
    space.set_physics(SpacePhysics {
        light: LightPhysics::None,
        ..SpacePhysics::default()
    });
    assert_eq!(space.light.light_update_queue.len(), 0);

    // This is the set_physics we're actually testing
    space.set_physics(SpacePhysics {
        light: LightPhysics::Rays {
            maximum_distance: 10,
        },
        ..SpacePhysics::default()
    });

    assert_eq!(space.light.contents.volume(), 2);
    assert_eq!(
        space.get_lighting([0, 0, 0]),
        space.light.block_sky.in_direction(Face6::NX)
    );
    assert_eq!(space.get_lighting([1, 0, 0]), PackedLight::OPAQUE);
    assert_eq!(space.light.light_update_queue.len(), 1);
    // TODO: test what change notifications are sent
}

#[test]
fn set_physics_notification() {
    let mut space = Space::empty_positive(1, 1, 1);
    let sink = Sink::new();
    space.listen(sink.listener());

    space.set_physics(space.physics.clone());
    assert_eq!(sink.drain(), vec![]);

    space.set_physics(SpacePhysics {
        gravity: Vector3D::zero(),
        ..SpacePhysics::default()
    });
    assert_eq!(sink.drain(), vec![SpaceChange::Physics]);
}

#[test]
fn block_tick_action_does_not_run_paused() {
    let vanisher = Block::builder()
        .color(Rgba::WHITE)
        .tick_action(TickAction {
            operation: Operation::Become(AIR),
            schedule: time::Schedule::EVERY_TICK,
        })
        .build();
    let mut space = Space::empty_positive(1, 1, 1);
    space.set([0, 0, 0], &vanisher).unwrap();
    let mut clock = time::Clock::new(time::TickSchedule::per_second(10), 0);

    // No effect when paused
    _ = space.step(None, clock.advance(true), time::DeadlineNt::Whenever);
    assert_eq!(space[[0, 0, 0]], vanisher);

    // Operation applied when unpaused
    _ = space.step(None, clock.advance(false), time::DeadlineNt::Whenever);
    assert_eq!(space[[0, 0, 0]], AIR);
}

#[test]
fn block_tick_action_timing() {
    let [mut block1, mut block2, block3] = make_some_blocks();

    // Hook them up to turn into each other
    fn connect(from: &mut Block, to: &Block) {
        from.freezing_get_attributes_mut().tick_action = Some(TickAction {
            operation: Operation::Become(to.clone()),
            schedule: time::Schedule::from_period(NonZeroU16::new(2).unwrap()),
        });
    }
    connect(&mut block2, &block3);
    connect(&mut block1, &block2);

    let mut space = Space::empty_positive(1, 1, 1);
    space.set([0, 0, 0], &block1).unwrap();

    // Setup done, now simulate.
    let mut clock = time::Clock::new(time::TickSchedule::per_second(10), 0);
    let mut blocks_found = Vec::new();
    for _ in 0..6 {
        // Record the current state in a readable fashion
        let found = &space[[0, 0, 0]];
        blocks_found.push(if found == &block1 {
            1
        } else if found == &block2 {
            2
        } else if found == &block3 {
            3
        } else {
            99
        });

        let (_info, step_txn) = space.step(None, clock.advance(false), time::DeadlineNt::Whenever);
        // TODO: the block effect isn't a returned transaction yet but it perhaps should be.
        // This test will need reworking at that point.
        assert_eq!(step_txn, UniverseTransaction::default());
    }

    // Check sequence of changes
    assert_eq!(blocks_found, [1, 2, 2, 3, 3, 3]);
}

/// TODO: Need more tests along with implementation; the conflict processing *only* handles this
/// simple case. In particular, test other actions proceeding while a conflict exists.
#[test]
fn block_tick_action_conflict() {
    use pretty_assertions::assert_eq;

    // Create an active block.
    let [
        mut modifies_px_neighbor,
        output1,
        mut modifies_nx_neighbor,
        output2,
    ] = make_some_blocks();
    fn connect(from: &mut Block, to: &Block, face: Face6) {
        from.freezing_get_attributes_mut().tick_action = Some(TickAction {
            // TODO: replace this with a better-behaved neighbor-modifying operation,
            // once we have one
            operation: Operation::Neighbors(
                [(
                    Cube::from(face.normal_vector().to_point()),
                    Operation::Become(to.clone()),
                )]
                .into(),
            ),
            schedule: time::Schedule::from_period(NonZeroU16::new(1).unwrap()),
        });
    }
    connect(&mut modifies_px_neighbor, &output1, Face6::PX);
    connect(&mut modifies_nx_neighbor, &output2, Face6::NX);

    // Create test setup.
    let fluff_sink = Sink::new();
    let mut clock = time::Clock::new(time::TickSchedule::per_second(10), 0);
    let mut space = Space::empty_positive(3, 1, 1);
    space.fluff().listen(fluff_sink.listener());
    // These two blocks will both try to mutate [1, 0, 0], with different results.
    let left = Cube::new(0, 0, 0);
    let middle = Cube::new(1, 0, 0);
    let right = Cube::new(2, 0, 0);
    space.set(left, &modifies_px_neighbor).unwrap();
    space.set(right, &modifies_nx_neighbor).unwrap();

    {
        let (_info, step_txn) = space.step(None, clock.advance(false), time::DeadlineNt::Whenever);
        assert_eq!(step_txn, UniverseTransaction::default());

        assert_eq!(
            [&space[left], &space[middle], &space[right]],
            [&modifies_px_neighbor, &AIR, &modifies_nx_neighbor],
            "expecting no change"
        );
        assert_eq!(
            fluff_sink.drain(),
            vec![
                SpaceFluff {
                    position: left,
                    fluff: Fluff::BlockFault(fluff::BlockFault::TickConflict(middle.grid_aab())),
                },
                SpaceFluff {
                    position: right,
                    fluff: Fluff::BlockFault(fluff::BlockFault::TickConflict(middle.grid_aab())),
                }
            ]
        );
    }

    // Now if we delete one of the conflicting blocks, the tick action of the remaining one
    // should take effect.
    space.set(right, &AIR).unwrap();

    {
        let (_info, step_txn) = space.step(None, clock.advance(false), time::DeadlineNt::Whenever);
        assert_eq!(step_txn, UniverseTransaction::default());
        assert_eq!(fluff_sink.drain(), vec![]);
        assert_eq!(
            [&space[left], &space[middle], &space[right]],
            [&modifies_px_neighbor, &output1, &AIR],
            "expecting change"
        );
    }
}

/// Test that a block tick action spontaneously repeats even if it doesn't replace the block
/// itself.
#[test]
fn block_tick_action_repeats() {
    let ticker = Block::builder()
        .color(Rgba::WHITE)
        .tick_action(TickAction {
            // This is an operation that *doesn't change the block that has it*.
            // AddModifiers is convenient because we can count how many times it was applied.
            operation: Operation::Neighbors(
                [(
                    Cube::new(0, 1, 0),
                    Operation::AddModifiers([block::Modifier::from(block::Quote::new())].into()),
                )]
                .into(),
            ),
            schedule: time::Schedule::EVERY_TICK,
        })
        .build();
    let mut space = Space::empty_positive(1, 2, 1);
    space.set([0, 0, 0], &ticker).unwrap();
    let mut clock = time::Clock::new(time::TickSchedule::per_second(10), 0);

    for t in 0..2 {
        _ = space.step(None, clock.advance(false), time::DeadlineNt::Whenever);
        assert_eq!(space[[0, 1, 0]].modifiers().len(), t + 1);
    }
}
