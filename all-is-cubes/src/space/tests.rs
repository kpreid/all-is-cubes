//! Tests for [`crate::space`].
//!
//! Note that some sub-modules have their own test modules.

use cgmath::EuclideanSpace as _;
use indoc::indoc;

use crate::block::{
    Block, BlockDef, BlockDefTransaction, EvalBlockError, Primitive, Resolution::*, AIR,
};
use crate::content::make_some_blocks;
use crate::drawing::VoxelBrush;
use crate::listen::{Listen as _, Sink};
use crate::math::{GridCoordinate, GridPoint, Rgba};
use crate::space::{
    GridAab, LightPhysics, PackedLight, SetCubeError, Space, SpaceChange, SpacePhysics,
};
use crate::time::{practically_infinite_deadline, Tick};
use crate::transaction;
use crate::universe::{Name, RefError, Universe, UniverseTransaction};

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

/// set() returns Ok when the cube was changed or already equal.
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

#[test]
fn set_failure_out_of_bounds() {
    let [block] = make_some_blocks();
    let pt = GridPoint::new(1, 0, 0);
    let ptg = GridAab::single_cube(pt);
    let space_bounds = GridAab::from_lower_size([0, 0, 0], [1, 1, 1]);
    let mut space = Space::empty(space_bounds);

    let error = Err(SetCubeError::OutOfBounds {
        modification: ptg,
        space_bounds,
    });
    assert_eq!(space.set(pt, &block), error);
    assert_eq!(space.set(pt, &AIR), error);

    space.consistency_check(); // bonus testing
}

/// This test case should also cover `RefError::Gone`.
#[test]
fn set_failure_borrow() {
    let mut u = Universe::new();
    let inner_space_ref = u
        .insert("bs".into(), Space::empty_positive(1, 1, 1))
        .unwrap();
    let block = Block::builder()
        .voxels_ref(R1, inner_space_ref.clone())
        .build();
    let mut outer_space = Space::empty_positive(1, 1, 1);

    inner_space_ref
        .try_modify(|_| {
            // Try to use `block` while we are allegedly mutating `inner_space`.
            assert_eq!(
                outer_space.set((0, 0, 0), &block),
                Err(SetCubeError::EvalBlock(RefError::InUse("bs".into()).into()))
            );
        })
        .unwrap();

    outer_space.consistency_check(); // bonus testing
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
            modification: GridAab::single_cube(GridPoint::new(1, 2, 3)),
            space_bounds: GridAab::from_lower_size([0, 0, 0], [2, 2, 2])
        }
        .to_string(),
        // TODO: simplify the single cube case
        "GridAab(1..2, 2..3, 3..4) is outside of the bounds GridAab(0..2, 0..2, 0..2)"
    );
    assert_eq!(
        SetCubeError::EvalBlock(EvalBlockError::DataRefIs(RefError::Gone("foo".into())))
            .to_string(),
        // TODO: This message is a bit "revealing our exact data structure"...
        "block evaluation failed: block data inaccessible: object was deleted: 'foo'"
    );
    assert_eq!(
        SetCubeError::TooManyBlocks().to_string(),
        "more than 65536 block types is not yet supported"
    );
}

/// `EvaluatedBlock` data is updated when a new block index is allocated.
#[test]
fn set_updates_evaluated_on_added_block() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(2, 1, 1);
    space.set((0, 0, 0), &block).unwrap();
    // Confirm the expected indices
    assert_eq!(Some(1), space.get_block_index((0, 0, 0)));
    assert_eq!(Some(0), space.get_block_index((1, 0, 0)));
    // Confirm the data is correct
    assert_eq!(space.get_evaluated((0, 0, 0)), &block.evaluate().unwrap());
    space.consistency_check(); // bonus testing
}

/// `EvaluatedBlock` data is updated when a block index is reused.
#[test]
fn set_updates_evaluated_on_replaced_block() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(1, 1, 1);
    space.set((0, 0, 0), &block).unwrap();
    // Confirm the expected indices
    assert_eq!(Some(0), space.get_block_index((0, 0, 0)));
    // Confirm the data is correct
    assert_eq!(space.get_evaluated((0, 0, 0)), &block.evaluate().unwrap());
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
fn change_listener() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(2, 1, 1);
    let sink = Sink::new();
    space.listen(sink.listener());

    assert_eq!(Ok(true), space.set((0, 0, 0), &block));
    assert_eq!(
        sink.drain(),
        vec![
            SpaceChange::Number(1),
            SpaceChange::Lighting(GridPoint::new(0, 0, 0)),
            SpaceChange::Block(GridPoint::new(0, 0, 0)),
        ],
    );

    // No change, no notification
    assert_eq!(Ok(false), space.set((0, 0, 0), &block));
    assert_eq!(sink.drain(), vec![]);
}

#[test]
fn extract_out_of_bounds() {
    let [block_0, block_1] = make_some_blocks();
    let mut space = Space::empty_positive(2, 1, 1);
    space.set((0, 0, 0), &block_0).unwrap();
    space.set((1, 0, 0), &block_1).unwrap();

    let extract_bounds = GridAab::from_lower_size([1, 0, 0], [1, 2, 1]);
    let extracted = space.extract(extract_bounds, |_index, block_data, _lighting| {
        // TODO: arrange to sanity check index and lighting
        let block = block_data.block().clone();
        assert_eq!(block.evaluate().unwrap(), block_data.evaluated);
        block
    });

    assert_eq!(extracted.bounds(), extract_bounds);
    assert_eq!(&extracted[(1, 0, 0)], &block_1);
    assert_eq!(&extracted[(1, 1, 0)], &AIR);
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
    let [block] = make_some_blocks();
    let bounds = GridAab::from_lower_size((0, 3, 0), (25 * 16, 16, 2));
    let mut space = Space::empty(bounds);
    space.fill(bounds, |_| Some(&block)).unwrap();
    space.consistency_check();
    for cube in bounds.interior_iter() {
        assert_eq!(&space[cube], &block);
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
    let block_def_ref = universe.insert_anonymous(BlockDef::new(Block::from(Rgba::WHITE)));
    let indirect = Block::from_primitive(Primitive::Indirect(block_def_ref.clone()));

    // Set up space and listener
    let mut space = Space::empty_positive(1, 1, 1);
    space.set((0, 0, 0), indirect).unwrap();
    let sink = Sink::new();
    space.listen(sink.listener());
    assert_eq!(sink.drain(), vec![]);

    // Now mutate the block def .
    let new_block = Block::from(Rgba::BLACK);
    let new_evaluated = new_block.evaluate().unwrap();
    block_def_ref
        .execute(
            &BlockDefTransaction::overwrite(new_block),
            &mut transaction::no_outputs,
        )
        .unwrap();
    // This does not result in an outgoing notification, because we don't want
    // computations like reevaluation to happen during the notification process.
    assert_eq!(sink.drain(), vec![]);
    // Instead, it only happens the next time the space is stepped.
    let (_, _) = space.step(None, Tick::arbitrary(), practically_infinite_deadline());
    // Now we should see a notification and the evaluated block data having changed.
    assert_eq!(sink.drain(), vec![SpaceChange::BlockValue(0)]);
    assert_eq!(space.get_evaluated((0, 0, 0)), &new_evaluated);
}

#[test]
fn indirect_becomes_evaluation_error() {
    let block_name = Name::from("block");

    // Set up 2 levels of indirect block
    // (because right now, a URef going away is silent...)
    let mut universe = Universe::new();
    let block_def_ref = universe
        .insert(block_name.clone(), BlockDef::new(Block::from(Rgba::WHITE)))
        .unwrap();
    let block = Block::from_primitive(Primitive::Indirect(block_def_ref.clone()));

    // Set up space and listener
    let mut space = Space::empty_positive(1, 1, 1);
    space.set((0, 0, 0), &block).unwrap();
    let sink = Sink::new();
    space.listen(sink.listener());

    // Make the block def refer to itself, guaranteeing an evaluation error
    block_def_ref
        .execute(&BlockDefTransaction::overwrite(block.clone()), &mut drop)
        .unwrap();

    // Step the space to let it notice.
    let (_, _) = space.step(None, Tick::arbitrary(), practically_infinite_deadline());

    // Now we should see a notification and the evaluated block data having changed.
    assert_eq!(sink.drain(), vec![SpaceChange::BlockValue(0)]);
    assert_eq!(
        space.get_evaluated((0, 0, 0)),
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
                block_data: [
                    SpaceBlockData {
                        count: 1,
                        block: Block {
                            primitive: Air,
                        },
                        ..
                    },
                ],
                physics: SpacePhysics {
                    gravity: (+0.000, -20.000, +0.000),
                    sky_color: Rgb(0.8962694, 0.8962694, 1.0),
                    light: None,
                },
                behaviors: BehaviorSet([]),
                cubes_wanting_ticks: {},
                ..
            }
        "}
    );
}

#[test]
fn set_physics_light_none() {
    let mut space = Space::empty_positive(1, 1, 1);
    space.set([0, 0, 0], Rgba::new(1.0, 1.0, 1.0, 0.5)).unwrap();
    assert_eq!(space.light_update_queue.len(), 1);
    // Check that a no-op update doesn't clear
    space.set_physics(SpacePhysics::default());
    assert_eq!(space.light_update_queue.len(), 1);

    space.set_physics(SpacePhysics {
        light: LightPhysics::None,
        ..SpacePhysics::default()
    });

    // No light data and no queue
    assert_eq!(space.light_update_queue.len(), 0);
    assert_eq!(space.lighting.len(), 0);
    // TODO: test what change notifications are sent
}

#[test]
fn set_physics_light_rays() {
    let mut space = Space::empty_positive(2, 1, 1);
    space.set([0, 0, 0], Rgba::new(1.0, 1.0, 1.0, 0.5)).unwrap();
    space.set([1, 0, 0], Rgba::new(1.0, 1.0, 1.0, 1.0)).unwrap();
    space.set_physics(SpacePhysics {
        light: LightPhysics::None,
        ..SpacePhysics::default()
    });
    assert_eq!(space.light_update_queue.len(), 0);

    // This is the set_physics we're actually testing
    space.set_physics(SpacePhysics {
        light: LightPhysics::Rays {
            maximum_distance: 10,
        },
        ..SpacePhysics::default()
    });

    assert_eq!(space.lighting.len(), 2);
    assert_eq!(space.get_lighting([0, 0, 0]), space.packed_sky_color);
    assert_eq!(space.get_lighting([1, 0, 0]), PackedLight::OPAQUE);
    assert_eq!(space.light_update_queue.len(), 1);
    // TODO: test what change notifications are sent
}

#[test]
fn block_tick_action() {
    let [mut block1, block2] = make_some_blocks();
    if let Primitive::Atom(attributes, _) = block1.primitive_mut() {
        attributes.tick_action = Some(VoxelBrush::single(block2.clone()));
    } else {
        panic!();
    }

    let mut space = Space::empty_positive(1, 1, 1);
    space.set([0, 0, 0], block1).unwrap();

    // TODO: the block effect isn't a transaction yet but it should be
    let (_info, step_txn) = space.step(None, Tick::arbitrary(), practically_infinite_deadline());
    assert_eq!(step_txn, UniverseTransaction::default());

    assert_eq!(&space[[0, 0, 0]], &block2);
}
