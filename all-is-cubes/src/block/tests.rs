// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

#![allow(clippy::bool_assert_comparison)]

use cgmath::EuclideanSpace as _;
use std::borrow::Cow;

use crate::block::{
    builder, Block, BlockAttributes, BlockBuilder, BlockCollision, BlockDef, EvalBlockError,
    Evoxel, Resolution, AIR,
};
use crate::content::make_some_blocks;
use crate::listen::{NullListener, Sink};
use crate::math::{GridPoint, GridVector, Rgb, Rgba};
use crate::space::{Grid, GridArray, Space};
use crate::universe::Universe;

#[test]
fn evaluate_opaque_atom_and_attributes() {
    let color = Rgba::new(1.0, 2.0, 3.0, 1.0);
    let attributes = BlockAttributes {
        display_name: Cow::Borrowed(&"hello world"),
        selectable: false,
        collision: BlockCollision::None,
        light_emission: Rgb::ONE,
        ..BlockAttributes::default()
    };
    let block = Block::Atom(attributes.clone(), color);
    let e = block.evaluate().unwrap();
    assert_eq!(e.attributes, attributes);
    assert_eq!(e.color, block.color());
    assert!(e.voxels.is_none());
    assert_eq!(e.resolution, 1);
    assert_eq!(e.opaque, true);
    assert_eq!(e.visible, true);
}

#[test]
fn evaluate_transparent_atom() {
    let color = Rgba::new(1.0, 2.0, 3.0, 0.5);
    let block = Block::Atom(BlockAttributes::default(), color);
    let e = block.evaluate().unwrap();
    assert_eq!(e.color, block.color());
    assert!(e.voxels.is_none());
    assert_eq!(e.opaque, false);
    assert_eq!(e.visible, true);
}

#[test]
fn evaluate_invisible_atom() {
    let block = Block::Atom(BlockAttributes::default(), Rgba::TRANSPARENT);
    let e = block.evaluate().unwrap();
    assert_eq!(e.color, Rgba::TRANSPARENT);
    assert!(e.voxels.is_none());
    assert_eq!(e.opaque, false);
    assert_eq!(e.visible, false);
}

#[test]
fn evaluate_voxels_checked_individually() {
    let resolution = 2;
    let mut universe = Universe::new();

    let attributes = BlockAttributes {
        display_name: Cow::Borrowed(&"hello world"),
        ..BlockAttributes::default()
    };
    let block = Block::builder()
        .attributes(attributes.clone())
        .voxels_fn(&mut universe, resolution, |point| {
            let point = point.cast::<f32>().unwrap();
            Block::from(Rgba::new(point.x, point.y, point.z, 1.0))
        })
        .unwrap()
        .build();

    let e = block.evaluate().unwrap();
    assert_eq!(e.attributes, attributes);
    assert_eq!(
        e.voxels,
        Some(GridArray::from_fn(Grid::for_block(resolution), |point| {
            let point = point.cast::<f32>().unwrap();
            Evoxel {
                color: Rgba::new(point.x, point.y, point.z, 1.0),
                selectable: true,
                collision: BlockCollision::Hard,
            }
        }))
    );
    assert_eq!(e.color, Rgba::new(0.5, 0.5, 0.5, 1.0));
    assert_eq!(e.resolution, resolution);
    assert_eq!(e.opaque, true);
    assert_eq!(e.visible, true);
}

#[test]
fn evaluate_transparent_voxels() {
    let mut universe = Universe::new();
    let resolution = 4;
    let block = Block::builder()
        .voxels_fn(&mut universe, resolution, |point| {
            Block::from(Rgba::new(
                0.0,
                0.0,
                0.0,
                if point == GridPoint::new(0, 0, 0) {
                    0.5
                } else {
                    1.0
                },
            ))
        })
        .unwrap()
        .build();

    let e = block.evaluate().unwrap();
    assert_eq!(
        e.color,
        Rgba::new(0.0, 0.0, 0.0, 1.0 - (0.5 / f32::from(resolution.pow(3))))
    );
    assert_eq!(e.opaque, false);
    assert_eq!(e.visible, true);
}

#[test]
fn evaluate_voxels_not_filling_block() {
    let resolution = 4;
    let mut universe = Universe::new();
    let block = Block::builder()
        .voxels_fn(&mut universe, resolution, |point| {
            Block::from(Rgba::new(
                0.0,
                0.0,
                0.0,
                if point == GridPoint::new(1, 1, 1) {
                    1.0
                } else {
                    0.0
                },
            ))
        })
        .unwrap()
        .build();

    let e = block.evaluate().unwrap();
    assert_eq!(
        e.color,
        Rgba::new(0.0, 0.0, 0.0, 1.0 / f32::from(resolution.pow(3)))
    );
    assert_eq!(e.resolution, 4);
    assert_eq!(e.opaque, false);
    assert_eq!(e.visible, true);
}

/// Test the situation where the space is smaller than the block: in particular,
/// even if the space is all opaque, the block should not be counted as opaque.
#[test]
fn evaluate_voxels_partial_not_filling() {
    let resolution = 4;
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(2, 4, 4);
    space
        .fill_uniform(space.grid(), Block::from(Rgba::WHITE))
        .unwrap();
    let space_ref = universe.insert_anonymous(space);
    let block = Block::builder()
        .voxels_ref(resolution as Resolution, space_ref.clone())
        .build();

    let e = block.evaluate().unwrap();
    assert_eq!(e.color, Rgba::new(1.0, 1.0, 1.0, 0.5));
    assert_eq!(e.resolution, 4);
    assert_eq!(e.opaque, false);
    assert_eq!(e.visible, true);
}

/// Tests that the `offset` field of `Block::Recur` is respected.
#[test]
fn recur_with_offset() {
    let resolution = 4;
    let offset = GridVector::new(resolution, 0, 0);
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(resolution * 2, resolution, resolution);
    space
        .fill(space.grid(), |point| {
            let point = point.cast::<f32>().unwrap();
            Some(Block::Atom(
                BlockAttributes::default(),
                Rgba::new(point.x, point.y, point.z, 1.0),
            ))
        })
        .unwrap();
    let space_ref = universe.insert_anonymous(space);
    let block_at_offset = Block::Recur {
        attributes: BlockAttributes::default(),
        offset: GridPoint::from_vec(offset),
        resolution: resolution as Resolution,
        space: space_ref.clone(),
    };

    let e = block_at_offset.evaluate().unwrap();
    assert_eq!(
        e.voxels,
        Some(GridArray::from_fn(
            Grid::for_block(resolution as Resolution),
            |point| {
                let point = (point + offset).cast::<f32>().unwrap();
                Evoxel {
                    color: Rgba::new(point.x, point.y, point.z, 1.0),
                    selectable: true,
                    collision: BlockCollision::Hard,
                }
            }
        ))
    );
}

#[test]
fn indirect_equivalence() {
    let resolution = 4;
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(resolution, resolution, resolution);
    // TODO: BlockGen should support constructing indirects (by default, even)
    // and we can use the more concise version
    space
        .fill(space.grid(), |point| {
            let point = point.cast::<f32>().unwrap();
            Some(Block::from(Rgba::new(point.x, point.y, point.z, 1.0)))
        })
        .unwrap();
    let space_ref = universe.insert_anonymous(space);
    let block = Block::builder()
        .voxels_ref(resolution as Resolution, space_ref.clone())
        .build();
    let eval_bare = block.evaluate();
    let block_def_ref = universe.insert_anonymous(BlockDef::new(block));
    let eval_def = block_def_ref.borrow().block.evaluate();
    assert_eq!(eval_bare, eval_def);
}

#[test]
fn listen_atom() {
    let block = Block::from(Rgba::WHITE);
    let mut sink = Sink::new();
    block.listen(sink.listener()).unwrap();
    assert_eq!(None, sink.next());
    // No notifications are possible, so nothing more to test.
}

#[test]
fn listen_indirect_atom() {
    let mut universe = Universe::new();
    let block_def_ref = universe.insert_anonymous(BlockDef::new(Block::from(Rgba::WHITE)));
    let indirect = Block::Indirect(block_def_ref.clone());
    let mut sink = Sink::new();
    indirect.listen(sink.listener()).unwrap();
    assert_eq!(None, sink.next());

    // Now mutate it and we should see a notification.
    *(block_def_ref.borrow_mut().modify()) = Block::from(Rgba::BLACK);
    assert!(sink.next().is_some());
}

/// Testing double indirection not because it's a case we expect to use routinely,
/// but because it exercises the generality of the notification mechanism.
#[test]
fn listen_indirect_double() {
    let mut universe = Universe::new();
    let block_def_ref1 = universe.insert_anonymous(BlockDef::new(Block::from(Rgba::WHITE)));
    let block_def_ref2 =
        universe.insert_anonymous(BlockDef::new(Block::Indirect(block_def_ref1.clone())));
    let indirect2 = Block::Indirect(block_def_ref2.clone());
    let mut sink = Sink::new();
    indirect2.listen(sink.listener()).unwrap();
    assert_eq!(None, sink.next());

    // Now mutate the original block and we should see a notification.
    *(block_def_ref1.borrow_mut().modify()) = Block::from(Rgba::BLACK);
    assert!(sink.next().is_some());

    // Remove block_def_ref1 from the contents of block_def_ref2...
    *(block_def_ref2.borrow_mut().modify()) = Block::from(Rgba::BLACK);
    assert!(sink.next().is_some());
    assert!(sink.next().is_none());
    // ...and then block_def_ref1's changes should NOT be forwarded.
    *(block_def_ref1.borrow_mut().modify()) = Block::from(Rgba::WHITE);
    assert!(sink.next().is_none());
}

/// Test that changes to a `Space` propagate to block listeners.
#[test]
fn listen_recur() {
    let mut universe = Universe::new();
    let [block_0, block_1] = make_some_blocks();
    let space_ref = universe.insert_anonymous(Space::empty_positive(2, 1, 1));
    let block = Block::builder().voxels_ref(1, space_ref.clone()).build();
    let mut sink = Sink::new();
    block.listen(sink.listener()).unwrap();
    assert_eq!(None, sink.next());

    // Now mutate the space and we should see a notification.
    space_ref.borrow_mut().set((0, 0, 0), &block_0).unwrap();
    assert!(sink.next().is_some());

    // TODO: Also test that we don't propagate lighting changes

    // A mutation out of bounds should not trigger a notification
    space_ref.borrow_mut().set((1, 0, 0), &block_1).unwrap();
    assert_eq!(sink.next(), None);
}

// TODO: test of evaluate where the block's space is the wrong size

#[test]
fn builder_defaults() {
    let color = Rgba::new(0.1, 0.2, 0.3, 0.4);
    assert_eq!(
        Block::builder().color(color).build(),
        Block::Atom(BlockAttributes::default(), color),
    );
}

#[test]
fn builder_every_field() {
    let color = Rgba::new(0.1, 0.2, 0.3, 0.4);
    let light_emission = Rgb::new(0.1, 3.0, 0.1);
    assert_eq!(
        Block::builder()
            .display_name("hello world")
            .collision(BlockCollision::None) // TODO: when we have more interesting values, use one
            .color(color)
            .selectable(false)
            .light_emission(light_emission)
            .build(),
        Block::Atom(
            BlockAttributes {
                display_name: "hello world".into(),
                collision: BlockCollision::None,
                selectable: false,
                light_emission
            },
            color
        ),
    );
}

#[test]
fn builder_voxels_from_space() {
    let mut universe = Universe::new();
    let space_ref = universe.insert_anonymous(Space::empty_positive(1, 1, 1));

    assert_eq!(
        Block::builder()
            .display_name("hello world")
            .voxels_ref(2, space_ref.clone())
            .build(),
        Block::Recur {
            attributes: BlockAttributes {
                display_name: "hello world".into(),
                ..BlockAttributes::default()
            },
            offset: GridPoint::origin(),
            resolution: 2, // not same as space size
            space: space_ref
        },
    );
}

#[test]
fn builder_default_equivalent() {
    assert_eq!(
        BlockBuilder::<builder::NeedsColorOrVoxels>::default(),
        <BlockBuilder<builder::NeedsColorOrVoxels> as Default>::default()
    );
}

#[test]
fn attributes_default_equivalent() {
    assert_eq!(
        BlockAttributes::default(),
        <BlockAttributes as Default>::default()
    );
}

#[test]
fn attributes_debug() {
    let default = BlockAttributes::default;
    fn debug(a: BlockAttributes) -> String {
        format!("{:?}", a)
    }
    assert_eq!(&*debug(BlockAttributes::default()), "BlockAttributes {}",);
    assert_eq!(
        &*debug(BlockAttributes {
            display_name: "x".into(),
            ..default()
        }),
        "BlockAttributes { display_name: \"x\" }",
    );
    assert_eq!(
        &*debug(BlockAttributes {
            selectable: false,
            ..default()
        }),
        "BlockAttributes { selectable: false }",
    );
    assert_eq!(
        &*debug(BlockAttributes {
            collision: BlockCollision::None,
            ..default()
        }),
        "BlockAttributes { collision: None }",
    );
    assert_eq!(
        &*debug(BlockAttributes {
            light_emission: Rgb::new(1.0, 2.0, 3.0),
            ..default()
        }),
        "BlockAttributes { light_emission: Rgb(1.0, 2.0, 3.0) }",
    );

    // Test a case of multiple attributes
    assert_eq!(
        &*debug(BlockAttributes {
            display_name: "y".into(),
            selectable: false,
            ..default()
        }),
        "BlockAttributes { display_name: \"y\", selectable: false }",
    );
}

#[test]
fn overflow_evaluate() {
    let mut universe = Universe::new();
    let block = self_referential_block(&mut universe);
    assert_eq!(block.evaluate(), Err(EvalBlockError::StackOverflow));
}

#[test]
fn overflow_listen() {
    let mut universe = Universe::new();
    let block = self_referential_block(&mut universe);
    // This does *not* produce an error, because Block::Indirect manages its own listening.
    // TODO: Probably Block::Indirect needs a recursion limit inside that so it doesn't
    // fire an infinite cycle of listening...? Or perhaps we need to make it difficult to
    // create recursion at all.
    assert_eq!(block.listen(NullListener), Ok(()));
}

/// Helper for overflow_ tests
fn self_referential_block(universe: &mut Universe) -> Block {
    let block_def = universe.insert_anonymous(BlockDef::new(AIR));
    let indirect = Block::Indirect(block_def.clone());
    *(block_def.borrow_mut().modify()) = indirect.clone();
    indirect
}
