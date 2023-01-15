//! Tests for [`Block`] as a whole.
//! The following modules also have their own tests:
//!
//! * [`super::attributes`]
//! * [`super::builder`]
//! * [`super::modifier`]

#![allow(clippy::bool_assert_comparison)]

use std::borrow::Cow;

use cgmath::EuclideanSpace as _;
use pretty_assertions::assert_eq;

use crate::block::{
    Block, BlockAttributes, BlockCollision, BlockDef, BlockDefTransaction, EvalBlockError, Evoxel,
    Modifier, Primitive, Resolution, Resolution::*, AIR, AIR_EVALUATED,
};
use crate::content::make_some_blocks;
use crate::listen::{NullListener, Sink};
use crate::math::{
    Face6, FaceMap, GridAab, GridArray, GridCoordinate, GridPoint, GridRotation, GridVector,
    OpacityCategory, Rgb, Rgba,
};
use crate::space::{Space, SpaceTransaction};
use crate::universe::Universe;

#[test]
fn block_is_approximately_a_pointer() {
    let block_size = std::mem::size_of::<Block>();
    let ptr_size = std::mem::size_of::<*const ()>();
    assert!(
        ptr_size < block_size && block_size <= 2 * ptr_size,
        "unexpected size: {block_size}",
    );
}

#[test]
fn block_static_eq_to_non_static() {
    let foo = AIR;
    let bar = Block::from_primitive(foo.primitive().clone());
    assert_eq!(foo, bar);
}

#[test]
fn block_debug_air() {
    assert_eq!(&format!("{:?}", &AIR), "Block { primitive: Air }");
}

#[test]
fn block_debug_with_modifiers() {
    assert_eq!(
        &format!(
            "{:?}",
            &Block::builder()
                .color(Rgba::new(1.0, 0.5, 0.0, 1.0))
                // TODO: When we have more modifiers, pick a different one, that isn't
                // essentially irrelevant to Primitive::Atom
                .modifier(Modifier::Rotate(GridRotation::Rxyz))
                .build()
        ),
        "Block { \
            primitive: Atom(\
                BlockAttributes {}, \
                Rgba(1.0, 0.5, 0.0, 1.0)), \
            modifiers: [Rotate(Rxyz)] \
        }"
    );
}

#[test]
fn evaluate_air_consistent() {
    assert_eq!(AIR.evaluate().unwrap(), AIR_EVALUATED);
    assert_eq!(
        Block::from(Primitive::Air).evaluate().unwrap(),
        AIR_EVALUATED
    );
}

#[test]
fn evaluate_opaque_atom_and_attributes() {
    let color = Rgba::new(1.0, 2.0, 3.0, 1.0);
    let attributes = BlockAttributes {
        display_name: Cow::Borrowed("hello world"),
        selectable: false,
        collision: BlockCollision::None,
        light_emission: Rgb::ONE,
        ..BlockAttributes::default()
    };
    let block = Block::from_primitive(Primitive::Atom(attributes.clone(), color));
    let e = block.evaluate().unwrap();
    assert_eq!(e.attributes, attributes);
    assert_eq!(e.color, block.color());
    assert!(e.voxels.is_none());
    assert_eq!(e.resolution, R1);
    assert_eq!(e.opaque, FaceMap::repeat(true));
    assert_eq!(e.visible, true);
    assert_eq!(
        e.voxel_opacity_mask,
        Some(GridArray::from_element(OpacityCategory::Opaque))
    )
}

#[test]
fn evaluate_transparent_atom() {
    let color = Rgba::new(1.0, 2.0, 3.0, 0.5);
    let block = Block::from_primitive(Primitive::Atom(BlockAttributes::default(), color));
    let e = block.evaluate().unwrap();
    assert_eq!(e.color, block.color());
    assert!(e.voxels.is_none());
    assert_eq!(e.opaque, FaceMap::repeat(false));
    assert_eq!(e.visible, true);
    assert_eq!(
        e.voxel_opacity_mask,
        Some(GridArray::from_element(OpacityCategory::Partial))
    )
}

#[test]
fn evaluate_invisible_atom() {
    let block = Block::from_primitive(Primitive::Atom(
        BlockAttributes::default(),
        Rgba::TRANSPARENT,
    ));
    let e = block.evaluate().unwrap();
    assert_eq!(e.color, Rgba::TRANSPARENT);
    assert!(e.voxels.is_none());
    assert_eq!(e.opaque, FaceMap::repeat(false));
    assert_eq!(e.visible, false);
    assert_eq!(e.voxel_opacity_mask, None)
}

#[test]
fn evaluate_voxels_checked_individually() {
    let resolution = R2;
    let mut universe = Universe::new();

    let attributes = BlockAttributes {
        display_name: Cow::Borrowed("hello world"),
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
        Some(GridArray::from_fn(
            GridAab::for_block(resolution),
            |point| {
                let point = point.cast::<f32>().unwrap();
                Evoxel {
                    color: Rgba::new(point.x, point.y, point.z, 1.0),
                    selectable: true,
                    collision: BlockCollision::Hard,
                }
            }
        ))
    );
    assert_eq!(e.color, Rgba::new(0.5, 0.5, 0.5, 1.0));
    assert_eq!(e.resolution, resolution);
    assert_eq!(e.opaque, FaceMap::repeat(true));
    assert_eq!(e.visible, true);
    assert_eq!(
        e.voxel_opacity_mask,
        Some(GridArray::repeat(
            GridAab::for_block(resolution),
            OpacityCategory::Opaque,
        ))
    )
}

#[test]
fn evaluate_transparent_voxels() {
    let mut universe = Universe::new();
    let resolution = R4;
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
        Rgba::new(0.0, 0.0, 0.0, 1.0 - (0.5 / f32::from(resolution).powi(3)))
    );
    assert_eq!(
        e.opaque,
        FaceMap {
            nx: false,
            ny: false,
            nz: false,
            px: true,
            py: true,
            pz: true,
        }
    );
    assert_eq!(e.visible, true);
}

#[test]
fn evaluate_voxels_not_filling_block() {
    let resolution = R4;
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
        Rgba::new(0.0, 0.0, 0.0, 1.0 / f32::from(resolution).powi(3))
    );
    assert_eq!(e.resolution, resolution);
    assert_eq!(e.opaque, FaceMap::repeat(false));
    assert_eq!(e.visible, true);
}

/// Test the situation where the space is smaller than the block: in particular,
/// even if the space is all opaque, the block should not be counted as opaque.
#[test]
fn evaluate_voxels_partial_not_filling() {
    let resolution = R4;
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(2, 4, 4);
    space
        .fill_uniform(space.bounds(), Block::from(Rgba::WHITE))
        .unwrap();
    let space_ref = universe.insert_anonymous(space);
    let block = Block::builder()
        .voxels_ref(resolution as Resolution, space_ref.clone())
        .build();

    let e = block.evaluate().unwrap();
    assert_eq!(e.color, Rgba::new(1.0, 1.0, 1.0, 0.5));
    assert_eq!(e.resolution, resolution);
    assert_eq!(e.opaque, FaceMap::repeat(false).with(Face6::NX, true));
    assert_eq!(e.visible, true);
}

/// Tests that the `offset` field of `Primitive::Recur` is respected.
#[test]
fn recur_with_offset() {
    let resolution = R4;
    let resolution_g = GridCoordinate::from(resolution);
    let offset = GridVector::new(resolution_g, 0, 0);
    let mut universe = Universe::new();
    let mut space = Space::empty_positive(resolution_g * 2, resolution_g, resolution_g);
    space
        .fill(space.bounds(), |point| {
            let point = point.cast::<f32>().unwrap();
            Some(Block::from(Rgba::new(point.x, point.y, point.z, 1.0)))
        })
        .unwrap();
    let space_ref = universe.insert_anonymous(space);
    let block_at_offset = Block::from_primitive(Primitive::Recur {
        attributes: BlockAttributes::default(),
        offset: GridPoint::from_vec(offset),
        resolution,
        space: space_ref.clone(),
    });

    let e = block_at_offset.evaluate().unwrap();
    assert_eq!(
        e.voxels,
        Some(GridArray::from_fn(
            GridAab::for_block(resolution as Resolution),
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

/// Fuzzer-derived regression test for numeric overflow
#[test]
fn recur_offset_negative_overflow() {
    let mut universe = Universe::new();
    let space = Space::builder(GridAab::from_lower_upper(
        [1743229108, 939544399, -2147463345],
        [1743229109, 939544400, -2147461505],
    ))
    .build();
    let block_at_offset = Block::from_primitive(Primitive::Recur {
        attributes: BlockAttributes::default(),
        offset: GridPoint::new(-414232629, -2147483648, -13697025),
        resolution: R128,
        space: universe.insert_anonymous(space),
    });

    let e = block_at_offset.evaluate().unwrap();
    assert!(!e.visible);
}

#[test]
fn indirect_equivalence() {
    let resolution = R4;
    let mut universe = Universe::new();
    let mut space = Space::empty(GridAab::for_block(resolution));
    // TODO: BlockGen should support constructing indirects (by default, even)
    // and we can use the more concise version
    space
        .fill(space.bounds(), |point| {
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
    let eval_def = block_def_ref.read().unwrap().evaluate();
    assert_eq!(eval_bare, eval_def);
}

#[test]
fn listen_atom() {
    let block = Block::from(Rgba::WHITE);
    let sink = Sink::new();
    block.listen(sink.listener()).unwrap();
    assert_eq!(sink.drain(), vec![]);
    // No notifications are possible, so nothing more to test.
}

#[test]
fn listen_indirect_atom() {
    let mut universe = Universe::new();
    let block_def_ref = universe.insert_anonymous(BlockDef::new(Block::from(Rgba::WHITE)));
    let indirect = Block::from_primitive(Primitive::Indirect(block_def_ref.clone()));
    let sink = Sink::new();
    indirect.listen(sink.listener()).unwrap();
    assert_eq!(sink.drain(), vec![]);

    // Now mutate it and we should see a notification.
    block_def_ref
        .execute(&BlockDefTransaction::overwrite(Block::from(Rgba::BLACK)))
        .unwrap();
    assert_eq!(sink.drain().len(), 1);
}

/// Testing double indirection not because it's a case we expect to use routinely,
/// but because it exercises the generality of the notification mechanism.
#[test]
fn listen_indirect_double() {
    let mut universe = Universe::new();
    let block_def_ref1 = universe.insert_anonymous(BlockDef::new(Block::from(Rgba::WHITE)));
    let block_def_ref2 = universe.insert_anonymous(BlockDef::new(Block::from_primitive(
        Primitive::Indirect(block_def_ref1.clone()),
    )));
    let indirect2 = Block::from_primitive(Primitive::Indirect(block_def_ref2.clone()));
    let sink = Sink::new();
    indirect2.listen(sink.listener()).unwrap();
    assert_eq!(sink.drain(), vec![]);

    // Now mutate the original block and we should see a notification.
    block_def_ref1
        .execute(&BlockDefTransaction::overwrite(Block::from(Rgba::BLACK)))
        .unwrap();
    assert_eq!(sink.drain().len(), 1);

    // Remove block_def_ref1 from the contents of block_def_ref2...
    block_def_ref2
        .execute(&BlockDefTransaction::overwrite(Block::from(Rgba::BLACK)))
        .unwrap();
    assert_eq!(sink.drain().len(), 1);
    // ...and then block_def_ref1's changes should NOT be forwarded.
    block_def_ref1
        .execute(&BlockDefTransaction::overwrite(Block::from(Rgba::WHITE)))
        .unwrap();
    assert_eq!(sink.drain(), vec![]);
}

/// Test that changes to a `Space` propagate to block listeners.
#[test]
fn listen_recur() {
    let mut universe = Universe::new();
    let [block_0, block_1] = make_some_blocks();
    let space_ref = universe.insert_anonymous(Space::empty_positive(2, 1, 1));
    let block = Block::builder().voxels_ref(R1, space_ref.clone()).build();
    let sink = Sink::new();
    block.listen(sink.listener()).unwrap();
    assert_eq!(sink.drain(), vec![]);

    // Now mutate the space and we should see a notification.
    space_ref
        .execute(&SpaceTransaction::set_cube([0, 0, 0], None, Some(block_0)))
        .unwrap();
    assert_eq!(sink.drain().len(), 1);

    // TODO: Also test that we don't propagate lighting changes

    // A mutation out of bounds should not trigger a notification
    space_ref
        .execute(&SpaceTransaction::set_cube([1, 0, 0], None, Some(block_1)))
        .unwrap();
    assert_eq!(sink.drain(), vec![]);
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
    // This does *not* produce an error, because BlockDef manages its own listening.
    // TODO: Probably Primitive::Indirect needs a recursion limit inside that so it doesn't
    // fire an infinite cycle of notifications...? Or perhaps we need to make it difficult to
    // create recursion at all.
    assert_eq!(block.listen(NullListener), Ok(()));
}

/// Helper for overflow_ tests
fn self_referential_block(universe: &mut Universe) -> Block {
    let block_def = universe.insert_anonymous(BlockDef::new(AIR));
    let indirect = Block::from_primitive(Primitive::Indirect(block_def.clone()));
    block_def
        .execute(&BlockDefTransaction::overwrite(indirect.clone()))
        .unwrap();
    indirect
}

mod txn {
    use super::*;
    use crate::block::BlockDefTransaction;
    use crate::transaction::{Merge, TransactionTester};
    use pretty_assertions::assert_eq;

    #[test]
    fn causes_notification() {
        // We're using a Primitive::Indirect in addition to the BlockDef to test a more
        // realistic scenario
        let [b1, b2] = make_some_blocks();
        let mut universe = Universe::new();
        let block_def_ref = universe.insert_anonymous(BlockDef::new(b1));
        let indirect = Block::from_primitive(Primitive::Indirect(block_def_ref.clone()));
        let sink = Sink::new();
        indirect.listen(sink.listener()).unwrap();
        assert_eq!(sink.drain(), vec![]);

        // Now mutate it and we should see a notification.
        block_def_ref
            .execute(&BlockDefTransaction::overwrite(b2))
            .unwrap();
        assert_eq!(sink.drain().len(), 1);
    }

    #[test]
    fn merge_allows_same_new() {
        let [new] = make_some_blocks();
        let t1 = BlockDefTransaction::overwrite(new);
        assert_eq!(t1.clone().merge(t1.clone()), Ok(t1));
    }

    #[test]
    fn merge_rejects_different_new() {
        let [new1, new2] = make_some_blocks();
        let t1 = BlockDefTransaction::overwrite(new1);
        let t2 = BlockDefTransaction::overwrite(new2);
        t1.merge(t2).unwrap_err();
    }

    #[test]
    fn merge_rejects_different_old() {
        let [old1, old2] = make_some_blocks();
        let t1 = BlockDefTransaction::expect(old1);
        let t2 = BlockDefTransaction::expect(old2);
        t1.merge(t2).unwrap_err();
    }

    #[test]
    fn merge_allows_same_old() {
        let [old, new] = make_some_blocks();
        let t1 = BlockDefTransaction::replace(old.clone(), new.clone());
        let t2 = BlockDefTransaction::replace(old.clone(), new.clone());
        assert_eq!(t1.clone(), t1.clone().merge(t2).unwrap());
    }

    #[test]
    fn systematic() {
        let [b1, b2, b3] = make_some_blocks();
        TransactionTester::new()
            .transaction(BlockDefTransaction::default(), |_, _| Ok(()))
            .transaction(
                BlockDefTransaction::replace(b1.clone(), b2.clone()),
                |before, after| {
                    if **before != b1 {
                        return Err("did not assert b1".into());
                    }
                    if **after != b2 {
                        return Err("did not set b2".into());
                    }
                    Ok(())
                },
            )
            .transaction(
                BlockDefTransaction::replace(b1.clone(), b3.clone()),
                |before, after| {
                    if **before != b1 {
                        return Err("did not assert b1".into());
                    }
                    if **after != b3 {
                        return Err("did not set b3".into());
                    }
                    Ok(())
                },
            )
            .transaction(BlockDefTransaction::overwrite(b2.clone()), |_, after| {
                if **after != b2 {
                    return Err("did not set b2".into());
                }
                Ok(())
            })
            .transaction(BlockDefTransaction::expect(b2.clone()), |before, _| {
                if **before != b2 {
                    return Err("did not assert b2".into());
                }
                Ok(())
            })
            .transaction(BlockDefTransaction::expect(b1.clone()), |before, _| {
                if **before != b1 {
                    return Err("did not assert b1".into());
                }
                Ok(())
            })
            .target(|| BlockDef::new(AIR))
            .target(|| BlockDef::new(b1.clone()))
            .target(|| BlockDef::new(b2.clone()))
            .test();
    }
}
