//! Tests for [`Block`] as a whole.
//! Other submodules also have their own tests.

use pretty_assertions::assert_eq;

use crate::block::{
    self, AIR, Block, BlockChange, BlockDef, BlockDefTransaction, EvalBlockError, Modifier,
    Primitive, Resolution::*,
};
use crate::content::make_some_blocks;
use crate::listen::{self, Log, NullListener};
use crate::math::{Face6, GridRotation, Rgba};
use crate::space::{Space, SpaceTransaction};
use crate::time;
use crate::transaction::{self, Transaction as _};
use crate::universe::{Universe, UniverseTransaction};

/// Just install a listener and discard the [`EvaluatedBlock`].
///
/// TODO: Expand this to, or otherwise create, a helper which checks that the evaluation result
/// changes only with notification.
fn listen(
    universe: &Universe,
    block: &Block,
    listener: impl listen::IntoListener<listen::DynListener<BlockChange>, BlockChange>,
) -> Result<(), EvalBlockError> {
    block
        .evaluate2(&block::EvalFilter {
            read_ticket: universe.read_ticket(),
            skip_eval: true,
            listener: Some(listener.into_listener()),
            budget: Default::default(),
        })
        .map(drop)
}

#[test]
fn block_size_is_pointerish() {
    let block_size = size_of::<Block>();
    let ptr_size = size_of::<*const ()>();
    assert!(
        ptr_size < block_size && block_size <= 2 * ptr_size,
        "unexpected size: {block_size}",
    );
}

#[test]
fn primitive_size() {
    let size = size_of::<Primitive>();
    assert!(
        size <= 96,
        "size_of::<Primitive>() = {size} unexpectedly large"
    );
}

#[test]
fn block_static_eq_to_non_static() {
    let foo = AIR;
    let bar = Block::from_primitive(foo.primitive().clone());
    assert_eq!(foo, bar);
}

#[cfg(fmt_debug = "full")]
#[test]
fn block_debug_air() {
    assert_eq!(&format!("{:?}", &AIR), "Block { primitive: Air }");
}

#[cfg(fmt_debug = "full")]
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
            primitive: Atom { \
                color: Rgba(1.0, 0.5, 0.0, 1.0), \
                collision: Hard }, \
            modifiers: [Rotate(Rxyz)] \
        }"
    );
}

#[test]
fn listen_atom() {
    let universe = Universe::new();
    let block = block::from_color!(Rgba::WHITE);
    let log = Log::new();

    listen(&universe, &block, log.listener()).unwrap();
    assert_eq!(log.drain(), vec![]);
    // No notifications are possible, so nothing more to test.
}

#[test]
fn listen_indirect_atom() {
    let mut universe = Universe::new();
    let block_def_handle = universe.insert_anonymous(BlockDef::new(
        universe.read_ticket(),
        block::from_color!(Rgba::WHITE),
    ));
    let indirect = Block::from(block_def_handle.clone());
    let log = Log::new();
    listen(&universe, &indirect, log.listener()).unwrap();
    assert_eq!(log.drain(), vec![]);

    // Now mutate it and we should see a notification.
    universe
        .execute_1(
            &block_def_handle,
            BlockDefTransaction::overwrite(block::from_color!(Rgba::BLACK)),
        )
        .unwrap();
    assert_eq!(log.drain().len(), 1);
}

/// Testing double indirection not because it's a case we expect to use routinely,
/// but because it exercises the generality of the notification and cache mechanisms.
/// Specifically, `block_def_handle1` is updated by transaction, but `block_def_handle2`
/// is updated by universe stepping since it was not directly mutated.
#[test]
fn listen_indirect_double() {
    let mut universe = Universe::new();
    let block_def_handle1 = universe
        .insert(
            "bd1".into(),
            BlockDef::new(universe.read_ticket(), block::from_color!(Rgba::WHITE)),
        )
        .unwrap();
    let indirect1 = Block::from(block_def_handle1.clone());
    let block_def_handle2 = universe
        .insert(
            "bd2".into(),
            BlockDef::new(universe.read_ticket(), indirect1.clone()),
        )
        .unwrap();
    let indirect2 = Block::from(block_def_handle2.clone());
    let log1 = Log::new();
    let log2 = Log::new();
    listen(&universe, &indirect1, log1.listener()).unwrap();
    listen(&universe, &indirect2, log2.listener()).unwrap();
    assert_eq!(log1.drain(), vec![]);
    assert_eq!(log2.drain(), vec![]);

    // Mutate the first BlockDef and we should see a notification for it alone.
    universe
        .execute_1(
            &block_def_handle1,
            BlockDefTransaction::overwrite(block::from_color!(Rgba::BLACK)),
        )
        .unwrap();
    assert_eq!([log1.drain().len(), log2.drain().len()], [1, 0]);

    // Step and get the other notification.
    universe.step(false, time::Deadline::Whenever);
    assert_eq!([log1.drain().len(), log2.drain().len()], [0, 1]);

    // Remove block_def_handle1 from the contents of block_def_handle2...
    universe
        .execute_1(
            &block_def_handle2,
            BlockDefTransaction::overwrite(block::from_color!(Rgba::BLACK)),
        )
        .unwrap();
    assert_eq!(log2.drain().len(), 1);
    // ...and then block_def_handle1's changes should NOT be forwarded.
    universe
        .execute_1(
            &block_def_handle1,
            BlockDefTransaction::overwrite(block::from_color!(Rgba::WHITE)),
        )
        .unwrap();
    assert_eq!(log2.drain(), vec![]);
}

/// Test that changes to a `Space` propagate to block listeners.
#[test]
fn listen_recur() {
    let mut universe = Universe::new();
    let [block_0, block_1] = make_some_blocks();
    let space_handle = universe.insert_anonymous(Space::empty_positive(2, 1, 1));
    let block = Block::builder().voxels_handle(R1, space_handle.clone()).build();
    let log = Log::new();
    listen(&universe, &block, log.listener()).unwrap();
    assert_eq!(log.drain(), vec![]);

    // Now mutate the space and we should see a notification.
    universe
        .execute_1(
            &space_handle,
            SpaceTransaction::set_cube([0, 0, 0], None, Some(block_0)),
        )
        .unwrap();
    assert_eq!(log.drain().len(), 1);

    // TODO: Also test that we don't propagate lighting changes

    // A mutation out of bounds should not trigger a notification
    universe
        .execute_1(
            &space_handle,
            SpaceTransaction::set_cube([1, 0, 0], None, Some(block_1)),
        )
        .unwrap();
    assert_eq!(log.drain(), vec![]);
}

#[test]
fn overflow_evaluate() {
    let universe = Universe::new();

    // The primitive counts as a component.
    let too_many_modifiers: u32 = block::Budget::default().components;

    let mut block = AIR;
    block.modifiers_mut().extend(std::iter::repeat_n(
        Modifier::Rotate(Face6::PY.clockwise()),
        too_many_modifiers as usize,
    ));
    assert_eq!(
        block.evaluate(universe.read_ticket()),
        Err(EvalBlockError {
            block,
            budget: block::Budget::default().to_cost(),
            used: block::Cost {
                components: too_many_modifiers,
                voxels: 0,
                recursion: 0
            },
            kind: block::ErrorKind::BudgetExceeded,
        })
    );
}

#[rstest::rstest]
fn self_referential_evaluate(#[values(false, true)] via_mutation: bool) {
    let mut universe = Universe::new();
    let block = self_referential_block(&mut universe, via_mutation);

    let result = block.evaluate(universe.read_ticket());

    if via_mutation {
        // In this case we end up with a successful evaluation of the initial value (`AIR`).
        // TODO: There should be some sort of error flag that doesn't stop simulation but alerts
        // users, because in this case the block value now has cached state that will be lost
        // on reload.
        assert_eq!(
            result,
            Ok(block::EvaluatedBlock {
                block: block.clone(),
                ..block::AIR_EVALUATED
            })
        );
    } else {
        // TODO: the HandleError details are not presented clearly (it is a WrongUniverse).
        // Arrange so that this situation has a clear error and match it.
        assert!(
            matches!(
                result,
                Err(EvalBlockError {
                    block: _,
                    budget: _,
                    used: block::Cost {
                        components: 1,
                        voxels: 0,
                        recursion: 0
                    },
                    kind: block::ErrorKind::Handle(_)
                })
            ),
            "{result:#?}"
        );
    }
}

#[rstest::rstest]
fn self_referential_listen(#[values(false, true)] via_mutation: bool) {
    let mut universe = Universe::new();
    let block = self_referential_block(&mut universe, via_mutation);
    // This should *not* produce an error, because BlockDef manages its own notifier and we want
    // it to be possible to listen to a currently-erring BlockDef.
    assert_eq!(listen(&universe, &block, NullListener), Ok(()));
}

/// Helper for `self_referential_` tests producing a `BlockDef` which refers to itself.
///
/// * `via_mutation: true`: the `BlockDef` is created with another value and then made
///   self-referential.
/// * `via_mutation: false`: the `BlockDef` never has a non-self-referential value.
fn self_referential_block(universe: &mut Universe, via_mutation: bool) -> Block {
    if via_mutation {
        let block_def = universe
            .insert(
                "self_referential".into(),
                BlockDef::new(universe.read_ticket(), AIR),
            )
            .unwrap();
        let indirect = Block::from(block_def.clone());
        universe
            .execute_1(&block_def, BlockDefTransaction::overwrite(indirect.clone()))
            .unwrap();
        indirect
    } else {
        let mut txn = UniverseTransaction::default();
        let block_def_handle = txn.insert_without_value("self_referential".into()).unwrap();
        let indirect_block = Block::from(block_def_handle.clone());
        txn.set_pending_value(
            &block_def_handle,
            BlockDef::new(universe.read_ticket(), indirect_block.clone()),
        );
        txn.execute(universe, (), &mut transaction::no_outputs).unwrap();
        indirect_block
    }
}

/// Tests for the behavior of `with_modifier()` and `rotate()` and such.
mod modify {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    pub fn rotate_atom_is_identity() {
        let block = block::from_color!(1.0, 0.0, 0.0);
        assert_eq!(block.clone().rotate(Face6::PY.clockwise()), block);
    }

    #[test]
    fn rotate_atom_with_symmetric_modifier_is_identity() {
        // Quote is an example of a modifer that doesn't add asymmetry.
        let block = block::from_color!(1.0, 0.0, 0.0).with_modifier(block::Quote::new());
        assert_eq!(block.clone().rotate(Face6::PY.clockwise()), block);
    }
}

mod txn {
    use super::*;
    use crate::transaction::{Merge, TransactionTester};
    use crate::universe::ReadTicket;
    use pretty_assertions::assert_eq;

    #[test]
    fn causes_notification() {
        // We're using a Primitive::Indirect in addition to the BlockDef to test a more
        // realistic scenario
        let [b1, b2] = make_some_blocks();
        let mut universe = Universe::new();
        let block_def_handle = universe.insert_anonymous(BlockDef::new(universe.read_ticket(), b1));
        let indirect = Block::from(block_def_handle.clone());
        let log = Log::new();
        listen(&universe, &indirect, log.listener()).unwrap();
        assert_eq!(log.drain(), vec![]);

        // Now mutate it and we should see a notification.
        universe
            .execute_1(&block_def_handle, BlockDefTransaction::overwrite(b2))
            .unwrap();
        assert_eq!(log.drain().len(), 1);
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
                    if *before.block() != b1 {
                        return Err("did not assert b1".into());
                    }
                    if *after.block() != b2 {
                        return Err("did not set b2".into());
                    }
                    Ok(())
                },
            )
            .transaction(
                BlockDefTransaction::replace(b1.clone(), b3.clone()),
                |before, after| {
                    if *before.block() != b1 {
                        return Err("did not assert b1".into());
                    }
                    if *after.block() != b3 {
                        return Err("did not set b3".into());
                    }
                    Ok(())
                },
            )
            .transaction(BlockDefTransaction::overwrite(b2.clone()), |_, after| {
                if *after.block() != b2 {
                    return Err("did not set b2".into());
                }
                Ok(())
            })
            .transaction(BlockDefTransaction::expect(b2.clone()), |before, _| {
                if *before.block() != b2 {
                    return Err("did not assert b2".into());
                }
                Ok(())
            })
            .transaction(BlockDefTransaction::expect(b1.clone()), |before, _| {
                if *before.block() != b1 {
                    return Err("did not assert b1".into());
                }
                Ok(())
            })
            .target(|| BlockDef::new(ReadTicket::stub(), AIR))
            .target(|| BlockDef::new(ReadTicket::stub(), b1.clone()))
            .target(|| BlockDef::new(ReadTicket::stub(), b2.clone()))
            .test(ReadTicket::stub());
    }
}
