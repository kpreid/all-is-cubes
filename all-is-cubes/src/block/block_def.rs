// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::ops::Deref;
use std::sync::Arc;

use crate::block::{Block, BlockChange};
use crate::listen::{Gate, Listener, Notifier};
use crate::transaction::{
    Merge, PreconditionFailed, Transaction, TransactionConflict, Transactional,
};
use crate::universe::{RefError, RefVisitor, VisitRefs};

/// Contains a [`Block`] and can be stored in a [`Universe`](crate::universe::Universe).
/// Together with [`Block::Indirect`], this allows mutation of a block definition such
/// that all its usages follow.
///
/// It is a distinct type from [`Block`] in order to ensure that change notifications
/// will be delivered on any mutation.
///
/// To perform a mutation, use [`BlockDefTransaction`].
#[derive(Debug)]
pub struct BlockDef {
    block: Block,
    // TODO: It might be a good idea to cache EvaluatedBlock here, since we're doing
    // mutation tracking anyway.
    notifier: Arc<Notifier<BlockChange>>,
    block_listen_gate: Gate,
}

impl BlockDef {
    pub fn new(block: Block) -> Self {
        let notifier = Arc::new(Notifier::new());
        let (gate, block_listener) = Notifier::forwarder(Arc::downgrade(&notifier)).gate();
        // TODO: Consider making it an error if listening fails. BlockDefTransaction::check will need to follow.
        let _ = block.listen(block_listener);
        BlockDef {
            block,
            notifier,
            block_listen_gate: gate,
        }
    }

    /// Registers a listener for mutations of any data sources which may affect the
    /// [`Block::evaluate`] result from blocks defined using this block definition.
    pub fn listen(
        &self,
        listener: impl Listener<BlockChange> + Send + Sync + 'static,
    ) -> Result<(), RefError> {
        // TODO: Need to arrange listening to the contained block, and either translate
        // that here or have our own notifier generate forwardings.
        self.notifier.listen(listener);
        Ok(())
    }
}

impl Deref for BlockDef {
    type Target = Block;

    fn deref(&self) -> &Block {
        &self.block
    }
}
impl AsRef<Block> for BlockDef {
    fn as_ref(&self) -> &Block {
        &self.block
    }
}

impl VisitRefs for BlockDef {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        self.block.visit_refs(visitor)
    }
}

impl VisitRefs for Block {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        match self {
            Block::Indirect(block_ref) => visitor.visit(block_ref),
            Block::Atom(_, _) => {}
            Block::Recur { space, .. } => visitor.visit(space),
            Block::Rotated(_, block) => block.visit_refs(visitor),
        }
    }
}

impl Transactional for BlockDef {
    type Transaction = BlockDefTransaction;
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct BlockDefTransaction {
    // TODO: This struct is the second occurrence (the first is space::CubeTransaction) of a "assign to a mutable location" transaction. If we figure out how to have conveniently _composable_ transactions then we should have an `impl Transaction<&mut T> for Assign<T>` transaction (targeting `&mut` to discourage use otherwise).
    /// If `None`, no precondition.
    old: Option<Block>,
    /// If `None`, no change is made and this transaction is only a precondition.
    new: Option<Block>,
}

impl BlockDefTransaction {
    pub fn expect(old: Block) -> Self {
        Self {
            old: Some(old),
            new: None,
        }
    }

    pub fn overwrite(new: Block) -> Self {
        Self {
            old: None,
            new: Some(new),
        }
    }

    pub fn replace(old: Block, new: Block) -> Self {
        Self {
            old: Some(old),
            new: Some(new),
        }
    }
}

impl Transaction<BlockDef> for BlockDefTransaction {
    type CommitCheck = ();
    type Output = ();

    fn check(
        &self,
        target: &BlockDef,
    ) -> Result<Self::CommitCheck, crate::transaction::PreconditionFailed> {
        if let Some(old) = &self.old {
            if **target != *old {
                return Err(PreconditionFailed {
                    location: "BlockDef",
                    problem: "existing block not as expected",
                });
            }
        }
        Ok(())
    }

    fn commit(
        &self,
        target: &mut BlockDef,
        (): Self::CommitCheck,
    ) -> Result<Self::Output, Box<dyn std::error::Error>> {
        if let Some(new) = &self.new {
            target.block = new.clone();

            // Swap out the forwarding listener to listen to the new block.
            let (gate, block_listener) =
                Notifier::forwarder(Arc::downgrade(&target.notifier)).gate();
            // TODO: Instead of ignoring the error from listen() here, we can fail the transaction by preparing the listener in check().
            let _ = target.block.listen(block_listener);
            target.block_listen_gate = gate; // old gate is now dropped

            target.notifier.notify(BlockChange::new());
        }
        Ok(())
    }
}

impl Merge for BlockDefTransaction {
    type MergeCheck = ();

    fn check_merge(
        &self,
        other: &Self,
    ) -> Result<Self::MergeCheck, crate::transaction::TransactionConflict> {
        if matches!((&self.old, &other.old), (Some(a), Some(b)) if a != b) {
            return Err(TransactionConflict {});
        }
        if matches!((&self.new, &other.new), (Some(a), Some(b)) if a != b) {
            return Err(TransactionConflict {});
        }
        Ok(())
    }

    fn commit_merge(self, other: Self, (): Self::MergeCheck) -> Self
    where
        Self: Sized,
    {
        Self {
            old: self.old.or(other.old),
            new: self.new.or(other.new),
        }
    }
}
