use std::ops::Deref;
use std::sync::Arc;

use crate::block::{self, Block, BlockChange, Primitive};
use crate::listen::{Gate, Listen, Listener, Notifier};
use crate::transaction::{self, Transaction};
use crate::universe::{RefVisitor, VisitRefs};

/// Contains a [`Block`] and can be stored in a [`Universe`](crate::universe::Universe).
/// Together with [`Primitive::Indirect`], this allows mutation of a block definition such
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
    /// Constructs a new [`BlockDef`] that stores the given block (which may be replaced
    /// in the future).
    pub fn new(block: Block) -> Self {
        let notifier = Arc::new(Notifier::new());
        let (gate, block_listener) = Notifier::forwarder(Arc::downgrade(&notifier)).gate();
        // TODO: Take the evaluation result (if successful) and cache it
        let _ = block.evaluate2(&block::EvalFilter {
            skip_eval: true,
            listener: Some(block_listener.erased()),
        });
        BlockDef {
            block,
            notifier,
            block_listen_gate: gate,
        }
    }
}

impl Listen for BlockDef {
    type Msg = BlockChange;

    /// Registers a listener for mutations of any data sources which may affect the
    /// [`Block::evaluate`] result from blocks defined using this block definition.
    fn listen<L: Listener<BlockChange> + Send + Sync + 'static>(&self, listener: L) {
        self.notifier.listen(listener)
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
        self.primitive().visit_refs(visitor);
        for modifier in self.modifiers() {
            modifier.visit_refs(visitor)
        }
    }
}

impl VisitRefs for Primitive {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        match self {
            Primitive::Indirect(block_ref) => visitor.visit(block_ref),
            Primitive::Atom(atom) => atom.visit_refs(visitor),
            Primitive::Air => {}
            Primitive::Recur {
                space,
                attributes,
                offset: _,
                resolution: _,
            } => {
                visitor.visit(space);
                attributes.visit_refs(visitor);
            }
        }
    }
}

impl VisitRefs for block::Atom {
    fn visit_refs(&self, visitor: &mut dyn RefVisitor) {
        let Self {
            attributes,
            color: _,
            collision: _,
        } = self;
        attributes.visit_refs(visitor);
    }
}

impl transaction::Transactional for BlockDef {
    type Transaction = BlockDefTransaction;
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for BlockDef {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(BlockDef::new(Block::arbitrary(u)?))
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        Block::size_hint(depth)
    }
}

/// A [`Transaction`] which replaces (or checks) the [`Block`] stored in a [`BlockDef`].
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
#[must_use]
pub struct BlockDefTransaction {
    // TODO: This struct is the second occurrence (the first is space::CubeTransaction) of a "assign to a mutable location" transaction. If we figure out how to have conveniently _composable_ transactions then we should have an `impl Transaction<&mut T> for Assign<T>` transaction (targeting `&mut` to discourage use otherwise).
    /// If `None`, no precondition.
    old: Option<Block>,
    /// If `None`, no change is made and this transaction is only a precondition.
    new: Option<Block>,
}

impl BlockDefTransaction {
    /// Returns a transaction which fails if the current value of the [`BlockDef`] is not
    /// equal to `old`.
    pub fn expect(old: Block) -> Self {
        Self {
            old: Some(old),
            new: None,
        }
    }

    /// Returns a transaction which replaces the current value of the [`BlockDef`] with `new`.
    pub fn overwrite(new: Block) -> Self {
        Self {
            old: None,
            new: Some(new),
        }
    }

    /// Returns a transaction which replaces the value of the [`BlockDef`] with `new`,
    /// if it is equal to `old`, and otherwise fails.
    pub fn replace(old: Block, new: Block) -> Self {
        Self {
            old: Some(old),
            new: Some(new),
        }
    }
}

impl Transaction<BlockDef> for BlockDefTransaction {
    type CommitCheck = ();
    type Output = transaction::NoOutput;

    fn check(
        &self,
        target: &BlockDef,
    ) -> Result<Self::CommitCheck, transaction::PreconditionFailed> {
        if let Some(old) = &self.old {
            if **target != *old {
                return Err(transaction::PreconditionFailed {
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
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        if let Some(new) = &self.new {
            target.block = new.clone();

            // Swap out the forwarding listener to listen to the new block.
            let (gate, block_listener) =
                Notifier::forwarder(Arc::downgrade(&target.notifier)).gate();
            // TODO: Take the evaluation result (if successful) and cache it
            let _ = target.block.evaluate2(&block::EvalFilter {
                skip_eval: true,
                listener: Some(block_listener.erased()),
            });
            target.block_listen_gate = gate; // old gate is now dropped

            target.notifier.notify(BlockChange::new());
        }
        Ok(())
    }
}

impl transaction::Merge for BlockDefTransaction {
    type MergeCheck = ();
    type Conflict = transaction::TransactionConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        if matches!((&self.old, &other.old), (Some(a), Some(b)) if a != b) {
            return Err(transaction::TransactionConflict {});
        }
        if matches!((&self.new, &other.new), (Some(a), Some(b)) if a != b) {
            return Err(transaction::TransactionConflict {});
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
