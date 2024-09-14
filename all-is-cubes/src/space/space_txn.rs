use alloc::borrow::Cow;
use alloc::collections::BTreeMap;
use alloc::collections::btree_map::Entry::*;
use alloc::string::ToString;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;
use core::mem;

use bevy_ecs::prelude as ecs;
use descriptive_unwrap::OptionExt as _;
use hashbrown::HashMap as HbHashMap;

use crate::behavior::{self, BehaviorSetTransaction};
use crate::block::{self, Block};
use crate::fluff::Fluff;
use crate::inv;
use crate::math::{Cube, GridCoordinate, GridPoint, Vol};
use crate::space::{
    self, ActivatableRegion, GridAab, Mutation, PendingEvaluation, SetCubeError, Space,
};
use crate::transaction::{
    CommitError, Equal, ExecuteError, Merge, Transaction, Transactional, no_outputs,
};
use crate::universe::{self, ReadTicket};
use crate::util::{ConciseDebug, Refmt as _};

#[cfg(doc)]
use crate::behavior::BehaviorSet;

// -------------------------------------------------------------------------------------------------

impl Transactional for Space {
    type Transaction = SpaceTransaction;
}

/// A [`Transaction`] that modifies a [`Space`].
#[derive(Clone, Default, Eq, PartialEq)]
#[must_use]
pub struct SpaceTransaction {
    cubes: BTreeMap<[GridCoordinate; 3], CubeTransaction>,
    behaviors: BehaviorSetTransaction<Space>,
}

impl SpaceTransaction {
    /// Allows modifying the part of this transaction which is a [`CubeTransaction`] at the given
    /// cube, creating it if necessary (as [`CubeTransaction::default()`]).
    ///
    /// You can replace the transaction or use [`CubeTransaction::merge_from()`] to merge in
    /// another transaction.
    ///
    /// This is for incremental construction of a complex transaction;
    /// to create a transaction affecting a single cube, [`CubeTransaction::at()`] will be more
    /// convenient.
    pub fn at(&mut self, cube: Cube) -> &mut CubeTransaction {
        let cube: GridPoint = cube.into();
        self.cubes.entry(cube.into()).or_default()
    }

    /// Construct a [`SpaceTransaction`] which modifies a volume by applying a [`CubeTransaction`]
    /// computed by `function` to each cube.
    pub fn filling<F>(region: GridAab, mut function: F) -> Self
    where
        F: FnMut(Cube) -> CubeTransaction,
    {
        // TODO: Try having a compact `Vol<Box<[CubeTransaction]>>` representation for this kind of
        // transaction with uniformly shaped contents.
        let mut txn = SpaceTransaction::default();
        for cube in region.interior_iter() {
            *txn.at(cube) = function(cube);
        }
        txn
    }

    /// Construct a [`SpaceTransaction`] for a single cube.
    ///
    /// If `old` is not [`None`], requires that the existing block is that block or the
    /// transaction will fail.
    /// If `new` is not [`None`], replaces the existing block with `new`.
    ///
    /// TODO: Consider replacing all uses of this with `CubeTransaction::replacing()`.
    pub fn set_cube(cube: impl Into<Cube>, old: Option<Block>, new: Option<Block>) -> Self {
        CubeTransaction::replacing(old, new).at(cube.into())
    }

    /// Marks all cube modifications in this transaction as [non-conservative].
    ///
    /// This means that two transactions which both place the same block in a given cube
    /// may be merged, whereas the default state is that they will conflict (on the
    /// principle that such a merge could cause there to be fewer total occurrences of
    /// that block than intended).
    ///
    /// Also, the transaction will not fail if some of its cubes are outside the bounds of
    /// the [`Space`].
    ///
    /// [non-conservative]: https://en.wikipedia.org/wiki/Conserved_quantity
    pub fn nonconserved(mut self) -> Self {
        for cube_txn in self.cubes.values_mut() {
            cube_txn.conserved = false;
        }
        self
    }

    /// Modify the space's [`BehaviorSet`].
    pub fn behaviors(t: BehaviorSetTransaction<Space>) -> Self {
        Self {
            behaviors: t,
            ..Default::default()
        }
    }

    /// Add a behavior to the [`Space`].
    /// This is a shortcut for creating a [`BehaviorSetTransaction`].
    pub fn add_behavior<B>(bounds: GridAab, behavior: B) -> Self
    where
        B: behavior::Behavior<Space> + 'static,
    {
        Self::behaviors(BehaviorSetTransaction::insert(
            super::SpaceBehaviorAttachment::new(bounds),
            Arc::new(behavior),
        ))
    }

    /// Computes the region of cubes directly affected by this transaction.
    /// Ignores behaviors.
    ///
    /// Returns [`None`] if no cubes are affected.
    ///
    /// TODO: Handle the case where the total volume is too large.
    /// (Maybe `GridAab` should lose that restriction.)
    pub fn bounds_only_cubes(&self) -> Option<GridAab> {
        // Destructuring to statically check that we consider all fields.
        let Self {
            cubes,
            behaviors: _,
        } = self;
        let mut bounds: Option<GridAab> = None;

        for &cube_array in cubes.keys() {
            let cube = Cube::from(cube_array);
            if let Some(bounds) = &mut bounds {
                *bounds = (*bounds).union_cube(cube);
            } else {
                bounds = Some(GridAab::single_cube(cube));
            }
        }

        bounds
    }

    /// Computes the region affected by this transaction.
    ///
    /// Returns [`None`] if no specific regions of the space are affected.
    pub fn bounds(&self) -> Option<GridAab> {
        // Destructuring to statically check that we consider all fields.
        let Self {
            cubes: _,
            behaviors,
        } = self;
        let mut bounds: Option<GridAab> = self.bounds_only_cubes();

        for attachment in behaviors.attachments_affected() {
            if let Some(bounds) = &mut bounds {
                *bounds = (*bounds).union_box(attachment.bounds);
            } else {
                bounds = Some(attachment.bounds);
            }
        }

        bounds
    }

    /// Shared implementation of transaction traits.
    fn check_common(
        &self,
        read_ticket: ReadTicket<'_>,
        palette: &space::Palette,
        contents: Vol<&[space::BlockIndex]>,
        behaviors: &behavior::BehaviorSet<Space>,
    ) -> Result<CommitCheck, SpaceTransactionMismatch> {
        let mut overridden_new_blocks: Vec<(Cube, Block)> = Vec::new();
        let mut new_block_evaluations: HbHashMap<Block, PendingEvaluation> = HbHashMap::new();

        for (
            &cube,
            CubeTransaction {
                old,
                new,
                conserved,
                inventory,
                activate_behavior: _,
                fluff: _,
            },
        ) in &self.cubes
        {
            let cube = Cube::from(cube);
            let Some(cube_index) = contents.index(cube) else {
                // Out of bounds.
                if *conserved || old.0.is_some() {
                    // It is an error for conserved cube txns to be out of bounds,
                    // or for a precondition to be not meetable because it is out of bounds.
                    // TODO: Should we allow `old: Some(AIR), new: None`, since we treat
                    // outside-space as being AIR? Let's wait until a use case appears rather than
                    // making AIR more special.
                    return Err(SpaceTransactionMismatch::OutOfBounds {
                        transaction: cube.grid_aab(),
                        space: contents.bounds(),
                    });
                } else {
                    // Otherwise, apply the rest of the transaction without this change.
                    continue;
                }
            };

            // Fetch the block currently in the space, if we will need it.
            let old_in_space: Option<&space::SpaceBlockData> =
                if inventory.is_some() || old.0.is_some() {
                    Some(palette.entry(contents.as_linear()[cube_index]))
                } else {
                    None
                };

            // If there is an inventory transaction, execute it now to compute the new block.
            let (new_block_with_inventory_applied, inventory_to_ignore): (
                Option<Cow<'_, Block>>,
                Option<usize>,
            ) = if let Some(inventory_txn) = inventory {
                let old_block_in_space = old_in_space.as_ref().none_is_unreachable().block();
                if let Some((old_inventory_modifier_index, old_inventory)) =
                    old_block_in_space.find_inventory()
                {
                    let inventory_txn_check = inventory_txn
                        .check(old_inventory, ())
                        // TODO: carry error out for more informative mismatches
                        .map_err(|_inventory_mismatch| SpaceTransactionMismatch::Cube(cube))?;

                    // Commit the transaction, on a copy of the inventory.
                    // We must do this immediately because we have the responsibility to
                    // evaluate all new blocks, so we must compute the new block to evaluate.
                    let mut new_inventory = old_inventory.clone();
                    inventory_txn
                        .clone()
                        .commit(&mut new_inventory, inventory_txn_check, &mut drop)
                        .expect("TODO: make commit failure non-fatal");
                    let new_inventory_modifier = block::Modifier::Inventory(new_inventory);

                    // If this transaction replaces the block, add the inventory to that block.
                    // Otherwise, replace the inventory the old block must already have.
                    let mut new_block;
                    match new.0 {
                        Some(ref new_block_from_txn) => {
                            new_block =
                                new_block_from_txn.clone().with_modifier(new_inventory_modifier)
                        }
                        None => {
                            new_block = old_block_in_space.clone();
                            new_block.modifiers_mut()[old_inventory_modifier_index] =
                                new_inventory_modifier;
                        }
                    }

                    overridden_new_blocks.push((cube, Block::clone(&new_block)));

                    (
                        Some(Cow::Owned(new_block)),
                        Some(old_inventory_modifier_index),
                    )
                } else {
                    // If there is no inventory, then the transaction cannot apply.
                    return Err(SpaceTransactionMismatch::Cube(cube));
                }
            } else {
                (new.0.as_ref().map(Cow::Borrowed), None)
            };

            if let Equal(Some(old)) = old {
                let old_block_in_space = old_in_space.as_ref().none_is_unreachable().block();
                if !compare_blocks_ignoring_one_modifier(
                    old,
                    old_block_in_space,
                    inventory_to_ignore,
                ) {
                    return Err(SpaceTransactionMismatch::Cube(cube));
                }
            }

            // Evaluate the block that we’re going to insert.
            // TODO: This is a lot of possibly redundant hash lookups.
            // `SpaceTransaction`s should have palettes just like `Space`s so that they can do this
            // once per block rather than once per cube.
            if let Some(new_block) = new_block_with_inventory_applied {
                if let hashbrown::hash_map::Entry::Vacant(ve) =
                    new_block_evaluations.entry(Block::clone(&new_block))
                {
                    ve.insert(PendingEvaluation::new(
                        read_ticket,
                        palette,
                        Cow::into_owned(new_block),
                    ));
                }
            }
        }
        Ok(CommitCheck {
            behaviors: self
                .behaviors
                .check(behaviors, ())
                .map_err(SpaceTransactionMismatch::Behaviors)?,
            overridden_new_blocks,
            new_block_evaluations,
        })
    }

    fn commit_common(
        self,
        m: &mut Mutation<'_, '_>,
        check: CommitCheck,
    ) -> Result<(), CommitError> {
        let mut to_activate = Vec::new();
        let CommitCheck {
            behaviors: behaviors_check,
            mut new_block_evaluations,
            overridden_new_blocks,
        } = check;

        let mut overridden_new_blocks = overridden_new_blocks.into_iter().peekable();

        for (
            cube,
            CubeTransaction {
                old: _, // check_common() has checked this
                new,
                conserved,
                inventory: _, // check_common() has computed the inventory effect
                activate_behavior: activate,
                fluff,
            },
        ) in self.cubes
        {
            let cube = Cube::from(cube);

            let new_block: Option<Block> =
                if overridden_new_blocks.peek().is_some_and(|(ov_cube, _)| *ov_cube == cube) {
                    Some(overridden_new_blocks.next().none_is_unreachable().1)
                } else {
                    new.0
                };

            if let Some(new_block) = new_block {
                match Space::set_impl(
                    m,
                    cube,
                    &new_block,
                    new_block_evaluations.remove(&new_block),
                ) {
                    Ok(_) => Ok(()),
                    Err(SetCubeError::OutOfBounds { .. }) if !conserved => {
                        // ignore
                        Ok(())
                    }
                    Err(other) => Err(CommitError::catch::<Self, _>(other)),
                }?;
            }

            if activate {
                // Deferred for slightly more consistency
                to_activate.push(cube);
            }

            #[expect(
                clippy::shadow_unrelated,
                reason = "https://github.com/rust-lang/rust-clippy/issues/11827"
            )]
            for fluff in fluff.iter().cloned() {
                m.fluff_buffer.push(super::SpaceFluff {
                    position: cube,
                    fluff,
                });
            }
        }

        self.behaviors
            .commit(m.behaviors, behaviors_check, &mut no_outputs)
            .map_err(|e| e.context("behaviors".into()))?;

        if !to_activate.is_empty() {
            'b: for query_item in m.behaviors.query::<ActivatableRegion>() {
                // TODO: error return from the function? error report for nonexistence?
                for cube in to_activate.iter().copied() {
                    // TODO: this should be part of the query instead, to allow efficient search
                    if query_item.attachment.bounds.contains_cube(cube) {
                        query_item.behavior.activate();
                        continue 'b;
                    }
                }
            }
        }

        Ok(())
    }

    /// As [`Transaction::execute()`], but taking a [`Mutation`] instead of a [`Space`].
    // TODO: better name
    #[expect(clippy::missing_errors_doc, reason = "explicitly delegating")]
    pub fn execute_m(self, target: &mut Mutation<'_, '_>) -> Result<(), ExecuteError<Self>> {
        let check = self
            .check_common(
                target.read_ticket,
                target.palette,
                target.contents.as_ref(),
                target.behaviors,
            )
            .map_err(ExecuteError::Check)?;
        self.commit_common(target, check).map_err(ExecuteError::Commit)
    }
}

#[doc(hidden)] // would be impl Trait if we could
#[derive(Debug)]
pub struct CommitCheck {
    behaviors: <BehaviorSetTransaction<Space> as Transaction>::CommitCheck,
    new_block_evaluations: hashbrown::HashMap<Block, PendingEvaluation>,

    /// New blocks computed during check phase.
    /// Currently, this is filled only with cubes where the [`CubeTransaction`] has an inventory
    /// transaction. Otherwise, we use the `new` block already stored in the transaction.
    /// Always in the same iteration order as the transaction.
    overridden_new_blocks: Vec<(Cube, Block)>,
}

impl Transaction for SpaceTransaction {
    type Target = Space;
    type Context<'a> = ReadTicket<'a>;
    type CommitCheck = CommitCheck;
    type Output = !;
    type Mismatch = SpaceTransactionMismatch;

    fn check(
        &self,
        space: &Space,
        read_ticket: Self::Context<'_>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        self.check_common(
            read_ticket,
            &space.palette,
            space.contents.as_ref(),
            &space.behaviors,
        )
    }

    fn commit(
        self,
        space: &mut Space,
        check: Self::CommitCheck,
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError> {
        // Passing a stub read ticket here is OK because all of our block evaluations will have been
        // performed by check(). TODO: Can we make that statically checked?
        space.mutate(ReadTicket::stub(), |m| self.commit_common(m, check))
    }
}

impl universe::TransactionOnEcs for SpaceTransaction {
    type WriteQueryData = (
        &'static mut space::Palette,
        &'static mut space::Contents,
        &'static mut space::LightStorage,
        &'static mut space::BehaviorSet<Space>,
        &'static mut space::DefaultSpawn,
        &'static space::Notifiers,
        &'static mut space::Ticks,
    );

    fn check(
        &self,
        target: space::Read<'_>,
        read_ticket: ReadTicket<'_>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        self.check_common(
            read_ticket,
            target.palette,
            target.contents,
            target.behaviors,
        )
    }

    fn commit(
        self,
        target: (
            ecs::Mut<'_, space::Palette>,
            ecs::Mut<'_, space::Contents>,
            ecs::Mut<'_, space::LightStorage>,
            ecs::Mut<'_, space::BehaviorSet<Space>>,
            ecs::Mut<'_, space::DefaultSpawn>,
            &space::Notifiers,
            ecs::Mut<'_, space::Ticks>,
        ),
        check: Self::CommitCheck,
    ) -> Result<(), CommitError> {
        // Passing a stub read ticket here is OK because all of our block evaluations will have been
        // performed by check(). TODO: Can we make that statically checked?
        Mutation::with_write_query(ReadTicket::stub(), target, |m| self.commit_common(m, check))
    }
}
impl Merge for SpaceTransaction {
    type MergeCheck = impl fmt::Debug;
    type Conflict = SpaceTransactionConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        let mut cubes1 = &self.cubes;
        let mut cubes2 = &other.cubes;
        if cubes1.len() > cubes2.len() {
            // The cost of the check is the cost of iterating over keys, so iterate over
            // the smaller map rather than the larger.
            // TODO: We can improve further by taking advantage of sortedness, using the
            // first and last of one set to iterate over a range of the other.
            // alloc::collections::btree_set::Intersection implements something like this,
            // but unfortunately, does not have an analogue for BTreeMap.
            mem::swap(&mut cubes1, &mut cubes2);
        }
        for (&cube, t1) in cubes1 {
            if let Some(t2) = cubes2.get(&cube) {
                let CubeMergeCheck {} =
                    t1.check_merge(t2).map_err(|conflict| SpaceTransactionConflict::Cube {
                        cube: cube.into(),
                        conflict,
                    })?;
            }
        }
        self.behaviors
            .check_merge(&other.behaviors)
            .map_err(SpaceTransactionConflict::Behaviors)
    }

    fn commit_merge(&mut self, mut other: Self, check: Self::MergeCheck) {
        let Self { cubes, behaviors } = self;

        if other.cubes.len() > cubes.len() {
            // Whichever cube set is shorter, iterate that one
            mem::swap(cubes, &mut other.cubes);
        }
        for (cube, t2) in other.cubes {
            match cubes.entry(cube) {
                Occupied(mut entry) => {
                    entry.get_mut().commit_merge(t2, CubeMergeCheck {});
                }
                Vacant(entry) => {
                    entry.insert(t2);
                }
            }
        }

        behaviors.commit_merge(other.behaviors, check);
    }
}

impl fmt::Debug for SpaceTransaction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { cubes, behaviors } = self;
        let mut ds = fmt.debug_struct("SpaceTransaction");
        for (cube, txn) in cubes {
            ds.field(&Cube::from(*cube).refmt(&ConciseDebug).to_string(), txn);
        }
        if !behaviors.is_empty() {
            ds.field("behaviors", &behaviors);
        }
        ds.finish()
    }
}

impl universe::VisitHandles for SpaceTransaction {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self { cubes, behaviors } = self;
        for cube_txn in cubes.values() {
            cube_txn.visit_handles(visitor);
        }
        behaviors.visit_handles(visitor);
    }
}

// -------------------------------------------------------------------------------------------------

/// Transaction precondition error type for a [`SpaceTransaction`].
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum SpaceTransactionMismatch {
    #[allow(missing_docs)]
    Cube(Cube),

    /// The transaction tried to modify something outside of the space bounds.
    OutOfBounds {
        /// Bounds within which the transaction attempted to make a change.
        /// (This is not necessarily equal to [`SpaceTransaction::bounds()`])
        transaction: GridAab,

        /// Bounds of the space.
        space: GridAab,
    },

    #[allow(missing_docs)]
    Behaviors(behavior::BehaviorTransactionMismatch),
}

/// Transaction conflict error type for a [`SpaceTransaction`].
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum SpaceTransactionConflict {
    #[allow(missing_docs)]
    Cube {
        cube: Cube, // TODO: GridAab instead?
        conflict: CubeConflict,
    },
    #[allow(missing_docs)]
    Behaviors(behavior::BehaviorTransactionConflict),
}

impl core::error::Error for SpaceTransactionMismatch {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            SpaceTransactionMismatch::Cube(_) => None,
            SpaceTransactionMismatch::OutOfBounds { .. } => None,
            SpaceTransactionMismatch::Behaviors(mismatch) => Some(mismatch),
        }
    }
}
impl core::error::Error for SpaceTransactionConflict {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            SpaceTransactionConflict::Cube { conflict, .. } => Some(conflict),
            SpaceTransactionConflict::Behaviors(conflict) => Some(conflict),
        }
    }
}

impl fmt::Display for SpaceTransactionMismatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SpaceTransactionMismatch::Cube(cube) => {
                write!(f, "mismatch at cube {c}", c = cube.refmt(&ConciseDebug))
            }

            SpaceTransactionMismatch::OutOfBounds { transaction, space } => {
                // TODO: don't use Debug formatting here — we'll need to decide what Display formatting for an AAB is
                write!(
                    f,
                    "transaction bounds {transaction:?} exceed space bounds {space:?}"
                )
            }
            SpaceTransactionMismatch::Behaviors(_) => write!(f, "in behaviors"),
        }
    }
}
impl fmt::Display for SpaceTransactionConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SpaceTransactionConflict::Cube { cube, conflict: _ } => {
                write!(f, "conflict at cube {c}", c = cube.refmt(&ConciseDebug))
            }
            SpaceTransactionConflict::Behaviors(_) => write!(f, "conflict in behaviors"),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// A modification to the contents of single cube of a [`Space`].
///
/// To make use of this, insert it into a [`SpaceTransaction`] to specify _which_ cube is
/// modified. This type does not function directly as a [`Transaction`] (though it does
/// implement [`Merge`]).
#[derive(Clone, Eq, PartialEq)]
pub struct CubeTransaction {
    /// Previous block which must occupy this cube.
    /// If `None`, no precondition.
    old: Equal<Block>,

    /// Block to be put in this cube.
    /// If `None`, this is only a precondition for modifying another block.
    new: Equal<Block>,

    /// If true, two transactions with the same `new` block may not be merged.
    conserved: bool,

    /// Transaction applied to the inventory of this block.
    ///
    /// If this is [`Some`] then, regardless of what the inventory transaction itself does,
    /// the `old` and `new` fields operate on the block as if its `Modifier::Inventory` were absent.
    ///
    /// For example, `inventory: Some(InventoryTransaction::default())` allows replacing the block
    /// while carrying over the contents of its inventory.
    inventory: Option<inv::InventoryTransaction>,

    /// The cube was “activated” (clicked on, more or less) and behaviors attached to
    /// that region of space should respond to that.
    activate_behavior: bool,

    /// [`Fluff`] to emit at this location when the transaction is committed.
    ///
    /// TODO: eventually will need rotation and possibly intra-cube positioning.
    ///
    /// TODO: define a merge ordering. should this be a multi-BTreeSet?
    ///
    /// TODO: Allow having a single entry with no allocation?
    fluff: Vec<Fluff>,
}

impl Default for CubeTransaction {
    fn default() -> Self {
        Self {
            old: Equal(None),
            new: Equal(None),
            conserved: false,
            inventory: None,
            activate_behavior: false,
            fluff: Vec::new(),
        }
    }
}

impl !Transaction for CubeTransaction {}

impl fmt::Debug for CubeTransaction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            old: Equal(old),
            new: Equal(new),
            conserved,
            inventory,
            activate_behavior,
            fluff,
        } = self;
        let mut ds = f.debug_struct("CubeTransaction");
        if old.is_some() || new.is_some() {
            ds.field("old", &old);
            ds.field("new", &new);
            ds.field("conserved", &conserved);
        }
        if *activate_behavior {
            ds.field("activate_behavior", &activate_behavior);
        }
        // None is significant, so print it always
        ds.field("inventory", &inventory);
        if !fluff.is_empty() {
            ds.field("fluff", &fluff);
        }
        ds.finish()
    }
}

impl CubeTransaction {
    /// Creates a [`SpaceTransaction`] that applies `self` to the given cube of the space.
    pub fn at(self, cube: Cube) -> SpaceTransaction {
        SpaceTransaction {
            cubes: BTreeMap::from([(<[i32; 3]>::from(cube), self)]),
            ..Default::default()
        }
    }

    // pub for testing only; the normal way to use this is `Tool::Activate`.
    #[doc(hidden)]
    pub const ACTIVATE_BEHAVIOR: Self = Self {
        old: Equal(None),
        new: Equal(None),
        conserved: false,
        inventory: None,
        activate_behavior: true,
        fluff: Vec::new(),
    };

    /// Construct a [`CubeTransaction`] that may check and may replace the block in the cube.
    ///
    /// If `old` is not [`None`], requires that the existing block is that block or the
    /// transaction will fail.
    /// If `new` is not [`None`], replaces the existing block with `new`.
    pub fn replacing(old: Option<Block>, new: Option<Block>) -> Self {
        CubeTransaction {
            old: Equal(old),
            new: Equal(new),
            conserved: true,
            ..Default::default()
        }
    }

    /// Construct a [`CubeTransaction`] that modifies the inventory of the block in the cube.
    pub fn inventory(inventory_transaction: inv::InventoryTransaction) -> Self {
        CubeTransaction {
            inventory: Some(inventory_transaction),
            ..Default::default()
        }
    }

    /// Add a transaction on the inventory of the block in this cube.
    ///
    /// In addition to manipulating the inventory as specified by `inventory_transaction`,
    /// this causes the transaction's other operations to act on the block as if it did not
    /// have a [`block::Modifier::Inventory`] attached, and to fail if one is not present.
    ///
    /// # Panics
    ///
    /// Panics if this transaction already includes an inventory transaction.
    #[track_caller]
    #[must_use]
    pub fn with_inventory(mut self, inventory_transaction: inv::InventoryTransaction) -> Self {
        match self.inventory {
            None => self.inventory = Some(inventory_transaction),
            Some(_) => panic!(
                "with_inventory may only be used without an inventory transaction already present"
            ),
        }
        self
    }

    /// Sets the block to be placed at this cube, replacing any existing modification instruction
    /// This does not affect a precondition on the existing block, or the conservative option.
    ///
    /// This is thus comparable to the effect of a direct [`Mutation::set()`] after the rest of the
    /// transaction.
    //---
    // TODO: no tests
    pub fn overwrite(&mut self, block: Block) {
        self.new = Equal(Some(block));
    }

    #[doc(hidden)] // TODO: good public API?
    pub fn new_mut(&mut self) -> Option<&mut Block> {
        self.new.0.as_mut()
    }

    /// Emit [`Fluff`] (sound/particle effects) at this cube when the transaction is committed.
    pub fn fluff(fluff: Fluff) -> Self {
        let mut this = Self::default();
        this.add_fluff(fluff);
        this
    }

    /// Emit [`Fluff`] (sound/particle effects) at this cube when the transaction is committed,
    /// in addition to its other effects.
    pub fn add_fluff(&mut self, fluff: Fluff) {
        self.fluff.push(fluff)
    }
}

impl Merge for CubeTransaction {
    /// Not opaque because [`SpaceTransaction`] uses it
    type MergeCheck = CubeMergeCheck;
    type Conflict = CubeConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        // Transactions which check/replace the block can only be merged if the other transaction
        // has the same choice of ignoring or including the inventory.
        // TODO: We should relax this, once we figure out a policy that doesn’t lead to any
        // undesirable merges. (Maybe the `inventory` field should have 3 states including a
        // "don't care"?)
        let ignore_inventory_conflict = (self.old.0.is_some()
            || other.old.0.is_some()
            || self.new.0.is_some()
            || other.new.0.is_some())
            && self.inventory.is_some() != other.inventory.is_some();

        // Incompatible preconditions will always fail.
        let old_conflict = self.old.check_merge(&other.old).is_err();

        let new_conflict = if self.conserved {
            // Replacing the same cube twice is not allowed -- even if they're
            // equal, doing so could violate an intended conservation law.
            self.new.0.is_some() && other.new.0.is_some()
        } else {
            // If nonconservative, then we simply require equal outcomes.
            self.new.check_merge(&other.new).is_err()
        };

        let _inventory_check: Option<()> = self
            .inventory
            .as_ref()
            .zip(other.inventory.as_ref())
            .map(|(a, b)| a.check_merge(b))
            .transpose()
            .map_err(|e| CubeConflict {
                old: old_conflict,
                new: new_conflict,
                inventory: Some(e),
                ignore_inventory_conflict,
            })?;

        if old_conflict || new_conflict || ignore_inventory_conflict {
            return Err(CubeConflict {
                old: old_conflict,
                new: new_conflict,
                inventory: None,
                ignore_inventory_conflict,
            });
        }

        Ok(CubeMergeCheck {})
    }

    fn commit_merge(&mut self, other: Self, CubeMergeCheck {}: Self::MergeCheck) {
        let Self {
            old,
            new,
            conserved,
            inventory,
            activate_behavior,
            fluff,
        } = self;

        // This would be more elegant if `conserved` was within the `self.new` Option.
        *conserved = (*conserved && new.0.is_some()) || (other.conserved && other.new.0.is_some());

        old.commit_merge(other.old, ());
        new.commit_merge(other.new, ());

        match (inventory.as_mut(), other.inventory) {
            (None, other) => *inventory = other,
            (Some(_), None) => {}
            (Some(our_inv), Some(other_inv)) => our_inv.commit_merge(other_inv, ()),
        }

        *activate_behavior |= other.activate_behavior;

        fluff.extend(other.fluff);
    }
}

impl universe::VisitHandles for CubeTransaction {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            old,
            new,
            conserved: _,
            inventory,
            activate_behavior: _,
            fluff,
        } = self;

        old.visit_handles(visitor);
        new.visit_handles(visitor);
        inventory.visit_handles(visitor);
        fluff.visit_handles(visitor);
    }
}

impl From<inv::InventoryTransaction> for CubeTransaction {
    fn from(inventory_transaction: inv::InventoryTransaction) -> Self {
        CubeTransaction::inventory(inventory_transaction)
    }
}

// -------------------------------------------------------------------------------------------------

#[doc(hidden)]
#[derive(Debug)]
#[non_exhaustive]
pub struct CubeMergeCheck {}

// -------------------------------------------------------------------------------------------------

/// Transaction conflict error type for a single [`CubeTransaction`] within a
/// [`SpaceTransaction`].
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct CubeConflict {
    /// The transactions have conflicting preconditions (`old` blocks).
    pub(crate) old: bool,
    /// The transactions are attempting to modify the same cube.
    pub(crate) new: bool,
    /// The transactions are attempting to make incompatible inventory changes.
    pub(crate) inventory: Option<inv::InventoryConflict>,
    /// The transactions disagree on whether to compare blocks ignoring inventory contents.
    pub(crate) ignore_inventory_conflict: bool,
}

impl core::error::Error for CubeConflict {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            Self {
                old: _,
                new: _,
                inventory: Some(inventory),
                ignore_inventory_conflict: _,
            } => Some(inventory),
            _ => None,
        }
    }
}

impl fmt::Display for CubeConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            CubeConflict {
                old: true,
                new: false,
                inventory: _,
                ignore_inventory_conflict: _,
            } => write!(f, "different block preconditions"),
            CubeConflict {
                old: false,
                new: true,
                inventory: _,
                ignore_inventory_conflict: _,
            } => write!(f, "cannot write the same cube twice"),
            CubeConflict {
                old: true,
                new: true,
                inventory: _,
                ignore_inventory_conflict: _,
            } => write!(f, "different block preconditions (with write)"),
            CubeConflict {
                old: false,
                new: false,
                inventory: Some(_),
                ignore_inventory_conflict: _,
            } => write!(f, "conflicting inventory changes"),
            CubeConflict {
                old: false,
                new: false,
                inventory: None,
                ignore_inventory_conflict: true,
            } => write!(f, "disagreement on whether to ignore inventory"),
            CubeConflict {
                old: false,
                new: false,
                inventory: None,
                ignore_inventory_conflict: false,
            } => unreachable!(),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Compare two [`Block`]s, treating `b` as if the modifier at index `ignore_in_b` was deleted,
/// or performing a normal comparison if `ignore_in_b` is [`None`].
///
/// # Panics
///
/// Panics if `ignore_in_b` is out of bounds of `b.modifiers()`.
fn compare_blocks_ignoring_one_modifier(a: &Block, b: &Block, ignore_in_b: Option<usize>) -> bool {
    let Some(ignore_in_b) = ignore_in_b else {
        return a == b;
    };
    if a.primitive() != b.primitive() {
        return false;
    }
    let am = a.modifiers();
    let bm = b.modifiers();
    am.len() == bm.len() - 1
        && am[..ignore_in_b] == bm[..ignore_in_b]
        && am[ignore_in_b..] == bm[ignore_in_b + 1..]
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::behavior::NoopBehavior;
    use crate::block::{self, AIR};
    use crate::content::make_some_blocks;
    use crate::transaction::TransactionTester;
    use crate::universe::EphemeralOpaque;
    use all_is_cubes_base::resolution::Resolution::R1;
    use alloc::vec;
    use core::sync::atomic::{AtomicU32, Ordering};
    use pretty_assertions::assert_eq;

    #[test]
    fn set_out_of_bounds_conserved_fails() {
        let [block] = make_some_blocks();
        // Note: by using .check() we validate that it doesn't fail in the commit phase
        SpaceTransaction::set_cube([1, 0, 0], None, Some(block))
            .check(&Space::empty_positive(1, 1, 1), ReadTicket::stub())
            .unwrap_err();
    }

    #[test]
    fn set_out_of_bounds_nonconserved_succeeds() {
        let [block] = make_some_blocks();
        SpaceTransaction::set_cube([1, 0, 0], None, Some(block))
            .nonconserved()
            .execute(
                &mut Space::empty_positive(1, 1, 1),
                ReadTicket::stub(),
                &mut no_outputs,
            )
            .unwrap();
    }

    #[test]
    fn compare_out_of_bounds_conserved_fails() {
        let [block] = make_some_blocks();
        SpaceTransaction::set_cube([1, 0, 0], Some(block), None)
            .check(&Space::empty_positive(1, 1, 1), ReadTicket::stub())
            .unwrap_err();
    }

    #[test]
    fn compare_out_of_bounds_nonconserved_fails() {
        let [block] = make_some_blocks();
        SpaceTransaction::set_cube([1, 0, 0], Some(block), None)
            .nonconserved()
            .check(&Space::empty_positive(1, 1, 1), ReadTicket::stub())
            .unwrap_err();
    }

    /// Test that when a block is modified during the check phase of the transaction and the
    /// commit phase — as might happen if the transaction also modifies a block definition —
    /// the space knows that it must reevaluate the block, even though it was evaluated during
    /// the check phase.
    ///
    /// Also test the outcome when there is no such change.
    #[macro_rules_attribute::apply(crate::util::cartesian_product_test)]
    fn block_changed_between_check_and_commit(
        #[case(no_change = false)]
        #[case(changed = true)]
        actually_change: bool,
    ) {
        let mut u = universe::Universe::new();
        let [initial_block_value, final_block_value] = make_some_blocks();
        let block_handle = u.insert_anonymous(block::BlockDef::new(
            u.read_ticket(),
            initial_block_value.clone(),
        ));
        let indirect_block = Block::from(block_handle.clone());
        let mut space = Space::builder(GridAab::ORIGIN_CUBE).build();

        let space_txn =
            SpaceTransaction::set_cube([0, 0, 0], Some(AIR), Some(indirect_block.clone()));
        let check = space_txn.check(&space, u.read_ticket()).unwrap();
        if actually_change {
            u.execute_1(
                &block_handle,
                block::BlockDefTransaction::overwrite(final_block_value.clone()),
            )
            .unwrap();
        }
        space_txn.commit(&mut space, check, &mut no_outputs).unwrap();

        assert_eq!(
            space.get_evaluated([0, 0, 0]).color(),
            initial_block_value.color(),
            "evaluation should always be from check time, regardless of change"
        );
        assert_eq!(
            space.palette.block_is_to_be_reevaluated(&indirect_block),
            actually_change,
            "space todo ≠ actual change"
        );
    }

    #[test]
    fn merge_allows_independent() {
        let [b1, b2, b3] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], Some(b1.clone()), Some(b2.clone()));
        let t2 = SpaceTransaction::set_cube([1, 0, 0], Some(b1.clone()), Some(b3.clone()));
        let t3 = t1.clone().merge(t2.clone()).unwrap();
        assert_eq!(
            Vec::from_iter(t3.cubes),
            [
                (
                    [0, 0, 0],
                    CubeTransaction {
                        old: Equal(Some(b1.clone())),
                        new: Equal(Some(b2.clone())),
                        conserved: true,
                        inventory: None,
                        activate_behavior: false,
                        fluff: vec![],
                    }
                ),
                (
                    [1, 0, 0],
                    CubeTransaction {
                        old: Equal(Some(b1.clone())),
                        new: Equal(Some(b3.clone())),
                        conserved: true,
                        inventory: None,
                        activate_behavior: false,
                        fluff: vec![],
                    }
                ),
            ]
        );
    }

    #[test]
    fn merge_rejects_same_new_conserved() {
        let [block] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block.clone()));
        let t2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block.clone()));
        t1.merge(t2).unwrap_err();
    }

    #[test]
    fn merge_allows_same_new_nonconserved() {
        let [old, new] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], Some(old), Some(new.clone())).nonconserved();
        let t2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(new.clone())).nonconserved();
        assert_eq!(t1.clone().merge(t2).unwrap(), t1);
    }

    #[test]
    fn merge_rejects_different_new_conserved() {
        let [b1, b2] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], None, Some(b1.clone()));
        let t2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(b2.clone()));
        t1.merge(t2).unwrap_err();
    }

    #[test]
    fn merge_rejects_different_new_nonconserved() {
        let [b1, b2] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], None, Some(b1.clone())).nonconserved();
        let t2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(b2.clone())).nonconserved();
        t1.merge(t2).unwrap_err();
    }

    #[test]
    fn merge_rejects_different_old() {
        let [b1, b2] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], Some(b1.clone()), None);
        let t2 = SpaceTransaction::set_cube([0, 0, 0], Some(b2.clone()), None);
        t1.merge(t2).unwrap_err();
    }

    #[test]
    fn merge_allows_same_old() {
        let [b1, b2] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], Some(b1.clone()), Some(b2.clone()));
        let t2 = SpaceTransaction::set_cube([0, 0, 0], Some(b1.clone()), None);
        assert_eq!(t1.clone(), t1.clone().merge(t2).unwrap());
    }

    #[test]
    fn activate() {
        let mut space = Space::empty_positive(1, 1, 1);
        let cube = Cube::new(0, 0, 0);

        let signal = Arc::new(AtomicU32::new(0));
        SpaceTransaction::add_behavior(
            GridAab::single_cube(cube),
            ActivatableRegion {
                // TODO: This sure is clunky
                effect: EphemeralOpaque::new(Arc::new({
                    let signal = signal.clone();
                    move || {
                        signal.fetch_add(1, Ordering::Relaxed);
                    }
                })),
            },
        )
        .execute(&mut space, ReadTicket::stub(), &mut no_outputs)
        .unwrap();

        CubeTransaction::ACTIVATE_BEHAVIOR
            .at(cube)
            .execute(&mut space, ReadTicket::stub(), &mut drop)
            .unwrap();
        assert_eq!(signal.load(Ordering::Relaxed), 1);
    }

    /// Helper for tests which apply an inventory transaction to a block.
    fn test_inventory_transaction(
        initial_block: Block,
        txn: CubeTransaction,
    ) -> (Space, Result<(), ExecuteError<SpaceTransaction>>) {
        let mut space = Space::builder(GridAab::ORIGIN_CUBE).filled_with(initial_block).build();

        let result = txn.at(Cube::ORIGIN).execute(&mut space, ReadTicket::stub(), &mut no_outputs);
        (space, result)
    }

    fn make_inventory_host_blocks<const N: usize>(slot_count: u16) -> [Block; N] {
        make_some_blocks()
            .map(|block| block.with_modifier(inv::InvInBlock::new(slot_count, R1, R1, [])))
    }

    #[test]
    fn inventory_txn_only() {
        let [host_block] = make_inventory_host_blocks(1);
        let txn = CubeTransaction::inventory(inv::InventoryTransaction::insert([inv::Slot::one(
            inv::Tool::Activate,
        )]));

        let (space, result) = test_inventory_transaction(
            host_block.clone().with_modifier(inv::Inventory::new(1)),
            txn,
        );
        result.unwrap();

        assert_eq!(
            space[Cube::ORIGIN],
            host_block.with_modifier(inv::Inventory::from_slots([inv::Slot::one(
                inv::Tool::Activate
            )]))
        );
    }

    /// TODO: Unclear whether this should succeed or fail. For now, it fails.
    #[test]
    fn inventory_txn_on_block_without_inventory() {
        let [host_block] = make_inventory_host_blocks(1);
        let txn = CubeTransaction::inventory(inv::InventoryTransaction::insert([inv::Slot::one(
            inv::Tool::Activate,
        )]));

        let (_space, result) = test_inventory_transaction(host_block.clone(), txn);

        assert_eq!(
            result.unwrap_err(),
            ExecuteError::Check(SpaceTransactionMismatch::Cube(Cube::ORIGIN))
        );
    }

    #[test]
    fn inventory_txn_also_changing_block() {
        let [block1, block2] = make_inventory_host_blocks(1);
        let txn = CubeTransaction::replacing(Some(block1.clone()), Some(block2.clone()))
            .with_inventory(inv::InventoryTransaction::insert([inv::Slot::one(
                inv::Tool::Activate,
            )]));

        let (space, result) =
            test_inventory_transaction(block1.with_modifier(inv::Inventory::new(1)), txn);
        result.unwrap();

        assert_eq!(
            space[Cube::ORIGIN],
            block2.with_modifier(inv::Inventory::from_slots([inv::Slot::one(
                inv::Tool::Activate
            )]))
        );
    }

    #[test]
    fn inventory_txn_with_block_precondition_failing() {
        let [block1, block2] = make_inventory_host_blocks(1);
        let txn = CubeTransaction::replacing(
            Some(block2.clone()), // not the same block as in the space
            None,
        )
        .with_inventory(inv::InventoryTransaction::insert([inv::Slot::one(
            inv::Tool::Activate,
        )]));

        let (_space, result) =
            test_inventory_transaction(block1.with_modifier(inv::Inventory::new(1)), txn);

        assert_eq!(
            result.unwrap_err(),
            ExecuteError::Check(SpaceTransactionMismatch::Cube(Cube::ORIGIN))
        );
    }

    #[test]
    fn inventory_txn_with_block_precondition_passing() {
        let [host_block] = make_inventory_host_blocks(1);
        let txn = CubeTransaction::replacing(Some(host_block.clone()), None).with_inventory(
            inv::InventoryTransaction::insert([inv::Slot::one(inv::Tool::Activate)]),
        );

        let (space, result) = test_inventory_transaction(
            host_block.clone().with_modifier(inv::Inventory::new(1)),
            txn,
        );
        result.unwrap();

        assert_eq!(
            space[Cube::ORIGIN],
            host_block.with_modifier(inv::Inventory::from_slots([inv::Slot::one(
                inv::Tool::Activate
            )]))
        );
    }

    #[test]
    fn inventory_txn_conflict() {
        let txn1 = CubeTransaction::inventory(inv::InventoryTransaction::replace(
            0,
            inv::Slot::Empty,
            inv::Slot::Empty,
        ));
        let txn2 = CubeTransaction::inventory(inv::InventoryTransaction::replace(
            0,
            inv::Slot::stack(1, inv::Tool::Activate),
            inv::Slot::stack(1, inv::Tool::Activate),
        ));

        let error = txn1.merge(txn2).unwrap_err();

        assert_eq!(
            error,
            CubeConflict {
                old: false,
                new: false,
                inventory: Some(inv::InventoryConflict::ReplaceSameSlot { slot: 0 }),
                ignore_inventory_conflict: false,
            }
        );
    }

    #[test]
    fn systematic() {
        use crate::inv::InventoryTransaction as It;
        use crate::space::SpaceTransaction as St;
        use crate::transaction::{assert, noop_predicate};

        let [b1, b2, b3] = make_some_blocks();
        TransactionTester::new()
            .transaction(St::default(), noop_predicate)
            .transaction(
                St::set_cube([0, 0, 0], Some(b1.clone()), Some(b2.clone())),
                |_, after| assert(after[[0, 0, 0]] == b2, "did not set b2 (with precondition)"),
            )
            .transaction(
                St::set_cube([0, 0, 0], Some(b1.clone()), Some(b3.clone())),
                |_, after| assert(after[[0, 0, 0]] == b3, "did not set b3 (with precondition)"),
            )
            .transaction(
                St::set_cube([0, 0, 0], None, Some(b2.clone())),
                |_, after| assert(after[[0, 0, 0]] == b2, "did not set b2 (no precondition)"),
            )
            .transaction(
                St::set_cube([0, 0, 0], Some(b2.clone()), None),
                |before, _| assert(before[[0, 0, 0]] == b2, "succeeded without finding b2"),
            )
            .transaction(
                St::set_cube([0, 0, 0], Some(b1.clone()), None),
                |before, _| assert(before[[0, 0, 0]] == b1, "succeeded without finding b1"),
            )
            .transaction(
                CubeTransaction::ACTIVATE_BEHAVIOR.at(Cube::new(0, 0, 0)),
                // TODO: Add a test that activation happened once that's possible
                noop_predicate,
            )
            .transaction(
                CubeTransaction::ACTIVATE_BEHAVIOR.at(Cube::new(1, 0, 0)),
                // TODO: Add a test that activation happened once that's possible
                noop_predicate,
            )
            .transaction(
                CubeTransaction::inventory(It::default()).at(Cube::new(0, 0, 0)),
                noop_predicate,
            )
            .transaction(
                CubeTransaction::inventory(It::insert([inv::Tool::CopyFromSpace]))
                    .at(Cube::new(0, 0, 0)),
                noop_predicate,
            )
            .transaction(
                CubeTransaction::inventory(It::insert([inv::Tool::Activate]))
                    .at(Cube::new(0, 0, 0)),
                noop_predicate,
            )
            .transaction(
                CubeTransaction::inventory(It::replace(
                    0,
                    inv::Slot::Empty,
                    inv::Tool::Activate.into(),
                ))
                .at(Cube::new(0, 0, 0)),
                noop_predicate,
            )
            .transaction(
                CubeTransaction::inventory(It::replace(
                    0,
                    inv::Tool::Activate.into(),
                    inv::Slot::Empty,
                ))
                .at(Cube::new(0, 0, 0)),
                noop_predicate,
            )
            .target(|| Space::empty_positive(2, 1, 1))
            .target(|| {
                Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
                    .build_and_mutate(|m| {
                        m.set([0, 0, 0], &b1)?;
                        Ok(())
                    })
                    .unwrap()
            })
            .target(|| {
                Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
                    .build_and_mutate(|m| {
                        m.set([0, 0, 0], &b2)?;
                        Ok(())
                    })
                    .unwrap()
            })
            .target(|| {
                // This space makes the test transactions at [0, 0, 0] out of bounds
                Space::empty(GridAab::from_lower_size([1, 0, 0], [1, 1, 1]))
            })
            .target(|| {
                Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
                    .filled_with(
                        b1.clone().with_modifier(inv::Inventory::from_slots([inv::Slot::Empty])),
                    )
                    .build()
            })
            .target(|| {
                Space::builder(GridAab::from_lower_size([0, 0, 0], [2, 1, 1]))
                    .filled_with(
                        b1.clone().with_modifier(inv::Inventory::from_slots([
                            inv::Tool::Activate.into()
                        ])),
                    )
                    .build()
            })
            // TODO: more spaces
            .test(ReadTicket::stub());
    }

    #[test]
    fn bounds_empty() {
        assert_eq!(SpaceTransaction::default().bounds(), None);
    }

    #[test]
    fn bounds_single_cube() {
        assert_eq!(
            SpaceTransaction::set_cube([-7, 3, 5], None, Some(AIR)).bounds(),
            Some(GridAab::single_cube(Cube::new(-7, 3, 5)))
        );
    }

    #[test]
    fn bounds_multi_cube() {
        let t1 = SpaceTransaction::set_cube([-7, 3, 5], None, Some(AIR));
        let t2 = SpaceTransaction::set_cube([10, 3, 5], None, Some(AIR));
        assert_eq!(
            t1.merge(t2).unwrap().bounds(),
            Some(GridAab::from_lower_upper([-7, 3, 5], [11, 4, 6]))
        );
    }

    #[test]
    fn bounds_behavior() {
        let bounds = GridAab::from_lower_size([1, 2, 3], [4, 5, 6]);
        let txn = SpaceTransaction::add_behavior(bounds, NoopBehavior(1));
        assert_eq!(txn.bounds(), Some(bounds));
    }
}
