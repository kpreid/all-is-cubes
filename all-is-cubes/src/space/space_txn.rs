//! TODO: Maybe this file is too small

use all_is_cubes_base::math::Vol;
use alloc::collections::BTreeMap;
use alloc::collections::btree_map::Entry::*;
use alloc::string::ToString;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::{fmt, mem};

use crate::behavior::{self, BehaviorSetTransaction};
use crate::block::Block;
use crate::drawing::DrawingPlane;
use crate::fluff::Fluff;
use crate::math::{Cube, GridCoordinate, GridPoint, Gridgid};
use crate::space::{self, ActivatableRegion, GridAab, Mutation, SetCubeError, Space};
use crate::transaction::{
    CommitError, Equal, ExecuteError, Merge, NoOutput, Transaction, Transactional, no_outputs,
};
use crate::universe::{self, ReadTicket};
use crate::util::{ConciseDebug, Refmt as _};

#[cfg(doc)]
use crate::behavior::BehaviorSet;

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

    /// Provides an [`DrawTarget`](embedded_graphics::prelude::DrawTarget)
    /// adapter for 2.5D drawing.
    ///
    /// For more information on how to use this, see
    /// [`all_is_cubes::drawing`](crate::drawing).
    pub fn draw_target<C>(&mut self, transform: Gridgid) -> DrawingPlane<'_, Self, C> {
        DrawingPlane::new(self, transform)
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
        for (_, cube_txn) in self.cubes.iter_mut() {
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

    /// As [`SpaceTransaction::check()`], but does not require borrowing the whole `Space`.
    ///
    /// This cannot practically be a [`Transaction`] implementation due to the lifetime
    /// parameters. TODO: Consider if there is a `Transaction` redesign to be had here.
    fn check_common(
        &self,
        palette: &space::Palette,
        contents: Vol<&[space::BlockIndex]>,
        behaviors: &behavior::BehaviorSet<Space>,
    ) -> Result<<BehaviorSetTransaction<Space> as Transaction>::CommitCheck, SpaceTransactionMismatch>
    {
        for (
            &cube,
            CubeTransaction {
                old,
                new: _,
                conserved,
                activate_behavior: _,
                fluff: _,
            },
        ) in &self.cubes
        {
            let cube = Cube::from(cube);
            if let Some(cube_index) = contents.index(cube) {
                if let Equal(Some(old)) = old {
                    // Raw lookup because we already computed the index for a bounds check
                    // (TODO: Put this in a function, like get_block_index)
                    if palette.entry(contents.as_linear()[cube_index]).block() != old {
                        return Err(SpaceTransactionMismatch::Cube(cube));
                    }
                }
            } else {
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
                }
            }
        }
        self.behaviors
            .check(behaviors)
            .map_err(SpaceTransactionMismatch::Behaviors)
    }

    fn commit_common(
        &self,
        m: &mut Mutation<'_, '_>,
        check: behavior::CommitCheck,
    ) -> Result<(), CommitError> {
        let mut to_activate = Vec::new();

        for (
            &cube,
            CubeTransaction {
                old: _,
                new,
                conserved,
                activate_behavior: activate,
                fluff,
            },
        ) in &self.cubes
        {
            let cube = Cube::from(cube);

            if let Equal(Some(new)) = new {
                match Space::set_impl(m, cube, new) {
                    Ok(_) => Ok(()),
                    Err(SetCubeError::OutOfBounds { .. }) if !conserved => {
                        // ignore
                        Ok(())
                    }
                    Err(other) => Err(CommitError::catch::<Self, _>(other)),
                }?;
            }

            if *activate {
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
            .commit(m.behaviors, (), check, &mut no_outputs)
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
    pub fn execute_m(&self, target: &mut Mutation<'_, '_>) -> Result<(), ExecuteError<Self>> {
        let check = self
            .check_common(target.palette, target.contents.as_ref(), target.behaviors)
            .map_err(ExecuteError::Check)?;
        self.commit_common(target, check)
            .map_err(ExecuteError::Commit)
    }
}

impl Transaction for SpaceTransaction {
    type Target = Space;
    type Context<'a> = ReadTicket<'a>;
    type CommitCheck = <BehaviorSetTransaction<Space> as Transaction>::CommitCheck;
    type Output = NoOutput;
    type Mismatch = SpaceTransactionMismatch;

    fn check(&self, space: &Space) -> Result<Self::CommitCheck, Self::Mismatch> {
        self.check_common(&space.palette, space.contents.as_ref(), &space.behaviors)
    }

    fn commit(
        &self,
        space: &mut Space,
        context: Self::Context<'_>,
        check: Self::CommitCheck,
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError> {
        space.mutate(context, |m| self.commit_common(m, check))
    }
}

impl Merge for SpaceTransaction {
    type MergeCheck = <BehaviorSetTransaction<Space> as Merge>::MergeCheck;
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
                    t1.check_merge(t2)
                        .map_err(|conflict| SpaceTransactionConflict::Cube {
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

/// A modification to the contents of single cube of a [`Space`].
///
/// To make use of this, insert it into a [`SpaceTransaction`] to specify _which_ cube is
/// modified. This type does not function directly as a [`Transaction`] (though it does
/// implement [`Merge`]).
#[derive(Clone, Default, Eq, PartialEq)]
pub struct CubeTransaction {
    /// Previous block which must occupy this cube.
    /// If `None`, no precondition.
    old: Equal<Block>,

    /// Block to be put in this cube.
    /// If `None`, this is only a precondition for modifying another block.
    new: Equal<Block>,

    /// If true, two transactions with the same `new` block may not be merged.
    conserved: bool,

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

impl fmt::Debug for CubeTransaction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            old: Equal(old),
            new: Equal(new),
            conserved,
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

    pub(crate) const ACTIVATE_BEHAVIOR: Self = Self {
        old: Equal(None),
        new: Equal(None),
        conserved: false,
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
    type MergeCheck = CubeMergeCheck;
    type Conflict = CubeConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        let conflict = CubeConflict {
            // Incompatible preconditions will always fail.
            old: self.old.check_merge(&other.old).is_err(),
            new: if self.conserved {
                // Replacing the same cube twice is not allowed -- even if they're
                // equal, doing so could violate an intended conservation law.
                self.new.0.is_some() && other.new.0.is_some()
            } else {
                // If nonconservative, then we simply require equal outcomes.
                self.new.check_merge(&other.new).is_err()
            },
        };

        if (conflict
            != CubeConflict {
                old: false,
                new: false,
            })
        {
            Err(conflict)
        } else {
            Ok(CubeMergeCheck {})
        }
    }

    fn commit_merge(&mut self, other: Self, CubeMergeCheck {}: Self::MergeCheck) {
        let Self {
            old,
            new,
            conserved,
            activate_behavior,
            fluff,
        } = self;

        // This would be more elegant if `conserved` was within the `self.new` Option.
        *conserved = (*conserved && new.0.is_some()) || (other.conserved && other.new.0.is_some());

        old.commit_merge(other.old, ());
        new.commit_merge(other.new, ());

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
            activate_behavior: _,
            fluff,
        } = self;

        old.visit_handles(visitor);
        new.visit_handles(visitor);
        fluff.visit_handles(visitor);
    }
}

#[doc(hidden)]
#[derive(Debug)]
#[non_exhaustive]
pub struct CubeMergeCheck {
    // This might end up having some data later.
    // For now, it's a placeholder to avoid passing () around
    // and getting clippy::let_unit_value warnings
}

/// Transaction conflict error type for a single [`CubeTransaction`] within a
/// [`SpaceTransaction`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct CubeConflict {
    /// The transactions have conflicting preconditions (`old` blocks).
    pub(crate) old: bool,
    /// The transactions are attempting to modify the same cube.
    pub(crate) new: bool,
}

impl core::error::Error for CubeConflict {}

impl fmt::Display for CubeConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            CubeConflict {
                old: true,
                new: false,
            } => write!(f, "different preconditions"),
            CubeConflict {
                old: false,
                new: true,
            } => write!(f, "cannot write the same cube twice"),
            CubeConflict {
                old: true,
                new: true,
            } => write!(f, "different preconditions (with write)"),
            CubeConflict {
                old: false,
                new: false,
            } => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::behavior::NoopBehavior;
    use crate::block::AIR;
    use crate::content::make_some_blocks;
    use crate::inv::EphemeralOpaque;
    use crate::transaction::TransactionTester;
    use core::sync::atomic::{AtomicU32, Ordering};
    use pretty_assertions::assert_eq;

    #[test]
    fn set_out_of_bounds_conserved_fails() {
        let [block] = make_some_blocks();
        // Note: by using .check() we validate that it doesn't fail in the commit phase
        SpaceTransaction::set_cube([1, 0, 0], None, Some(block))
            .check(&Space::empty_positive(1, 1, 1))
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
            .check(&Space::empty_positive(1, 1, 1))
            .unwrap_err();
    }

    #[test]
    fn compare_out_of_bounds_nonconserved_fails() {
        let [block] = make_some_blocks();
        SpaceTransaction::set_cube([1, 0, 0], Some(block), None)
            .nonconserved()
            .check(&Space::empty_positive(1, 1, 1))
            .unwrap_err();
    }

    #[test]
    fn merge_allows_independent() {
        let [b1, b2, b3] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], Some(b1.clone()), Some(b2.clone()));
        let t2 = SpaceTransaction::set_cube([1, 0, 0], Some(b1.clone()), Some(b3.clone()));
        let t3 = t1.clone().merge(t2.clone()).unwrap();
        assert_eq!(
            t3.cubes.into_iter().collect::<Vec<_>>(),
            vec![
                (
                    [0, 0, 0],
                    CubeTransaction {
                        old: Equal(Some(b1.clone())),
                        new: Equal(Some(b2.clone())),
                        conserved: true,
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

    #[test]
    fn systematic() {
        let [b1, b2, b3] = make_some_blocks();
        TransactionTester::new()
            .transaction(SpaceTransaction::default(), |_, _| Ok(()))
            .transaction(
                SpaceTransaction::set_cube([0, 0, 0], Some(b1.clone()), Some(b2.clone())),
                |_, after| {
                    if after[[0, 0, 0]] != b2 {
                        return Err("did not set b2".into());
                    }
                    Ok(())
                },
            )
            .transaction(
                SpaceTransaction::set_cube([0, 0, 0], Some(b1.clone()), Some(b3.clone())),
                |_, after| {
                    if after[[0, 0, 0]] != b3 {
                        return Err("did not set b3".into());
                    }
                    Ok(())
                },
            )
            .transaction(
                SpaceTransaction::set_cube([0, 0, 0], None, Some(b2.clone())),
                |_, after| {
                    if after[[0, 0, 0]] != b2 {
                        return Err("did not set b2".into());
                    }
                    Ok(())
                },
            )
            .transaction(
                SpaceTransaction::set_cube([0, 0, 0], Some(b2.clone()), None),
                |_, _| Ok(()),
            )
            .transaction(
                SpaceTransaction::set_cube([0, 0, 0], Some(b1.clone()), None),
                |_, _| Ok(()),
            )
            .transaction(
                CubeTransaction::ACTIVATE_BEHAVIOR.at(Cube::new(0, 0, 0)),
                // TODO: Add a test that activation happened once that's possible
                |_, _| Ok(()),
            )
            .transaction(
                CubeTransaction::ACTIVATE_BEHAVIOR.at(Cube::new(1, 0, 0)),
                // TODO: Add a test that activation happened once that's possible
                |_, _| Ok(()),
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
