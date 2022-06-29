// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! TODO: Maybe this file is too small

use std::collections::btree_map::Entry::*;
use std::collections::BTreeMap;
use std::{fmt, mem};

use crate::behavior::{BehaviorSet, BehaviorSetTransaction};
use crate::block::Block;
use crate::drawing::DrawingPlane;
use crate::math::{GridCoordinate, GridMatrix, GridPoint};
use crate::space::{Grid, SetCubeError, Space};
use crate::transaction::{
    CommitError, Merge, PreconditionFailed, Transaction, TransactionConflict, Transactional,
};
use crate::util::{ConciseDebug, CustomFormat as _};
use crate::vui::ActivatableRegion;

impl Transactional for Space {
    type Transaction = SpaceTransaction;
}

#[derive(Clone, Default, Eq, PartialEq)]
#[must_use]
pub struct SpaceTransaction {
    cubes: BTreeMap<[GridCoordinate; 3], CubeTransaction>,
    behaviors: BehaviorSetTransaction<Space>,
}

impl SpaceTransaction {
    /// Construct a [`SpaceTransaction`] for a single cube.
    ///
    /// If `old` is not [`None`], requires that the existing block is that block or the
    /// transaction will fail.
    /// If `new` is not [`None`], replaces the existing block with `new`.
    ///
    /// TODO: This name is a poor name now that [`Self::set`] exists.
    pub fn set_cube(cube: impl Into<GridPoint>, old: Option<Block>, new: Option<Block>) -> Self {
        Self::single(
            cube,
            CubeTransaction {
                old,
                new,
                conserved: true,
                ..Default::default()
            },
        )
    }

    /// Expand this transaction to include modifying the given cube, or return an error if
    /// that would conflict (by the same definition as transaction merging).
    ///
    /// If `old` is not [`None`], requires that the existing block is that block or the
    /// transaction will fail.
    /// If `new` is not [`None`], replaces the existing block with `new`.
    pub fn set(
        &mut self,
        cube: impl Into<GridPoint>,
        old: Option<Block>,
        new: Option<Block>,
    ) -> Result<(), TransactionConflict> {
        let ct = CubeTransaction {
            old,
            new,
            conserved: true,
            ..Default::default()
        };
        match self.cubes.entry(cube.into().into()) {
            Vacant(entry) => {
                entry.insert(ct);
                Ok(())
            }
            Occupied(mut entry) => {
                let existing_ref = entry.get_mut();
                let check = existing_ref.check_merge(&ct)?;
                *existing_ref = mem::take(existing_ref).commit_merge(ct, check);
                Ok(())
            }
        }
    }

    /// Expand this transaction to include modifying the given cube, replacing any
    /// existing modification instruction (but not an existing `old` block precondition).
    /// This is thus comparable to a direct [`Space::set()`] after the rest of the
    /// transaction.
    // TODO: no tests
    pub fn set_overwrite(&mut self, cube: impl Into<GridPoint>, block: Block) {
        match self.cubes.entry(cube.into().into()) {
            Vacant(entry) => {
                entry.insert(CubeTransaction {
                    old: None,
                    new: Some(block),
                    ..Default::default()
                });
            }
            Occupied(mut entry) => {
                entry.get_mut().new = Some(block);
            }
        }
    }

    /// Provides an [`DrawTarget`](embedded_graphics::prelude::DrawTarget)
    /// adapter for 2.5D drawing.
    ///
    /// For more information on how to use this, see
    /// [`all_is_cubes::drawing`](crate::drawing).
    pub fn draw_target<C>(&mut self, transform: GridMatrix) -> DrawingPlane<'_, Self, C> {
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

    fn single(cube: impl Into<GridPoint>, transaction: CubeTransaction) -> Self {
        let cube: GridPoint = cube.into();
        let mut cubes = BTreeMap::new();
        cubes.insert(cube.into(/* array */), transaction);
        SpaceTransaction {
            cubes,
            ..Default::default()
        }
    }

    pub fn behaviors(t: BehaviorSetTransaction<Space>) -> Self {
        Self {
            behaviors: t,
            ..Default::default()
        }
    }

    pub(crate) fn activate_block(cube: GridPoint) -> Self {
        Self::single(cube, CubeTransaction::ACTIVATE)
    }

    /// Computes the region affected by this transaction.
    ///
    /// TODO: This does not currently report behaviors but it should, once they have
    /// formalized regions of attachment.
    ///
    /// TODO: Handle the case where the total volume is too large. (Maybe Grid should lose
    /// that restriction.)
    pub(crate) fn bounds(&self) -> Option<Grid> {
        // Destructuring to statically check that we consider all fields.
        let Self {
            cubes,
            behaviors: _,
        } = self;

        let mut bounds: Option<Grid> = None;
        for &cube_array in cubes.keys() {
            let cube = GridPoint::from(cube_array);
            if let Some(bounds) = &mut bounds {
                *bounds = (*bounds).union(Grid::single_cube(cube)).unwrap();
            } else {
                bounds = Some(Grid::single_cube(cube));
            }
        }
        bounds
    }
}

impl Transaction<Space> for SpaceTransaction {
    type CommitCheck =
        <BehaviorSetTransaction<Space> as Transaction<BehaviorSet<Space>>>::CommitCheck;
    type Output = ();

    fn check(&self, space: &Space) -> Result<Self::CommitCheck, PreconditionFailed> {
        for (
            &cube,
            CubeTransaction {
                old,
                new: _,
                conserved,
                activate: _,
            },
        ) in &self.cubes
        {
            if let Some(cube_index) = space.grid().index(cube) {
                if let Some(old) = old {
                    // Raw lookup because we already computed the index for a bounds check
                    // (TODO: Put this in a function, like get_block_index)
                    if space.block_data[space.contents[cube_index] as usize].block != *old {
                        return Err(PreconditionFailed {
                            location: "Space",
                            problem: "existing block not as expected",
                        });
                    }
                }
            } else {
                if *conserved || old.is_some() {
                    // It is an error for conserved cube txns to be out of bounds,
                    // or for a precondition to be not meetable because it is out of bounds.
                    // TODO: Should we allow `old: Some(AIR), new: None`, since we treat
                    // outside-space as being AIR? Let's wait until a use case appears rather than
                    // making AIR more special.
                    return Err(PreconditionFailed {
                        location: "Space",
                        problem: "cube out of space's bounds",
                    });
                }
            }
        }
        self.behaviors.check(&space.behaviors)
    }

    fn commit(&self, space: &mut Space, check: Self::CommitCheck) -> Result<(), CommitError> {
        let mut to_activate = Vec::new();
        for (
            &cube,
            CubeTransaction {
                old: _,
                new,
                conserved,
                activate,
            },
        ) in &self.cubes
        {
            if let Some(new) = new {
                match space.set(cube, new) {
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
        }
        self.behaviors
            .commit(&mut space.behaviors, check)
            .map_err(|e| e.context("behaviors".into()))?;
        if !to_activate.is_empty() {
            'b: for behavior in space.behaviors.query::<ActivatableRegion>() {
                // TODO: error return from the function? error report for nonexistence?
                for cube in to_activate.iter().copied() {
                    if behavior.region.contains_cube(cube) {
                        behavior.activate();
                        continue 'b;
                    }
                }
            }
        }
        Ok(())
    }
}

impl Merge for SpaceTransaction {
    type MergeCheck = <BehaviorSetTransaction<Space> as Merge>::MergeCheck;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        let mut cubes1 = &self.cubes;
        let mut cubes2 = &other.cubes;
        if cubes1.len() > cubes2.len() {
            // The cost of the check is the cost of iterating over keys, so iterate over
            // the smaller map rather than the larger.
            // TODO: We can improve further by taking advantage of sortedness, using the
            // first and last of one set to iterate over a range of the other.
            // std::collections::btree_set::Intersection implements something like this,
            // but unfortunately, does not have an analogue for BTreeMap.
            mem::swap(&mut cubes1, &mut cubes2);
        }
        for (cube, t1) in cubes1.iter() {
            if let Some(t2) = cubes2.get(cube) {
                let CubeMergeCheck {} = t1.check_merge(t2)?;
            }
        }
        self.behaviors.check_merge(&other.behaviors)
    }

    fn commit_merge(mut self, mut other: Self, check: Self::MergeCheck) -> Self {
        if other.cubes.len() > self.cubes.len() {
            mem::swap(&mut self, &mut other);
        }
        for (cube, t2) in other.cubes {
            match self.cubes.entry(cube) {
                Occupied(mut entry) => {
                    let t1_ref = entry.get_mut();
                    *t1_ref = mem::take(t1_ref).commit_merge(t2, CubeMergeCheck {});
                }
                Vacant(entry) => {
                    entry.insert(t2);
                }
            }
        }
        self.behaviors = self.behaviors.commit_merge(other.behaviors, check);
        self
    }
}

impl fmt::Debug for SpaceTransaction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = fmt.debug_struct("SpaceTransaction");
        for (cube, txn) in &self.cubes {
            ds.field(
                &GridPoint::from(*cube)
                    .custom_format(ConciseDebug)
                    .to_string(),
                txn,
            );
        }
        if !self.behaviors.is_empty() {
            ds.field("behaviors", &self.behaviors);
        }
        ds.finish()
    }
}

/// Data for a single cube in a [`SpaceTransaction`]. This does not function as a
/// transaction on its own, though it does implement [`Merge`].
#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct CubeTransaction {
    /// Previous block which must occupy this cube.
    /// If `None`, no precondition.
    old: Option<Block>,

    /// Block to be put in this cube.
    /// If `None`, this is only a precondition for modifying another block.
    new: Option<Block>,

    /// If true, two transactions with the same `new` block may not be merged.
    conserved: bool,

    /// The cube was “activated” (clicked on, more or less) and should
    /// respond to that.
    activate: bool,
}

impl CubeTransaction {
    const ACTIVATE: Self = Self {
        old: None,
        new: None,
        conserved: false,
        activate: true,
    };
}

impl Merge for CubeTransaction {
    type MergeCheck = CubeMergeCheck;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        if matches!((&self.old, &other.old), (Some(a), Some(b)) if a != b) {
            // Incompatible preconditions will always fail.
            return Err(TransactionConflict {});
        }
        if self.new.is_some() && other.new.is_some() && self.conserved {
            // Replacing the same cube twice is not allowed -- even if they're
            // equal, doing so could violate an intended conservation law.
            return Err(TransactionConflict {});
        }
        Ok(CubeMergeCheck {})
    }

    fn commit_merge(self, other: Self, CubeMergeCheck {}: Self::MergeCheck) -> Self
where {
        CubeTransaction {
            // This would be more elegant if `conserved` was within the `self.new` Option.
            conserved: (self.conserved && self.new.is_some())
                || (other.conserved && other.new.is_some()),

            old: self.old.or(other.old),
            new: self.new.or(other.new),
            activate: self.activate || other.activate,
        }
    }
}

struct CubeMergeCheck {
    // This might end up having some data later.
    // For now, it's a placeholder to avoid passing () around
    // and getting clippy::let_unit_value warnings
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicU32, Ordering};
    use std::sync::Arc;

    use pretty_assertions::assert_eq;

    use crate::block::AIR;
    use crate::content::make_some_blocks;
    use crate::inv::EphemeralOpaque;
    use crate::space::Grid;
    use crate::transaction::TransactionTester;

    use super::*;

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
            .execute(&mut Space::empty_positive(1, 1, 1))
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
    fn set_cube_mutate_equivalent_to_merge() {
        let [b1, b2, b3] = make_some_blocks();

        // A basic single-cube transaction is the same either way.
        let mut t = SpaceTransaction::default();
        t.set([0, 0, 0], Some(b1.clone()), Some(b2.clone()))
            .unwrap();
        assert_eq!(
            t,
            SpaceTransaction::set_cube([0, 0, 0], Some(b1.clone()), Some(b2.clone())),
        );

        // Two-cube transaction.
        let prev = t.clone();
        t.set([1, 0, 0], Some(b2.clone()), Some(b3.clone()))
            .unwrap();
        assert_eq!(
            t,
            prev.merge(SpaceTransaction::set_cube(
                [1, 0, 0],
                Some(b2.clone()),
                Some(b3.clone())
            ))
            .unwrap(),
        );

        // Conflict
        let prev = t.clone();
        t.set([0, 0, 0], Some(b1.clone()), Some(b3.clone()))
            .expect_err("should have merge failure");
        assert_eq!(t, prev,);
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
                        old: Some(b1.clone()),
                        new: Some(b2.clone()),
                        conserved: true,
                        activate: false,
                    }
                ),
                (
                    [1, 0, 0],
                    CubeTransaction {
                        old: Some(b1.clone()),
                        new: Some(b3.clone()),
                        conserved: true,
                        activate: false,
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
    fn merge_rejects_different_new() {
        let [b1, b2] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], None, Some(b1.clone()));
        let t2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(b2.clone()));
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
        let cube = GridPoint::new(0, 0, 0);

        let signal = Arc::new(AtomicU32::new(0));
        space.add_behavior(ActivatableRegion {
            region: Grid::single_cube(cube),
            // TODO: This sure is clunky
            effect: EphemeralOpaque::from(Arc::new({
                let signal = signal.clone();
                move || {
                    signal.fetch_add(1, Ordering::Relaxed);
                }
            }) as Arc<dyn Fn() + Send + Sync>),
        });

        SpaceTransaction::activate_block(cube)
            .execute(&mut space)
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
                SpaceTransaction::activate_block(GridPoint::new(0, 0, 0)),
                // TODO: Add a test that activation happened once that's possible
                |_, _| Ok(()),
            )
            .transaction(
                SpaceTransaction::activate_block(GridPoint::new(1, 0, 0)),
                // TODO: Add a test that activation happened once that's possible
                |_, _| Ok(()),
            )
            .target(|| Space::empty_positive(2, 1, 1))
            .target(|| {
                let mut space = Space::empty_positive(2, 1, 1);
                space.set([0, 0, 0], &b1).unwrap();
                space
            })
            .target(|| {
                let mut space = Space::empty_positive(2, 1, 1);
                space.set([0, 0, 0], &b2).unwrap();
                space
            })
            .target(|| {
                // This space makes the test transactions at [0, 0, 0] out of bounds
                Space::builder(Grid::new([1, 0, 0], [1, 1, 1])).build_empty()
            })
            // TODO: more spaces
            .test();
    }

    #[test]
    fn bounds_empty() {
        assert_eq!(SpaceTransaction::default().bounds(), None);
    }

    #[test]
    fn bounds_single_cube() {
        assert_eq!(
            SpaceTransaction::set_cube([-7, 3, 5], None, Some(AIR)).bounds(),
            Some(Grid::single_cube(GridPoint::new(-7, 3, 5)))
        );
    }

    #[test]
    fn bounds_multi_cube() {
        let t1 = SpaceTransaction::set_cube([-7, 3, 5], None, Some(AIR));
        let t2 = SpaceTransaction::set_cube([10, 3, 5], None, Some(AIR));
        assert_eq!(
            t1.merge(t2).unwrap().bounds(),
            Some(Grid::from_lower_upper([-7, 3, 5], [11, 4, 6]))
        );
    }
}
