// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! TODO: Maybe this file is too small

use std::collections::btree_map::Entry::*;
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt::Debug;

use super::Space;
use crate::block::Block;
use crate::math::{GridCoordinate, GridPoint};
use crate::transactions::PreconditionFailed;
use crate::transactions::{Transaction, TransactionConflict, Transactional};
use crate::util::{ConciseDebug, CustomFormat as _};

impl Transactional for Space {
    type Transaction = SpaceTransaction;
}

#[derive(Clone, Default, PartialEq)]
pub struct SpaceTransaction {
    cubes: BTreeMap<[GridCoordinate; 3], CubeTransaction>,
}

impl SpaceTransaction {
    // TODO: tests
    /// Construct a [`SpaceTransaction`] for a single cube.
    ///
    /// If `old` is not [`None`], requires that the existing block is that block or the
    /// transaction will fail.
    /// If `new` is not [`None`], replaces the existing block with `new`.
    pub fn set_cube(cube: impl Into<GridPoint>, old: Option<Block>, new: Option<Block>) -> Self {
        Self::single(cube, CubeTransaction { old, new })
    }

    fn single(cube: impl Into<GridPoint>, transaction: CubeTransaction) -> Self {
        let cube: GridPoint = cube.into();
        let mut cubes = BTreeMap::new();
        cubes.insert(cube.into(/* array */), transaction);
        SpaceTransaction { cubes }
    }
}

impl Transaction<Space> for SpaceTransaction {
    type CommitCheck = ();
    type MergeCheck = ();
    type Output = ();

    fn check(&self, space: &Space) -> Result<Self::CommitCheck, PreconditionFailed> {
        for (&cube, CubeTransaction { old, new: _ }) in &self.cubes {
            if let Some(old) = old {
                if space[cube] != *old {
                    return Err(PreconditionFailed {});
                }
            }
        }
        Ok(())
    }

    fn commit(&self, target: &mut Space, _check: Self::CommitCheck) -> Result<(), Box<dyn Error>> {
        for (&cube, CubeTransaction { old: _, new }) in &self.cubes {
            if let Some(new) = new {
                target.set(cube, new)?;
            }
        }
        Ok(())
    }

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, TransactionConflict> {
        for (cube, t1) in self.cubes.iter() {
            if let Some(t2) = other.cubes.get(cube) {
                if matches!((&t1.old, &t2.old), (Some(a), Some(b)) if a != b) {
                    // Incompatible preconditions will always fail.
                    return Err(TransactionConflict {});
                }
                if t1.new.is_some() && t2.new.is_some() {
                    // Replacing the same cube twice is not allowed -- even if they're
                    // equal, since doing so could violate an intended conservation law.
                    // TODO: Might want to make that optional.
                    return Err(TransactionConflict {});
                }
            }
        }
        Ok(())
    }

    fn commit_merge(mut self, other: Self, (): Self::MergeCheck) -> Self {
        for (cube, t2) in other.cubes {
            match self.cubes.entry(cube) {
                Occupied(mut entry) => {
                    let t1 = entry.get_mut();
                    if t2.old.is_some() {
                        t1.old = t2.old;
                    }
                    if t2.new.is_some() {
                        t1.new = t2.new;
                    }
                }
                Vacant(entry) => {
                    entry.insert(t2);
                }
            }
        }
        self
    }
}

impl Debug for SpaceTransaction {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut ds = fmt.debug_struct("SpaceTransaction");
        for (cube, txn) in &self.cubes {
            ds.field(
                &GridPoint::from(*cube)
                    .custom_format(ConciseDebug)
                    .to_string(),
                txn,
            );
        }
        ds.finish()
    }
}

#[derive(Clone, Debug, PartialEq)]
struct CubeTransaction {
    /// If `None`, no precondition.
    old: Option<Block>,
    /// If `None`, this is only a precondition for modifying another block.
    new: Option<Block>,
}

#[cfg(test)]
mod tests {
    use crate::content::make_some_blocks;
    use crate::transactions::TransactionTester;

    use super::*;

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
                        new: Some(b2.clone())
                    }
                ),
                (
                    [1, 0, 0],
                    CubeTransaction {
                        old: Some(b1.clone()),
                        new: Some(b3.clone())
                    }
                ),
            ]
        );
    }

    #[test]
    fn merge_rejects_same_new() {
        let [block] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block.clone()));
        let t2 = SpaceTransaction::set_cube([0, 0, 0], None, Some(block.clone()));
        t1.merge(t2).unwrap_err();
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
            .target(|| Space::empty_positive(1, 1, 1))
            .target(|| {
                let mut space = Space::empty_positive(1, 1, 1);
                space.set([0, 0, 0], &b1).unwrap();
                space
            })
            .target(|| {
                let mut space = Space::empty_positive(1, 1, 1);
                space.set([0, 0, 0], &b2).unwrap();
                space
            })
            // TODO: more spaces
            .test();
    }
}
