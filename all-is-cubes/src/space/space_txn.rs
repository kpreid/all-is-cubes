// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! TODO: Maybe this file is too small

use std::collections::btree_map::Entry::*;
use std::collections::BTreeMap;
use std::error::Error;

use super::Space;
use crate::block::Block;
use crate::math::{GridCoordinate, GridPoint};
use crate::transactions::Transaction;

#[derive(Clone, Debug, PartialEq)]
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
    pub fn set_cube(cube: GridPoint, old: Option<Block>, new: Option<Block>) -> Self {
        Self::single(cube, CubeTransaction { old, new })
    }

    fn single(cube: GridPoint, transaction: CubeTransaction) -> Self {
        let mut cubes = BTreeMap::new();
        cubes.insert(cube.into(), transaction);
        SpaceTransaction { cubes }
    }
}

impl Transaction<Space> for SpaceTransaction {
    type Check = ();

    fn check(&self, space: &Space) -> Result<Self::Check, ()> {
        for (&cube, CubeTransaction { old, new: _ }) in &self.cubes {
            if let Some(old) = old {
                if space[cube] != *old {
                    return Err(());
                }
            }
        }
        Ok(())
    }

    fn commit(&self, target: &mut Space, _check: Self::Check) -> Result<(), Box<dyn Error>> {
        for (&cube, CubeTransaction { old: _, new }) in &self.cubes {
            if let Some(new) = new {
                target.set(cube, new)?;
            }
        }
        Ok(())
    }

    fn merge(mut self, other: Self) -> Result<Self, (Self, Self)> {
        // Check everything before mutating.
        for (cube, t1) in self.cubes.iter() {
            if let Some(t2) = other.cubes.get(cube) {
                if matches!((&t1.old, &t2.old), (Some(a), Some(b)) if a != b) {
                    // Incompatible preconditions will always fail.
                    return Err((self, other));
                }
                if t1.new.is_some() && t2.new.is_some() {
                    // Replacing the same cube twice is not allowed -- even if they're
                    // equal, since doing so could violate an intended conservation law.
                    // TODO: Might want to make that optional.
                    return Err((self, other));
                }
            }
        }

        // Perform the merge infallibly.
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

        Ok(self)
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
    use crate::transactions::merge_is_rejected;

    use super::*;

    #[test]
    fn merge_allows_independent() {
        let [b1, b2, b3] = make_some_blocks();
        let t1 =
            SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), Some(b1.clone()), Some(b2.clone()));
        let t2 =
            SpaceTransaction::set_cube(GridPoint::new(1, 0, 0), Some(b1.clone()), Some(b3.clone()));
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
        let t1 = SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), None, Some(block.clone()));
        let t2 = SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), None, Some(block.clone()));
        merge_is_rejected(t1, t2).unwrap();
    }

    #[test]
    fn merge_rejects_different_new() {
        let [b1, b2] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), None, Some(b1.clone()));
        let t2 = SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), None, Some(b2.clone()));
        merge_is_rejected(t1, t2).unwrap();
    }

    #[test]
    fn merge_rejects_different_old() {
        let [b1, b2] = make_some_blocks();
        let t1 = SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), Some(b1.clone()), None);
        let t2 = SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), Some(b2.clone()), None);
        merge_is_rejected(t1, t2).unwrap();
    }

    #[test]
    fn merge_allows_same_old() {
        let [b1, b2] = make_some_blocks();
        let t1 =
            SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), Some(b1.clone()), Some(b2.clone()));
        let t2 = SpaceTransaction::set_cube(GridPoint::new(0, 0, 0), Some(b1.clone()), None);
        assert_eq!(t1.clone(), t1.clone().merge(t2).unwrap());
    }
}
