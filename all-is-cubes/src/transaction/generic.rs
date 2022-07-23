use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;
use std::mem;

use crate::transaction::{Merge, TransactionConflict};

impl<K, V> Merge for BTreeMap<K, V>
where
    K: Clone + Ord + 'static,
    V: Default + Merge,
{
    type MergeCheck = BTreeMap<K, <V as Merge>::MergeCheck>;

    fn check_merge<'a>(
        &'a self,
        mut map2: &'a Self,
    ) -> Result<Self::MergeCheck, TransactionConflict> {
        let mut map1 = self;
        if map1.len() > map2.len() {
            // The cost of the check is the cost of iterating over keys, so iterate over
            // the smaller map rather than the larger.
            // TODO: We can improve further by taking advantage of sortedness, using the
            // first and last of one set to iterate over a range of the other.
            // std::collections::btree_set::Intersection implements something like this,
            // but unfortunately, does not have an analogue for BTreeMap.
            mem::swap(&mut map1, &mut map2);
        }
        let mut checks = BTreeMap::new();
        for (k, v1) in map1.iter() {
            if let Some(v2) = map2.get(k) {
                checks.insert(k.clone(), v1.check_merge(v2)?);
            }
        }
        Ok(checks)
    }

    fn commit_merge(mut self, mut other: Self, mut check: Self::MergeCheck) -> Self {
        if other.len() > self.len() {
            mem::swap(&mut self, &mut other);
        }
        for (k, v2) in other {
            use std::collections::btree_map::Entry::*;
            match self.entry(k) {
                Occupied(mut entry) => {
                    let v1 = mem::take(entry.get_mut());
                    let entry_check = check.remove(entry.key()).unwrap();
                    *entry.get_mut() = v1.commit_merge(v2, entry_check);
                }
                Vacant(entry) => {
                    entry.insert(v2);
                }
            }
        }
        self
    }
}

impl<K, V> Merge for HashMap<K, V>
where
    K: Clone + Eq + Hash + 'static,
    V: Default + Merge,
{
    type MergeCheck = HashMap<K, <V as Merge>::MergeCheck>;

    fn check_merge<'a>(
        &'a self,
        mut map2: &'a Self,
    ) -> Result<Self::MergeCheck, TransactionConflict> {
        let mut map1 = self;
        if map1.len() > map2.len() {
            // The cost of the check is the cost of iterating over keys, so iterate over
            // the smaller map rather than the larger.
            mem::swap(&mut map1, &mut map2);
        }
        let mut checks = HashMap::with_hasher(map1.hasher().clone());
        for (k, v1) in map1.iter() {
            if let Some(v2) = map2.get(k) {
                checks.insert(k.clone(), v1.check_merge(v2)?);
            }
        }
        Ok(checks)
    }

    fn commit_merge(mut self, mut other: Self, mut check: Self::MergeCheck) -> Self {
        if other.len() > self.len() {
            mem::swap(&mut self, &mut other);
        }
        for (k, v2) in other {
            use std::collections::hash_map::Entry::*;
            match self.entry(k) {
                Occupied(mut entry) => {
                    let v1 = mem::take(entry.get_mut());
                    let entry_check = check.remove(entry.key()).unwrap();
                    *entry.get_mut() = v1.commit_merge(v2, entry_check);
                }
                Vacant(entry) => {
                    entry.insert(v2);
                }
            }
        }
        self
    }
}
