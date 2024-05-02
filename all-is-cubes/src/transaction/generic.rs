use alloc::collections::BTreeMap;
use core::hash::Hash;
use core::{fmt, mem};

use crate::transaction::Merge;

/// Transaction conflict error type for transactions on map types such as [`BTreeMap`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct MapConflict<K, C> {
    /// The key in the map for which `self.conflict` occurred.
    pub key: K,
    /// The conflict that occurred with two transactions for the same map value.
    pub conflict: C,
}

cfg_should_impl_error! {
    impl<K: fmt::Debug, C: std::error::Error + 'static> std::error::Error for MapConflict<K, C> {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            Some(&self.conflict)
        }
    }
}

impl<K: fmt::Debug, C> fmt::Display for MapConflict<K, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let MapConflict { key, conflict: _ } = self;
        write!(f, "transaction conflict at key {key:?}")
    }
}

impl<K, V> Merge for BTreeMap<K, V>
where
    K: Clone + Ord + fmt::Debug + 'static,
    V: Default + Merge,
{
    type MergeCheck = BTreeMap<K, <V as Merge>::MergeCheck>;
    type Conflict = MapConflict<K, <V as Merge>::Conflict>;

    fn check_merge<'a>(&'a self, mut map2: &'a Self) -> Result<Self::MergeCheck, Self::Conflict> {
        let mut map1 = self;
        if map1.len() > map2.len() {
            // The cost of the check is the cost of iterating over keys, so iterate over
            // the smaller map rather than the larger.
            // TODO: We can improve further by taking advantage of sortedness, using the
            // first and last of one set to iterate over a range of the other.
            // alloc::collections::btree_set::Intersection implements something like this,
            // but unfortunately, does not have an analogue for BTreeMap.
            mem::swap(&mut map1, &mut map2);
        }
        let mut checks = BTreeMap::new();
        for (k, v1) in map1.iter() {
            if let Some(v2) = map2.get(k) {
                checks.insert(
                    k.clone(),
                    v1.check_merge(v2).map_err(|conflict| MapConflict {
                        key: k.clone(),
                        conflict,
                    })?,
                );
            }
        }
        Ok(checks)
    }

    fn commit_merge(&mut self, mut other: Self, mut check: Self::MergeCheck) {
        if other.len() > self.len() {
            mem::swap(self, &mut other);
        }
        for (k, v2) in other {
            use alloc::collections::btree_map::Entry::*;
            match self.entry(k) {
                Occupied(mut entry) => {
                    let entry_check = check.remove(entry.key()).unwrap();
                    entry.get_mut().commit_merge(v2, entry_check);
                }
                Vacant(entry) => {
                    entry.insert(v2);
                }
            }
        }
    }
}

macro_rules! hashmap_merge {
    ($module:ident) => {
        impl<K, V, S> Merge for $module::HashMap<K, V, S>
        where
            K: Clone + Eq + Hash + fmt::Debug + 'static,
            V: Default + Merge,
            S: core::hash::BuildHasher + Clone + 'static,
        {
            type MergeCheck = $module::HashMap<K, <V as Merge>::MergeCheck, S>;
            type Conflict = MapConflict<K, <V as Merge>::Conflict>;

            fn check_merge<'a>(
                &'a self,
                mut map2: &'a Self,
            ) -> Result<Self::MergeCheck, Self::Conflict> {
                let mut map1 = self;
                if map1.len() > map2.len() {
                    // The cost of the check is the cost of iterating over keys, so iterate over
                    // the smaller map rather than the larger.
                    mem::swap(&mut map1, &mut map2);
                }
                let mut checks = $module::HashMap::with_hasher(map1.hasher().clone());
                for (k, v1) in map1.iter() {
                    if let Some(v2) = map2.get(k) {
                        checks.insert(
                            k.clone(),
                            v1.check_merge(v2).map_err(|conflict| MapConflict {
                                key: k.clone(),
                                conflict,
                            })?,
                        );
                    }
                }
                Ok(checks)
            }

            fn commit_merge(&mut self, mut other: Self, mut check: Self::MergeCheck) {
                if other.len() > self.len() {
                    mem::swap(self, &mut other);
                }
                for (k, v2) in other {
                    use $module::Entry::*;
                    match self.entry(k) {
                        Occupied(mut entry) => {
                            let entry_check = check.remove(entry.key()).unwrap();
                            entry.get_mut().commit_merge(v2, entry_check);
                        }
                        Vacant(entry) => {
                            entry.insert(v2);
                        }
                    }
                }
            }
        }
    };
}

#[cfg(feature = "std")]
use std::collections::hash_map as std_map;
#[cfg(feature = "std")]
hashmap_merge!(std_map);

use hashbrown::hash_map as hb_map;
hashmap_merge!(hb_map);
