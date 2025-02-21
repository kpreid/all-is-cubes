use alloc::collections::BTreeMap;
use core::hash::Hash;
use core::{fmt, mem};

use crate::transaction::{Merge, NoOutput, Transaction};

/// Transaction precondition error type for transactions on map types such as [`BTreeMap`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct MapMismatch<K, E> {
    /// The key in the map for which `self.mismatch` occurred.
    pub key: K,
    /// The precondition which failed.
    pub mismatch: E,
}

/// Transaction conflict error type for transactions on map types such as [`BTreeMap`].
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct MapConflict<K, C> {
    /// The key in the map for which `self.conflict` occurred.
    pub key: K,
    /// The conflict that occurred with two transactions for the same map value.
    pub conflict: C,
}

impl<K: fmt::Debug, E: core::error::Error + 'static> core::error::Error for MapMismatch<K, E> {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        Some(&self.mismatch)
    }
}

impl<K: fmt::Debug, C: core::error::Error + 'static> core::error::Error for MapConflict<K, C> {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        Some(&self.conflict)
    }
}

impl<K: fmt::Debug, E> fmt::Display for MapMismatch<K, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let MapMismatch { key, mismatch: _ } = self;
        write!(f, "transaction precondition not met at key {key:?}")
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
            S: core::hash::BuildHasher + Default + 'static,
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
                let mut checks = $module::HashMap::default();
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

#[allow(
    exported_private_dependencies,
    reason = "false positive on transitive dependencies"
)]
mod hb_impl {
    use super::*;
    use hashbrown::hash_map as hb_map;
    hashmap_merge!(hb_map);
}

/// This recursive macro generates implementations of [`Transaction`] and [`Merge`] for
/// tuples of various non-zero lengths.
///
/// It might be better as a derive macro, but that'd require a macro crate.
macro_rules! impl_transaction_for_tuple {
    ( $count:literal : $( $name:literal ),* ) => {
        paste::paste! {
            /// A tuple of transactions may act as a transaction on tuples.
            ///
            /// TODO: This functionality is not currently used and is of dubious value.
            impl<$( [<Tr $name>] ),*>
                Transaction for ($( [<Tr $name>], )*)
            where
                $( [<Tr $name>]: Transaction<Output = NoOutput>  ),*
            {
                type Target = ($( [<Tr $name>]::Target, )*);
                type Context<'a> = ($( [<Tr $name>]::Context<'a>, )*);
                type CommitCheck = (
                    $( <[<Tr $name>] as Transaction>::CommitCheck, )*
                );
                type Output = NoOutput;
                type Mismatch = [< TupleError $count >]<
                    $( <[<Tr $name >] as Transaction>::Mismatch, )*
                >;

                #[allow(unused_variables, reason = "empty tuple case")]
                fn check(
                    &self,
                    target: &($( [<Tr $name>]::Target, )*),
                    context: ($( [<Tr $name>]::Context<'_>, )*),
                ) -> Result<Self::CommitCheck, Self::Mismatch> {
                    let ($( [<txn_ $name>], )*) = self;
                    let ($( [<target_ $name>], )*) = target;
                    let ($( [<context_ $name>], )*) = context;
                    Ok((
                        $(
                            [<txn_ $name>].check([<target_ $name>], [<context_ $name>])
                                .map_err([< TupleError $count >]::[<At $name>])?,
                        )*
                    ))
                }

                fn commit(
                    self,
                    #[allow(unused_variables, reason = "empty tuple case")]
                    target: &mut ($( [<Tr $name>]::Target, )*),
                    check: Self::CommitCheck,
                    outputs: &mut dyn FnMut(Self::Output),
                ) -> Result<(), super::CommitError> {
                    let ($( [<txn_ $name>], )*) = self;
                    let ($( [<check_ $name>], )*) = check;
                    let ($( [<target_ $name>], )*) = target;
                    $( [<txn_ $name>].commit([<target_ $name>], [<check_ $name>], outputs)?; )*
                    Ok(())
                }
            }

            impl<$( [<T $name >] ),*> Merge for ($( [<T $name >], )*)
            where
                $( [<T $name >]: Merge  ),*
            {
                type MergeCheck = (
                    $( <[<T $name >] as Merge>::MergeCheck, )*
                );
                type Conflict = [< TupleConflict $count >]<
                    $( <[<T $name >] as Merge>::Conflict, )*
                >;

                fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
                    let ($( [<txn1_ $name>], )*) = self;
                    let ($( [<txn2_ $name>], )*) = other;
                    Ok((
                        $(
                            [<txn1_ $name>].check_merge([<txn2_ $name>])
                                .map_err([< TupleConflict $count >]::[<At $name>])?,
                        )*
                    ))
                }

                fn commit_merge(&mut self, other: Self, check: Self::MergeCheck) {
                    let ($( [<txn1_ $name>], )*) = self;
                    let ($( [<txn2_ $name>], )*) = other;
                    let ($( [<check_ $name>], )*) = check;
                    $( [<txn1_ $name>].commit_merge([<txn2_ $name>], [<check_ $name>]); )*
                }
            }

            #[doc = concat!("Transaction precondition error type for tuples of length ", $count, ".")]
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            #[expect(clippy::exhaustive_enums)]
            pub enum [< TupleError $count >]<$( [<E $name>], )*> {
                $(
                    #[doc = concat!("Error at tuple element ", $name, ".")]
                    [<At $name>]([<E $name>]),
                )*
            }
            #[doc = concat!("Transaction conflict error type for tuples of length ", $count, ".")]
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            #[expect(clippy::exhaustive_enums)]
            pub enum [< TupleConflict $count >]<$( [<C $name>], )*> {
                $(
                    #[doc = concat!("Conflict at tuple element ", $name, ".")]
                    [<At $name>]([<C $name>]),
                )*
            }

            // TODO: TupleConflict should have its own message to report the position,
            // instead of delegating.
                impl<$( [<E $name>]: core::error::Error, )*> core::error::Error for
                        [< TupleError $count >]<$( [<E $name>], )*> {
                    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
                        match *self {
                            $( Self::[<At $name>](ref [<e $name>]) => [<e $name>].source(), )*
                        }
                    }
                }
                impl<$( [<C $name>]: core::error::Error, )*> core::error::Error for
                        [< TupleConflict $count >]<$( [<C $name>], )*> {
                    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
                        match *self {
                            $( Self::[<At $name>](ref [<c $name>]) => [<c $name>].source(), )*
                        }
                    }
                }


            impl<$( [<E $name>]: fmt::Display, )*> fmt::Display for
                    [< TupleError $count >]<$( [<E $name>], )*> {
                fn fmt(&self, #[allow(unused)] f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    match *self {
                        $( Self::[<At $name>](ref [<e $name>]) => [<e $name>].fmt(f), )*
                    }
                }
            }

            impl<$( [<C $name>]: fmt::Display, )*> fmt::Display for
                    [< TupleConflict $count >]<$( [<C $name>], )*> {
                fn fmt(&self, #[allow(unused)] f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    match *self {
                        $( Self::[<At $name>](ref [<c $name>]) => [<c $name>].fmt(f), )*
                    }
                }
            }

        }
    };
}

impl_transaction_for_tuple!(1: 0);
impl_transaction_for_tuple!(2: 0, 1);
impl_transaction_for_tuple!(3: 0, 1, 2);
impl_transaction_for_tuple!(4: 0, 1, 2, 3);
impl_transaction_for_tuple!(5: 0, 1, 2, 3, 4);
impl_transaction_for_tuple!(6: 0, 1, 2, 3, 4, 5);

/// Does nothing.
// The empty tuple gets a special implementation because it cannot fail to commit,
// and this is best represented without using a custom type.
// Other than that, this is identical to the macro-generated code.
impl Transaction for () {
    type Target = ();
    type Context<'a> = ();
    type CommitCheck = ();
    type Output = core::convert::Infallible;
    type Mismatch = core::convert::Infallible;

    fn check(&self, (): &(), (): Self::Context<'_>) -> Result<Self::CommitCheck, Self::Mismatch> {
        Ok(())
    }

    fn commit(
        self,
        (): &mut (),
        (): Self::CommitCheck,
        _: &mut dyn FnMut(Self::Output),
    ) -> Result<(), super::CommitError> {
        Ok(())
    }
}

// The empty tuple gets a special implementation because it cannot fail to merge,
// and this is best represented without using a custom type.
// Other than that, this is identical to the macro-generated code.
impl Merge for () {
    type MergeCheck = ();

    type Conflict = core::convert::Infallible;

    fn check_merge(&self, (): &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        Ok(())
    }

    fn commit_merge(&mut self, (): Self, (): Self::MergeCheck) {}
}
