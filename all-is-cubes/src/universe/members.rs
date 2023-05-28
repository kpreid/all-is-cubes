//! Module defining various traits and impls relating to [`Universe`] containing different
//! types of members.
//!
//! This module is not public and that is part of the protection of several items
//! inside it (the public-in-private trick).

use std::collections::BTreeMap;

use crate::block::BlockDef;
use crate::character::Character;
use crate::space::Space;
use crate::universe::{InsertError, Name, PartialUniverse, URef, URootRef, Universe, UniverseIter};

/// A `BTreeMap` is used to ensure that the iteration order is deterministic across
/// runs/versions.
pub(super) type Storage<T> = BTreeMap<Name, URootRef<T>>;

/// Trait for every type which can be a named member of a universe.
///
/// This trait is also public-in-private and thus prevents anything bounded by it from
/// being called or implemented by an unsupported type (“sealing”). However, because of
/// that, it also cannot mention anything we don't also want to make public or
/// public-in-private.
#[doc(hidden)]
pub trait UniverseMember: Sized + 'static {
    /// Generic constructor for [`AnyURef`].
    fn into_any_ref(r: URef<Self>) -> AnyURef;
}

/// For each type `T` that can be a member of a [`Universe`], this trait is implemented,
/// `impl UniverseTable<T> for Universe`, in order to provide the collection of members
/// of that type.
///
/// This trait is not public and is used only within the implementation of [`Universe`];
/// thus, the `Table` associated type does not need to be a public type.
///
/// TODO: Implement this on [`UniverseTables`] instead.
pub(super) trait UniverseTable<T> {
    type Table;

    fn table(&self) -> &Self::Table;

    fn table_mut(&mut self) -> &mut Self::Table;
}

/// Trait implemented by [`Universe`] once for each type of object that can be stored in a
/// [`Universe`], that permits lookups of that type.
///
/// This trait must be public(-in-private) so it can be a bound on public methods.
/// It could be just public, but it's cleaner to not require importing it everywhere.
#[doc(hidden)]
pub trait UniverseOps<T>
where
    T: UniverseMember,
{
    // Internal: Implementations of this are in the [`members`] module.

    fn get(&self, name: &Name) -> Option<URef<T>>;

    fn insert(&mut self, name: Name, value: T) -> Result<URef<T>, InsertError>;

    fn iter_by_type(&self) -> UniverseIter<'_, T>;
}

/// Trait implemented by [`PartialUniverse`] once for each type of object that can be
/// stored in a [`Universe`], that permits lookups of that type.
///
/// This trait must be public(-in-private) so it can be a bound on public methods.
/// It could be just public, but it's cleaner to not require importing it everywhere.
#[doc(hidden)]
pub trait PartialUniverseOps<T>
where
    T: UniverseMember,
{
    fn from_set(members: impl IntoIterator<Item = URef<T>>) -> Self
    where
        Self: Sized;
}

// Helper functions to implement `UniverseOps` without putting everything
// in the macro body.
pub(super) fn ops_get<T>(this: &Universe, name: &Name) -> Option<URef<T>>
where
    Universe: UniverseTable<T, Table = Storage<T>>,
{
    <Universe as UniverseTable<T>>::table(this)
        .get(name)
        .map(URootRef::downgrade)
}
/// Implementation of inserting an item in a universe.
/// Note that the same logic also exists in `UniverseTransaction`.
pub(super) fn ops_insert<T>(
    this: &mut Universe,
    mut name: Name,
    value: T,
) -> Result<URef<T>, InsertError>
where
    Universe: UniverseTable<T, Table = Storage<T>>,
{
    use std::collections::btree_map::Entry::*;

    name = this.allocate_name(&name)?;

    this.wants_gc = true;

    let id = this.id;
    match this.table_mut().entry(name.clone()) {
        Occupied(_) => unreachable!(/* should have already checked for existence */),
        Vacant(vacant) => {
            let root_ref = URootRef::new(id, name, value);
            let returned_ref = root_ref.downgrade();
            vacant.insert(root_ref);
            Ok(returned_ref)
        }
    }
}

/// Generates impls for a specific Universe member type.
macro_rules! impl_universe_for_member {
    ($member_type:ident, $table:ident) => {
        impl UniverseMember for $member_type {
            fn into_any_ref(r: URef<$member_type>) -> AnyURef {
                AnyURef::$member_type(r)
            }
        }

        impl UniverseTable<$member_type> for Universe {
            type Table = Storage<$member_type>;

            fn table(&self) -> &Storage<$member_type> {
                &self.tables.$table
            }
            fn table_mut(&mut self) -> &mut Storage<$member_type> {
                &mut self.tables.$table
            }
        }

        impl UniverseTable<$member_type> for PartialUniverse {
            type Table = Vec<URef<$member_type>>;

            fn table(&self) -> &Self::Table {
                &self.$table
            }
            fn table_mut(&mut self) -> &mut Self::Table {
                &mut self.$table
            }
        }

        impl UniverseOps<$member_type> for Universe {
            fn get(&self, name: &Name) -> Option<URef<$member_type>> {
                ops_get(self, name)
            }
            fn insert(
                &mut self,
                name: Name,
                value: $member_type,
            ) -> Result<URef<$member_type>, InsertError> {
                ops_insert(self, name, value)
            }
            fn iter_by_type(&self) -> UniverseIter<'_, $member_type> {
                UniverseIter(UniverseTable::<$member_type>::table(self).iter())
            }
        }

        impl PartialUniverseOps<$member_type> for PartialUniverse {
            fn from_set(members: impl IntoIterator<Item = URef<$member_type>>) -> Self {
                // TODO: enforce exactly one universe id
                let mut new_self = Self::default();
                UniverseTable::<$member_type>::table_mut(&mut new_self).extend(members);
                new_self
            }
        }
    };
}

/// Generates data structures which cover all universe member types.
macro_rules! member_enums_and_impls {
    ( $( ($member_type:ident, $table_name:ident), )* ) => {
        /// Sub-structure of [`Universe`] that actually stores the members.
        #[derive(Debug, Default)]
        pub(super) struct UniverseTables {
            $( pub(crate) $table_name: Storage<$member_type>, )*
        }

        /// Holds any one of the [`URef`] types that can be in a [`Universe`].
        ///
        /// See also [`URefErased`], which is implemented by `URef`s rather than owning one.
        ///
        /// This type is public-in-private because it is mentioned by the [`UniverseMember`]
        /// public-in-private trait.
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        #[doc(hidden)] // actually public-in-private but if we make a mistake, hide it
        pub enum AnyURef {
            $( $member_type(URef<$member_type>), )*
        }

        impl AnyURef {
            pub(crate) fn check_upgrade_pending(
                &self,
                universe_id: $crate::universe::UniverseId,
            ) -> Result<(), $crate::transaction::PreconditionFailed> {
                match self {
                    $( Self::$member_type(r) => r.check_upgrade_pending(universe_id), )*
                }
            }

            fn as_erased(&self) -> &dyn $crate::universe::URefErased {
                match self {
                    $( Self::$member_type(r) => r as &dyn $crate::universe::URefErased, )*
                }
            }
        }

        $( impl_universe_for_member!($member_type, $table_name); )*
    }
}

// This macro does not handle everything.
// To add another type, it is also necessary to update:
//    impl Debug for Universe
//    Universe::get_any
//    Universe::delete
//    Universe::gc
//    Universe::step
//    transaction::universe_txn::*
member_enums_and_impls!((BlockDef, blocks), (Character, characters), (Space, spaces),);

impl super::URefErased for AnyURef {
    fn name(&self) -> Name {
        self.as_erased().name()
    }
    fn universe_id(&self) -> Option<super::UniverseId> {
        self.as_erased().universe_id()
    }
}
