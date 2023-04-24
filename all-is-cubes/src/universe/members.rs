//! Module defining various traits and impls relating to [`Universe`] containing different
//! types of members.
//!
//! This module is not public and that is part of the protection of several items
//! inside it (the public-in-private trick).

use std::collections::BTreeMap;

use crate::block::BlockDef;
use crate::character::Character;
use crate::space::Space;
use crate::universe::{InsertError, Name, URef, URootRef, Universe, UniverseIndex, UniverseIter};

/// A `BTreeMap` is used to ensure that the iteration order is deterministic across
/// runs/versions.
pub(super) type Storage<T> = BTreeMap<Name, URootRef<T>>;

/// Trait for every type which can be a named member of a universe.
///
/// This trait is also public-in-private and thus prevents anything bounded by it from
/// being called or implemented by an unsupported type (“sealing”). However, because of
/// that, it also cannot mention anything we don't also want to make public or
/// public-in-private.
pub trait UniverseMember: Sized + 'static {
    /// Generic constructor for [`AnyURef`].
    fn into_any_ref(r: URef<Self>) -> AnyURef;
}

/// Trait implemented once for each type of object that can be stored in a [`Universe`]
/// that internally provides the table for that type. This trait differs from
/// [`UniverseIndex`] in that it is not public.
pub(super) trait UniverseTable<T> {
    fn table(&self) -> &Storage<T>;

    fn table_mut(&mut self) -> &mut Storage<T>;
}

// Helper functions to implement UniverseIndex. Can't be trait provided methods
// because UniverseTable is private
fn index_get<T>(this: &Universe, name: &Name) -> Option<URef<T>>
where
    Universe: UniverseTable<T>,
{
    this.table().get(name).map(URootRef::downgrade)
}
/// Implementation of inserting an item in a universe.
/// Note that the same logic also exists in `UniverseTransaction`.
fn index_insert<T>(this: &mut Universe, mut name: Name, value: T) -> Result<URef<T>, InsertError>
where
    Universe: UniverseTable<T>,
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
            fn table(&self) -> &Storage<$member_type> {
                &self.$table
            }
            fn table_mut(&mut self) -> &mut Storage<$member_type> {
                &mut self.$table
            }
        }

        impl UniverseIndex<$member_type> for Universe {
            fn get(&self, name: &Name) -> Option<URef<$member_type>> {
                index_get(self, name)
            }
            fn insert(
                &mut self,
                name: Name,
                value: $member_type,
            ) -> Result<URef<$member_type>, InsertError> {
                index_insert(self, name, value)
            }
            fn iter_by_type(&self) -> UniverseIter<'_, $member_type> {
                UniverseIter(self.table().iter())
            }
        }
    };
}

/// Generates enums which cover all universe types.
macro_rules! member_enums {
    ( $( ($member_type:ident), )* ) => {
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
    }
}

// This macro only handles trait implementations.
// To add another type, it is also necessary to update:
//    struct Universe
//    impl Debug for Universe
//    Universe::get_any
//    Universe::step
//    transaction::universe_txn::*
impl_universe_for_member!(BlockDef, blocks);
impl_universe_for_member!(Character, characters);
impl_universe_for_member!(Space, spaces);

member_enums!((BlockDef), (Character), (Space),);

impl super::URefErased for AnyURef {
    fn name(&self) -> Name {
        self.as_erased().name()
    }
    fn universe_id(&self) -> Option<super::UniverseId> {
        self.as_erased().universe_id()
    }
}
