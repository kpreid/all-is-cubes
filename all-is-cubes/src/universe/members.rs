// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::collections::BTreeMap;

use crate::block::BlockDef;
use crate::character::Character;
use crate::space::Space;
use crate::universe::{InsertError, Name, URef, URootRef, Universe, UniverseIndex, UniverseIter};

/// A BTreeMap is used to ensure that the iteration order is deterministic across
/// runs/versions.
pub(super) type Storage<T> = BTreeMap<Name, URootRef<T>>;

/// Trait for every type which can be a named member of a universe.
/// This trait is also public-in-private and serves to “seal” the [`UniverseIndex`]
/// trait.
pub trait UniverseMember {
    /// Generic constructor for [`MemberValue`].
    fn into_member_value(self) -> MemberValue;
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
fn index_insert<T>(this: &mut Universe, name: Name, value: T) -> Result<URef<T>, InsertError>
where
    Universe: UniverseTable<T>,
{
    use std::collections::btree_map::Entry::*;

    if this.get_any(&name).is_some() {
        return Err(InsertError::AlreadyExists(name));
    }

    let table = this.table_mut();
    match table.entry(name.clone()) {
        Occupied(_) => unreachable!(/* should have already checked for existence */),
        Vacant(vacant) => {
            let root_ref = URootRef::new(name, value);
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
            fn into_member_value(self) -> MemberValue {
                MemberValue::$member_type(Box::new(self))
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
        /// Holds any one of the value types that can be in a [`Universe`].
        #[derive(Debug)]
        #[doc(hidden)] // actually public-in-private but if we make a mistake, hide it
        pub enum MemberValue {
            $( $member_type(Box<$member_type>), )*
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
