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
    // TODO: prohibit existing names under any type, not just the same type
    let table = this.table_mut();
    match table.entry(name.clone()) {
        Occupied(_) => Err(InsertError::AlreadyExists(name)),
        Vacant(vacant) => {
            let root_ref = URootRef::new(name, value);
            let returned_ref = root_ref.downgrade();
            vacant.insert(root_ref);
            Ok(returned_ref)
        }
    }
}

macro_rules! impl_universe_for_member {
    ($member_type:ty, $table:ident) => {
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

// This macro only handles trait implementations.
// To add another type, it is also necessary to update:
//    struct Universe
//    impl Debug for Universe
//    Universe::step
impl_universe_for_member!(BlockDef, blocks);
impl_universe_for_member!(Character, characters);
impl_universe_for_member!(Space, spaces);
