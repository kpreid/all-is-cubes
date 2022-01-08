// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Top-level game state container.
//!
//! ## Thread-safety
//!
//! A caution: While [`Universe`], [`URef`], and their contents implement [`Send`] and
//! [`Sync`] in a safe manner, they do not yet provide the tools that would be necessary
//! for multiple threads to operate on a Universe. In particular, (TODO:) there are no
//! methods to wait for a [`URef`]'s lock to be available.
//!
//! <!-- TODO: And once we have waiting, we have a deadlock problem:
//!
//! ... without causing **deadlocks.** For the
//! time being, stick to the following usage patterns to avoid deadlock:
//!
//! * Mutable access should be confined to a single thread — do not have multiple threads
//!   using [`URef::try_modify`] or [`URef::execute`]. That thread need not stay
//!   the same (as if the whole package were [`Send`] but not [`Sync`]).
//! * Other threads should never hold more than one [`URef`] lock at once.
//!
//! -->

use std::fmt;
use std::marker::PhantomData;
use std::sync::Arc;
use std::time::Duration;

use instant::Instant;

use crate::apps::Tick;
use crate::block::BlockDef;
use crate::character::Character;
use crate::space::{Space, SpaceStepInfo};
use crate::transaction::Transaction;
use crate::util::{CustomFormat, StatusText, TypeName};

mod members;
use members::*;

mod uref;
pub use uref::*;

mod visit;
pub use visit::*;

#[cfg(test)]
mod tests;

/// Name/key of an object in a [`Universe`].
///
/// Internally uses [`Arc`] to be cheap to clone. Might be interned in future versions.
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub enum Name {
    /// An explicitly set name.
    Specific(Arc<str>),
    /// An automatically assigned name.
    Anonym(usize),
}
impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self::Specific(value.into())
    }
}
impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Specific(name) => write!(f, "'{}'", name),
            Name::Anonym(index) => write!(f, "[anonymous #{}]", index),
        }
    }
}

/// A collection of named objects which can refer to each other via [`URef`]. In the
/// future, it will enable garbage collection and inter-object invariants.
///
/// See also the [`UniverseIndex`] trait for methods for adding and removing objects.
///
/// **Thread-safety caveat:** See the documentation on [avoiding deadlock].
///
/// [avoiding deadlock]: crate::universe#thread-safety
pub struct Universe {
    blocks: Storage<BlockDef>,
    characters: Storage<Character>,
    spaces: Storage<Space>,
    next_anonym: usize,
}

impl Universe {
    /// Construct an empty [`Universe`].
    pub fn new() -> Self {
        Universe {
            blocks: Storage::new(),
            spaces: Storage::new(),
            // TODO: bodies so body-in-world stepping
            characters: Storage::new(),
            next_anonym: 0,
        }
    }

    // TODO: temporary shortcuts to be replaced with more nuance
    pub fn get_default_character(&self) -> Option<URef<Character>> {
        self.get(&"character".into())
    }

    /// Advance time for all members.
    pub fn step(&mut self, tick: Tick) -> UniverseStepInfo {
        let mut info = UniverseStepInfo::default();
        let start_time = Instant::now();

        let mut transactions = Vec::new();

        for space_root in self.spaces.values() {
            let space_ref = space_root.downgrade();
            let (space_info, transaction) = space_ref
                .try_modify(|space| space.step(Some(&space_ref), tick))
                .expect("space borrowed during universe.step()");
            transactions.push(transaction);
            info.space_step += space_info;
        }

        for character_root in self.characters.values() {
            let character_ref = character_root.downgrade();
            let (_body_step_info, transaction) = character_ref
                .try_modify(|ch| ch.step(Some(&character_ref), tick))
                .expect("character borrowed during universe.step()");
            transactions.push(transaction);
        }

        // TODO: Quick hack -- we would actually like to execute non-conflicting transactions and skip conflicting ones...
        for t in transactions {
            if let Err(e) = t.execute(self) {
                // TODO: Need to report these failures back to the source
                // ... and perhaps in the UniverseStepInfo
                log::info!("Transaction failure: {}", e);
            }
        }

        info.computation_time = Instant::now().duration_since(start_time);
        info
    }

    /// Inserts a new object without giving it a specific name, and returns
    /// a reference to it.
    pub fn insert_anonymous<T>(&mut self, value: T) -> URef<T>
    where
        Self: UniverseIndex<T>,
    {
        let name = Name::Anonym(self.next_anonym);
        self.next_anonym += 1;
        self.insert(name, value)
            .expect("shouldn't happen: newly created anonym already in use")
    }
}

impl fmt::Debug for Universe {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = fmt.debug_struct("Universe");
        format_members::<BlockDef>(self, &mut ds);
        format_members::<Character>(self, &mut ds);
        format_members::<Space>(self, &mut ds);
        ds.finish()
    }
}
fn format_members<T>(universe: &Universe, ds: &mut fmt::DebugStruct<'_, '_>)
where
    Universe: UniverseTable<T>,
{
    for name in universe.table().keys() {
        // match root.strong_ref.try_borrow() {
        //     Ok(entry) => ds.field(&name.to_string(), &entry.data),
        //     Err(_) => ds.field(&name.to_string(), &"<in use>"),
        // };
        ds.field(&name.to_string(), &PhantomData::<T>.custom_format(TypeName));
    }
}

impl sealed_gimmick::Sealed for Universe {}

/// Trait implemented once for each type of object that can be stored in a [`Universe`]
/// that permits lookups of that type.
pub trait UniverseIndex<T>: sealed_gimmick::Sealed {
    // Internal: Implementations of this are in the [`members`] module.

    /// Translates a name for an object of type `T` into a [`URef`] for it, which
    /// allows borrowing the actual object.
    ///
    /// Returns [`None`] if no object exists for the name.
    fn get(&self, name: &Name) -> Option<URef<T>>;

    /// Inserts a new object with a specific name.
    ///
    /// Returns an error if the name is already in use.
    fn insert(&mut self, name: Name, value: T) -> Result<URef<T>, InsertError>;

    /// Iterate over all of the objects of type `T`.
    /// Note that this includes anonymous objects.
    ///
    /// ```
    /// use all_is_cubes::block::{Block, BlockDef};
    /// use all_is_cubes::content::make_some_blocks;
    /// use all_is_cubes::universe::{Name, Universe, UniverseIndex, URef};
    ///
    /// let mut universe = Universe::new();
    /// let [block_1, block_2] = make_some_blocks();
    /// universe.insert(Name::from("b1"), BlockDef::new(block_1.clone()));
    /// universe.insert(Name::from("b2"), BlockDef::new(block_2.clone()));
    ///
    /// let mut found_blocks = universe.iter_by_type()
    ///     .map(|(name, value): (Name, URef<BlockDef>)| (name, Block::clone(&value.borrow())))
    ///     .collect::<Vec<_>>();
    /// found_blocks.sort_by_key(|(name, _)| name.to_string());
    /// assert_eq!(
    ///     found_blocks,
    ///     vec![Name::from("b1"), Name::from("b2")].into_iter()
    ///         .zip(vec![block_1, block_2])
    ///         .collect::<Vec<_>>(),
    /// );
    /// ```
    fn iter_by_type(&self) -> UniverseIter<'_, T>;
}

/// Iterator type for [`UniverseIndex::iter_by_type`].
pub struct UniverseIter<'u, T>(std::collections::btree_map::Iter<'u, Name, URootRef<T>>);
impl<'u, T> Iterator for UniverseIter<'u, T> {
    type Item = (Name, URef<T>);
    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|(name, root)| (name.clone(), root.downgrade()))
    }
}

impl Default for Universe {
    fn default() -> Self {
        Self::new()
    }
}

/// Errors resulting from attempting to insert an object in a `Universe`.
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
#[non_exhaustive]
pub enum InsertError {
    #[error("an object already exists with name {0}")]
    AlreadyExists(Name),
}

/// Performance data returned by [`Universe::step`]. The exact contents of this structure
/// are unstable; use only `Debug` formatting to examine its contents unless you have
/// a specific need for one of the values.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct UniverseStepInfo {
    computation_time: Duration,
    space_step: SpaceStepInfo,
}
impl std::ops::AddAssign<UniverseStepInfo> for UniverseStepInfo {
    fn add_assign(&mut self, other: Self) {
        self.space_step += other.space_step;
    }
}
impl CustomFormat<StatusText> for UniverseStepInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        writeln!(
            fmt,
            "Step computation: {}",
            self.computation_time.custom_format(StatusText),
        )?;
        write!(fmt, "{}", self.space_step.custom_format(StatusText))?;
        Ok(())
    }
}

mod sealed_gimmick {
    /// As a supertrait, this prevents a trait from being implemented outside the crate.
    pub trait Sealed {}
}
