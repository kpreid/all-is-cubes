// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Top-level game state container.

use std::borrow::{Borrow, BorrowMut};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::hash_map::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};
use std::sync::Arc;
use std::time::Duration;

use instant::Instant; // wasm-compatible replacement for std::time::Instant
use owning_ref::{OwningHandle, OwningRef, OwningRefMut};

use crate::apps::Tick;
use crate::block::BlockDef;
use crate::character::Character;
use crate::space::{Space, SpaceStepInfo};
use crate::transactions::Transaction as _;
use crate::util::{CustomFormat, StatusText, TypeName};

/// Name/key of an object in a [`Universe`].
#[allow(clippy::exhaustive_enums)]
#[derive(Clone, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub enum Name {
    /// An explicitly set name.
    Specific(String),
    /// An automatically assigned name.
    Anonym(usize),
}
impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self::Specific(value.to_string())
    }
}
impl Display for Name {
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
pub struct Universe {
    blocks: HashMap<Name, URootRef<BlockDef>>,
    characters: HashMap<Name, URootRef<Character>>,
    spaces: HashMap<Name, URootRef<Space>>,
    next_anonym: usize,
}

impl Universe {
    /// Construct an empty [`Universe`].
    pub fn new() -> Self {
        Universe {
            blocks: HashMap::new(),
            spaces: HashMap::new(),
            // TODO: bodies so body-in-world stepping
            characters: HashMap::new(),
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

        for space in self.spaces.values() {
            let (space_info, transaction) = space
                .try_borrow_mut()
                .expect("space borrowed during universe.step()")
                .step(Some(&space.downgrade()), tick);
            transactions.push(transaction);
            info.space_step += space_info;
        }

        for character in self.characters.values() {
            // TODO: Make URootRef::downgrade() non-allocating
            let (_body_step_info, transaction) = character
                .try_borrow_mut()
                .expect("character borrowed during universe.step()")
                .step(Some(&character.downgrade()), tick);
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
/// that internally provides the table for that type. This trait differs from
/// [`UniverseIndex`] in that it is not public.
trait UniverseTable<T> {
    fn table(&self) -> &HashMap<Name, URootRef<T>>;
    fn table_mut(&mut self) -> &mut HashMap<Name, URootRef<T>>;
}
impl UniverseTable<BlockDef> for Universe {
    fn table(&self) -> &HashMap<Name, URootRef<BlockDef>> {
        &self.blocks
    }
    fn table_mut(&mut self) -> &mut HashMap<Name, URootRef<BlockDef>> {
        &mut self.blocks
    }
}
impl UniverseTable<Character> for Universe {
    fn table(&self) -> &HashMap<Name, URootRef<Character>> {
        &self.characters
    }
    fn table_mut(&mut self) -> &mut HashMap<Name, URootRef<Character>> {
        &mut self.characters
    }
}
impl UniverseTable<Space> for Universe {
    fn table(&self) -> &HashMap<Name, URootRef<Space>> {
        &self.spaces
    }
    fn table_mut(&mut self) -> &mut HashMap<Name, URootRef<Space>> {
        &mut self.spaces
    }
}

/// Trait implemented once for each type of object that can be stored in a [`Universe`]
/// that permits lookups of that type.
pub trait UniverseIndex<T>: sealed_gimmick::Sealed {
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
impl UniverseIndex<BlockDef> for Universe {
    fn get(&self, name: &Name) -> Option<URef<BlockDef>> {
        index_get(self, name)
    }
    fn insert(&mut self, name: Name, value: BlockDef) -> Result<URef<BlockDef>, InsertError> {
        index_insert(self, name, value)
    }
    fn iter_by_type(&self) -> UniverseIter<'_, BlockDef> {
        UniverseIter(self.table().iter())
    }
}
impl UniverseIndex<Character> for Universe {
    fn get(&self, name: &Name) -> Option<URef<Character>> {
        index_get(self, name)
    }
    fn insert(&mut self, name: Name, value: Character) -> Result<URef<Character>, InsertError> {
        index_insert(self, name, value)
    }
    fn iter_by_type(&self) -> UniverseIter<'_, Character> {
        UniverseIter(self.table().iter())
    }
}
impl UniverseIndex<Space> for Universe {
    fn get(&self, name: &Name) -> Option<URef<Space>> {
        index_get(self, name)
    }
    fn insert(&mut self, name: Name, value: Space) -> Result<URef<Space>, InsertError> {
        index_insert(self, name, value)
    }
    fn iter_by_type(&self) -> UniverseIter<'_, Space> {
        UniverseIter(self.table().iter())
    }
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
    use std::collections::hash_map::Entry::*;
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

/// Iterator type for [`UniverseIndex::iter_by_type`].
pub struct UniverseIter<'u, T>(std::collections::hash_map::Iter<'u, Name, URootRef<T>>);
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

/// Type of a strong reference to an entry in a [`Universe`]. Defined to make types
/// parameterized with this somewhat less hairy.
type StrongEntryRef<T> = Rc<RefCell<UEntry<T>>>;

/// A reference from an object in a [`Universe`] to another.
///
/// If they are held by objects outside of the [`Universe`], it is not guaranteed
/// that they will remain valid (in which case using the `URef` will return an error
/// or panic depending on the method).
/// To ensure an object does not vanish while operating on it, [`URef::borrow`] it.
/// (TODO: Should there be an operation in the style of `Weak::upgrade`?)
pub struct URef<T> {
    // TODO: We're going to want to either track reference counts or implement a garbage
    // collector for the graph of URefs. Reference counts would be an easy way to ensure
    // nothing is deleted while it is in use from a UI perspective.
    /// Reference to the object. Weak because we don't want to create reference cycles;
    /// the assumption is that the overall game system will keep the [`Universe`] alive
    /// and that [`Universe`] will ensure no entry goes away while referenced.
    weak_ref: Weak<RefCell<UEntry<T>>>,
    name: Arc<Name>,
}

impl<T: 'static> URef<T> {
    pub fn name(&self) -> &Arc<Name> {
        &self.name
    }

    /// Borrow the value, in the sense of [`RefCell::borrow`], and panic on failure.
    #[track_caller]
    pub fn borrow(&self) -> UBorrow<T> {
        self.try_borrow().unwrap()
    }

    /// Borrow the value mutably, in the sense of [`RefCell::borrow_mut`], and panic
    /// on failure.
    #[track_caller]
    pub fn borrow_mut(&self) -> UBorrowMut<T> {
        self.try_borrow_mut().unwrap()
    }

    /// Borrow the value, in the sense of [`RefCell::try_borrow`].
    pub fn try_borrow(&self) -> Result<UBorrow<T>, RefError> {
        let strong: Rc<RefCell<UEntry<T>>> = self.upgrade()?;

        // Kludge: OwningHandle doesn't let us try_borrow, so waste one to check.
        strong
            .try_borrow()
            .map_err(|_| RefError::InUse(Arc::clone(&self.name)))?;

        Ok(UBorrow(
            OwningRef::new(OwningHandle::new(strong)).map(|entry| &entry.data),
        ))
    }

    /// Borrow the value mutably, in the sense of [`RefCell::try_borrow_mut`].
    pub fn try_borrow_mut(&self) -> Result<UBorrowMut<T>, RefError> {
        let strong: Rc<RefCell<UEntry<T>>> = self.upgrade()?;

        // Kludge: OwningHandle doesn't let us try_borrow, so waste one to check.
        strong
            .try_borrow_mut()
            .map_err(|_| RefError::InUse(Arc::clone(&self.name)))?;

        Ok(UBorrowMut(
            OwningRefMut::new(OwningHandle::new_mut(strong)).map_mut(|entry| &mut entry.data),
        ))
    }

    fn upgrade(&self) -> Result<StrongEntryRef<T>, RefError> {
        self.weak_ref
            .upgrade()
            .ok_or_else(|| RefError::Gone(Arc::clone(&self.name)))
    }
}

impl<T> Debug for URef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: Maybe print dead refs differently?
        write!(f, "URef({})", self.name)
    }
}

/// `URef`s are compared by pointer equality: they are equal only if they refer to
/// the same mutable cell.
impl<T> PartialEq for URef<T> {
    fn eq(&self, other: &Self) -> bool {
        Weak::ptr_eq(&self.weak_ref, &other.weak_ref)
    }
}
/// `URef`s are compared by pointer equality.
impl<T> Eq for URef<T> {}
impl<T> Hash for URef<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

/// Manual implementation of Clone that does not require T to be Clone.
impl<T> Clone for URef<T> {
    fn clone(&self) -> Self {
        URef {
            weak_ref: self.weak_ref.clone(),
            name: self.name.clone(),
        }
    }
}

/// Errors resulting from attempting to borrow/dereference a [`URef`].
#[allow(clippy::exhaustive_enums)] // If this has to change it will be a major semantic change
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
pub enum RefError {
    /// Target was deleted, or its entire universe was dropped.
    #[error("object was deleted: {0}")]
    Gone(Arc<Name>),
    /// Target is currently incompatibly borrowed.
    #[error("object was in use at the same time: {0}")]
    InUse(Arc<Name>),
}

/// A wrapper type for an immutably borrowed value from an [`URef`].
pub struct UBorrow<T: 'static>(
    OwningRef<OwningHandle<StrongEntryRef<T>, Ref<'static, UEntry<T>>>, T>,
);
/// A wrapper type for a mutably borrowed value from an [`URef`].
pub struct UBorrowMut<T: 'static>(
    OwningRefMut<OwningHandle<StrongEntryRef<T>, RefMut<'static, UEntry<T>>>, T>,
);
impl<T> Deref for UBorrow<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.deref()
    }
}
impl<T> Deref for UBorrowMut<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0.deref()
    }
}
impl<T> DerefMut for UBorrowMut<T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0.deref_mut()
    }
}
impl<T> AsRef<T> for UBorrow<T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}
impl<T> AsRef<T> for UBorrowMut<T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}
impl<T> AsMut<T> for UBorrowMut<T> {
    fn as_mut(&mut self) -> &mut T {
        self.deref_mut()
    }
}
impl<T> Borrow<T> for UBorrow<T> {
    fn borrow(&self) -> &T {
        self.deref()
    }
}
impl<T> Borrow<T> for UBorrowMut<T> {
    fn borrow(&self) -> &T {
        self.deref()
    }
}
impl<T> BorrowMut<T> for UBorrowMut<T> {
    fn borrow_mut(&mut self) -> &mut T {
        self.deref_mut()
    }
}

impl<T: Debug> Debug for UBorrow<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "UBorrow({:?})", **self)
    }
}
impl<T: Debug> Debug for UBorrowMut<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "UBorrowMut({:?})", **self)
    }
}

/// The data of an entry in a `Universe`.
#[derive(Debug)]
struct UEntry<T> {
    // Note: It might make more sense for data to be a RefCell<T> (instead of the
    // RefCell containing UEntry. However. it will require fiddling with the
    // owning_ref pileup to do that, and might not be possible.
    data: T,
    name: Arc<Name>,
}

/// The unique reference to an entry in a `Universe` from that `Universe`.
/// Normal usage is via `URef` instead.
#[derive(Debug)]
struct URootRef<T> {
    strong_ref: StrongEntryRef<T>,
    name: Arc<Name>,
}

impl<T> URootRef<T> {
    fn new(name: Name, initial_value: T) -> Self {
        let name = Arc::new(name);
        URootRef {
            strong_ref: Rc::new(RefCell::new(UEntry {
                data: initial_value,
                name: name.clone(),
            })),
            name,
        }
    }

    /// Convert to `URef`.
    ///
    /// TODO: As we add graph analysis features, this will need additional arguments
    /// like where the ref is being held, and it will probably need to be renamed.
    fn downgrade(&self) -> URef<T> {
        URef {
            weak_ref: Rc::downgrade(&self.strong_ref),
            name: Arc::clone(&self.name),
        }
    }

    /// Borrow the value mutably, in the sense of [`RefCell::try_borrow_mut`].
    fn try_borrow_mut(&self) -> Result<UBorrowMut<T>, RefError> {
        self.downgrade().try_borrow_mut()
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::AIR;
    use crate::content::make_some_blocks;

    #[test]
    fn universe_debug_empty() {
        assert_eq!(format!("{:?}", Universe::new()), "Universe");
        assert_eq!(format!("{:#?}", Universe::new()), "Universe");
    }

    /// Universe does not print contents of members, on the assumption this would be too verbose.
    #[test]
    fn universe_debug_elements() {
        let mut u = Universe::new();
        u.insert("foo".into(), Space::empty_positive(1, 2, 3))
            .unwrap();
        u.insert_anonymous(BlockDef::new(AIR));
        assert_eq!(
            format!("{:?}", u),
            "Universe { [anonymous #0]: all_is_cubes::block::BlockDef, 'foo': all_is_cubes::space::Space }"
        );
        assert_eq!(
            format!("{:#?}", u),
            "\
Universe {
    [anonymous #0]: all_is_cubes::block::BlockDef,
    'foo': all_is_cubes::space::Space,
}\
            "
        );
    }

    #[test]
    fn uref_debug() {
        let mut u = Universe::new();
        let r = u
            .insert("foo".into(), Space::empty_positive(1, 2, 3))
            .unwrap();
        assert_eq!(format!("{:?}", r), "URef('foo')");
        assert_eq!(format!("{:#?}", r), "URef('foo')");
    }

    #[test]
    fn uref_try_borrow_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.borrow_mut();
        assert_eq!(
            r.try_borrow().unwrap_err(),
            RefError::InUse(Arc::new(Name::Anonym(0)))
        );
    }

    #[test]
    fn uref_try_borrow_mut_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.borrow();
        assert_eq!(
            r.try_borrow_mut().unwrap_err(),
            RefError::InUse(Arc::new(Name::Anonym(0)))
        );
    }

    #[test]
    fn ref_error_format() {
        assert_eq!(
            RefError::InUse(Arc::new("foo".into())).to_string(),
            "object was in use at the same time: 'foo'"
        );
        assert_eq!(
            RefError::Gone(Arc::new("foo".into())).to_string(),
            "object was deleted: 'foo'"
        );
        assert_eq!(
            RefError::Gone(Arc::new(Name::Anonym(123))).to_string(),
            "object was deleted: [anonymous #123]"
        );
    }

    #[test]
    #[allow(clippy::eq_op)]
    fn uref_equality_is_pointer_equality() {
        let root_a = URootRef::new("space".into(), Space::empty_positive(1, 1, 1));
        let root_b = URootRef::new("space".into(), Space::empty_positive(1, 1, 1));
        let ref_a_1 = root_a.downgrade();
        let ref_a_2 = root_a.downgrade();
        let ref_b_1 = root_b.downgrade();
        assert_eq!(ref_a_1, ref_a_1, "reflexive eq");
        assert_eq!(ref_a_1, ref_a_2, "separately constructed are equal");
        assert!(ref_a_1 != ref_b_1, "not equal");
    }

    // TODO: more tests of the hairy reference logic

    #[test]
    fn insert_anonymous_makes_distinct_names() {
        let [block_0, block_1] = make_some_blocks();
        let mut u = Universe::new();
        let ref_a = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let ref_b = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        ref_a.borrow_mut().set((0, 0, 0), &block_0).unwrap();
        ref_b.borrow_mut().set((0, 0, 0), &block_1).unwrap();
        assert_ne!(ref_a, ref_b, "not equal");
        assert_ne!(
            ref_a.borrow()[(0, 0, 0)],
            ref_b.borrow()[(0, 0, 0)],
            "different values"
        );
    }

    #[test]
    fn insert_duplicate_name() {
        let mut u = Universe::new();
        u.insert("test_block".into(), BlockDef::new(AIR)).unwrap();
        assert_eq!(
            u.insert("test_block".into(), BlockDef::new(AIR)),
            Err(InsertError::AlreadyExists("test_block".into()))
        );
    }
}
