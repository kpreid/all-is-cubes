// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Top-level game state container.

use owning_ref::{OwningHandle, OwningRef, OwningRefMut};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::hash_map::HashMap;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};
use std::time::{Duration, Instant};

use crate::block::BlockDef;
use crate::camera::Camera;
use crate::space::{Space, SpaceStepInfo};

/// Name/key of an object in a [`Universe`].
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
#[derive(Debug)] // TODO: impl Debug with a less verbose result
pub struct Universe {
    blocks: HashMap<Name, URootRef<BlockDef>>,
    cameras: HashMap<Name, URootRef<Camera>>,
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
            cameras: HashMap::new(),
            next_anonym: 0,
        }
    }

    // TODO: temporary shortcuts to be replaced with more nuance
    pub fn get_default_space(&self) -> URef<Space> {
        self.get(&"space".into()).unwrap()
    }
    pub fn get_default_camera(&self) -> URef<Camera> {
        self.get(&"camera".into()).unwrap()
    }

    /// Advance time for all members.
    pub fn step(&mut self, timestep: Duration) -> UniverseStepInfo {
        let mut info = UniverseStepInfo::default();
        for space in self.spaces.values() {
            info.space_step += space
                .try_borrow_mut()
                .expect("space borrowed during universe.step()")
                .step(timestep);
        }
        for camera in self.cameras.values() {
            let _camera_info = camera
                .try_borrow_mut()
                .expect("camera borrowed during universe.step()")
                .step(timestep);
        }
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
impl UniverseTable<Camera> for Universe {
    fn table(&self) -> &HashMap<Name, URootRef<Camera>> {
        &self.cameras
    }
    fn table_mut(&mut self) -> &mut HashMap<Name, URootRef<Camera>> {
        &mut self.cameras
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
    /// use all_is_cubes::blockgen::make_some_blocks;
    /// use all_is_cubes::universe::{Name, Universe, UniverseIndex, URef};
    ///
    /// let mut universe = Universe::new();
    /// let blocks = make_some_blocks(2);
    /// universe.insert(Name::from("b1"), BlockDef::new(blocks[0].clone()));
    /// universe.insert(Name::from("b2"), BlockDef::new(blocks[1].clone()));
    ///
    /// let mut found_blocks = universe.iter_by_type()
    ///     .map(|(name, value): (Name, URef<BlockDef>)| (name, Block::clone(&value.borrow())))
    ///     .collect::<Vec<_>>();
    /// found_blocks.sort_by_key(|(name, _)| name.to_string());
    /// assert_eq!(
    ///     found_blocks,
    ///     vec![Name::from("b1"), Name::from("b2")].into_iter()
    ///         .zip(blocks.into_iter())
    ///         .collect::<Vec<_>>(),
    /// );
    /// ```
    fn iter_by_type(&self) -> UniverseIter<T>;
}
impl UniverseIndex<BlockDef> for Universe {
    fn get(&self, name: &Name) -> Option<URef<BlockDef>> {
        index_get(self, name)
    }
    fn insert(&mut self, name: Name, value: BlockDef) -> Result<URef<BlockDef>, InsertError> {
        index_insert(self, name, value)
    }
    fn iter_by_type(&self) -> UniverseIter<BlockDef> {
        UniverseIter(self.table().iter())
    }
}
impl UniverseIndex<Camera> for Universe {
    fn get(&self, name: &Name) -> Option<URef<Camera>> {
        index_get(self, name)
    }
    fn insert(&mut self, name: Name, value: Camera) -> Result<URef<Camera>, InsertError> {
        index_insert(self, name, value)
    }
    fn iter_by_type(&self) -> UniverseIter<Camera> {
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
    fn iter_by_type(&self) -> UniverseIter<Space> {
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
pub struct UniverseIter<'u, T: 'u>(std::collections::hash_map::Iter<'u, Name, URootRef<T>>);
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
#[derive(Debug)]
pub struct URef<T> {
    // TODO: We're going to want to either track reference counts or implement a garbage
    // collector for the graph of URefs. Reference counts would be an easy way to ensure
    // nothing is deleted while it is in use from a UI perspective.
    /// Reference to the object. Weak because we don't want to create reference cycles;
    /// the assumption is that the overall game system will keep the [`Universe`] alive
    /// and that [`Universe`] will ensure no entry goes away while referenced.
    weak_ref: Weak<RefCell<UEntry<T>>>,
    name: Rc<Name>,
}

impl<T: 'static> URef<T> {
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
            .map_err(|_| RefError::InUse(Rc::clone(&self.name)))?;

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
            .map_err(|_| RefError::InUse(Rc::clone(&self.name)))?;

        Ok(UBorrowMut(
            OwningRefMut::new(OwningHandle::new_mut(strong)).map_mut(|entry| &mut entry.data),
        ))
    }

    fn upgrade(&self) -> Result<StrongEntryRef<T>, RefError> {
        self.weak_ref
            .upgrade()
            .ok_or_else(|| RefError::Gone(Rc::clone(&self.name)))
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
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
pub enum RefError {
    /// Target was deleted, or its entire universe was dropped.
    #[error("object was deleted: {0}")]
    Gone(Rc<Name>),
    /// Target is currently incompatibly borrowed.
    #[error("object was in use at the same time: {0}")]
    InUse(Rc<Name>),
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

impl<T: Debug> Debug for UBorrow<T> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "UBorrow({:?})", **self)
    }
}
impl<T: Debug> Debug for UBorrowMut<T> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
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
    name: Rc<Name>,
}

/// The unique reference to an entry in a `Universe` from that `Universe`.
/// Normal usage is via `URef` instead.
#[derive(Debug)]
struct URootRef<T> {
    strong_ref: StrongEntryRef<T>,
    name: Rc<Name>,
}

impl<T> URootRef<T> {
    fn new(name: Name, initial_value: T) -> Self {
        let name = Rc::new(name);
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
            name: Rc::clone(&self.name),
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
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct UniverseStepInfo {
    space_step: SpaceStepInfo,
}
impl std::ops::AddAssign<UniverseStepInfo> for UniverseStepInfo {
    fn add_assign(&mut self, other: Self) {
        self.space_step += other.space_step;
    }
}

/// Algorithm for deciding how to execute simulation and rendering frames.
/// Platform-independent; does not consult any clocks, only makes decisions
/// given the provided information.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FrameClock {
    last_absolute_time: Option<Instant>,
    /// Whether there was a step and we should therefore draw a frame.
    /// TODO: This might go away in favor of actual dirty-notifications.
    render_dirty: bool,
    accumulated_step_time: Duration,
}

impl FrameClock {
    const STEP_LENGTH: Duration = Duration::from_micros(1_000_000 / 60);
    const ACCUMULATOR_CAP: Duration = Duration::from_millis(500);

    /// Constructs a new [`FrameClock`].
    ///
    /// This operation is independent of the system clock.
    pub fn new() -> Self {
        Self {
            last_absolute_time: None,
            render_dirty: true,
            accumulated_step_time: Duration::default(),
        }
    }

    /// Advance the clock using a source of absolute time.
    ///
    /// This cannot be meaningfully used in combination with
    /// [`FrameClock::request_frame()`].
    pub fn advance_to(&mut self, instant: Instant) {
        if let Some(last_absolute_time) = self.last_absolute_time {
            let delta = instant - last_absolute_time;
            self.accumulated_step_time += delta;
            self.cap_step_time();
        }
        self.last_absolute_time = Some(instant);
    }

    /// Reacts to a callback from the environment requesting drawing a frame ASAP if
    /// we're going to (i.e. `requestAnimationFrame` on the web). Drives the simulation
    /// clock based on this input (it will not advance if no requests are made).
    ///
    /// Returns whether a frame should actually be rendered now. The caller should also
    /// consult [`FrameClock::should_step()`] afterward to schedule game state steps.
    ///
    /// This cannot be meaningfully used in combination with [`FrameClock::advance_to()`].
    #[must_use]
    pub fn request_frame(&mut self, time_since_last_frame: Duration) -> bool {
        let result = self.should_draw();
        self.did_draw();

        self.accumulated_step_time += time_since_last_frame;
        self.cap_step_time();

        result
    }

    /// Indicates whether a new frame should be drawn, given the amount of time that this
    /// [`FrameClock`] has been informed has passed.
    ///
    /// When a frame *is* drawn, [`FrameClock::did_draw`]] must be called; otherwise, this
    /// will always return true.
    pub fn should_draw(&self) -> bool {
        self.render_dirty
    }

    /// Informs the [`FrameClock`] that a frame was just drawn.
    pub fn did_draw(&mut self) {
        self.render_dirty = false;
    }

    /// Indicates whether [`Universe::step`] should be performed, given the amount of time
    /// that this [`FrameClock`] has been informed has passed.
    ///
    /// When a step *is* performd, [`FrameClock::did_step`] must be called; otherwise, this
    /// will always return true.
    pub fn should_step(&self) -> bool {
        self.accumulated_step_time >= Self::STEP_LENGTH
    }

    /// Informs the [`FrameClock`] that a step was just performed.
    pub fn did_step(&mut self) {
        self.accumulated_step_time -= Self::STEP_LENGTH;
        self.render_dirty = true;
    }

    /// The timestep value that should be passed to [`Universe::step`] when stepping in
    /// response to [`FrameClock::should_step`] returning true.
    pub fn step_length(&self) -> Duration {
        Self::STEP_LENGTH
    }

    fn cap_step_time(&mut self) {
        if self.accumulated_step_time > Self::ACCUMULATOR_CAP {
            self.accumulated_step_time = Self::ACCUMULATOR_CAP;
        }
    }
}

impl Default for FrameClock {
    fn default() -> Self {
        Self::new()
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
    use crate::blockgen::make_some_blocks;

    #[test]
    fn uref_try_borrow_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.borrow_mut();
        assert_eq!(
            r.try_borrow().unwrap_err(),
            RefError::InUse(Rc::new(Name::Anonym(0)))
        );
    }

    #[test]
    fn uref_try_borrow_mut_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.borrow();
        assert_eq!(
            r.try_borrow_mut().unwrap_err(),
            RefError::InUse(Rc::new(Name::Anonym(0)))
        );
    }

    #[test]
    fn ref_error_format() {
        assert_eq!(
            RefError::InUse(Rc::new("foo".into())).to_string(),
            "object was in use at the same time: 'foo'"
        );
        assert_eq!(
            RefError::Gone(Rc::new("foo".into())).to_string(),
            "object was deleted: 'foo'"
        );
        assert_eq!(
            RefError::Gone(Rc::new(Name::Anonym(123))).to_string(),
            "object was deleted: [anonymous #123]"
        );
    }

    #[test]
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
        let blocks = make_some_blocks(2);
        let mut u = Universe::new();
        let ref_a = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let ref_b = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        ref_a.borrow_mut().set((0, 0, 0), &blocks[0]).unwrap();
        ref_b.borrow_mut().set((0, 0, 0), &blocks[1]).unwrap();
        assert!(ref_a != ref_b, "not equal");
        assert!(
            ref_a.borrow()[(0, 0, 0)] != ref_b.borrow()[(0, 0, 0)],
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
