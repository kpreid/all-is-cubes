// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Top-level game state container.

use owning_ref::{OwningHandle, OwningRef, OwningRefMut};
use std::collections::hash_map::{DefaultHasher, HashMap};
use std::cell::{Ref, RefCell, RefMut};
use std::hash::{Hash, Hasher};
use std::rc::{Rc, Weak};
use std::ops::{Deref, DerefMut};
use std::time::{Duration, Instant};

use crate::camera::Camera;
use crate::space::{Space, SpaceStepInfo};

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Name {
    Specific(String),
    Anonym(usize),
}
impl From<&str> for Name {
    fn from(value: &str) -> Self { Self::Specific(value.to_string()) }
}

/// A collection of named objects which can refer to each other via `URef`. In the future,
/// it will enable multiple references, garbage collection, change notification, and
/// inter-object invariants.
pub struct Universe {
    spaces: HashMap<Name, URootRef<Space>>,
    cameras: HashMap<Name, URootRef<Camera>>,
    next_anonym: usize,
}

impl Universe {
    pub fn new() -> Self {
        Universe {
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

    /// Advance time.
    pub fn step(&mut self, timestep: Duration) -> (SpaceStepInfo, ()) {
        let mut space_info = SpaceStepInfo::default();
        for space in self.spaces.values() {
            space_info += space.borrow_mut().step(timestep);
        }
        for camera in self.cameras.values() {
            let _camera_info = camera.borrow_mut().step(timestep);
        }
        (space_info, ())
    }

    pub fn insert_anonymous<T>(&mut self, value: T) -> URef<T> where Self: UniverseIndex<T> {
        // TODO: Names should not be strings, so these can be guaranteed unique.
        let name = Name::Anonym(self.next_anonym);
        self.next_anonym += 1;
        self.insert(name, value)
    }
}

/// Trait implemented once for each type of object that can be stored in a `Universe`.
pub trait UniverseIndex<T> {
    fn get(&self, name: &Name) -> Option<URef<T>>;
    fn insert(&mut self, name: Name, value: T) -> URef<T>;
}
impl UniverseIndex<Space> for Universe {
    fn get(&self, name: &Name) -> Option<URef<Space>> {
        self.spaces.get(name).map(URootRef::downgrade)
    }
    fn insert(&mut self, name: Name, value: Space) -> URef<Space> {
        // TODO: prohibit existing names under any type
        let root_ref = URootRef::new(name.clone(), value);
        let returned_ref = root_ref.downgrade();
        self.spaces.insert(name, root_ref);
        returned_ref
    }
}
impl UniverseIndex<Camera> for Universe {
    fn get(&self, name: &Name) -> Option<URef<Camera>> {
        self.cameras.get(name).map(URootRef::downgrade)
    }
    fn insert(&mut self, name: Name, value: Camera) -> URef<Camera> {
        // TODO: prohibit existing names under any type
        let root_ref = URootRef::new(name.clone(), value);
        let returned_ref = root_ref.downgrade();
        self.cameras.insert(name, root_ref);
        returned_ref
    }
}

impl Default for Universe {
   fn default() -> Self { Self::new() }
}

/// Type of a strong reference to an entry in a `Universe`. Defined to make types
/// parameterized with this somewhat less hairy.
type StrongEntryRef<T> = Rc<RefCell<UEntry<T>>>;

/// A reference from an object in a `Universe` to another.
///
/// If they are held by objects outside of the `Universe`, it is not guaranteed
/// that they will remain valid (in which case using the `URef` will panic).
/// To ensure an object does not vanish while operating on it, `.borrow()` it.
/// (TODO: Should there be an operation in the style of `Weak::upgrade`?)
#[derive(Clone, Debug)]
pub struct URef<T> {
    // TODO: We're going to want to either track reference counts or implement a garbage
    // collector for the graph of URefs. Reference counts would be an easy way to ensure
    // nothing is deleted while it is in use from a UI perspective.

    /// Reference to the object. Weak because we don't want to create reference cycles;
    /// the assumption is that the overall game system will keep the `Universe` alive
    /// and that `Universe` will ensure no entry goes away while referenced.
    weak_ref: Weak<RefCell<UEntry<T>>>,
    hash: u64,
}

impl<T: 'static> URef<T> {
    /// Borrow the value, in the sense of std::RefCell::borrow.
    pub fn borrow(&self) -> UBorrow<T> {
        UBorrow(OwningRef::new(
            OwningHandle::new(self.upgrade())
        ).map(|entry| &entry.data))
    }

    /// Borrow the value mutably, in the sense of std::RefCell::borrow_mut.
    pub fn borrow_mut(&self) -> UBorrowMut<T> {
        UBorrowMut(OwningRefMut::new(
            OwningHandle::new_mut(self.upgrade())
        ).map_mut(|entry| &mut entry.data))
    }

    fn upgrade(&self) -> Rc<RefCell<UEntry<T>>> {
        self.weak_ref.upgrade().expect("stale URef")
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
        self.hash.hash(state);
    }
}

/// A wrapper type for an immutably borrowed value from an `URef<T>`.
pub struct UBorrow<T: 'static>(
    OwningRef<
        OwningHandle<
            StrongEntryRef<T>,
            Ref<'static, UEntry<T>>>,
        T>);
/// A wrapper type for a mutably borrowed value from an `URef<T>`.
pub struct UBorrowMut<T: 'static>(
    OwningRefMut<
        OwningHandle<
            StrongEntryRef<T>,
            RefMut<'static, UEntry<T>>>,
        T>);
impl<T> Deref for UBorrow<T> {
    type Target = T;
    fn deref(&self) -> &T { self.0.deref() }
}
impl<T> Deref for UBorrowMut<T> {
    type Target = T;
    fn deref(&self) -> &T { self.0.deref() }
}
impl<T> DerefMut for UBorrowMut<T> {
    fn deref_mut(&mut self) -> &mut T { self.0.deref_mut() }
}

/// The data of an entry in a `Universe`.
#[derive(Debug)]
struct UEntry<T> {
    data: T,
    name: Name,
}

/// The unique reference to an entry in a `Universe` from that `Universe`.
/// Normal usage is via `URef` instead.
#[derive(Debug)]
struct URootRef<T> {
    strong_ref: StrongEntryRef<T>,
    /// Arbitrary hash code assigned to this entry's `URef`s.
    hash: u64,
}

impl<T> URootRef<T> {
    fn new(name: Name, initial_value: T) -> Self {
        // Grab whatever we can to make a random unique hash code, which isn't much.
        // Hopefully we're rarely creating many refs with the same name that will
        // be compared.
        let mut hasher = DefaultHasher::new();
        name.hash(&mut hasher);

        URootRef {
            strong_ref: Rc::new(RefCell::new(UEntry {
                data: initial_value,
                name,
            })),
            hash: hasher.finish(),
        }
    }

    /// Convert to `URef`.
    ///
    /// TODO: As we add graph analysis features, this will need additional arguments
    /// like where the ref is being held, and it will probably need to be renamed.
    fn downgrade(&self) -> URef<T> {
        URef {
            weak_ref: Rc::downgrade(&self.strong_ref),
            hash: self.hash
        }
    }

    /// Borrow the value mutably, in the sense of std::RefCell::borrow_mut.
    fn borrow_mut(&self) -> UBorrowMut<T> {
        self.downgrade().borrow_mut()
    }
}

/// Algorithm for deciding how to execute simulation and rendering frames.
/// Platform-independent; only returns decisions given provided information.
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

    pub fn new() -> Self {
        Self {
            last_absolute_time: None,
            render_dirty: true,
            accumulated_step_time: Duration::default(),
        }
    }

    /// Advance the clock using a source of absolute time.
    ///
    /// This cannot be meaningfully used in combination with `request_frame()`.
    pub fn advance_to(&mut self, instant: Instant) {
        if let Some(last_absolute_time) = self.last_absolute_time {
            let delta = instant - last_absolute_time;
            self.accumulated_step_time += delta;
            self.cap_step_time();
        }
        self.last_absolute_time = Some(instant);
    }

    /// Reacts to a callback from the environment requesting drawing a frame ASAP if
    /// we're going to (i.e. requestAnimationFrame on the web). Drives the simulation
    /// clock based on this input (it will not advance if no requests are made).
    ///
    /// Returns whether a frame should actually be rendered now. The caller should also
    /// consult `should_step()` afterward to schedule game state steps.
    ///
    /// This cannot be meaningfully used in combination with `advance_to()`.
    #[must_use]
    pub fn request_frame(&mut self, time_since_last_frame: Duration) -> bool {
        let result = self.should_draw();
        self.did_draw();

        self.accumulated_step_time += time_since_last_frame;
        self.cap_step_time();

        result
    }

    pub fn should_draw(&self) -> bool {
        self.render_dirty
    }

    pub fn did_draw(&mut self) {
        self.render_dirty = false;
    }

    pub fn should_step(&self) -> bool {
        self.accumulated_step_time >= Self::STEP_LENGTH
    }

    pub fn step_length(&self) -> Duration {
        Self::STEP_LENGTH
    }

    pub fn did_step(&mut self) {
        self.accumulated_step_time -= Self::STEP_LENGTH;
        self.render_dirty = true;
    }

    fn cap_step_time(&mut self) {
        if self.accumulated_step_time > Self::ACCUMULATOR_CAP {
            self.accumulated_step_time = Self::ACCUMULATOR_CAP;
        }
    }
}

impl Default for FrameClock {
    fn default() -> Self { Self::new() }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::blockgen::make_some_blocks;

    #[test]
    pub fn uref_equality_is_pointer_equality() {
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
    pub fn insert_anonymous_makes_distinct_names() {
        let blocks = make_some_blocks(2);
        let mut u = Universe::new();
        let ref_a = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let ref_b = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        ref_a.borrow_mut().set((0, 0, 0), &blocks[0]);
        ref_b.borrow_mut().set((0, 0, 0), &blocks[1]);
        assert!(ref_a != ref_b, "not equal");
        assert!(ref_a.borrow()[(0, 0, 0)] != ref_b.borrow()[(0, 0, 0)], "different values");
    }
}
