// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Top-level game state container.

use owning_ref::{OwningHandle, OwningRef, OwningRefMut};
use std::cell::{Ref, RefCell, RefMut};
use std::rc::{Rc, Weak};
use std::ops::{Deref, DerefMut};
use std::time::Duration;

use crate::space::{Grid, Space, SpaceStepInfo};
use crate::camera::Camera;
use crate::worldgen::{axes, plain_color_blocks, wavy_landscape};

/// In the future, a Universe will be a collection of named objects, which can refer
/// to each other. It will enable multiple references, garbage collection, change
/// notification, and inter-object invariants.
///
/// For now, it's a hardcoded container of all the mutable/steppable game state, which
/// stores each entry in the fashion which imposes constraints needed for the above.
pub struct Universe {
    space: URootRef<Space>,
    camera: URootRef<Camera>,
}

impl Universe {
    /// Creates a Universe with some content for a "new game", as much as that can exist.
    pub fn new_test_universe() -> Self {
        let mut space = Space::empty(Grid::new((-10, -10, -10), (21, 21, 21)));
        let blocks = plain_color_blocks();
        wavy_landscape(&mut space, blocks, 1.0);
        axes(&mut space);

        let camera = Camera::for_grid(space.grid());

        Self {
            space: URootRef::new("space", space),
            camera: URootRef::new("camera", camera),
        }
    }

    // TODO: These should eventually go away as the universe becomes more complex.
    pub fn space(&self) -> UBorrow<Space> { self.space.borrow() }
    pub fn camera(&self) -> UBorrow<Camera> { self.camera.borrow() }
    pub fn camera_mut(&mut self) -> UBorrowMut<Camera> { self.camera.borrow_mut() }

    /// Advance time.
    pub fn step(&mut self, timestep: Duration) -> (SpaceStepInfo, ()) {
        let space_info = self.space.borrow_mut().step(timestep);
        let camera_info = self.camera.borrow_mut().step(timestep, &*self.space());
        (space_info, camera_info)
    }
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
        return Weak::ptr_eq(&self.weak_ref, &other.weak_ref);
    }
}
/// `URef`s are compared by pointer equality.
impl<T> Eq for URef<T> {}

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
    name: String,
}

/// The unique reference to an entry in a `Universe` from that `Universe`.
#[derive(Debug)]
struct URootRef<T> {
    strong_ref: StrongEntryRef<T>,
}

impl<T> URootRef<T> {
    fn new(name: impl Into<String>, initial_value: T) -> Self {
        URootRef {
            strong_ref: Rc::new(RefCell::new(UEntry {
                data: initial_value,
                name: name.into(),
            })),
        }
    }

    /// Convert to `URef`.
    ///
    /// TODO: As we add graph analysis features, this will need additional arguments
    /// like where the ref is being held, and it will probably need to be renamed.
    fn downgrade(&self) -> URef<T> {
        URef { weak_ref: Rc::downgrade(&self.strong_ref) }
    }

    /// Borrow the value, in the sense of std::RefCell::borrow.
    fn borrow(&self) -> UBorrow<T> {
        self.downgrade().borrow()
    }

    /// Borrow the value mutably, in the sense of std::RefCell::borrow_mut.
    fn borrow_mut(&self) -> UBorrowMut<T> {
        self.downgrade().borrow_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn new_test_universe_smoke_test() {
        let mut u = Universe::new_test_universe();
        u.space();
        u.camera();
        u.camera_mut();
        u.step(Duration::from_millis(10));
    }

    #[test]
    pub fn uref_equality_is_pointer_equality() {
        let root_a = URootRef::new("space", Space::empty_positive(1, 1, 1));
        let root_b = URootRef::new("space", Space::empty_positive(1, 1, 1));
        let ref_a_1 = root_a.downgrade();
        let ref_a_2 = root_a.downgrade();
        let ref_b_1 = root_b.downgrade();
        assert_eq!(ref_a_1, ref_a_1, "reflexive eq");
        assert_eq!(ref_a_1, ref_a_2, "separately constructed are equal");
        assert!(ref_a_1 != ref_b_1, "not equal");
    }

    // TODO: more tests of the hairy reference logic
}
