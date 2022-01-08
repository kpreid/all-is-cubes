// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::borrow::Borrow;
use std::error::Error;
use std::fmt;
use std::hash;
use std::ops::Deref;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard, Weak};

use ouroboros::self_referencing;

use crate::transaction::Transaction;
use crate::transaction::Transactional;
use crate::universe::Name;
#[cfg(doc)]
use crate::universe::Universe;

/// Type of a strong reference to an entry in a [`Universe`]. Defined to make types
/// parameterized with this somewhat less hairy.
type StrongEntryRef<T> = Arc<RwLock<UEntry<T>>>;

/// A reference from an object in a [`Universe`] to another.
///
/// If they are held by objects outside of the [`Universe`], it is not guaranteed
/// that they will remain valid (in which case using the `URef` will return an error
/// or panic depending on the method).
/// To ensure an object does not vanish while operating on it, [`URef::borrow`] it.
/// (TODO: Should there be an operation in the style of `Weak::upgrade`?)
///
/// **Thread-safety caveat:** See the documentation on [avoiding deadlock].
///
/// [avoiding deadlock]: crate::universe#thread-safety
pub struct URef<T> {
    // TODO: We're going to want to either track reference counts or implement a garbage
    // collector for the graph of URefs. Reference counts would be an easy way to ensure
    // nothing is deleted while it is in use from a UI perspective.
    /// Reference to the object. Weak because we don't want to create reference cycles;
    /// the assumption is that the overall game system will keep the [`Universe`] alive
    /// and that [`Universe`] will ensure no entry goes away while referenced.
    weak_ref: Weak<RwLock<UEntry<T>>>,
    name: Name,
}

impl<T: 'static> URef<T> {
    pub fn name(&self) -> &Name {
        &self.name
    }

    /// Borrow the value, in the sense of `RefCell::borrow`, and panic on failure.
    ///
    /// TODO: Update docs to discuss RwLock instead of RefCell, once we have a policy
    /// about waiting for locks.
    #[track_caller]
    pub fn borrow(&self) -> UBorrow<T> {
        self.try_borrow().unwrap()
    }

    /// Borrow the value, in the sense of `RefCell::try_borrow`.
    ///
    /// TODO: Update docs to discuss RwLock instead of RefCell, once we have a policy
    /// about waiting for locks.
    pub fn try_borrow(&self) -> Result<UBorrow<T>, RefError> {
        let inner = UBorrowImpl::try_new(self.upgrade()?, |strong: &Arc<RwLock<UEntry<T>>>| {
            strong
                .try_read()
                .map_err(|_| RefError::InUse(self.name.clone()))
        })?;
        Ok(UBorrow(inner))
    }

    /// Apply the given function to the `&mut T` inside.
    ///
    /// TODO: If possible, replace this operation with transactions, to ensure change notification integrity.
    pub fn try_modify<F, Out>(&self, function: F) -> Result<Out, RefError>
    where
        F: FnOnce(&mut T) -> Out,
    {
        let strong: Arc<RwLock<UEntry<T>>> = self.upgrade()?;
        let mut borrow = strong
            .try_write()
            .map_err(|_| RefError::InUse(self.name.clone()))?;
        Ok(function(&mut borrow.data))
    }

    /// Gain mutable access but don't use it immediately.
    ///
    /// This function is not exposed publicly, but only used in transactions to allow
    /// the check-then-commit pattern; use [`URef::try_modify`] instead for other
    /// purposes.
    pub(crate) fn try_borrow_mut(&self) -> Result<UBorrowMutImpl<T>, RefError> {
        UBorrowMutImpl::try_new(self.upgrade()?, |strong: &Arc<RwLock<UEntry<T>>>| {
            strong
                .try_write()
                .map_err(|_| RefError::InUse(self.name.clone()))
        })
    }

    /// Shortcut for executing a transaction.
    #[allow(dead_code)] // Currently only used in tests
    pub(crate) fn execute(
        &self,
        transaction: &<T as Transactional>::Transaction,
    ) -> Result<<<T as Transactional>::Transaction as Transaction<T>>::Output, Box<dyn Error>>
    where
        T: Transactional,
    {
        self.try_modify(|data| transaction.execute(data))?
    }

    fn upgrade(&self) -> Result<StrongEntryRef<T>, RefError> {
        self.weak_ref
            .upgrade()
            .ok_or_else(|| RefError::Gone(self.name.clone()))
    }
}

impl<T> fmt::Debug for URef<T> {
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
impl<T> hash::Hash for URef<T> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
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
    Gone(Name),
    /// Target is currently incompatibly borrowed.
    #[error("object was in use at the same time: {0}")]
    InUse(Name),
}

/// A wrapper type for an immutably borrowed value from an [`URef`].
pub struct UBorrow<T: 'static>(UBorrowImpl<T>);

impl<T: fmt::Debug> fmt::Debug for UBorrow<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "UBorrow({:?})", **self)
    }
}
impl<T> Deref for UBorrow<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0.borrow_guard().data
    }
}
impl<T> AsRef<T> for UBorrow<T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}
impl<T> Borrow<T> for UBorrow<T> {
    fn borrow(&self) -> &T {
        self.deref()
    }
}

/// Implementation of [`UBorrow`], split out to hide all `self_referencing` details.
#[self_referencing]
struct UBorrowImpl<T: 'static> {
    strong: StrongEntryRef<T>,
    #[borrows(strong)]
    #[covariant]
    guard: RwLockReadGuard<'this, UEntry<T>>,
}

/// Parallel to [`UBorrowImpl`], but for mutable access.
///
/// This type is not exposed publicly, but only used in transactions to allow
/// the check-then-commit pattern; use [`URef::try_modify`] instead for other
/// purposes.
#[self_referencing]
#[derive(Debug)]
pub(crate) struct UBorrowMutImpl<T: 'static> {
    strong: StrongEntryRef<T>,
    #[borrows(strong)]
    #[not_covariant]
    guard: RwLockWriteGuard<'this, UEntry<T>>,
}

impl<T> UBorrowMutImpl<T> {
    pub(crate) fn with_data_mut<F, Out>(&mut self, function: F) -> Out
    where
        F: FnOnce(&mut T) -> Out,
    {
        self.with_guard_mut(|entry| function(&mut entry.data))
    }
}

/// The data of an entry in a `Universe`.
#[derive(Debug)]
struct UEntry<T> {
    // TODO: It might make more sense for data to be a RwLock<T> (instead of the
    // RwLock containing UEntry), but we don't have enough examples to be certain yet.
    data: T,
}

/// The unique reference to an entry in a `Universe` from that `Universe`.
/// Normal usage is via `URef` instead.
#[derive(Debug)]
pub(super) struct URootRef<T> {
    strong_ref: StrongEntryRef<T>,
    name: Name,
}

impl<T> URootRef<T> {
    pub(super) fn new(name: Name, initial_value: T) -> Self {
        URootRef {
            strong_ref: Arc::new(RwLock::new(UEntry {
                data: initial_value,
            })),
            name,
        }
    }

    /// Convert to `URef`.
    ///
    /// TODO: As we add graph analysis features, this will need additional arguments
    /// like where the ref is being held, and it will probably need to be renamed.
    pub(crate) fn downgrade(&self) -> URef<T> {
        URef {
            weak_ref: Arc::downgrade(&self.strong_ref),
            name: self.name.clone(),
        }
    }
}

/// Object-safe trait implemented for [`URef`].
///
/// TODO: seal this trait?
pub trait URefErased: core::any::Any {
    fn name(&self) -> &Name;
}

impl<T: 'static> URefErased for URef<T> {
    fn name(&self) -> &Name {
        URef::name(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::space::Space;
    use crate::universe::{Universe, UniverseIndex};

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
        r.try_modify(|_| {
            assert_eq!(
                r.try_borrow().unwrap_err(),
                RefError::InUse(Name::Anonym(0))
            );
        })
        .unwrap();
    }

    #[test]
    fn uref_try_borrow_mut_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.borrow();
        assert_eq!(
            r.try_borrow_mut().unwrap_err(),
            RefError::InUse(Name::Anonym(0))
        );
    }

    #[test]
    fn uref_try_modify_in_use() {
        let mut u = Universe::new();
        let r = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let _borrow_1 = r.borrow();
        assert_eq!(
            r.try_modify(|_| {}).unwrap_err(),
            RefError::InUse(Name::Anonym(0))
        );
    }

    #[test]
    fn ref_error_format() {
        assert_eq!(
            RefError::InUse("foo".into()).to_string(),
            "object was in use at the same time: 'foo'"
        );
        assert_eq!(
            RefError::Gone("foo".into()).to_string(),
            "object was deleted: 'foo'"
        );
        assert_eq!(
            RefError::Gone(Name::Anonym(123)).to_string(),
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
}
