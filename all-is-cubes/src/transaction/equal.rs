use core::fmt;
use core::hash::Hash;

use crate::transaction::Merge;
use crate::universe;

/// A component of a transaction which either optionally compares a value or optionally sets a
/// value, but in either case, fails to merge with another transaction unless the value is equal.
#[doc(hidden)] // for internal use only, not stable API
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)]
#[repr(transparent)]
pub struct Equal<T>(pub Option<T>);

impl<T> Equal<T> {
    /// Use this [`Equal`] as a transaction precondition, checking that the target is equal to this.
    pub fn check<U>(&self, target: &U) -> Result<(), Unequal>
    where
        T: PartialEq<U>,
    {
        match &self.0 {
            Some(expected) if *expected != *target => Err(Unequal),
            _ => Ok(()),
        }
    }

    /// Use this [`Equal`] as a transaction effect, overwriting the target if `self` has a
    /// new value. Cannot fail.
    pub fn commit(&self, target: &mut T)
    where
        T: Clone,
    {
        if let Some(new_value) = &self.0 {
            target.clone_from(new_value);
        }
    }
}

impl<T: PartialEq> Merge for Equal<T> {
    type MergeCheck = ();

    type Conflict = Unequal;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        match (&self.0, &other.0) {
            (Some(a), Some(b)) if a != b => Err(Unequal),
            _ => Ok(()),
        }
    }

    fn commit_merge(&mut self, other: Self, (): Self::MergeCheck) {
        match (&mut self.0, other.0) {
            // Merge other into this
            (this @ None, other @ Some(_)) => *this = other,

            // Conflict
            (&mut Some(ref this), Some(ref other)) if this != other => {
                panic!("Equal::merge() was given invalid merge check")
            }

            // Either the value is already in self or neither has a value; nothing to do
            _ => {}
        }
    }
}

impl<T> Default for Equal<T> {
    fn default() -> Self {
        Self(None)
    }
}

impl<T: universe::VisitHandles> universe::VisitHandles for Equal<T> {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self(value) = self;
        value.visit_handles(visitor);
    }
}

/// Transaction conflict error type for [`Equal`].
///
/// Don't incorporate this into public errors, as it conveys no information.
#[doc(hidden)] // for internal use only, not stable API
#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct Unequal;

impl core::error::Error for Unequal {}

impl fmt::Display for Unequal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("unequal values")
    }
}
