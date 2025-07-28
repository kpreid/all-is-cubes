//! DIY replacement for the `wgpu` `.global_id()` functionality which is being removed
//! in `wgpu` version 23.

use alloc::sync::Arc;
use core::fmt;
use core::marker::PhantomData;
use core::ops;
use core::sync::atomic::{self, Ordering};

/// Adds a globally unique identifier to a value.
///
/// This identifier may be used to compare two values which do not implement [`PartialEq`],
/// or remember their identities without necessarily retaining the values themselves.
#[derive(Clone, Debug)]
pub(crate) struct Identified<T> {
    id: Id<T>,
    value: T,
}

/// A unique identifier for some [`Identified`] value.
pub(crate) struct Id<T> {
    id: u64,
    _phantom: PhantomData<Arc<T>>,
}

impl<T> Identified<T> {
    /// Wraps the given value and assigns a new unique [`Id`] for it.
    pub fn new(value: T) -> Self {
        static ID_COUNTER: atomic::AtomicU64 = atomic::AtomicU64::new(0);
        let id = ID_COUNTER.fetch_add(1, Ordering::Relaxed);
        Self {
            id: Id {
                id,
                _phantom: PhantomData,
            },
            value,
        }
    }

    pub fn global_id(&self) -> Id<T> {
        self.id
    }
}

impl<T> const ops::Deref for Identified<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> Eq for Id<T> {}
impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T> std::hash::Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Id").field(&self.id).finish()
    }
}

impl<T> Copy for Id<T> {}
impl<T> const Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}
