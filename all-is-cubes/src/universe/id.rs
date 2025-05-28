use core::num::NonZeroU64;
use core::sync::atomic;
use core::sync::atomic::Ordering::{Acquire, Relaxed};

use bevy_ecs::prelude as ecs;

#[cfg(doc)]
use crate::universe::{Handle, Universe};

// -------------------------------------------------------------------------------------------------

/// Copiable unique (within this process) identifier for a [`Universe`].
///
/// Used to check whether [`Handle`]s belong to particular [`Universe`]s.
//---
// IDs are nonzero in order to help implement `OnceUniverseId`.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, ecs::Resource)]
pub struct UniverseId(NonZeroU64);

impl UniverseId {
    pub(in crate::universe) fn new() -> Self {
        #![allow(
            clippy::useless_conversion,
            clippy::unnecessary_fallible_conversions,
            reason = "depends on pointer width and atomic support"
        )]

        static UNIVERSE_ID_COUNTER: AtomicUpTo64 = AtomicUpTo64::new(1);

        let id = from_atomic_value(
            UNIVERSE_ID_COUNTER
                .fetch_update(Relaxed, Relaxed, |counter| counter.checked_add(1))
                .expect("universe id overflow"),
        )
        .expect("uncaught universe id overflow??");

        Self(id)
    }
}

// -------------------------------------------------------------------------------------------------

/// Atomically writable `OnceCell<UniverseId>`.
#[derive(Debug)]
pub(in crate::universe) struct OnceUniverseId(AtomicUpTo64);

impl OnceUniverseId {
    pub fn new() -> Self {
        Self(AtomicUpTo64::new(0))
    }

    /// Set the ID value or return [`Err`] if it was already set.
    ///
    /// The atomic store is done using [`Acquire`] ordering.
    ///
    /// On failure, returns the already-stored ID.
    pub fn set(&self, new_id: UniverseId) -> Result<(), UniverseId> {
        match self
            .0
            .compare_exchange(0, to_atomic_value(new_id.0), Acquire, Relaxed)
        {
            Ok(_) => Ok(()),
            Err(existing) => Err(UniverseId(from_atomic_value(existing).unwrap())),
        }
    }

    #[inline]
    pub fn get(&self, ordering: atomic::Ordering) -> Option<UniverseId> {
        from_atomic_value(self.0.load(ordering)).map(UniverseId)
    }
}

impl From<UniverseId> for OnceUniverseId {
    fn from(id: UniverseId) -> Self {
        Self(AtomicUpTo64::new(to_atomic_value(id.0)))
    }
}

// -------------------------------------------------------------------------------------------------

// Choose the best available atomic type.
cfg_if::cfg_if! {
    // Use 64 bit if possible, because 64 bits is enough to be infeasible to overflow
    // by counting one at a time. If not, compromise on a smaller counter.
    if #[cfg(target_has_atomic = "64")] {
        type AtomicUpTo64 = atomic::AtomicU64;
        type NonAtomicUpTo64 = u64;
        fn from_atomic_value(value: u64) -> Option<NonZeroU64> {
            NonZeroU64::new(value)
        }
    } else if #[cfg(target_has_atomic = "32")] {
        type AtomicUpTo64 = atomic::AtomicU32;
        type NonAtomicUpTo64 = u32;
        fn from_atomic_value(value: u32) -> Option<NonZeroU64> {
            NonZeroU64::new(u64::from(value))
        }
    } else {
        // If this doesn't work we'll give up.
        type AtomicUpTo64 = atomic::AtomicUsize;
        type NonAtomicUpTo64 = usize;
        fn from_atomic_value(value: usize) -> Option<NonZeroU64> {
            NonZeroU64::new(u64::try_from(value).expect(
                "You have >64-bit usize and you overflowed u64? Impressive."
            ))
        }
    }
}

#[allow(
    trivial_numeric_casts,
    clippy::cast_possible_truncation,
    reason = "platform dependent"
)]
fn to_atomic_value(value: NonZeroU64) -> NonAtomicUpTo64 {
    // This should never overflow because we'll overflow UNIVERSE_ID_COUNTER first.
    value.get() as NonAtomicUpTo64
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn once_universe_id() {
        let some_id = UniverseId::new();
        let other_id = UniverseId::new();
        let cell = OnceUniverseId::new();

        assert_eq!(cell.get(Relaxed), None);
        cell.set(some_id).unwrap();
        assert_eq!(cell.get(Relaxed), Some(some_id));

        // Cannot be re-set regardless of value
        cell.set(some_id).unwrap_err();
        cell.set(other_id).unwrap_err();
        assert_eq!(cell.get(Relaxed), Some(some_id));
    }
}
