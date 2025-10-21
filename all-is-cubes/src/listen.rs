//! Broadcasting of notifications of state changes, and other messages.
//!
//! This module is a re-export of selected items from [`nosy`].
//! Caution: if the `"std"` feature is disabled, they will change in non-additive ways.

// -------------------------------------------------------------------------------------------------

pub use ::nosy::{
    Buffer, Constant, Flag, FromListener, Gate, GateListener, IntoListener, Listen, Listener, Log,
    NullListener, Source, Store, StoreLock, future::WakeFlag,
};

#[cfg(feature = "std")]
pub use ::nosy::sync::{DynListener, DynSource, Notifier, constant};
#[cfg(not(feature = "std"))]
pub use ::nosy::unsync::{DynListener, DynSource, Notifier, constant};

mod listeners;
pub use listeners::FnListener;

// -------------------------------------------------------------------------------------------------

/// [`nosy::Cell`] with value mutex provided by `bevy_platform`.
pub type Cell<T> = nosy::Cell<mutex::Mutex<T>, DynListener<()>>;
/// [`nosy::CellWithLocal`] with value mutex provided by `bevy_platform`.
pub type CellWithLocal<T> = nosy::Cell<mutex::Mutex<T>, DynListener<()>>;

/// Module for public-in-private hidden type.
mod mutex {
    use core::{fmt, mem};

    /// Mutex type for use with [`nosy::LoadStore`].
    ///
    /// This is a wrapper type which exists in order to effectively
    /// `impl nosy::LoadStore for bevy_platform::sync::Mutex`
    /// without violating trait implementation coherence rules.
    #[allow(unnameable_types)]
    pub struct Mutex<T: ?Sized>(bevy_platform::sync::Mutex<T>);

    impl<T: Clone> nosy::LoadStore for Mutex<T> {
        type Value = T;

        fn new(value: T) -> Self
        where
            Self: Sized,
        {
            Mutex(bevy_platform::sync::Mutex::new(value))
        }

        fn get(&self) -> T {
            unpoison(self.0.lock()).clone()
        }
        fn replace(&self, new_value: T) -> T {
            mem::replace(&mut *unpoison(self.0.lock()), new_value)
        }
        fn replace_if_unequal(&self, new_value: Self::Value) -> Result<T, T>
        where
            Self::Value: PartialEq,
        {
            let mut guard: bevy_platform::sync::MutexGuard<'_, T> = unpoison(self.0.lock());
            if new_value == *guard {
                Err(new_value)
            } else {
                Ok(mem::replace(&mut *guard, new_value))
            }
        }
    }

    impl<T: ?Sized + fmt::Debug> fmt::Debug for Mutex<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.0.fmt(f)
        }
    }

    /// Lock poisoning can be ignored for `LoadStore`, because the only way we ever modify the
    /// value in the mutex is by [`mem::replace`] which does not panic, so the value is, at worst,
    /// stale, not internally inconsistent.
    fn unpoison<T>(result: Result<T, bevy_platform::sync::PoisonError<T>>) -> T {
        match result {
            Ok(guard) => guard,
            Err(error) => error.into_inner(),
        }
    }
}
