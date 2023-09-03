//! Serialization/persistence/saved games.

use std::fmt;

use futures_core::future::BoxFuture;

use crate::universe::Universe;
use crate::util::YieldProgress;

mod compress;
pub(crate) mod conversion;
pub(crate) mod schema;

#[cfg(test)]
mod tests;

/// Specifies a file or other data storage a [`Universe`] can be read from or written to.
///
/// This trait serves as common vocabulary between other high-level components of All is Cubes
/// (user interface, import/export, and procedural generation); that is why there are no
/// interesting implementations here in the core crate.
pub trait WhenceUniverse: fmt::Debug + Send + Sync + downcast_rs::Downcast + 'static {
    /// Returns a string suitable for use as a window title or other user interface element
    /// identifying this universe-document.
    ///
    /// If None, then this indicates that the universe should not be attributed any
    /// specific identity (e.g. if it is empty or was procedurally generated by code that
    /// did not give it any identification).
    fn document_name(&self) -> Option<String>;

    /// Returns whether this implements [`Self::load()`]. If this returns `false`, then
    /// `load()` may be expected to always return an error.
    fn can_load(&self) -> bool;

    /// Returns whether this implements [`Self::save()`]. If this returns `false`, then
    /// `save()` may be expected to always return an error.
    fn can_save(&self) -> bool;

    /// Read a new copy of the universe from storage into memory.
    ///
    /// This may fail due to IO errors, validation errors, or if loading is not implemented.
    ///
    /// If this is called even though [`Self::can_load()`] returned false, it should return
    /// an `Err`, not panic.
    ///
    /// TODO: Define an error type for this to have at least broad categories.
    fn load(
        &self,
        progress: YieldProgress,
    ) -> BoxFuture<'static, Result<Universe, Box<dyn std::error::Error + Send + Sync>>>;

    /// Write the current state of the given universe into the storage denoted by `self`.
    ///
    /// The implementation should implement locking or other mechanisms as necessary to
    /// prevent `load()` and `save()` calls from corrupting each other.
    ///
    /// If this is called even though [`Self::can_save()`] returned false, it should return
    /// an `Err`, not panic.
    ///
    /// TODO: Define an error type for this to have at least broad categories.
    fn save(
        &self,
        universe: &Universe,
        progress: YieldProgress,
    ) -> BoxFuture<'static, Result<(), Box<dyn std::error::Error + Send + Sync>>>;
}

downcast_rs::impl_downcast!(WhenceUniverse);

/// Implementation of [`WhenceUniverse`] used by [`Universe`]s freshly created.
impl WhenceUniverse for () {
    fn document_name(&self) -> Option<String> {
        None
    }

    fn can_load(&self) -> bool {
        false
    }

    fn can_save(&self) -> bool {
        false
    }

    fn load(
        &self,
        _: YieldProgress,
    ) -> BoxFuture<'static, Result<Universe, Box<dyn std::error::Error + Send + Sync>>> {
        Box::pin(std::future::ready(Err(
            "this universe cannot be reloaded because it has no source".into(),
        )))
    }

    fn save(
        &self,
        _universe: &Universe,
        _progress: YieldProgress,
    ) -> BoxFuture<'static, Result<(), Box<dyn std::error::Error + Send + Sync>>> {
        Box::pin(std::future::ready(Err(
            "this universe cannot be saved because a destination has not been specified".into(),
        )))
    }
}
