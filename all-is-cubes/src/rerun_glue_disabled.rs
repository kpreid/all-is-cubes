use core::fmt;
use core::marker::PhantomData;
use core::mem;

use bevy_ecs::prelude as ecs;

// -------------------------------------------------------------------------------------------------

/// Information that an entity or parent of entities can store in order to know where to
/// send their Rerun logging data.
///
/// This is currently stubbed out because `feature = "rerun"` is not enabled.
/// This struct stores nothing and has no methods.
#[derive(Clone, Default, ecs::Component)]
#[non_exhaustive]
pub struct Destination {}

impl fmt::Debug for Destination {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Destination").finish_non_exhaustive()
    }
}

// -------------------------------------------------------------------------------------------------

/// [`ecs::SystemParam`] to use to log that a system is executing.
///
/// You *must* call [`LogExecution::name()`] to begin logging.
#[derive(bevy_ecs::system::SystemParam)]
pub(crate) struct LogExecution<'w> {
    _phantom: PhantomData<&'w Destination>,
}

pub(crate) struct LogExecutionActive {
    _phantom: PhantomData<()>,
}

impl Drop for LogExecution<'_> {
    fn drop(&mut self) {
        panic!("`LogExecution` must be given a name to log");
    }
}

impl LogExecution<'_> {
    /// Use this when you have a `&mut World` and can't use the `SystemParam` form.
    #[must_use = "you must assign the result to a variable"]
    pub fn from_world(
        world: &ecs::World,
        name: &'static str,
        initial_state: &'static str,
    ) -> LogExecutionActive {
        _ = world;
        _ = name;
        _ = initial_state;
        LogExecutionActive {
            _phantom: PhantomData,
        }
    }

    #[must_use = "you must assign the result to a variable"]
    pub fn name(self, name: &'static str, initial_state: &'static str) -> LogExecutionActive {
        assert_ne!(initial_state, "");
        _ = name;
        mem::forget(self); // disarm Drop panic
        LogExecutionActive {
            _phantom: PhantomData,
        }
    }
}

impl LogExecutionActive {
    pub fn set_state(&self, state: &'static str) {
        assert_ne!(state, "");
        _ = self;
    }
}

impl Drop for LogExecutionActive {
    fn drop(&mut self) {}
}
