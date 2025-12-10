//! Tools that we could imagine being in the Rust standard library, but aren't.

// -------------------------------------------------------------------------------------------------
// Re-exports

#[doc(no_inline)]
pub use yield_progress::{Builder as YieldProgressBuilder, YieldProgress};

#[doc(no_inline)]
pub use manyfmt::{Fmt, Refmt, refmt};

// Unfortunately, we can't use a glob re-export here or hidden items end up visible when they
// shouldn't be, mysteriously. So, explicit everything instead, with their various visibilities
// and cfgs. TODO: Does that only apply to macros?
pub use all_is_cubes_base::util::{ConciseDebug, Executor};
#[doc(hidden)]
pub use all_is_cubes_base::util::{
    ErrorChain, MapExtend, TypeName, assert_conditional_send_sync, assert_send_future,
    assert_send_sync, log,
};

// -------------------------------------------------------------------------------------------------
// Modules from this crate

pub(crate) mod atomic_cell;

mod status_text;
pub use status_text::*;

// -------------------------------------------------------------------------------------------------

#[doc(hidden)]
pub fn yield_progress_for_testing() -> YieldProgress {
    // Theoretically we should use Tokio's yield function, but it shouldn't matter for
    // tests and I don't want the dependency here.
    yield_progress::Builder::new().build()
}

/// Ignores the poison flag in the result of locking a [`Mutex`].
///
/// Mutex poisoning is intended to avoid depending on the results of a previous operation
/// that was unexpectedly interrupted by a panic.
/// It is valid to ignore poisoning, such as via this function, in specific cases such as:
///
/// * The operation is logically append-only (the new value is no wronger than the previous).
/// * The effects of incomplete data are not contagious and will only affect the entities
///   which are presumed to already be in a bad state due to the previous failure.
/// * The value is to be completely replaced (the previous value does not matter).
///   In this case, don't use this function, because you should call `clear_poison()` too.
pub(crate) fn ignore_poison<T>(result: Result<T, bevy_platform::sync::PoisonError<T>>) -> T {
    match result {
        Ok(guard) => guard,
        Err(poison_error) => poison_error.into_inner(),
    }
}
