//! Tools that we could imagine being in the Rust standard library, but aren't.

#[doc(no_inline)]
pub use yield_progress::{Builder as YieldProgressBuilder, YieldProgress};

#[doc(no_inline)]
pub use manyfmt::{refmt, Fmt, Refmt};

// Unfortunately, we can't use a glob re-export here or `ErrorIfStd` ends up visible when it
// shouldn't be, mysteriously. So, explicit everything instead, with their various visibilities
// and cfgs.
#[cfg(feature = "std")]
#[doc(hidden)]
pub use all_is_cubes_base::util::ErrorChain;
#[doc(hidden)]
pub use all_is_cubes_base::util::{
    assert_conditional_send_sync, assert_send_sync, ErrorIfStd, MapExtend, TypeName,
};
pub use all_is_cubes_base::util::{ConciseDebug, Executor, TimeStats};

// macros can only be exported from their defining crate at the root, but we can fix the path here
#[doc(hidden)]
pub use all_is_cubes_base::cfg_should_impl_error;

#[doc(hidden)] // public to allow our other crates to match, only
pub mod maybe_sync;

mod status_text;
pub use status_text::*;

#[doc(hidden)]
pub fn yield_progress_for_testing() -> YieldProgress {
    // Theoretically we should use Tokio's yield function, but it shouldn't matter for
    // tests and I don't want the dependency here.
    yield_progress::Builder::new().build()
}
