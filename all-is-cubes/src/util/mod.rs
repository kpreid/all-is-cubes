//! Tools that we could imagine being in the Rust standard library, but aren't.

#[doc(no_inline)]
pub use yield_progress::{Builder as YieldProgressBuilder, YieldProgress};

#[doc(no_inline)]
pub use manyfmt::{Fmt, Refmt, refmt};

// Unfortunately, we can't use a glob re-export here or hidden items end up visible when they
// shouldn't be, mysteriously. So, explicit everything instead, with their various visibilities
// and cfgs. TODO: Does that only apply to macros?
#[doc(hidden)]
pub use all_is_cubes_base::util::ErrorChain;
pub use all_is_cubes_base::util::{ConciseDebug, Executor, TimeStats};
#[doc(hidden)]
pub use all_is_cubes_base::util::{
    MapExtend, TypeName, assert_conditional_send_sync, assert_send_sync,
};

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
