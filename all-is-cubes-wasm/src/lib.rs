//! Web client for All is Cubes.
//!
//! If this documentation looks strangely blank, it's because non-wasm builds have
//! most of the modules excluded from compilation.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]
// TODO: When Rust 1.87 is stable, change this to expect(wasm_c_abi),
// then remove in the future when the lint is gone
#![allow(unknown_lints, reason = "wasm_c_abi not yet stable")]
#![allow(
    wasm_c_abi,
    reason = "tested and confirmed not affected by ABI transition"
)]

use core::ops;
use core::time::Duration;

#[cfg(target_family = "wasm")]
mod init;
#[cfg(target_family = "wasm")]
#[doc(hidden)] // public for testing
pub mod js_bindings;
#[cfg(target_family = "wasm")]
mod settings;
#[cfg(any(target_family = "wasm", test))]
mod url_params;
#[cfg_attr(not(target_family = "wasm"), expect(unused))]
mod web_glue;
#[cfg(target_family = "wasm")]
mod web_session;

/// Wrapper to implement [`all_is_cubes::time::Instant`] for [`web_time::Instant`].
///
/// Note: This code exists in multiple locations because duplicating it is easier than
/// arranging for a shared dependency.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct AdaptedInstant(web_time::Instant);

impl all_is_cubes::time::Instant for AdaptedInstant {
    fn now() -> Self {
        Self(web_time::Instant::now())
    }

    fn saturating_duration_since(self, other: Self) -> Duration {
        web_time::Instant::saturating_duration_since(&self.0, other.0)
    }
}
impl ops::Add<Duration> for AdaptedInstant {
    type Output = Self;
    fn add(self, rhs: Duration) -> Self::Output {
        Self(self.0 + rhs)
    }
}
impl ops::Sub<Duration> for AdaptedInstant {
    type Output = Self;
    #[allow(clippy::unchecked_duration_subtraction)]
    fn sub(self, rhs: Duration) -> Self::Output {
        Self(self.0 - rhs)
    }
}
