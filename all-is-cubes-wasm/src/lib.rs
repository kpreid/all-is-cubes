//! Web client for All is Cubes.
//!
//! If this documentation looks strangely blank, it's because non-wasm builds have
//! most of the modules excluded from compilation.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

use core::ops;
use core::time::Duration;

#[cfg(target_family = "wasm")]
mod gameapp;
#[cfg(target_family = "wasm")]
mod init;
#[cfg(target_family = "wasm")]
#[doc(hidden)] // public for testing
pub mod js_bindings;
#[cfg(any(target_family = "wasm", test))]
mod url_params;
#[cfg_attr(not(target_family = "wasm"), allow(unused))]
mod web_glue;

/// Wrapper to implement [`all_is_cubes::time::Instant`] for [`instant::Instant`].
///
/// Note: This code exists in multiple locations because duplicating it is easier than
/// arranging for a shared dependency.
///
/// TODO: Replace this with just a direct web API binding, not [`instant`]?
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct AdaptedInstant(instant::Instant);

impl all_is_cubes::time::Instant for AdaptedInstant {
    fn now() -> Self {
        Self(instant::Instant::now())
    }

    fn saturating_duration_since(self, other: Self) -> Duration {
        instant::Instant::saturating_duration_since(&self.0, other.0)
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
    fn sub(self, rhs: Duration) -> Self::Output {
        Self(self.0 - rhs)
    }
}
