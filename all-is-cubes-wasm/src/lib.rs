//! Web client for All is Cubes.
//!
//! If this documentation looks strangely blank, it's because non-wasm builds have
//! most of the modules excluded from compilation.

// Basic lint settings, which should be identical across all all-is-cubes project crates.
// This list is sorted.
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::needless_update)]
#![allow(clippy::single_match)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::doc_markdown)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::modulo_arithmetic)]
#![warn(clippy::return_self_not_must_use)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::uninlined_format_args)]
#![warn(clippy::unnecessary_self_imports)]
#![warn(clippy::wrong_self_convention)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(trivial_numeric_casts)]
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
// Lenience for tests.
#![cfg_attr(test,
    allow(clippy::float_cmp), // deterministic tests
    allow(clippy::redundant_clone), // prefer regularity over efficiency
)]
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review

// Crate-specific lint settings.
// * TODO: warn(missing_docs), eventually.
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
#[cfg(target_family = "wasm")]
mod web_glue;

/// Wrapper to implement [`all_is_cubes::time::Instant`] for [`instant::Instant`].
///
/// TODO: Replace this with just a direct web API binding?
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
