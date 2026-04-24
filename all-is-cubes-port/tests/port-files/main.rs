//! Tests for [`all_is_cubes_port`] which import, or compare against, provided data files.
//!
//! These tests live in a separate test target so that they can be cleanly excluded from the
//! published package, to avoid distributing test data files unnecessarily.

extern crate all_is_cubes_port as port;

#[cfg(feature = "gltf")]
mod gltf;
#[cfg(feature = "dot-vox")]
mod mv;
#[cfg(feature = "native")]
mod native;
