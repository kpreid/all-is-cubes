//! Tests for [`all_is_cubes_port`] which import, or compare against, provided data files.
//!
//! These tests live in a separate test target so that they can be cleanly excluded from the
//! published package, to avoid distributing test data files unnecessarily.

#![cfg_attr(
    not(all(feature = "export", feature = "import", feature = "gltf")),
    allow(
        unused_extern_crates,
        unused_imports,
        reason = "there may be unused imports unless all features are enabled, \
                  and it’s not worth our time to annotate them individually"
    )
)]

extern crate all_is_cubes_port as port;

// glTF is export only
#[cfg(all(feature = "gltf", feature = "export"))]
mod gltf;

#[cfg(feature = "dot-vox")]
mod mv;

#[cfg(feature = "native")]
mod native;
