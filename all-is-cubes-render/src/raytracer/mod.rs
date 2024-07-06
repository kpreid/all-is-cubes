//! CPU raytracer for All is Cubes content.
//!
//! ## Why?
//!
//! The original reason this exists is that I thought “we have [`all_is_cubes::raycast`],
//! and that's nearly all the work, so why not?” Secondarily, it was written before
//! the mesh-based renderer, and was useful as a cross-check since
//! it is much simpler. It continues to serve as a “reference implementation”, and is used
//! by the terminal UI and in unit tests via [`print_space`].

// As a workaround for <https://github.com/rust-lang/rust/issues/127445>,
// we list some items explicitly even though they should be redundant with
// the glob re-exports.

pub use all_is_cubes::raytracer::{
    print_space, Accumulate, SpaceRaytracer, UpdatingSpaceRaytracer, *,
};

mod renderer;
pub use renderer::{RtRenderer, RtScene};
