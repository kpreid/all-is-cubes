//! CPU raytracer for All is Cubes content.
//!
//! While not fast enough for serious interactive use,
//! this raytracer serves as the reference implementation of rendering.
//! Some of its components are also used in areas like computing derived properties of blocks.
//! It may also be used for visual output in tests via [`print_space()`].

// As a workaround for <https://github.com/rust-lang/rust/issues/127445>,
// we list some items explicitly even though they should be redundant with
// the glob re-exports.

#[cfg(feature = "std")]
pub use all_is_cubes::raytracer::print_space;
pub use all_is_cubes::raytracer::{
    Accumulate, CharacterBuf, RtBlockData, SpaceRaytracer, UpdatingSpaceRaytracer, *,
};

mod renderer;
pub use renderer::{RtRenderer, RtScene};

#[doc(hidden)] // experimental/internal, used only by test-renderers right now
pub mod ortho;
