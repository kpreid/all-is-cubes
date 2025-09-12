//! CPU raytracer for All is Cubes content.
//!
//! While not fast enough for serious interactive use,
//! this raytracer serves as the reference implementation of rendering.
//! Some of its components are also used in areas like computing derived properties of blocks.
//! It may also be used for visual output in tests via [`print_space()`].

// As a workaround for <https://github.com/rust-lang/rust/issues/127445>,
// we list all items explicitly even though glob re-exports would be closer
// to the intent here.

#[cfg(feature = "std")]
pub use all_is_cubes::raytracer::print_space;
#[allow(clippy::module_name_repetitions)] // TODO: consider renamings of *Raytracer* items
pub use all_is_cubes::raytracer::{
    Accumulate, CharacterBuf, CharacterRtData, ColorBuf, Exception, Hit, Position, RaytraceInfo,
    RtBlockData, RtOptionsRef, SpaceRaytracer, UpdatingSpaceRaytracer,
};

mod renderer;
pub use renderer::{RtRenderer, RtScene};

#[doc(hidden)] // experimental/internal, used only by test-renderers right now
pub mod ortho;

mod accum;
pub use accum::DepthBuf;
