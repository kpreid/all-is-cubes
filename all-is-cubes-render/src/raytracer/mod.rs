//! CPU raytracer for All is Cubes content.
//!
//! While not fast enough for serious interactive use,
//! this raytracer serves as the reference implementation of rendering.
//! Some of its components are also used in areas like computing derived properties of blocks.
//! It may also be used for visual output in tests via [`print_space()`].

#[cfg(doc)]
use all_is_cubes::math::Rgba;

// -------------------------------------------------------------------------------------------------

/// Implements [`Accumulate`] for RGB(A) color with [`f32`] components,
/// and conversion to [`Rgba`].
///
/// In addition to its use in constructing images, this type is also used as an intermediate
/// format for surface colors presented to any other [`Accumulate`] implementation.
#[doc(inline)]
pub use all_is_cubes::raytracer_components::ColorBuf;

mod accum;
pub use accum::{Accumulate, DepthBuf, RtBlockData, RtOptionsRef};

mod hit;
pub use hit::{Exception, Hit, Position};

mod raycast_traits;

mod sr;
#[expect(clippy::module_name_repetitions)] // TODO: consider renamings of *Raytracer* items
pub use sr::{RaytraceInfo, SpaceRaytracer};
use sr::{TracingBlock, TracingCubeData};

mod renderer;
pub use renderer::{RtRenderer, RtScene};

#[doc(hidden)] // experimental/internal, used only by test-renderers right now
pub mod ortho;

mod surface;

mod text;
#[cfg(feature = "std")]
pub use text::print_space;
pub use text::{CharacterBuf, CharacterRtData};

mod updating;
#[expect(clippy::module_name_repetitions)]
pub use updating::UpdatingSpaceRaytracer;

// -------------------------------------------------------------------------------------------------

type BounceRng = rand::rngs::SmallRng;
