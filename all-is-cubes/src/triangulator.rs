// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for converting blocks/voxels to triangle meshes for rendering
//! (as opposed to raytracing, voxel display hardware, or whatever else).
//!
//! All of the algorithms here are independent of graphics API but may presume that
//! one exists and has specific data types to specialize in.
//!
//! Note on terminology: Some sources say that “tesselation” would be a better name
//! for this operation than “triangulation”. However, “tesselation” means a specific
//! other operation in OpenGL graphics programming, and “triangulation” seems to
//! be the more commonly used term.

use crate::camera::{GraphicsOptions, LightingOption, TransparencyOption};

mod block_vertex;
pub use block_vertex::*;
mod block_tri;
pub use block_tri::*;
mod chunked_tri;
pub(crate) use chunked_tri::*; // TODO: candidate for being public
mod space_tri;
pub use space_tri::*;
mod planar;
use planar::*;
mod texalloc;
pub use texalloc::*;

#[cfg(test)]
mod tests;

/// Inputs required for triangulation that aren't the block/space data itself
/// (or the texture allocator, since that may need to be mutable).
///
/// Creating this and comparing it against a previous instance is appropriate for
/// determining when to invalidate previous results. This type is also intended
/// to make the API future-proof against additional configuration being needed.
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TriangulatorOptions {
    /// Input to TransparencyOption::limit_alpha.
    transparency: TransparencyOption,
    /// Whether to copy light from the space to the vertices (true), or substitute
    /// [`Rgb::ONE`](crate::math::Rgb::ONE) (false).
    use_space_light: bool,
}

impl TriangulatorOptions {
    /// Take the triangulation-relevant options from the [`GraphicsOptions`].
    pub fn new(graphics_options: &GraphicsOptions) -> Self {
        Self {
            transparency: graphics_options.transparency.clone(),
            use_space_light: match graphics_options.lighting_display {
                LightingOption::None => false,
                LightingOption::Flat | LightingOption::Smooth => true,
            },
        }
    }

    /// Placeholder for use in tests which do not care about any of the triangulator
    /// behaviors that are affected by options (yet).
    #[doc(hidden)]
    pub fn dont_care_for_test() -> Self {
        Self {
            transparency: TransparencyOption::Volumetric,
            use_space_light: true,
        }
    }
}
