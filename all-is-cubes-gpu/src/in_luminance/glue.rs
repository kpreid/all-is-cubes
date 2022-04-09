// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use luminance::framebuffer::FramebufferError;
use luminance::pipeline::PipelineError;
use luminance::tess::TessError;
use luminance::texture::TextureError;

use all_is_cubes::cgmath::Point3;
use all_is_cubes::math::{FreeCoordinate, Geometry, Rgba};
use all_is_cubes::util::MapExtend;

use crate::in_luminance::types::LinesVertex;
use crate::GraphicsResourceError;

/// Add the wireframe of `geometry` to `vertices` (to be drawn in [`Line`](Mode::Line)
/// mode) with the given `color`.
pub(crate) fn wireframe_vertices<E, G>(vertices: &mut E, color: Rgba, geometry: G)
where
    E: Extend<LinesVertex>,
    G: Geometry,
{
    geometry.wireframe_points(&mut MapExtend::new(
        vertices,
        |(p, vertex_color): (Point3<FreeCoordinate>, Option<Rgba>)| {
            LinesVertex::new_basic(p, vertex_color.unwrap_or(color))
        },
    ))
}

// Conversions for [`GraphicsResourceError`]
impl From<FramebufferError> for GraphicsResourceError {
    fn from(source: FramebufferError) -> Self {
        GraphicsResourceError::new(source)
    }
}
impl From<PipelineError> for GraphicsResourceError {
    fn from(source: PipelineError) -> Self {
        GraphicsResourceError::new(source)
    }
}
impl From<TessError> for GraphicsResourceError {
    fn from(source: TessError) -> Self {
        GraphicsResourceError::new(source)
    }
}
impl From<TextureError> for GraphicsResourceError {
    fn from(source: TextureError) -> Self {
        GraphicsResourceError::new(source)
    }
}
