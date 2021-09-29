// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use cgmath::{InnerSpace as _, Vector3};

use crate::camera::GraphicsOptions;
use crate::math::{Face, Rgb, Rgba};

/// Description of a surface the ray passes through (or from the volumetric perspective,
/// a transition from one material to another).
// TODO: make public?
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub(crate) struct Surface<'a, D> {
    pub block_data: &'a D,
    // pub voxel_data: ...?,
    pub diffuse_color: Rgba,
    pub illumination: Rgb,
    pub normal: Face,
}

impl<D> Surface<'_, D> {
    /// Convert the surface and its lighting to a single RGBA value as determined by
    /// the given graphics options, or [`None`] if it is invisible.
    ///
    /// Note this is not true volumetric ray tracing: we're considering each
    /// voxel surface to be discrete.
    #[inline]
    pub(crate) fn to_lit_color(&self, options: &GraphicsOptions) -> Option<Rgba> {
        let diffuse_color = options.transparency.limit_alpha(self.diffuse_color);
        if diffuse_color.fully_transparent() {
            return None;
        }
        let adjusted_rgb =
            diffuse_color.to_rgb() * self.illumination * fixed_directional_lighting(self.normal);
        Some(adjusted_rgb.with_alpha(diffuse_color.alpha()))
    }
}

/// Simple directional lighting used to give corners extra definition.
/// Note that this algorithm is also implemented in the fragment shader for GPU rendering.
fn fixed_directional_lighting(face: Face) -> f32 {
    let normal = face.normal_vector();
    const LIGHT_1_DIRECTION: Vector3<f32> = Vector3::new(0.4, -0.1, 0.0);
    const LIGHT_2_DIRECTION: Vector3<f32> = Vector3::new(-0.4, 0.35, 0.25);
    (1.0 - 1.0 / 16.0)
        + 0.25 * (LIGHT_1_DIRECTION.dot(normal).max(0.0) + LIGHT_2_DIRECTION.dot(normal).max(0.0))
}
