// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use all_is_cubes::camera::{Camera, FogOption};
use all_is_cubes::cgmath::{EuclideanSpace, Matrix4, Vector3};
use all_is_cubes::math::Rgb;

use crate::in_wgpu::glue::PaddedVec3;

/// Information corresponding to [`Camera`] but in a form
/// suitable for passing in a uniform buffer. Also includes some miscellaneous
/// data for rendering [`Space`], which hasn't yet demonstrated enough distinction
/// to be worth putting in a separate buffer yet.
#[repr(C)]
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
pub(crate) struct WgpuCamera {
    projection_matrix: [[f32; 4]; 4],
    view_matrix: [[f32; 4]; 4],
    /// Eye position in world coordinates. Used for computing distance
    /// in volumetric rendering.
    view_position: PaddedVec3,

    /// Translation which converts mesh cube coordinates to `light_texture` texel
    /// coordinates.
    ///
    /// This is not strictly part of the [`Camera`], but it is expected to change under
    /// the same sort of conditions.
    light_lookup_offset: [i32; 4], // padded from 3 to 4

    /// Color for the fog.
    fog_color: PaddedVec3,
    /// Fog equation blending: 0 is realistic fog and 1 is distant more abrupt fog.
    fog_mode_blend: f32,
    /// How far out should be fully fogged?
    fog_distance: f32,

    /// Scale factor for scene brightness.
    exposure: f32,

    /// pad out to multiple of vec4<f32>
    _padding: f32,
}

impl WgpuCamera {
    pub fn new(camera: &Camera, sky_color: Rgb, light_lookup_offset: Vector3<i32>) -> Self {
        let options = camera.options();
        let view_distance = camera.view_distance() as f32;
        let (fog_mode_blend, fog_distance) = match options.fog {
            FogOption::Abrupt => (1.0, view_distance),
            FogOption::Compromise => (0.5, view_distance),
            FogOption::Physical => (0.0, view_distance),
            /* FogOption::None | */ _ => (0.0, f32::INFINITY),
        };

        Self {
            projection_matrix: convert_matrix(OPENGL_TO_WGPU_PROJECTION * camera.projection()),
            view_matrix: convert_matrix(camera.view_matrix()),
            view_position: camera.view_position().map(|s| s as f32).to_vec().into(),

            light_lookup_offset: light_lookup_offset.extend(0).into(),

            fog_color: Vector3::<f32>::from(sky_color).into(),
            fog_mode_blend,
            fog_distance,

            exposure: camera.exposure.into_inner(),

            _padding: 0.,
        }
    }
}
fn convert_matrix(matrix: Matrix4<f64>) -> [[f32; 4]; 4] {
    matrix.cast::<f32>().unwrap(/* f64 to f32 is infallible */).into()
}

// TODO: does it make sense to break this out really?
const OPENGL_TO_WGPU_PROJECTION: Matrix4<f64> = Matrix4::new(
    1.0, 0.0, 0.0, 0.0, //
    0.0, 1.0, 0.0, 0.0, //
    0.0, 0.0, 0.5, 0.0, //
    0.0, 0.0, 0.5, 1.0, //
);
