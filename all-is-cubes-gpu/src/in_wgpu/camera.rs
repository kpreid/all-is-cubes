use all_is_cubes::camera::{Camera, FogOption, LightingOption, Ndc};
use all_is_cubes::euclid::Transform3D;
use all_is_cubes::math::{GridVector, Rgb, VectorOps};

use crate::in_wgpu::glue::PaddedVec3;

/// Information corresponding to [`Camera`] but in a form suitable for passing in a
/// uniform buffer to the `blocks-and-lines.wgsl` shader. Also includes some miscellaneous
/// data for rendering [`Space`], which hasn't yet demonstrated enough distinction
/// to be worth putting in a separate buffer.
#[repr(C, align(16))] // align triggers bytemuck error if the size doesn't turn out to be a multiple
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
pub(crate) struct ShaderSpaceCamera {
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
    light_lookup_offset: [i32; 3], // next field functions as the required 4th component/padding
    /// Light rendering style to use; a copy of [`GraphicsOptions::lighting_display`].
    light_option: i32,

    /// Color for the fog.
    fog_color: [f32; 3],
    /// Fog equation blending: 0 is realistic fog and 1 is distant more abrupt fog.
    fog_mode_blend: f32,
    /// How far out should be fully fogged?
    fog_distance: f32,

    /// Scale factor for scene brightness.
    exposure: f32,

    /// pad out to multiple of vec4<something32>
    _padding: [i32; 2],
}

impl ShaderSpaceCamera {
    pub fn new(camera: &Camera, sky_color: Rgb, light_lookup_offset: GridVector) -> Self {
        let options = camera.options();
        let view_distance = camera.view_distance() as f32;
        let (fog_mode_blend, fog_distance) = match options.fog {
            FogOption::Abrupt => (1.0, view_distance),
            FogOption::Compromise => (0.5, view_distance),
            FogOption::Physical => (0.0, view_distance),
            /* FogOption::None | */ _ => (0.0, f32::INFINITY),
        };

        Self {
            projection_matrix: convert_matrix(camera.projection().then(&OPENGL_TO_WGPU_PROJECTION)),
            view_matrix: convert_matrix(camera.view_matrix()),
            view_position: camera.view_position().map(|s| s as f32).to_vector().into(),

            light_lookup_offset: light_lookup_offset.into(),
            light_option: match options.lighting_display {
                LightingOption::None => 0,
                LightingOption::Flat => 1,
                LightingOption::Smooth => 2,
                _ => unreachable!(
                    "Unhandled LightingOption value {:?}",
                    options.lighting_display
                ),
            },

            fog_color: sky_color.into(),
            fog_mode_blend,
            fog_distance,

            exposure: camera.exposure().into_inner(),

            _padding: Default::default(),
        }
    }
}
fn convert_matrix<Src, Dst>(matrix: Transform3D<f64, Src, Dst>) -> [[f32; 4]; 4] {
    matrix.cast::<f32>().to_arrays()
}

// TODO: does it make sense to break this out really?
const OPENGL_TO_WGPU_PROJECTION: Transform3D<f64, Ndc, Ndc> = Transform3D::new(
    1.0, 0.0, 0.0, 0.0, //
    0.0, 1.0, 0.0, 0.0, //
    0.0, 0.0, 0.5, 0.0, //
    0.0, 0.0, 0.5, 1.0, //
);
