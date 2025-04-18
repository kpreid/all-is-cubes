use naga_rust_embed::rt;

use all_is_cubes::euclid::Transform3D;
use all_is_cubes_render::camera::{AntialiasingOption, Camera, FogOption, LightingOption};

// -------------------------------------------------------------------------------------------------

/// Information corresponding to [`Camera`] but in a form suitable for passing in a
/// uniform buffer to the `blocks-and-lines.wgsl` shader. Also includes some miscellaneous
/// data for rendering [`Space`], which hasn't yet demonstrated enough distinction
/// to be worth putting in a separate buffer.
pub(crate) use crate::shaders::blocks_and_lines::ShaderSpaceCamera;

impl ShaderSpaceCamera {
    // We can’t call this function `new()` because it conflicts with the automatic shader-style
    // new() function. TODO: consider making naga-rust-embed not hog that name.
    pub fn from_camera(camera: &Camera) -> Self {
        let options = camera.options();
        let view_distance = camera.view_distance().into_inner() as f32;

        let effective_fog = if options.debug_pixel_cost {
            &FogOption::None
        } else {
            &options.fog
        };
        let (fog_mode_blend, fog_distance) = match effective_fog {
            FogOption::Abrupt => (1.0, view_distance),
            FogOption::Compromise => (0.5, view_distance),
            FogOption::Physical => (0.0, view_distance),
            /* FogOption::None | */ _ => (0.0, f32::INFINITY),
        };

        let projection_matrix = camera.projection_matrix();

        // If the matrix isn't invertible, then what we're rendering must be degenerate (e.g.
        // zero FOV), so use a mostly harmless placeholder.
        let inverse_projection =
            convert_matrix(projection_matrix.inverse().unwrap_or(Transform3D::identity()));

        Self {
            projection: convert_matrix(projection_matrix),
            inverse_projection,
            view_matrix: convert_matrix(camera.view_matrix()),
            view_position: camera.view_position().map(|s| s as f32).to_vector().to_array().into(),

            light_option: match options.lighting_display {
                LightingOption::None => 0,
                LightingOption::Flat => 1,
                LightingOption::Linear | LightingOption::Bounce { .. } => 2,
                LightingOption::Smoothstep => 3,
                LightingOption::Coarse => 4,
                ref u => unreachable!("Unhandled LightingOption value {u:?}"),
            },

            antialiasing_option: match options.antialiasing {
                AntialiasingOption::None => 0,
                AntialiasingOption::IfCheap | AntialiasingOption::Always => 1,
                ref u => unreachable!("Unhandled AntialiasingOption value {u:?}"),
            },

            fog_mode_blend,
            fog_distance,

            exposure: if options.debug_pixel_cost {
                1.0
            } else {
                camera.exposure().into_inner()
            },
        }
    }
}

pub(crate) fn convert_matrix<Src, Dst>(matrix: Transform3D<f64, Src, Dst>) -> rt::Mat4x4<f32> {
    rt::Mat4x4::from_column_arrays(matrix.cast::<f32>().to_arrays())
}
