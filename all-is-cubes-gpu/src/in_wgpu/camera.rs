use all_is_cubes::euclid::Transform3D;
use all_is_cubes_render::camera::{AntialiasingOption, Camera, FogOption, LightingOption};

/// Information corresponding to [`Camera`] but in a form suitable for passing in a
/// uniform buffer to the `blocks-and-lines.wgsl` shader. Also includes some miscellaneous
/// data for rendering [`Space`], which hasn't yet demonstrated enough distinction
/// to be worth putting in a separate buffer.
#[repr(C, align(16))] // align triggers bytemuck error if the size doesn't turn out to be a multiple
#[derive(Debug, Copy, Clone, bytemuck::Pod, bytemuck::Zeroable)]
pub(crate) struct ShaderSpaceCamera {
    projection_matrix: [[f32; 4]; 4],
    pub(super) inverse_projection_matrix: [[f32; 4]; 4],
    view_matrix: [[f32; 4]; 4],

    // --- 16-byte aligned point ---
    /// Eye position in world coordinates. Used for computing distance
    /// in volumetric rendering.
    view_position: [f32; 3],

    /// Scale factor for scene brightness.
    exposure: f32,

    // --- 16-byte aligned point ---
    /// Light rendering style to use; a copy of [`GraphicsOptions::lighting_display`].
    light_option: i32,

    /// 0 or 1 indicating whether to apply antialiasing to block texture texel edges.
    antialiasing_option: i32,

    /// Fog equation blending: 0 is realistic fog and 1 is distant more abrupt fog.
    fog_mode_blend: f32,
    /// How far out should be fully fogged?
    fog_distance: f32,
}

impl ShaderSpaceCamera {
    pub fn new(camera: &Camera) -> Self {
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
        let inverse_projection_matrix = convert_matrix(
            projection_matrix
                .inverse()
                .unwrap_or(Transform3D::identity()),
        );

        Self {
            projection_matrix: convert_matrix(projection_matrix),
            inverse_projection_matrix,
            view_matrix: convert_matrix(camera.view_matrix()),
            view_position: camera.view_position().map(|s| s as f32).to_vector().into(),

            light_option: match options.lighting_display {
                LightingOption::None => 0,
                LightingOption::Flat => 1,
                LightingOption::Smooth | LightingOption::Bounce => 2,
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

pub(crate) fn convert_matrix<Src, Dst>(matrix: Transform3D<f64, Src, Dst>) -> [[f32; 4]; 4] {
    matrix.cast::<f32>().to_arrays()
}
