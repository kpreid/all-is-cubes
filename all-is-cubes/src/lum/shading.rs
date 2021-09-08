// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Shaders, uniforms, etc.

use cgmath::Matrix4;
use instant::Instant;
use luminance::context::GraphicsContext;
use luminance::pipeline::TextureBinding;
use luminance::pixel::NormUnsigned;
use luminance::shader::{BuiltProgram, Program, ProgramError, ProgramInterface, Uniform};
use luminance::texture::Dim3;
use luminance::UniformInterface;
use luminance_front::Backend;

use crate::camera::{GraphicsOptions, LightingOption, TransparencyOption};
use crate::lum::block_texture::BoundBlockTexture;
use crate::lum::space::SpaceRendererBound;
use crate::lum::types::VertexSemantics;
use crate::lum::GraphicsResourceError;
use crate::math::FreeCoordinate;

/// Type of the block shader program (output of [`prepare_block_program`]).
pub type BlockProgram = Program<Backend, VertexSemantics, (), BlockUniformInterface>;

/// Collection of shaders for rendering blocks, which all share the `BlockUniformInterface`.
pub(crate) struct BlockPrograms {
    pub(crate) opaque: BlockProgram,
    pub(crate) transparent: BlockProgram,
}

impl BlockPrograms {
    pub(crate) fn compile<C>(
        context: &mut C,
        options: &GraphicsOptions,
    ) -> Result<BlockPrograms, GraphicsResourceError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let mut base_defines = Vec::new();
        if options.lighting_display != LightingOption::None {
            base_defines.push(("LIGHTING", "1"));
        }
        if options.lighting_display == LightingOption::Smooth {
            base_defines.push(("SMOOTH_LIGHTING", "1"));
        }
        match options.transparency {
            TransparencyOption::Surface | TransparencyOption::Threshold(_) => {}
            TransparencyOption::Volumetric => {
                base_defines.push(("VOLUMETRIC", "1"));
            }
        }

        Ok(BlockPrograms {
            opaque: prepare_block_program(context, base_defines.iter().copied())?,
            transparent: prepare_block_program(
                context,
                base_defines
                    .iter()
                    .chain([("ALLOW_TRANSPARENCY", "1")].iter())
                    .copied(),
            )?,
        })
    }
}

/// Compile the block shader program for the given [`GraphicsContext`].
fn prepare_block_program<'a, C>(
    context: &mut C,
    defines: impl IntoIterator<Item = (&'a str, &'a str)>,
) -> Result<BlockProgram, GraphicsResourceError>
where
    C: GraphicsContext<Backend = Backend>,
{
    let defines: String = defines
        .into_iter()
        .map(|(k, v)| format!("#define {} {}\n", k, v))
        .collect();

    let concatenated_vertex_shader: String = defines.clone()
        + "\n#line 1 0\n"
        + SHADER_COMMON
        + "\n#line 1 1\n"
        + SHADER_VERTEX_COMMON
        + "\n#line 1 2\n"
        + SHADER_VERTEX_BLOCK;
    let concatenated_fragment_shader: String =
        defines + "\n#line 1 0\n" + SHADER_COMMON + "#line 1 1\n" + SHADER_FRAGMENT;

    let start_compile_time = Instant::now();
    let result = map_shader_result(
        context
            .new_shader_program::<VertexSemantics, (), BlockUniformInterface>()
            .from_strings(
                &concatenated_vertex_shader,
                None,
                None,
                &concatenated_fragment_shader,
            ),
    );
    log::trace!(
        "Shader compilation took {:.3} s",
        Instant::now()
            .duration_since(start_compile_time)
            .as_secs_f32()
    );
    result
}

/// Unwraps [`BuiltProgram`] and logs any warnings.
pub(crate) fn map_shader_result<Sem, Out, Uni>(
    program_attempt: Result<BuiltProgram<Backend, Sem, Out, Uni>, ProgramError>,
) -> Result<Program<Backend, Sem, Out, Uni>, GraphicsResourceError> {
    // TODO:
    match program_attempt {
        Err(error) => Err(GraphicsResourceError::new(error)),
        Ok(BuiltProgram { program, warnings }) => {
            for warning in warnings {
                log::warn!("{}", warning);
            }
            Ok(program)
        }
    }
}

const SHADER_COMMON: &str = include_str!("shaders/common.glsl");
const SHADER_FRAGMENT: &str = include_str!("shaders/fragment.glsl");
const SHADER_VERTEX_BLOCK: &str = include_str!("shaders/vertex-block.glsl");
const SHADER_VERTEX_COMMON: &str = include_str!("shaders/vertex-common.glsl");

/// Uniform interface for the block shader program.
#[derive(Debug, UniformInterface)]
pub struct BlockUniformInterface {
    projection_matrix: Uniform<[[f32; 4]; 4]>,
    view_matrix: Uniform<[[f32; 4]; 4]>,
    /// Eye position in world coordinates.
    /// Marked unbound because it is unused by the `BlockPrograms::opaque` program.
    #[uniform(unbound)]
    view_position: Uniform<[f32; 3]>,
    block_texture: Uniform<TextureBinding<Dim3, NormUnsigned>>,

    /// Texture containing light map.
    #[uniform(unbound)] // unbound if LightingOption::None
    light_texture: Uniform<TextureBinding<Dim3, NormUnsigned>>,
    /// Offset applied to vertex coordinates to get light map coordinates.
    #[uniform(unbound)] // unbound if LightingOption::None
    light_offset: Uniform<[i32; 3]>,

    /// Fog equation blending: 0 is realistic fog and 1 is distant more abrupt fog.
    /// TODO: Replace this uniform with a compiled-in flag since it doesn't need to be continuously changing.
    fog_mode_blend: Uniform<f32>,
    /// How far out should be fully fogged?
    fog_distance: Uniform<f32>,
    /// Color for the fog.
    fog_color: Uniform<[f32; 3]>,
}

impl BlockUniformInterface {
    /// Set all the uniforms, given necessary parameters.
    pub(super) fn initialize(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        space: &SpaceRendererBound<'_>,
    ) {
        let camera = &space.data.camera;
        let options: &GraphicsOptions = camera.options();
        self.set_projection_matrix(program_iface, camera.projection());
        self.set_view_matrix(program_iface, camera.view_matrix());
        program_iface.set(
            &self.view_position,
            camera.view_position().map(|s| s as f32).into(),
        );
        self.set_block_texture(program_iface, &space.bound_block_texture);

        program_iface.set(
            &self.light_texture,
            space.bound_light_texture.texture.binding(),
        );
        program_iface.set(&self.light_offset, space.bound_light_texture.offset.into());

        let view_distance = camera.view_distance() as f32;
        let (fog_mode_blend, fog_distance) = match options.fog {
            crate::camera::FogOption::None => (0.0, f32::INFINITY),
            crate::camera::FogOption::Abrupt => (1.0, view_distance),
            crate::camera::FogOption::Compromise => (0.5, view_distance),
            crate::camera::FogOption::Physical => (0.0, view_distance),
        };
        program_iface.set(&self.fog_mode_blend, fog_mode_blend);
        program_iface.set(&self.fog_distance, fog_distance);
        program_iface.set(&self.fog_color, space.data.sky_color.into());
    }

    /// Type converting wrapper for [`Self::projection_matrix`].
    pub fn set_projection_matrix(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        projection_matrix: Matrix4<FreeCoordinate>,
    ) {
        program_iface.set(
            &self.projection_matrix,
            projection_matrix.cast::<f32>().unwrap().into(),
        );
    }

    /// Type converting wrapper for [`Self::view_matrix`].
    pub fn set_view_matrix(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        view_matrix: Matrix4<FreeCoordinate>,
    ) {
        program_iface.set(&self.view_matrix, view_matrix.cast::<f32>().unwrap().into());
    }

    /// Type converting wrapper for [`Self::block_texture`].
    pub fn set_block_texture(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        texture: &BoundBlockTexture<'_>,
    ) {
        program_iface.set(&self.block_texture, texture.binding());
    }
}
