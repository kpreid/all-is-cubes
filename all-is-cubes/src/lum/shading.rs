// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Shaders, uniforms, etc.

use cgmath::Matrix4;
use instant::Instant;
use luminance::backend::shader::Uniformable;
use luminance::context::GraphicsContext;
use luminance::pipeline::TextureBinding;
use luminance::pixel::NormUnsigned;
use luminance::shader::{BuiltProgram, Program, ProgramError, ProgramInterface, Uniform};
use luminance::texture::{Dim2, Dim3};
use luminance::UniformInterface;

use crate::camera::{GraphicsOptions, LightingOption, ToneMappingOperator, TransparencyOption};
use crate::lum::block_texture::BoundBlockTexture;
use crate::lum::space::SpaceRendererBound;
use crate::lum::types::{AicLumBackend, VertexSemantics};
use crate::lum::GraphicsResourceError;
use crate::math::FreeCoordinate;

/// Subset of [`GraphicsOptions`] that requires shader recompilation if changed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ShaderConstants {
    lighting_display: LightingOption,
    volumetric_transparency: bool,
    tone_mapping: ToneMappingOperator,
}

impl From<&GraphicsOptions> for ShaderConstants {
    fn from(options: &GraphicsOptions) -> Self {
        Self {
            lighting_display: options.lighting_display.clone(),
            volumetric_transparency: match options.transparency {
                TransparencyOption::Surface | TransparencyOption::Threshold(_) => false,
                TransparencyOption::Volumetric => true,
            },
            tone_mapping: options.tone_mapping.clone(),
        }
    }
}

/// Type of the block shader program (output of [`prepare_block_program`]).
pub type BlockProgram<Backend> = Program<Backend, VertexSemantics, (), BlockUniformInterface>;

/// Collection of shaders for rendering blocks, which all share the `BlockUniformInterface`.
pub(crate) struct BlockPrograms<Backend: AicLumBackend> {
    pub(crate) constants: ShaderConstants,
    pub(crate) opaque: BlockProgram<Backend>,
    pub(crate) transparent: BlockProgram<Backend>,
}

impl<Backend: AicLumBackend> BlockPrograms<Backend> {
    pub(crate) fn compile<C>(
        context: &mut C,
        constants: ShaderConstants,
    ) -> Result<BlockPrograms<Backend>, GraphicsResourceError>
    where
        C: GraphicsContext<Backend = Backend>,
        f32: Uniformable<C::Backend>,
        [i32; 3]: Uniformable<C::Backend>,
        [f32; 3]: Uniformable<C::Backend>,
        [[f32; 4]; 4]: Uniformable<C::Backend>,
        TextureBinding<Dim2, NormUnsigned>: Uniformable<C::Backend>,
        TextureBinding<Dim3, NormUnsigned>: Uniformable<C::Backend>,
    {
        // base_defines is shared by both of the program variants
        let mut base_defines: Vec<(&str, &str)> = Vec::with_capacity(4);
        base_defines.push((
            "TONE_MAPPING_ID",
            match constants.tone_mapping {
                ToneMappingOperator::Clamp => "0",
                ToneMappingOperator::Reinhard => "1",
            },
        ));
        if constants.lighting_display != LightingOption::None {
            base_defines.push(("LIGHTING", "1"));
        }
        if constants.lighting_display == LightingOption::Smooth {
            base_defines.push(("SMOOTH_LIGHTING", "1"));
        }
        if constants.volumetric_transparency {
            base_defines.push(("VOLUMETRIC", "1"));
        }

        Ok(BlockPrograms {
            constants,
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
) -> Result<BlockProgram<C::Backend>, GraphicsResourceError>
where
    C: GraphicsContext,
    C::Backend: AicLumBackend,
    f32: Uniformable<C::Backend>,
    [i32; 3]: Uniformable<C::Backend>,
    [f32; 3]: Uniformable<C::Backend>,
    [[f32; 4]; 4]: Uniformable<C::Backend>,
    TextureBinding<Dim2, NormUnsigned>: Uniformable<C::Backend>,
    TextureBinding<Dim3, NormUnsigned>: Uniformable<C::Backend>,
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
pub(crate) fn map_shader_result<Backend, Sem, Out, Uni>(
    program_attempt: Result<BuiltProgram<Backend, Sem, Out, Uni>, ProgramError>,
) -> Result<Program<Backend, Sem, Out, Uni>, GraphicsResourceError>
where
    Backend: luminance::backend::shader::Shader,
{
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

    exposure: Uniform<f32>,
}

impl BlockUniformInterface {
    /// Set all the uniforms, given necessary parameters.
    pub(super) fn initialize<Backend: AicLumBackend>(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        space: &SpaceRendererBound<'_, Backend>,
    ) where
        f32: Uniformable<Backend>,
        [i32; 3]: Uniformable<Backend>,
        [f32; 3]: Uniformable<Backend>,
        [[f32; 4]; 4]: Uniformable<Backend>,
        TextureBinding<Dim2, NormUnsigned>: Uniformable<Backend>,
        TextureBinding<Dim3, NormUnsigned>: Uniformable<Backend>,
    {
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

        program_iface.set(&self.exposure, camera.exposure);
    }

    /// Type converting wrapper for [`Self::projection_matrix`].
    pub fn set_projection_matrix<Backend: AicLumBackend>(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        projection_matrix: Matrix4<FreeCoordinate>,
    ) where
        [[f32; 4]; 4]: Uniformable<Backend>,
    {
        program_iface.set(
            &self.projection_matrix,
            projection_matrix.cast::<f32>().unwrap().into(),
        );
    }

    /// Type converting wrapper for [`Self::view_matrix`].
    pub fn set_view_matrix<Backend: AicLumBackend>(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        view_matrix: Matrix4<FreeCoordinate>,
    ) where
        [[f32; 4]; 4]: Uniformable<Backend>,
    {
        program_iface.set(&self.view_matrix, view_matrix.cast::<f32>().unwrap().into());
    }

    /// Type converting wrapper for [`Self::block_texture`].
    pub fn set_block_texture<Backend: AicLumBackend>(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        texture: &BoundBlockTexture<'_, Backend>,
    ) where
        TextureBinding<Dim3, NormUnsigned>: Uniformable<Backend>,
    {
        program_iface.set(&self.block_texture, texture.binding());
    }
}
