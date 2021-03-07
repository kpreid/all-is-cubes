// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Shaders, uniforms, etc.

use cgmath::Matrix4;
use luminance::UniformInterface;
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::TextureBinding;
use luminance_front::pixel::NormUnsigned;
use luminance_front::shader::{BuiltProgram, Program, ProgramError, ProgramInterface, Uniform};
use luminance_front::texture::Dim3;
use luminance_front::Backend;

use crate::camera::GraphicsOptions;
use crate::lum::block_texture::BoundBlockTexture;
use crate::lum::space::SpaceRendererBound;
use crate::lum::types::VertexSemantics;
use crate::math::FreeCoordinate;
use crate::util::WarningsResult;

/// Type of the block shader program (output of [`prepare_block_program`]).
pub type BlockProgram = Program<VertexSemantics, (), BlockUniformInterface>;

// TODO: Make higher-level abstractions such that this doesn't need to be public
/// Compile the block shader program for the given [`GraphicsContext`].
pub fn prepare_block_program<C>(context: &mut C) -> WarningsResult<BlockProgram, String, String>
where
    C: GraphicsContext<Backend = Backend>,
{
    let program_attempt: Result<BuiltProgram<_, _, _>, ProgramError> = context
        .new_shader_program::<VertexSemantics, (), BlockUniformInterface>()
        .from_strings(
            &(SHADER_COMMON.to_owned()
                + "\n#line 1 1\n"
                + SHADER_VERTEX_COMMON
                + "\n#line 1 2\n"
                + SHADER_VERTEX_BLOCK),
            None,
            None,
            &(SHADER_COMMON.to_owned() + "#line 1 1\n" + SHADER_FRAGMENT),
        );
    match program_attempt {
        Err(error) => Err((format!("{}", error), vec![])),
        Ok(BuiltProgram {
            program: block_program,
            warnings,
        }) => Ok((
            block_program,
            warnings.into_iter().map(|w| format!("{}", w)).collect(),
        )),
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
    block_texture: Uniform<TextureBinding<Dim3, NormUnsigned>>,

    /// Texture containing light map.
    light_texture: Uniform<TextureBinding<Dim3, NormUnsigned>>,
    /// Offset applied to vertex coordinates to get light map coordinates.
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
    pub fn initialize(
        &self,
        program_iface: &mut ProgramInterface<'_>,
        space: &SpaceRendererBound<'_>,
    ) {
        let options: &GraphicsOptions = space.camera.options();
        self.set_projection_matrix(program_iface, space.camera.projection());
        self.set_view_matrix(program_iface, space.camera.view_matrix());
        self.set_block_texture(program_iface, &space.bound_block_texture);

        program_iface.set(
            &self.light_texture,
            space.bound_light_texture.texture.binding(),
        );
        program_iface.set(&self.light_offset, space.bound_light_texture.offset.into());

        let view_distance = space.camera.view_distance() as f32;
        let (fog_mode_blend, fog_distance) = match options.fog {
            crate::camera::FogOption::None => (0.0, f32::INFINITY),
            crate::camera::FogOption::Abrupt => (1.0, view_distance),
            crate::camera::FogOption::Compromise => (0.5, view_distance),
            crate::camera::FogOption::Physical => (0.0, view_distance),
        };
        program_iface.set(&self.fog_mode_blend, fog_mode_blend);
        program_iface.set(&self.fog_distance, fog_distance);
        program_iface.set(&self.fog_color, space.sky_color.into());
    }

    /// Type converting wrapper for [`Self::projection_matrix`].
    pub fn set_projection_matrix(
        &self,
        program_iface: &mut ProgramInterface<'_>,
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
        program_iface: &mut ProgramInterface<'_>,
        view_matrix: Matrix4<FreeCoordinate>,
    ) {
        program_iface.set(&self.view_matrix, view_matrix.cast::<f32>().unwrap().into());
    }

    /// Type converting wrapper for [`Self::block_texture`].
    pub fn set_block_texture(
        &self,
        program_iface: &mut ProgramInterface<'_>,
        texture: &BoundBlockTexture<'_>,
    ) {
        program_iface.set(&self.block_texture, texture.binding());
    }
}
