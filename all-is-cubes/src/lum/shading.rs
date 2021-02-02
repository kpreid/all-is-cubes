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

use crate::camera::ProjectionHelper;
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

    /// How far out should be fully fogged?
    fog_distance: Uniform<f32>,
    /// Color for the fog.
    fog_color: Uniform<[f32; 3]>,
}

impl BlockUniformInterface {
    /// Set all the uniforms, given necessary parameters.
    pub fn initialize(
        &self,
        program_iface: &mut ProgramInterface,
        ph: &ProjectionHelper,
        space: &SpaceRendererBound<'_>,
    ) {
        self.set_projection_matrix(program_iface, ph.projection());
        self.set_view_matrix(program_iface, space.view_matrix);
        self.set_block_texture(program_iface, &space.bound_block_texture);
        program_iface.set(&self.fog_distance, ph.view_distance() as f32);
        program_iface.set(&self.fog_color, space.sky_color.into());
    }

    /// Type converting wrapper for [`Self::projection_matrix`].
    pub fn set_projection_matrix(
        &self,
        program_iface: &mut ProgramInterface,
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
        program_iface: &mut ProgramInterface,
        view_matrix: Matrix4<FreeCoordinate>,
    ) {
        program_iface.set(&self.view_matrix, view_matrix.cast::<f32>().unwrap().into());
    }

    /// Type converting wrapper for [`Self::block_texture`].
    pub fn set_block_texture(
        &self,
        program_iface: &mut ProgramInterface,
        texture: &BoundBlockTexture,
    ) {
        program_iface.set(&self.block_texture, texture.binding());
    }
}
