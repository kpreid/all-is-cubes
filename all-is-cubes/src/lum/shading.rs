// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

// Shaders, uniforms, etc.

use cgmath::Matrix4;
use luminance_derive::UniformInterface;
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::TextureBinding;
use luminance_front::pixel::NormUnsigned;
use luminance_front::shader::{BuiltProgram, Program, ProgramError, ProgramInterface, Uniform};
use luminance_front::texture::Dim2Array;
use luminance_front::Backend;

use crate::lum::block_texture::BoundBlockTexture;
use crate::lum::types::VertexSemantics;
use crate::math::FreeCoordinate;
use crate::util::WarningsResult;

pub type BlockProgram = Program<VertexSemantics, (), BlockUniformInterface>;

// TODO: Make higher-level abstractions such that this doesn't need to be public
pub fn prepare_block_program<C>(context: &mut C) -> WarningsResult<BlockProgram, String, String>
where
    C: GraphicsContext<Backend = Backend>,
{
    let program_attempt: Result<BuiltProgram<_, _, _>, ProgramError> = context
        .new_shader_program::<VertexSemantics, (), BlockUniformInterface>()
        .from_strings(
            &(SHADER_COMMON.to_owned()
                + "#line 1 1\n"
                + SHADER_VERTEX_COMMON
                + "#line 1 2\n"
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

#[derive(Debug, UniformInterface)]
#[rustfmt::skip]
pub struct BlockUniformInterface {
    // TODO: Passing matrices as four vectors due to bug
    //     https://github.com/phaazon/luminance-rs/issues/434
    #[uniform(unbound)] projection_matrix0: Uniform<[f32; 4]>,
    #[uniform(unbound)] projection_matrix1: Uniform<[f32; 4]>,
    #[uniform(unbound)] projection_matrix2: Uniform<[f32; 4]>,
    #[uniform(unbound)] projection_matrix3: Uniform<[f32; 4]>,

    #[uniform(unbound)] view_matrix0: Uniform<[f32; 4]>,
    #[uniform(unbound)] view_matrix1: Uniform<[f32; 4]>,
    #[uniform(unbound)] view_matrix2: Uniform<[f32; 4]>,
    #[uniform(unbound)] view_matrix3: Uniform<[f32; 4]>,

    #[uniform(unbound)] block_texture: Uniform<TextureBinding<Dim2Array, NormUnsigned>>,
}

impl BlockUniformInterface {
    pub fn set_projection_matrix(
        &self,
        program_iface: &mut ProgramInterface,
        projection_matrix: Matrix4<FreeCoordinate>,
    ) {
        let pm: [[f32; 4]; 4] = projection_matrix.cast::<f32>().unwrap().into();
        program_iface.set(&self.projection_matrix0, pm[0]);
        program_iface.set(&self.projection_matrix1, pm[1]);
        program_iface.set(&self.projection_matrix2, pm[2]);
        program_iface.set(&self.projection_matrix3, pm[3]);
    }

    pub fn set_view_matrix(
        &self,
        program_iface: &mut ProgramInterface,
        view_matrix: Matrix4<FreeCoordinate>,
    ) {
        let pm: [[f32; 4]; 4] = view_matrix.cast::<f32>().unwrap().into();
        program_iface.set(&self.view_matrix0, pm[0]);
        program_iface.set(&self.view_matrix1, pm[1]);
        program_iface.set(&self.view_matrix2, pm[2]);
        program_iface.set(&self.view_matrix3, pm[3]);
    }

    pub fn set_block_texture(
        &self,
        program_iface: &mut ProgramInterface,
        texture: &BoundBlockTexture,
    ) {
        program_iface.set(&self.block_texture, texture.binding());
    }
}
