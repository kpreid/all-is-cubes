// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

// Shaders, uniforms, etc.

//use cgmath::{Matrix4, Point2, SquareMatrix as _, Vector3, Zero as _};
use luminance_derive::UniformInterface;
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::TextureBinding;
use luminance_front::pixel::NormUnsigned;
use luminance_front::shader::{BuiltProgram, Program, ProgramError, StageError, Uniform};
use luminance_front::texture::Dim2Array;
use luminance_front::Backend;

use crate::lum::types::VertexSemantics;

pub type BlockProgram = Program<VertexSemantics, (), BlockShaderInterface>;

// TODO: Make higher-level abstractions such that this doesn't need to be public
pub fn prepare_block_program<C, EH>(
    context: &mut C,
    error_handler: EH,
) -> (BlockProgram, Vec<luminance_front::shader::ProgramError>)
where
    C: GraphicsContext<Backend = Backend>,
    EH: FnOnce(&str) -> (),
{
    let program_attempt: Result<BuiltProgram<_, _, _>, ProgramError> = context
        .new_shader_program::<VertexSemantics, (), BlockShaderInterface>()
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
    let BuiltProgram {
        program: block_program,
        warnings,
    } = program_attempt.unwrap_or_else(|error| {
        // Extract text so we get a form that's not quoted
        let error_text = match error {
            ProgramError::CreationFailed(text) => text,
            ProgramError::StageError(StageError::CompilationFailed(_, text)) => text,
            ProgramError::LinkFailed(text) => text,
            _ => format!("{:?}", error),
        };
        error_handler(error_text.as_ref());
        panic!(error_text);
    });

    (block_program, warnings)
}

const SHADER_COMMON: &str = include_str!("shaders/common.glsl");
const SHADER_FRAGMENT: &str = include_str!("shaders/fragment.glsl");
const SHADER_VERTEX_BLOCK: &str = include_str!("shaders/vertex-block.glsl");
const SHADER_VERTEX_COMMON: &str = include_str!("shaders/vertex-common.glsl");

#[derive(Debug, UniformInterface)]
#[rustfmt::skip]
pub struct BlockShaderInterface {
    // TODO: pub temporarily for refactoring
    // TODO: Passing matrices as four vectors due to bug
    //     https://github.com/phaazon/luminance-rs/issues/434
    #[uniform(unbound)] pub projection_matrix0: Uniform<[f32; 4]>,
    #[uniform(unbound)] pub projection_matrix1: Uniform<[f32; 4]>,
    #[uniform(unbound)] pub projection_matrix2: Uniform<[f32; 4]>,
    #[uniform(unbound)] pub projection_matrix3: Uniform<[f32; 4]>,

    #[uniform(unbound)] pub view_matrix0: Uniform<[f32; 4]>,
    #[uniform(unbound)] pub view_matrix1: Uniform<[f32; 4]>,
    #[uniform(unbound)] pub view_matrix2: Uniform<[f32; 4]>,
    #[uniform(unbound)] pub view_matrix3: Uniform<[f32; 4]>,

    #[uniform(unbound)] pub block_texture: Uniform<TextureBinding<Dim2Array, NormUnsigned>>,
}
