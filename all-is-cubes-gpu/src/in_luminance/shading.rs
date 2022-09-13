//! Shaders, uniforms, etc.

use std::fmt::Debug;

use instant::Instant;
use luminance::context::GraphicsContext;
use luminance::pipeline::TextureBinding;
use luminance::pixel::NormUnsigned;
use luminance::shader::types::{Mat44, Vec3};
use luminance::shader::{BuiltProgram, Program, ProgramError, ProgramInterface, Uniform};
use luminance::texture::Dim3;
use luminance::UniformInterface;
use once_cell::sync::Lazy;

use all_is_cubes::camera::{
    FogOption, GraphicsOptions, LightingOption, ToneMappingOperator, TransparencyOption,
};
use all_is_cubes::cgmath::Matrix4;
use all_is_cubes::listen::Listener;
use all_is_cubes::math::FreeCoordinate;

use crate::in_luminance::space::SpaceRendererBound;
use crate::in_luminance::types::{AicLumBackend, VertexSemantics};
use crate::reloadable::{reloadable_str, Reloadable};
use crate::GraphicsResourceError;

/// Subset of [`GraphicsOptions`] that requires shader recompilation if changed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ShaderConstants {
    lighting_display: LightingOption,
    volumetric_transparency: bool,
    tone_mapping: ToneMappingOperator,
}

impl ShaderConstants {
    fn to_defines(&self) -> Vec<(&'static str, &'static str)> {
        let mut defines: Vec<(&str, &str)> = Vec::with_capacity(4);
        defines.push((
            "TONE_MAPPING_ID",
            match self.tone_mapping {
                ToneMappingOperator::Clamp => "0",
                ToneMappingOperator::Reinhard => "1",
                ref tmo => panic!("Missing implementation for tone mapping operator {:?}", tmo),
            },
        ));
        if self.lighting_display != LightingOption::None {
            defines.push(("LIGHTING", "1"));
        }
        if self.lighting_display == LightingOption::Smooth {
            defines.push(("SMOOTH_LIGHTING", "1"));
        }
        if self.volumetric_transparency {
            defines.push(("VOLUMETRIC", "1"));
        }
        defines
    }
}

impl From<&GraphicsOptions> for ShaderConstants {
    fn from(options: &GraphicsOptions) -> Self {
        Self {
            lighting_display: options.lighting_display.clone(),
            volumetric_transparency: match options.transparency {
                TransparencyOption::Surface | TransparencyOption::Threshold(_) => false,
                TransparencyOption::Volumetric => true,
                _ => false,
            },
            tone_mapping: options.tone_mapping.clone(),
        }
    }
}

/// Type of the block shader program (output of [`prepare_block_program`]).
pub(crate) type BlockProgram<Backend> =
    Program<Backend, VertexSemantics, (), BlockUniformInterface>;

pub(crate) type LinesProgram<Backend> =
    Program<Backend, VertexSemantics, (), LinesUniformInterface>;

/// Collection of shaders for rendering blocks, which all share the `BlockUniformInterface`.
pub(crate) struct BlockPrograms<Backend: AicLumBackend> {
    pub(crate) constants: ShaderConstants,
    pub(crate) opaque: BlockProgram<Backend>,
    pub(crate) transparent: BlockProgram<Backend>,
}

impl<Backend: AicLumBackend> BlockPrograms<Backend> {
    pub(crate) fn listen(listener: impl Listener<()> + Send + Sync + Clone + 'static) {
        SHADER_SOURCES.listen(listener)
    }

    pub(crate) fn compile<C>(
        context: &mut C,
        constants: ShaderConstants,
    ) -> Result<BlockPrograms<Backend>, GraphicsResourceError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        Ok(BlockPrograms {
            opaque: prepare_block_program(context, &(&constants, "opaque"), &constants, false)?,
            transparent: prepare_block_program(
                context,
                &(&constants, "transparent"),
                &constants,
                true,
            )?,
            constants,
        })
    }
}

/// Compile the block shader program for the given [`GraphicsContext`].
fn prepare_block_program<C>(
    context: &mut C,
    description: &dyn Debug,
    constants: &ShaderConstants,
    transparent: bool,
) -> Result<BlockProgram<C::Backend>, GraphicsResourceError>
where
    C: GraphicsContext,
    C::Backend: AicLumBackend,
{
    let (concatenated_vertex_shader, concatenated_fragment_shader) =
        SHADER_SOURCES.block_program_sources(constants, transparent);

    let start_compile_time = Instant::now();
    let result = map_shader_result(
        description,
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
        "Shader compilation ({description:?}) took {:.3} s",
        Instant::now()
            .duration_since(start_compile_time)
            .as_secs_f32()
    );
    result
}

/// Compile the block shader program for the given [`GraphicsContext`].
pub(crate) fn prepare_lines_program<C>(
    context: &mut C,
    constants: &ShaderConstants,
) -> Result<LinesProgram<C::Backend>, GraphicsResourceError>
where
    C: GraphicsContext,
    C::Backend: AicLumBackend,
{
    let (concatenated_vertex_shader, concatenated_fragment_shader) =
        SHADER_SOURCES.lines_program_sources(constants);

    map_shader_result(
        &"LinesProgram",
        context
            .new_shader_program::<VertexSemantics, (), LinesUniformInterface>()
            .from_strings(
                &concatenated_vertex_shader,
                None,
                None,
                &concatenated_fragment_shader,
            ),
    )
}

/// Unwraps [`BuiltProgram`] and logs any warnings.
pub(crate) fn map_shader_result<Backend, Sem, Out, Uni>(
    description: &dyn Debug,
    program_attempt: Result<BuiltProgram<Backend, Sem, Out, Uni>, ProgramError>,
) -> Result<Program<Backend, Sem, Out, Uni>, GraphicsResourceError>
where
    Backend: luminance::backend::shader::Shader,
{
    match program_attempt {
        Err(error) => Err(GraphicsResourceError::new(error)),
        Ok(BuiltProgram { program, warnings }) => {
            // NOTE: Logs disabled. Currently, I am not intending to develop the luminance
            // renderer further, and the existing warnings (unused uniforms) are therefore
            // just noise that makes it harder to read render test output.
            if false {
                if !warnings.is_empty() {
                    log::warn!(
                        "Warnings occurred while compiling shader for {:?}",
                        description
                    );
                    for warning in warnings {
                        log::warn!("  {}", warning);
                    }
                }
            }
            Ok(program)
        }
    }
}

struct ShaderSources {
    common: Reloadable,
    common_vertex: Reloadable,
    block_vertex: Reloadable,
    block_fragment: Reloadable,
    lines_vertex: Reloadable,
    lines_fragment: Reloadable,
}

impl ShaderSources {
    /// Prepare source text for the block shader program.
    ///
    /// Return value is (vertex shader, fragment shader).
    fn block_program_sources(
        &self,
        constants: &ShaderConstants,
        transparent: bool,
    ) -> (String, String) {
        let mut defines = constants.to_defines();
        if transparent {
            defines.push(("ALLOW_TRANSPARENCY", "1"));
        }
        let defines_glsl = glsl_defines(defines);

        let concatenated_vertex_shader: String = defines_glsl.clone()
            + "\n#line 1 0\n"
            + &*self.common.get()
            + "\n#line 1 1\n"
            + &*self.common_vertex.get()
            + "\n#line 1 2\n"
            + &*self.block_vertex.get();
        let concatenated_fragment_shader: String = defines_glsl
            + "\n#line 1 0\n"
            + &*self.common.get()
            + "#line 1 1\n"
            + &*self.block_fragment.get();

        (concatenated_vertex_shader, concatenated_fragment_shader)
    }

    /// Prepare source text for the debug-lines shader program.
    ///
    /// Return value is (vertex shader, fragment shader).
    fn lines_program_sources(&self, constants: &ShaderConstants) -> (String, String) {
        // TODO: repeated code — write a helper for concatenating GLSL
        let defines_glsl = glsl_defines(constants.to_defines());
        (
            defines_glsl.clone()
                + "\n#line 1 0\n"
                + &*self.common.get()
                + "\n#line 1 1\n"
                + &*self.common_vertex.get()
                + "\n#line 1 2\n"
                + &*self.lines_vertex.get(),
            defines_glsl
                + "\n#line 1 0\n"
                + &*self.common.get()
                + "#line 1 1\n"
                + &*self.lines_fragment.get(),
        )
    }

    pub(crate) fn listen(&self, listener: impl Listener<()> + Clone + Send + Sync + 'static) {
        self.common.as_source().listen(listener.clone());
        self.common_vertex.as_source().listen(listener.clone());
        self.block_vertex.as_source().listen(listener.clone());
        self.block_fragment.as_source().listen(listener.clone());
        self.lines_vertex.as_source().listen(listener.clone());
        self.lines_fragment.as_source().listen(listener);
    }
}

/// GLSL shader source code.
///
static SHADER_SOURCES: Lazy<ShaderSources> = Lazy::new(|| ShaderSources {
    common: reloadable_str!("src/in_luminance/shaders/common.glsl"),
    common_vertex: reloadable_str!("src/in_luminance/shaders/common-vertex.glsl"),
    block_vertex: reloadable_str!("src/in_luminance/shaders/block-vertex.glsl"),
    block_fragment: reloadable_str!("src/in_luminance/shaders/block-fragment.glsl"),
    lines_vertex: reloadable_str!("src/in_luminance/shaders/lines-vertex.glsl"),
    lines_fragment: reloadable_str!("src/in_luminance/shaders/lines-fragment.glsl"),
});

/// Uniform interface for [`BlockProgram`]
#[derive(Debug, UniformInterface)]
pub struct BlockUniformInterface {
    projection_matrix: Uniform<Mat44<f32>>,
    view_matrix: Uniform<Mat44<f32>>,
    /// Eye position in world coordinates.
    /// Marked unbound because it is unused by the `BlockPrograms::opaque` program.
    #[uniform(unbound)]
    view_position: Uniform<Vec3<f32>>,
    block_texture: Uniform<TextureBinding<Dim3, NormUnsigned>>,

    /// Texture containing light map.
    #[uniform(unbound)] // unbound if LightingOption::None
    light_texture: Uniform<TextureBinding<Dim3, NormUnsigned>>,
    /// Offset applied to vertex coordinates to get light map coordinates.
    #[uniform(unbound)] // unbound if LightingOption::None
    light_offset: Uniform<Vec3<i32>>,

    /// Fog equation blending: 0 is realistic fog and 1 is distant more abrupt fog.
    /// TODO: Replace this uniform with a compiled-in flag since it doesn't need to be continuously changing.
    fog_mode_blend: Uniform<f32>,
    /// How far out should be fully fogged?
    fog_distance: Uniform<f32>,
    /// Color for the fog.
    fog_color: Uniform<Vec3<f32>>,

    exposure: Uniform<f32>,
}

impl BlockUniformInterface {
    /// Set all the uniforms, given necessary parameters.
    pub(super) fn initialize<Backend: AicLumBackend>(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        space: &SpaceRendererBound<'_, Backend>,
    ) {
        let camera = &space.data.camera;
        let options: &GraphicsOptions = camera.options();
        program_iface.set(&self.projection_matrix, convert_matrix(camera.projection()));
        self.set_view_matrix(program_iface, camera.view_matrix());
        program_iface.set(
            &self.view_position,
            Vec3(camera.view_position().map(|s| s as f32).into()),
        );
        program_iface.set(&self.block_texture, space.bound_block_texture.binding());

        program_iface.set(
            &self.light_texture,
            space.bound_light_texture.texture.binding(),
        );
        program_iface.set(
            &self.light_offset,
            Vec3(space.bound_light_texture.offset.into()),
        );

        let view_distance = camera.view_distance() as f32;
        let (fog_mode_blend, fog_distance) = match options.fog {
            FogOption::Abrupt => (1.0, view_distance),
            FogOption::Compromise => (0.5, view_distance),
            FogOption::Physical => (0.0, view_distance),
            /* FogOption::None | */ _ => (0.0, f32::INFINITY),
        };
        program_iface.set(&self.fog_mode_blend, fog_mode_blend);
        program_iface.set(&self.fog_distance, fog_distance);
        program_iface.set(&self.fog_color, Vec3(space.data.sky_color.into()));

        program_iface.set(&self.exposure, camera.exposure().into_inner());
    }

    /// Type converting wrapper for [`Self::view_matrix`].
    pub fn set_view_matrix<Backend: AicLumBackend>(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        view_matrix: Matrix4<FreeCoordinate>,
    ) {
        program_iface.set(&self.view_matrix, convert_matrix(view_matrix));
    }
}

/// Uniform interface for [`LinesProgram`].
#[derive(Debug, UniformInterface)]
pub struct LinesUniformInterface {
    projection_matrix: Uniform<Mat44<f32>>,
    view_matrix: Uniform<Mat44<f32>>,
}

impl LinesUniformInterface {
    /// Set all the uniforms, given necessary parameters.
    ///
    /// TODO: Deduplicate code between this and `BlockUniformInterface`
    pub(super) fn initialize<Backend: AicLumBackend>(
        &self,
        program_iface: &mut ProgramInterface<'_, Backend>,
        space: &SpaceRendererBound<'_, Backend>,
        view_modifier_matrix: Matrix4<FreeCoordinate>,
    ) {
        let camera = &space.data.camera;
        program_iface.set(&self.projection_matrix, convert_matrix(camera.projection()));
        program_iface.set(
            &self.view_matrix,
            convert_matrix(camera.view_matrix() * view_modifier_matrix),
        );
    }
}

fn convert_matrix(matrix: Matrix4<f64>) -> Mat44<f32> {
    Mat44(matrix.cast::<f32>().unwrap(/* f64 to f32 is infallible */).into())
}

fn glsl_defines<'a>(defines: impl IntoIterator<Item = (&'a str, &'a str)>) -> String {
    defines
        .into_iter()
        .map(|(k, v)| format!("#define {} {}\n", k, v))
        .collect()
}
