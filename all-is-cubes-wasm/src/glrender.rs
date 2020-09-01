// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! OpenGL-based graphics rendering.

use cgmath::{Matrix4, SquareMatrix as _};
use luminance_derive::UniformInterface;
use luminance_front::context::GraphicsContext;
use luminance_front::face_culling::{FaceCulling, FaceCullingMode, FaceCullingOrder};
use luminance_front::framebuffer::Framebuffer;
use luminance_front::pipeline::{PipelineState, TextureBinding};
use luminance_front::pixel::NormUnsigned;
use luminance_front::render_state::RenderState;
use luminance_front::shader::{BuiltProgram, Program, ProgramError, StageError, Uniform};
use luminance_front::tess::{Interleaved, Mode, Tess, VerticesMut};
use luminance_front::tess_gate::TessGate;
use luminance_front::texture::{Dim2, Dim2Array};
use luminance_front::Backend;
use wasm_bindgen::prelude::JsValue;
use web_sys::console;

use all_is_cubes::camera::{Camera, ProjectionHelper};
use all_is_cubes::math::{Face, FaceMap};
use all_is_cubes::raycast::Raycaster;
use all_is_cubes::space::Space;
use all_is_cubes::triangulator::{triangulate_space, BlocksRenderData};
use all_is_cubes::universe::URef;

use crate::block_texture::{BlockGLRenderData, BlockGLTexture};
use crate::js_bindings::CanvasHelper;
use crate::types::{GLBlockVertex, Vertex, VertexSemantics};

const SHADER_COMMON: &str = include_str!("shaders/common.glsl");
const SHADER_FRAGMENT: &str = include_str!("shaders/fragment.glsl");
const SHADER_VERTEX_BLOCK: &str = include_str!("shaders/vertex-block.glsl");
const SHADER_VERTEX_COMMON: &str = include_str!("shaders/vertex-common.glsl");

pub struct GLRenderer<C>
where
    C: GraphicsContext<Backend = Backend>,
{
    // Graphics objects
    surface: C,
    back_buffer: Framebuffer<Dim2, (), ()>,
    block_program: Program<VertexSemantics, (), ShaderInterface>,

    // Rendering state
    space: Option<URef<Space>>,
    block_data_cache: Option<BlockGLRenderData>, // TODO: quick hack, needs an invalidation strategy
    chunk: Chunk,

    // Miscellaneous
    canvas_helper: CanvasHelper,
    proj: ProjectionHelper,
}

impl<C> GLRenderer<C>
where
    C: GraphicsContext<Backend = Backend>,
{
    pub fn new(mut surface: C, canvas_helper: CanvasHelper) -> Self {
        let program_attempt: Result<BuiltProgram<_, _, _>, ProgramError> = surface
            .new_shader_program::<VertexSemantics, (), ShaderInterface>()
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
            // Extract text and send to console so we get less quoting
            let error_text = match error {
                ProgramError::CreationFailed(text) => text,
                ProgramError::StageError(StageError::CompilationFailed(_, text)) => text,
                ProgramError::LinkFailed(text) => text,
                _ => format!("{:?}", error),
            };
            console::error_1(&JsValue::from_str(&format!("GLSL error:\n{}", error_text)));
            panic!("shader compilation failed");
        });
        for warning in warnings {
            console::warn_1(&JsValue::from_str(&format!("GLSL warning: {:?}", warning)));
        }

        let proj = ProjectionHelper::new(1.0, canvas_helper.viewport_px());
        let back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut surface,
            canvas_helper.viewport_dev(),
        )
        .unwrap(); // TODO error handling
        Self {
            surface,
            back_buffer,
            block_program,
            space: None,
            block_data_cache: None,
            chunk: Chunk::new(),
            canvas_helper,
            proj,
        }
    }

    pub fn update_viewport(&mut self) {
        self.proj.set_viewport(self.canvas_helper.viewport_px());
        self.back_buffer = luminance::framebuffer::Framebuffer::back_buffer(
            &mut self.surface,
            self.canvas_helper.viewport_dev(),
        )
        .unwrap(); // TODO error handling
    }

    pub fn set_space(&mut self, space: Option<URef<Space>>) {
        self.space = space;
    }

    pub fn render_frame(&mut self, camera: &Camera) -> RenderInfo {
        let mut info = RenderInfo::default();

        // Not used directly for rendering, but used for cursor.
        self.proj.set_view_matrix(camera.view());

        let space: &Space = &*(if let Some(space_ref) = &self.space {
            space_ref.borrow()
        } else {
            return info;
        });
        let surface = &mut self.surface;
        let block_program = &mut self.block_program;
        let projection_matrix = self.proj.projection();

        // TODO: quick hack; we need actual invalidation, not memoization
        let block_data = self.block_data_cache.get_or_insert_with(|| {
            BlockGLRenderData::prepare(surface, space).expect("texture failure")
        });

        self.chunk
            .update(surface, &space, &block_data.block_render_data);
        let ct = &self.chunk;

        let render_state = RenderState::default().set_face_culling(FaceCulling {
            order: FaceCullingOrder::CCW,
            mode: FaceCullingMode::Back,
        });

        // let debug_tess = block_data.texture_allocator.debug_atlas_tess(surface);

        let render = surface
            .new_pipeline_gate()
            .pipeline(
                &self.back_buffer,
                // TODO: port skybox cube map code
                &PipelineState::default().set_clear_color([0.6, 0.7, 1.0, 1.]),
                |pipeline, mut shading_gate| {
                    let bound_block_texture = pipeline.bind_texture(block_data.texture())?;

                    shading_gate.shade(block_program, |mut shader_iface, u, mut render_gate| {
                        let pm: [[f32; 4]; 4] = projection_matrix.cast::<f32>().unwrap().into();
                        shader_iface.set(&u.projection_matrix0, pm[0]);
                        shader_iface.set(&u.projection_matrix1, pm[1]);
                        shader_iface.set(&u.projection_matrix2, pm[2]);
                        shader_iface.set(&u.projection_matrix3, pm[3]);

                        let vm: [[f32; 4]; 4] = camera.view().cast::<f32>().unwrap().into();
                        shader_iface.set(&u.view_matrix0, vm[0]);
                        shader_iface.set(&u.view_matrix1, vm[1]);
                        shader_iface.set(&u.view_matrix2, vm[2]);
                        shader_iface.set(&u.view_matrix3, vm[3]);

                        shader_iface.set(&u.block_texture, bound_block_texture.binding());

                        render_gate.render(&render_state, |mut tess_gate| {
                            info.square_count += ct.render(&mut tess_gate)?;
                            Ok(())
                        })?;

                        if false {
                            // Reset matrices for screen-aligned debug rendering
                            let pm: [[f32; 4]; 4] = Matrix4::identity().into();
                            shader_iface.set(&u.projection_matrix0, pm[0]);
                            shader_iface.set(&u.projection_matrix1, pm[1]);
                            shader_iface.set(&u.projection_matrix2, pm[2]);
                            shader_iface.set(&u.projection_matrix3, pm[3]);

                            let vm: [[f32; 4]; 4] = Matrix4::identity().into();
                            shader_iface.set(&u.view_matrix0, vm[0]);
                            shader_iface.set(&u.view_matrix1, vm[1]);
                            shader_iface.set(&u.view_matrix2, vm[2]);
                            shader_iface.set(&u.view_matrix3, vm[3]);

                            shader_iface.set(&u.block_texture, bound_block_texture.binding());

                            // render_gate.render(&render_state, |mut tess_gate| {
                            //     tess_gate.render(&debug_tess)?;
                            //     Ok(())
                            // })?;
                        }

                        Ok(())
                    })
                },
            )
            .assume(); // TODO error handlint

        if !render.is_ok() {
            panic!("not ok"); // TODO what good error handling goes here?
        }

        // There is no swap_buffers operation because WebGL implicitly does so.
        info
    }

    pub fn cursor_raycaster(&self /* TODO: offset ... or move this function */) -> Raycaster {
        let (origin, direction) = self.proj.project_ndc_into_world(0.0, 0.0);
        Raycaster::new(origin, direction)
    }
}

/// Information about render performance
#[derive(Clone, Copy, Debug, Default)]
pub struct RenderInfo {
    square_count: usize,
}

#[derive(Debug, UniformInterface)]
#[rustfmt::skip]
struct ShaderInterface {
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

/// Not yet actually chunked rendering, but bundles the data that would be used in one.
struct Chunk {
    // bounds: Grid,
    /// Vertices grouped by the direction they face
    vertices: FaceMap<Vec<Vertex>>,
    tesses: FaceMap<Option<Tess<Vertex>>>,
    last_valid_counter: u32,
}

impl Chunk {
    fn new() -> Self {
        Chunk {
            vertices: FaceMap::default(),
            tesses: FaceMap::default(),
            last_valid_counter: 0,
        }
    }

    fn update<C: GraphicsContext<Backend = Backend>>(
        &mut self,
        context: &mut C,
        space: &Space,
        blocks_render_data: &BlocksRenderData<GLBlockVertex, BlockGLTexture>,
    ) {
        // TODO: quick hack to avoid rerendering unnecessarily.
        // Doesn't account for block render data changes.
        if space.mutation_counter == self.last_valid_counter {
            return;
        }
        self.last_valid_counter = space.mutation_counter;

        triangulate_space(space, blocks_render_data, &mut self.vertices);

        for &face in Face::ALL_SEVEN {
            let tess_option = &mut self.tesses[face];
            let new_vertices: &[Vertex] = self.vertices[face].as_ref();

            if tess_option.as_ref().map(|tess| tess.vert_nb()) != Some(new_vertices.len()) {
                // Existing buffer, if any, is not the right length. Discard it.
                *tess_option = None;
            }

            // TODO: replace unwrap()s with an error logging/flagging mechanism
            if new_vertices.is_empty() {
                // Render zero vertices by not rendering anything.
                *tess_option = None;
            } else if let Some(tess) = tess_option.as_mut() {
                // We already have a buffer, and it is a matching length.
                // TODO: Generalize this to be able to shrink buffers via degenerate triangles.
                let mut buffer_slice: VerticesMut<Vertex, (), (), Interleaved, Vertex> = tess
                    .vertices_mut()
                    .expect("failed to map vertices for copying");
                buffer_slice.copy_from_slice(new_vertices);
            } else {
                // Allocate and populate new buffer.
                *tess_option = Some(
                    context
                        .new_tess()
                        .set_vertices(self.vertices[face].clone())
                        .set_mode(Mode::Triangle)
                        .build()
                        .unwrap(),
                );
            }
        }
    }

    fn render<E>(&self, tess_gate: &mut TessGate) -> Result<usize, E> {
        let mut count = 0;
        for &face in Face::ALL_SEVEN {
            if let Some(tess) = self.tesses[face].as_ref() {
                count += tess.vert_nb() / 6;
                tess_gate.render(tess)?;
            }
        }
        Ok(count)
    }
}
