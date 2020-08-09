// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! OpenGL-based graphics rendering.

use cgmath::{Vector3};
use luminance_derive::{Semantics, Vertex, UniformInterface};
use luminance_front::face_culling::{FaceCulling, FaceCullingMode, FaceCullingOrder};
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::PipelineState;
use luminance_front::render_state::RenderState;
use luminance_front::shader::{BuiltProgram, Program, Uniform};
use luminance_front::tess::Mode;
use luminance_web_sys::{WebSysWebGL2Surface, WebSysWebGL2SurfaceError};
use luminance_windowing::WindowOpt;
use std::time::Duration;
use wasm_bindgen::prelude::*;
use web_sys::console;

use all_is_cubes::block::{Color};
use all_is_cubes::camera::Camera;
use all_is_cubes::math::{FreeCoordinate};
use all_is_cubes::space::Space;
use all_is_cubes::triangulator::{BlockVertex, BlocksRenderData, triangulate_blocks, triangulate_space};

const SHADER_COMMON: &str = include_str!("shaders/common.glsl");
const SHADER_FRAGMENT: &str = include_str!("shaders/fragment.glsl");
const SHADER_VERTEX_BLOCK: &str = include_str!("shaders/vertex-block.glsl");
const SHADER_VERTEX_COMMON: &str = include_str!("shaders/vertex-common.glsl");

pub struct GLRenderer {
    surface: WebSysWebGL2Surface,
    block_program: Program<VertexSemantics, (), ShaderInterface>,
    block_data_cache: Option<BlocksRenderData<Vertex>>,  // TODO: quick hack, needs an invalidation strategy
    fake_time: Duration,
}

impl GLRenderer {
    pub fn new(canvas_id: &str) -> Result<Self, WebSysWebGL2SurfaceError> {
        let mut surface = WebSysWebGL2Surface::new(canvas_id, WindowOpt::default())?;

        let BuiltProgram {program: block_program, warnings} = surface
            .new_shader_program::<VertexSemantics, (), ShaderInterface>()
            .from_strings(
                &(SHADER_COMMON.to_owned() + SHADER_VERTEX_COMMON + SHADER_VERTEX_BLOCK),
                None,
                None,
                &(SHADER_COMMON.to_owned() + SHADER_FRAGMENT))
            .expect("shader compilation failure");
        for warning in warnings {
            console::warn_1(&JsValue::from_str(&format!("GLSL warning: {:?}", warning)));
        }

        Ok(Self {
            surface,
            block_program,
            block_data_cache: None,
            fake_time: Duration::default(),
        })
    }

    pub fn render_frame(&mut self, space: &Space, camera: &Camera) {
        let mut surface = &mut self.surface;
        let block_program = &mut self.block_program;

        // TODO: quick hack; we need actual invalidation, not memoization
        let block_render_data = self.block_data_cache
            .get_or_insert_with(|| triangulate_blocks(space));

        self.fake_time += Duration::from_millis(1000/60);  // TODO

        let back_buffer = surface.back_buffer().unwrap();  // TODO error handling

        let tess = tess_space(&mut surface, &space, block_render_data);

        let render_state = RenderState::default()
            .set_face_culling(FaceCulling{order: FaceCullingOrder::CCW, mode: FaceCullingMode::Back});

        let render = surface.new_pipeline_gate().pipeline(
            &back_buffer,
            // TODO: port skybox cube map code
            &PipelineState::default().set_clear_color([0.6, 0.7, 1.0, 1.]),
            |_, mut shading_gate| {
                shading_gate.shade(block_program, |mut shader_iface, u, mut render_gate| {
                    let pm: [[f32; 4]; 4] = camera.projection().cast::<f32>().unwrap().into();
                    shader_iface.set(&u.projection_matrix0, pm[0]);
                    shader_iface.set(&u.projection_matrix1, pm[1]);
                    shader_iface.set(&u.projection_matrix2, pm[2]);
                    shader_iface.set(&u.projection_matrix3, pm[3]);

                    let vm: [[f32; 4]; 4] = camera.view().cast::<f32>().unwrap().into();
                    shader_iface.set(&u.view_matrix0, vm[0]);
                    shader_iface.set(&u.view_matrix1, vm[1]);
                    shader_iface.set(&u.view_matrix2, vm[2]);
                    shader_iface.set(&u.view_matrix3, vm[3]);

                    render_gate.render(&render_state, |mut tess_gate| {
                         tess_gate.render(&tess)
                    })
                })
            },
        ).assume();  // TODO error handlint

        if !render.is_ok() {
            panic!("not ok");  // TODO what good error handling goes here?
        }

        // There is no swap_buffers operation because WebGL implicitly does so.
    }
}

#[derive(Debug, UniformInterface)]
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
}

/// Defines vertex array structure for luminance framework.
/// Note that each "wrapper" names a new type generated by the derive(Semantics).
/// Also the macro requires the enum to be `pub` for some reason.
#[derive(Copy, Clone, Debug, Semantics)]
pub enum VertexSemantics {
    // TODO: revisit compact representations
    #[sem(name = "position", repr = "[f32; 3]", wrapper = "VertexPosition")]
    Position,
    #[sem(name = "color", repr = "[f32; 4]", wrapper = "VertexRGBA")]
    Color,
}

#[derive(Clone, Copy, Debug, Vertex)]
#[vertex(sem = "VertexSemantics")]
struct Vertex {
    #[allow(dead_code)]  // read by shader
    position: VertexPosition,

    #[allow(dead_code)]  // read by shader
    color: VertexRGBA,
}

impl BlockVertex for Vertex {
    fn from_block_vertex_parts(position: Vector3<FreeCoordinate>, color: Color) -> Self {
        let mut color_attribute = VertexRGBA::new(color.to_rgba_array());
        color_attribute[3] = 1.0;  // Force alpha to 1 until we have a better answer.

        Self {
            position: VertexPosition::new(position.cast::<f32>().unwrap().into()),
            color: color_attribute
        }
    }

    fn translate(&mut self, offset: Vector3<FreeCoordinate>) {
        self.position.repr[0] += offset.x as f32;
        self.position.repr[1] += offset.y as f32;
        self.position.repr[2] += offset.z as f32;
    }
}



fn tess_space(context :&mut WebSysWebGL2Surface, space: &Space, blocks_render_data: &BlocksRenderData<Vertex>) -> luminance_front::tess::Tess<Vertex> {
    let tess_vertices = triangulate_space(space, blocks_render_data);

    context
        .new_tess()
        .set_vertices(tess_vertices)
        .set_mode(Mode::Triangle)
        .build()
        .unwrap()  // TODO error handling
}