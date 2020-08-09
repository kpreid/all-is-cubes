// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! OpenGL-based graphics rendering.

// Right now, we don't do 'native' rendering, only wasm, and I haven't figured out
// how to write _actually_ generic luminance code.
#![cfg(feature = "wasm")]

use cgmath::{Point3, Transform as _};
use luminance_derive::{Semantics, Vertex, UniformInterface};
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

use crate::camera::Camera;
use crate::math::{Face, FaceMap};
use crate::space::Space;

const SHADER_COMMON: &str = include_str!("shaders/common.glsl");
const SHADER_FRAGMENT: &str = include_str!("shaders/fragment.glsl");
const SHADER_VERTEX_BLOCK: &str = include_str!("shaders/vertex-block.glsl");
const SHADER_VERTEX_COMMON: &str = include_str!("shaders/vertex-common.glsl");

pub(crate) struct GLRenderer {
    surface: WebSysWebGL2Surface,
    block_program: Program<VertexSemantics, (), ShaderInterface>,
    block_data_cache: Option<BlocksRenderData>,  // TODO: quick hack, needs an invalidation strategy
    fake_time: Duration,
}

impl GLRenderer {
    #[cfg(feature = "wasm")]
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
            .get_or_insert_with(|| polygonize_blocks(space));

        self.fake_time += Duration::from_millis(1000/60);  // TODO

        let back_buffer = surface.back_buffer().unwrap();  // TODO error handling

        let triangle = polygonize_space(&mut surface, &space, block_render_data);

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

                    render_gate.render(&RenderState::default(), |mut tess_gate| {
                         tess_gate.render(&triangle)
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

// Describes how to draw one `Face` of a `Block`.
#[derive(Debug)]
struct FaceRenderData {
    vertices: Box<[Vertex]>,
    fully_opaque: bool,
}

impl Default for FaceRenderData {
    fn default() -> Self {
        FaceRenderData {
            vertices: Box::new([]),
            fully_opaque: false,
        }
    }
}

type BlocksRenderData = Vec<FaceMap<FaceRenderData>>;

lazy_static! {
    static ref EMPTY_BLOCK_RENDER_DATA: FaceMap<FaceRenderData> =
        FaceMap::generate(|_| FaceRenderData::default());
}

/// Precomputes vertices for blocks present in a space.
///
/// The resulting Vec is indexed by the `Space`'s internal unstable IDs.
fn polygonize_blocks(space: &Space) -> BlocksRenderData {
    type S = f32;  // TODO have a sensible alias somewhere that agrees with others

    let mut results: BlocksRenderData = Vec::new();
    for block in space.distinct_blocks_unfiltered() {
        results.push(FaceMap::generate(|face| {
            if face == Face::WITHIN {
                // Until we have blocks with interior detail, nothing to do here.
                return FaceRenderData::default();
            }

            let mut face_vertices: Vec<Vertex> = Vec::new();
            let transform = face.matrix();
            let color = block.color();
            let fully_opaque = color.binary_opaque();

            // TODO: Port over pseudo-transparency mechanism, then change this to a
            // within-epsilon-of-zero test.
            if fully_opaque {
                let mut color_attribute = VertexRGBA::new(color.to_rgba_array());
                color_attribute[3] = 1.0;  // Force alpha to 1 until we have a better answer.

                let mut push_1 = |p: Point3<S>| {
                    face_vertices.push(Vertex {
                        position: VertexPosition::new(transform.transform_point(p).into()),
                        color: color_attribute
                    });
                };

                // Two-triangle quad.
                // TODO: We can save CPU/memory/bandwidth by using a tessellation shader
                // to generate all six vertices from just one, right?
                push_1(Point3::new(0.0, 0.0, 0.0));
                push_1(Point3::new(1.0, 0.0, 0.0));
                push_1(Point3::new(0.0, 1.0, 0.0));
                push_1(Point3::new(1.0, 0.0, 0.0));
                push_1(Point3::new(0.0, 1.0, 0.0));
                push_1(Point3::new(1.0, 1.0, 0.0));
            }

            FaceRenderData {
                vertices: face_vertices.into_boxed_slice(),
                fully_opaque: fully_opaque,
            }
        }));
    }
    results
}

fn polygonize_space(context :&mut WebSysWebGL2Surface, space: &Space, blocks_render_data: &BlocksRenderData) -> luminance_front::tess::Tess<Vertex> {
    // TODO: take a Grid parameter for chunked rendering

    let lookup = |cube| {
        match space.get_block_index(cube) {
            Some(index) => &blocks_render_data[index as usize],
            None => &*EMPTY_BLOCK_RENDER_DATA,
        }
    };

    let mut tess_vertices: Vec<Vertex> = Vec::new();
    for cube in space.grid().interior_iter() {
        let precomputed = lookup(cube);
        let low_corner = cube.cast::<f32>().unwrap();
        // TODO: Tidy this up by implementing an Iterator for FaceMap.
        for &face in Face::all_six() {
            if lookup(cube + face.normal_vector())[face.opposite()].fully_opaque {
                // Don't draw obscured faces
                continue;
            }

            let face_vertices = &precomputed[face].vertices;
            // Copy vertices, offset to the block position.
            for vertex in face_vertices.into_iter() {
                let mut positioned_vertex: Vertex = *vertex;
                positioned_vertex.position.repr[0] += low_corner.x;
                positioned_vertex.position.repr[1] += low_corner.y;
                positioned_vertex.position.repr[2] += low_corner.z;
                tess_vertices.push(positioned_vertex);
            }
        }
    }

    context
        .new_tess()
        .set_vertices(tess_vertices)
        .set_mode(Mode::Triangle)
        .build()
        .unwrap()  // TODO error handling
}