// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! OpenGL-based graphics rendering.

use cgmath::{Vector3};
use luminance_derive::{Semantics, Vertex, UniformInterface};
use luminance_front::face_culling::{FaceCulling, FaceCullingMode, FaceCullingOrder};
use luminance_front::context::GraphicsContext;
use luminance_front::framebuffer::Framebuffer;
use luminance_front::pipeline::{PipelineState, TextureBinding};
use luminance_front::pixel::{NormRGBA8UI, NormUnsigned};
use luminance_front::render_state::RenderState;
use luminance_front::shader::{BuiltProgram, Program, ProgramError, StageError, Uniform};
use luminance_front::tess::{Interleaved, Mode, Tess, VerticesMut};
use luminance_front::tess_gate::TessGate;
use luminance_front::texture::{Dim2, Dim2Array, GenMipmaps, MagFilter, Sampler, Texture, TextureError, Wrap};
use luminance_front::Backend;
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::{Rc, Weak};
use std::time::Duration;
use wasm_bindgen::prelude::*;
use web_sys::console;

use all_is_cubes::camera::{Camera, ProjectionHelper};
use all_is_cubes::math::{Face, FaceMap, FreeCoordinate, GridCoordinate};
use all_is_cubes::space::{PackedLight, Space};
use all_is_cubes::raycast::Raycaster;
use all_is_cubes::triangulator::{
    BlockVertex, BlocksRenderData,
    Coloring,
    Texel, TextureAllocator, TextureCoordinate, TextureTile,
    ToGfxVertex,
    triangulate_blocks, triangulate_space};

use crate::js_bindings::{CanvasHelper};

const SHADER_COMMON: &str = include_str!("shaders/common.glsl");
const SHADER_FRAGMENT: &str = include_str!("shaders/fragment.glsl");
const SHADER_VERTEX_BLOCK: &str = include_str!("shaders/vertex-block.glsl");
const SHADER_VERTEX_COMMON: &str = include_str!("shaders/vertex-common.glsl");

pub struct GLRenderer<C> where C: GraphicsContext<Backend = Backend> {
    canvas_helper: CanvasHelper,
    surface: C,
    back_buffer: Framebuffer<Dim2, (), ()>,
    proj: ProjectionHelper,
    block_program: Program<VertexSemantics, (), ShaderInterface>,
    block_data_cache: Option<BlockGLRenderData>,  // TODO: quick hack, needs an invalidation strategy
    chunk: Chunk,
    fake_time: Duration,
}

impl<C> GLRenderer<C> where C: GraphicsContext<Backend = Backend> {
    pub fn new(mut surface: C, canvas_helper: CanvasHelper) -> Self {
        let program_attempt: Result<BuiltProgram<_, _, _>, ProgramError> = surface
            .new_shader_program::<VertexSemantics, (), ShaderInterface>()
            .from_strings(
                &(SHADER_COMMON.to_owned()
                    + "#line 1 1\n" + SHADER_VERTEX_COMMON
                    + "#line 1 2\n" + SHADER_VERTEX_BLOCK),
                None,
                None,
                &(SHADER_COMMON.to_owned()
                    + "#line 1 1\n" + SHADER_FRAGMENT));
        let BuiltProgram {program: block_program, warnings} = program_attempt
            .unwrap_or_else(|error| {
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
        let back_buffer = luminance::framebuffer::Framebuffer::back_buffer(&mut surface, canvas_helper.viewport_dev()).unwrap();  // TODO error handling
        Self {
            canvas_helper,
            surface,
            back_buffer,
            proj,
            block_program,
            block_data_cache: None,
            chunk: Chunk::new(),
            fake_time: Duration::default(),
        }
    }

    pub fn update_viewport(&mut self) {
        self.proj.set_viewport(self.canvas_helper.viewport_px());
        self.back_buffer = luminance::framebuffer::Framebuffer::back_buffer(&mut self.surface, self.canvas_helper.viewport_dev()).unwrap();  // TODO error handling
    }

    pub fn render_frame(&mut self, space: &Space, camera: &Camera) -> RenderInfo {
        // Not used directly for rendering, but used for cursor.
        self.proj.set_view_matrix(camera.view());

        let surface = &mut self.surface;
        let block_program = &mut self.block_program;
        let projection_matrix = self.proj.projection();

        let mut info = RenderInfo::default();

        // TODO: quick hack; we need actual invalidation, not memoization
        let block_data = self.block_data_cache
            .get_or_insert_with(|| BlockGLRenderData::prepare(surface, space).expect("texture failure"));

        self.fake_time += Duration::from_millis(1000/60);  // TODO

        self.chunk.update(surface, &space, &block_data.block_render_data);
        let ct = &self.chunk;

        let render_state = RenderState::default()
            .set_face_culling(FaceCulling{order: FaceCullingOrder::CCW, mode: FaceCullingMode::Back});

        let render = surface.new_pipeline_gate().pipeline(
            &self.back_buffer,
            // TODO: port skybox cube map code
            &PipelineState::default().set_clear_color([0.6, 0.7, 1.0, 1.]),
            |pipeline, mut shading_gate| {
                let bound_block_texture = pipeline.bind_texture(&mut block_data.texture_allocator.texture)?;

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
                    })
                })
            },
        ).assume();  // TODO error handlint

        if !render.is_ok() {
            panic!("not ok");  // TODO what good error handling goes here?
        }

        // There is no swap_buffers operation because WebGL implicitly does so.
        info
    }

    pub fn cursor_raycaster(&self, /* TODO: offset ... or move this function */) -> Raycaster {
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

/// Defines vertex array structure for luminance framework.
/// Note that each "wrapper" names a new type generated by the derive(Semantics).
/// Also the macro requires the enum to be `pub` for some reason.
#[derive(Copy, Clone, Debug, Semantics)]
pub enum VertexSemantics {
    // TODO: revisit compact representations
    #[sem(name = "a_position", repr = "[f32; 3]", wrapper = "VertexPosition")]
    Position,
    #[sem(name = "a_normal", repr = "[f32; 3]", wrapper = "VertexNormal")]
    Normal,
    /// Packed format:
    /// * If [3] is in the range 0.0 to 1.0, then the attribute is a solid RGBA color.
    /// * If [3] is -1.0, then the first three components are array texture coordinates.
    #[sem(name = "a_color_or_texture", repr = "[f32; 4]", wrapper = "VertexColorOrTexture")]
    ColorOrTexture,
    // TODO: look into packed repr for lighting
    #[sem(name = "a_lighting", repr = "[f32; 3]", wrapper = "VertexLighting")]
    Lighting,
}

#[derive(Clone, Copy, Debug, Vertex)]
#[vertex(sem = "VertexSemantics")]
struct Vertex {
    #[allow(dead_code)]  // read by shader
    position: VertexPosition,

    #[allow(dead_code)]  // read by shader
    normal: VertexNormal,

    #[allow(dead_code)]  // read by shader
    color_or_texture: VertexColorOrTexture,

    #[allow(dead_code)]  // read by shader
    lighting: VertexLighting,
}

impl ToGfxVertex<Vertex> for BlockVertex {
    // TODO: Stop using BlockVertex as our block vertex type, because it stores FreeCoordinate
    // and we want f32, forcing us to translate at every instantiation.

    fn instantiate(&self, offset: Vector3<FreeCoordinate>, lighting: PackedLight) -> Vertex {
        Vertex {
            position: VertexPosition::new((self.position + offset).cast::<f32>().unwrap().into()),
            normal: VertexNormal::new(self.normal.cast::<f32>().unwrap().into()),
            color_or_texture: match self.coloring {
                Coloring::Solid(color) => {
                    let mut color_attribute = VertexColorOrTexture::new(color.into());
                    // Force alpha to 1 until we have a better transparency answer. When we do,
                    // also make sure to clamp it to meet the VertexColorOrTexture protocol.
                    color_attribute[3] = 1.0;
                    color_attribute
                },
                Coloring::Texture(tc) => VertexColorOrTexture::new([tc[0], tc[1], tc[2], -1.0]),
            },
            lighting: VertexLighting::new(lighting.into()),
        }
    }
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
        blocks_render_data: &BlocksRenderData<BlockVertex, BlockGLTexture>,
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
            let new_vertices :&[Vertex] = self.vertices[face].as_ref();

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
                let mut buffer_slice: VerticesMut<Vertex, (), (), Interleaved, Vertex> =
                    tess.vertices_mut().expect("failed to map vertices for copying");
                buffer_slice.copy_from_slice(new_vertices);
            } else {
                // Allocate and populate new buffer.
                *tess_option = Some(context.new_tess()
                    .set_vertices(self.vertices[face].clone())
                    .set_mode(Mode::Triangle)
                    .build()
                    .unwrap());
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

struct BlockGLRenderData {
    block_render_data: BlocksRenderData<BlockVertex, BlockGLTexture>,
    texture_allocator: BlockGLTexture,
}

impl BlockGLRenderData {
    fn prepare<C>(context: &mut C, space: &Space) -> Result<Self, TextureError> where C: GraphicsContext<Backend = Backend> {
        let mut texture_allocator = BlockGLTexture::new(context)?;
        let mut result = BlockGLRenderData {
            block_render_data: triangulate_blocks(space, &mut texture_allocator),
            texture_allocator,
        };
        result.texture_allocator.flush()?;
        Ok(result)
    }
}

/// Manages a block face texture, which is an array texture (each face gets a separate
/// texture layer).
struct BlockGLTexture {
    texture: Texture<Dim2Array, NormRGBA8UI>,
    tile_size: u32,
    layer_count: u32,  // u32 is the type luminance uses
    next_free: u32,
    in_use: Vec<Weak<RefCell<TileBacking>>>,
}
#[derive(Clone, Debug)]
struct GLTile {
    index: u32,
    backing: Rc<RefCell<TileBacking>>,  // TODO: Instead of runtime interior mutability, can we arrange to enable mutability through having a mutable ref to the allocator?
}
#[derive(Debug)]
struct TileBacking {
    index: u32,
    data: Option<Box<[Texel]>>,
}

impl BlockGLTexture {
    fn new<C>(context: &mut C) -> Result<Self, TextureError>  where C: GraphicsContext<Backend = Backend> {
        let layer_count = 1000;  // TODO implement reallocation
        let tile_size = 16;
        Ok(Self {
            texture: Texture::new(
                context,
                ([tile_size, tile_size], layer_count),
                0,  // mipmaps
                Sampler {
                    wrap_s: Wrap::ClampToEdge,
                    wrap_t: Wrap::ClampToEdge,
                    mag_filter: MagFilter::Nearest,
                    ..Sampler::default()
                })?,
            tile_size,
            layer_count,
            next_free: 0,
            in_use: Vec::new(),
        })
    }

    fn flush(&mut self) -> Result<(), TextureError> {
        let tile_size_in_array = self.tile_size_in_array();
        // Allocate contiguous storage for uploading.
        // TODO: Should we keep this allocated? Probably.
        let mut texels = vec![
            (255, 0, 255, 255);
            (self.layer_count * self.tile_size * self.tile_size)
                .try_into().expect("texture too big for memory?!")];
        let mut count_written = 0;

        // TODO: Add dirty flags to enable deciding not to upload after all
        self.in_use.retain(|weak_backing| {
            // Process the non-dropped weak references
            weak_backing.upgrade().map_or(false, |strong_backing| {
                let backing: &TileBacking = &strong_backing.borrow();
                if let Some(data) = backing.data.as_ref() {
                    let tile_index: usize = backing.index as usize;
                    texels[(tile_index * tile_size_in_array)
                           ..((tile_index + 1) * tile_size_in_array)].copy_from_slice(&*data);
                    count_written += 1;
                }
                true  // retain in self.in_use
            })
        });

        self.texture.upload(GenMipmaps::Yes, &texels)?;
        console::info_1(&JsValue::from_str(&format!(
            "flushed block texture with {:?} tiles out of {:?}",
            count_written,
            self.in_use.len())));
        Ok(())
    }

    /// Number of texel array elements a single tile spans.
    fn tile_size_in_array(&self) -> usize {
        (self.tile_size * self.tile_size) as usize
    }
}

impl TextureAllocator for BlockGLTexture {
    type Tile = GLTile;

    fn size(&self) -> GridCoordinate {
        self.tile_size as GridCoordinate
    }

    fn allocate(&mut self) -> GLTile {
        if self.next_free == self.layer_count {
            todo!("ran out of tile space, but reallocation is not implemented");
        }
        let result = GLTile {
            index: self.next_free,
            backing: Rc::new(RefCell::new(TileBacking {
                index: self.next_free,
                data: None,
            })),
        };
        self.next_free += 1;
        self.in_use.push(Rc::downgrade(&result.backing));
        result
    }
}
impl TextureTile for GLTile {
    fn index(&self) -> TextureCoordinate { self.index as TextureCoordinate }
    fn write(&mut self, data: &[Texel]) {
        self.backing.borrow_mut().data = Some(data.into());
    }
}
