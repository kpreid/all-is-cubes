// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! OpenGL-based graphics rendering.

use cgmath::{EuclideanSpace as _, Matrix4, SquareMatrix as _, Vector2, Vector3};
use luminance_derive::{Semantics, UniformInterface, Vertex};
use luminance_front::context::GraphicsContext;
use luminance_front::face_culling::{FaceCulling, FaceCullingMode, FaceCullingOrder};
use luminance_front::framebuffer::Framebuffer;
use luminance_front::pipeline::{PipelineState, TextureBinding};
use luminance_front::pixel::{NormRGBA8UI, NormUnsigned};
use luminance_front::render_state::RenderState;
use luminance_front::shader::{BuiltProgram, Program, ProgramError, StageError, Uniform};
use luminance_front::tess::{Interleaved, Mode, Tess, VerticesMut};
use luminance_front::tess_gate::TessGate;
use luminance_front::texture::{
    Dim2, Dim2Array, Dimensionable, GenMipmaps, MagFilter, Sampler, Texture, TextureError, Wrap,
};
use luminance_front::Backend;
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::{Rc, Weak};
use wasm_bindgen::prelude::*;
use web_sys::console;

use all_is_cubes::camera::{Camera, ProjectionHelper};
use all_is_cubes::math::{Face, FaceMap, GridCoordinate};
use all_is_cubes::raycast::Raycaster;
use all_is_cubes::space::{PackedLight, Space};
use all_is_cubes::triangulator::{
    triangulate_blocks, triangulate_space, BlockVertex, BlocksRenderData, Coloring, Texel,
    TextureAllocator, TextureCoordinate, TextureTile, ToGfxVertex,
};
use all_is_cubes::universe::URef;

use crate::js_bindings::CanvasHelper;

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
                    let bound_block_texture =
                        pipeline.bind_texture(&mut block_data.texture_allocator.texture)?;

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

/// Defines vertex array structure for luminance framework.
/// Note that each "wrapper" names a new type generated by the derive(Semantics).
/// Also the macro requires the enum to be `pub` for some reason.
#[derive(Copy, Clone, Debug, Semantics)]
#[rustfmt::skip]
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

/// Vertex type sent to GPU. See also ``
#[derive(Clone, Copy, Debug, Vertex)]
#[vertex(sem = "VertexSemantics")]
struct Vertex {
    #[allow(dead_code)] // read by shader
    position: VertexPosition,

    #[allow(dead_code)] // read by shader
    normal: VertexNormal,

    #[allow(dead_code)] // read by shader
    color_or_texture: VertexColorOrTexture,

    #[allow(dead_code)] // read by shader
    lighting: VertexLighting,
}

impl Vertex {
    /// Make an axis-aligned rectangle. Convenience for debug code.
    /// Assumes `size` is positive.
    #[allow(dead_code)]
    fn rectangle(
        origin: Vector3<f32>,
        size: Vector3<f32>,
        tex_origin: Vector3<f32>,
        tex_size: Vector3<f32>,
    ) -> Box<[Self; 6]> {
        let normal: Vector3<f32> = Vector3::new(0.0, 0.0, 1.0);
        let dx = Vector3::new(size.x, 0.0, 0.0);
        let dy = Vector3::new(0.0, size.y, 0.0);
        let tdx = Vector3::new(tex_size.x, 0.0, 0.0);
        let tdy = Vector3::new(0.0, tex_size.y, 0.0);
        let v = |p: Vector3<f32>, t: Vector3<f32>| Vertex {
            position: VertexPosition::new(p.into()),
            normal: VertexNormal::new(normal.into()),
            color_or_texture: VertexColorOrTexture::new(t.extend(-1.0).into()),
            lighting: VertexLighting::new([1.0, 1.0, 1.0]),
        };
        Box::new([
            v(origin, tex_origin),
            v(origin + dx, tex_origin + tdx),
            v(origin + dx + dy, tex_origin + tdx + tdy),
            v(origin + dx + dy, tex_origin + tdx + tdy),
            v(origin + dy, tex_origin + tdy),
            v(origin, tex_origin),
        ])
    }
}

/// Vertex format for `triangulate_blocks` that is as close as possible to
/// `Vertex` without containing unnecessary parts.
struct GLBlockVertex {
    position: Vector3<f32>,
    normal: VertexNormal,
    color_or_texture: VertexColorOrTexture,
}

impl From<BlockVertex> for GLBlockVertex {
    fn from(vertex: BlockVertex) -> Self {
        Self {
            position: vertex.position.cast::<f32>().unwrap().to_vec(),
            normal: VertexNormal::new(vertex.normal.cast::<f32>().unwrap().into()),
            color_or_texture: match vertex.coloring {
                Coloring::Solid(color) => {
                    let mut color_attribute = VertexColorOrTexture::new(color.into());
                    // Force alpha to 1 until we have a better transparency answer. When we do,
                    // also make sure to clamp it to meet the VertexColorOrTexture protocol.
                    color_attribute[3] = 1.0;
                    color_attribute
                }
                Coloring::Texture(tc) => VertexColorOrTexture::new([tc[0], tc[1], tc[2], -1.0]),
            },
        }
    }
}

impl ToGfxVertex<Vertex> for GLBlockVertex {
    type Coordinate = f32;

    fn instantiate(&self, offset: Vector3<Self::Coordinate>, lighting: PackedLight) -> Vertex {
        Vertex {
            position: VertexPosition::new((self.position + offset).into()),
            normal: self.normal,
            color_or_texture: self.color_or_texture,
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

struct BlockGLRenderData {
    block_render_data: BlocksRenderData<GLBlockVertex, BlockGLTexture>,
    texture_allocator: BlockGLTexture,
}

impl BlockGLRenderData {
    fn prepare<C>(context: &mut C, space: &Space) -> Result<Self, TextureError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let mut texture_allocator = BlockGLTexture::new(context)?;
        let mut result = BlockGLRenderData {
            block_render_data: triangulate_blocks(space, &mut texture_allocator),
            texture_allocator,
        };
        result.texture_allocator.flush()?;
        Ok(result)
    }
}

/// Manages a block face texture, which is an atlased array texture (to minimize
/// the chance of hitting any size limits).
struct BlockGLTexture {
    texture: Texture<Dim2Array, NormRGBA8UI>,
    tile_size: u32,
    // Number of tiles in texture atlas along one edge (square root of total tiles).
    atlas_row_length: u32,
    // Number of array texture layers in use.
    layer_count: u32, // u32 is the type luminance uses
    next_free: u32,
    in_use: Vec<Weak<RefCell<TileBacking>>>,
}
#[derive(Clone, Debug)]
struct GLTile {
    /// Actual storage and metadata about the tile; may be updated as needed by the
    /// allocator to grow the texture.
    backing: Rc<RefCell<TileBacking>>,
}
#[derive(Debug)]
struct TileBacking {
    /// Index in the linear ordering of the texture atlas.
    index: u32,
    /// Origin of the tile in the texture.
    ///
    /// Technically redundant with `index`, but precomputed.
    origin: Vector3<TextureCoordinate>,
    /// Scale factor for tile coordinates (0..1) to texture coordinates (some fraction of that).
    scale: f32,
    data: Option<Box<[Texel]>>,
}

impl BlockGLTexture {
    fn new<C>(context: &mut C) -> Result<Self, TextureError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let tile_size = 16;

        // TODO implement reallocation
        let atlas_row_length = 16;
        let texture_edge_length = atlas_row_length * tile_size;
        let layer_count = 32;
        Ok(Self {
            texture: Texture::new(
                context,
                ([texture_edge_length, texture_edge_length], layer_count),
                0, // mipmaps
                Sampler {
                    wrap_s: Wrap::ClampToEdge,
                    wrap_t: Wrap::ClampToEdge,
                    mag_filter: MagFilter::Nearest,
                    ..Sampler::default()
                },
            )?,
            tile_size,
            atlas_row_length,
            layer_count,
            next_free: 0,
            in_use: Vec::new(),
        })
    }

    fn tiles_in_texture(&self) -> u32 {
        self.layer_count * self.atlas_row_length * self.atlas_row_length
    }

    /// Compute location in the atlas of a tile.
    fn index_to_location(&self, index: u32) -> (u16, u16, u16) {
        assert!(
            self.atlas_row_length <= u16::MAX as u32,
            "can't happen: texture coordinate overflow"
        );
        // u16 intermediates prove that we can losslessly convert to TextureCoordinate.
        // TODO: But do they have a runtime performance impact?
        let column = (index % self.atlas_row_length) as u16;
        let row_and_layer = index / self.atlas_row_length;
        let row = (row_and_layer % self.atlas_row_length) as u16;
        let layer = (row_and_layer / self.atlas_row_length) as u16;
        (column, row, layer)
    }

    /// Compute location in the atlas of a tile, as [0, 1] texture coordinates.
    fn index_to_origin(&self, index: u32) -> Vector3<TextureCoordinate> {
        let scale = (self.atlas_row_length as TextureCoordinate).recip();
        let (column, row, layer) = self.index_to_location(index);
        Vector3::new(
            TextureCoordinate::from(column) * scale,
            TextureCoordinate::from(row) * scale,
            // Array texture layers are integers, but passed to the shader as float.
            TextureCoordinate::from(layer),
        )
    }

    fn flush(&mut self) -> Result<(), TextureError> {
        // Set up constants for array indexing which wants usize.
        // These should never fail unless we have less than 32-bit usize.
        let tile_size: usize = self.tile_size.try_into().unwrap();
        let row_length_tiles: usize = self.atlas_row_length.try_into().unwrap();
        let row_length_texels: usize = row_length_tiles * tile_size;

        // Allocate contiguous storage for uploading.
        // TODO: Should we keep this allocated? Probably
        let mut texels = vec![(255, 0, 255, 255); Dim2Array::count(self.texture.size())];
        let mut count_written = 0;

        // TODO: Add dirty flags to enable deciding not to upload after all
        self.in_use.retain(|weak_backing| {
            // Process the non-dropped weak references
            weak_backing.upgrade().map_or(false, |strong_backing| {
                let backing: &TileBacking = &strong_backing.borrow();
                if let Some(data) = backing.data.as_ref() {
                    let tile_index: usize = backing.index.try_into().unwrap();

                    // Non-contiguously copy texels;  must skip over all the columns that
                    // belong to other tiles. We don't care about the distinction between
                    // rows and layers.
                    let atlas_memory_column: usize = tile_index % row_length_tiles;
                    let atlas_memory_row: usize = tile_index / row_length_tiles * tile_size;
                    let copy_stride = row_length_texels;
                    let copy_origin =
                        atlas_memory_row * row_length_texels + atlas_memory_column * tile_size;
                    for tile_texel_row in 0..tile_size {
                        texels[copy_origin + tile_texel_row * copy_stride
                            ..copy_origin + tile_texel_row * copy_stride + tile_size]
                            .copy_from_slice(
                                &data[(tile_texel_row * tile_size)
                                    ..((tile_texel_row + 1) * tile_size)],
                            );
                    }
                    count_written += 1;
                }
                true // retain in self.in_use
            })
        });

        self.texture.upload(GenMipmaps::Yes, &texels)?;
        console::info_1(&JsValue::from_str(&format!(
            "flushed block texture with {:?} tiles out of {:?}",
            count_written,
            self.in_use.len()
        )));
        Ok(())
    }

    #[allow(dead_code)]
    fn debug_atlas_tess<C>(&self, context: &mut C) -> Tess<Vertex>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let mut vertices = Vec::new();
        //for layer in 0..self.layer_count {
        vertices.extend(&*Vertex::rectangle(
            // position
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(1.0, 1.0, 0.0),
            // texture
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(1.0, 1.0, 0.0),
        ));
        //}
        context
            .new_tess()
            .set_vertices(vertices)
            .set_mode(Mode::Triangle)
            .build()
            .expect("failed to construct debug tess")
    }
}

impl TextureAllocator for BlockGLTexture {
    type Tile = GLTile;

    fn size(&self) -> GridCoordinate {
        self.tile_size as GridCoordinate
    }

    fn allocate(&mut self) -> GLTile {
        if self.next_free == self.tiles_in_texture() {
            todo!("ran out of tile space, but reallocation is not implemented");
        }
        let index = self.next_free;
        self.next_free += 1;
        let result = GLTile {
            backing: Rc::new(RefCell::new(TileBacking {
                index,
                origin: self.index_to_origin(index),
                scale: (self.atlas_row_length as TextureCoordinate).recip(),
                data: None,
            })),
        };
        self.in_use.push(Rc::downgrade(&result.backing));
        result
    }
}
impl TextureTile for GLTile {
    fn texcoord(&self, in_tile: Vector2<TextureCoordinate>) -> Vector3<TextureCoordinate> {
        let backing = self.backing.borrow();
        (in_tile * backing.scale).extend(0.0) + backing.origin
    }
    fn write(&mut self, data: &[Texel]) {
        self.backing.borrow_mut().data = Some(data.into());
    }
}
