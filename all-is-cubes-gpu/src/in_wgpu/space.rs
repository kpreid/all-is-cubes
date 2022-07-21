// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Manages meshes for rendering a [`Space`].

use std::cell::RefCell;
use std::collections::HashSet;
use std::sync::{Arc, Mutex, Weak};

use instant::Instant;

use all_is_cubes::camera::Camera;
use all_is_cubes::cgmath::{EuclideanSpace, Point3, Transform, Vector3};
use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::content::palette;
use all_is_cubes::listen::Listener;
use all_is_cubes::math::{
    Aab, Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridPoint, Rgb,
};
use all_is_cubes::mesh::chunked_mesh::{ChunkMeshUpdate, ChunkedSpaceMesh};
use all_is_cubes::mesh::DepthOrdering;
use all_is_cubes::space::{Space, SpaceChange};
use all_is_cubes::universe::URef;

use crate::in_wgpu::glue::{size_vector_to_extent, write_texture_by_aab};
use crate::in_wgpu::pipelines::Pipelines;
use crate::in_wgpu::vertex::WgpuLinesVertex;
use crate::in_wgpu::{
    block_texture::{AtlasAllocator, AtlasTile},
    camera::ShaderSpaceCamera,
    glue::{to_wgpu_index_range, BeltWritingParts, ResizingBuffer},
    vertex::WgpuBlockVertex,
};
use crate::{
    wireframe_vertices, DebugLineVertex, GraphicsResourceError, SpaceDrawInfo, SpaceUpdateInfo,
};

const CHUNK_SIZE: GridCoordinate = 16;

/// Manages cached data and GPU resources for drawing a single [`Space`] and
/// following its changes.
#[derive(Debug)]
pub(crate) struct SpaceRenderer {
    /// A debugging label for the space's render pass.
    /// (Derived from constructor's space_label)
    render_pass_label: String,
    space_label: String,

    /// Tracks information we need to update from the `Space`.
    /// Note that `self.csm` has its own todo listener too.
    todo: Arc<Mutex<SpaceRendererTodo>>,

    /// Cached copy of `space.physics.sky_color`.
    pub(crate) sky_color: Rgb,

    block_texture: AtlasAllocator,
    light_texture: SpaceLightTexture,

    /// Buffer containing the [`ShaderSpaceCamera`] configured for this Space.
    camera_buffer: wgpu::Buffer,
    /// Bind group for camera_buffer.
    pub(crate) camera_bind_group: wgpu::BindGroup,

    /// Bind group containing our block texture and light texture.
    space_bind_group: wgpu::BindGroup,

    csm: ChunkedSpaceMesh<Option<ChunkBuffers>, WgpuBlockVertex, AtlasAllocator, CHUNK_SIZE>,
    //TODO: debug_chunk_boxes_tess: Option<Tess<Backend, LinesVertex>>,
}

#[derive(Debug, Default)]
struct ChunkBuffers {
    vertex_buf: ResizingBuffer,
    index_buf: ResizingBuffer,
}
const INDEX_FORMAT: wgpu::IndexFormat = wgpu::IndexFormat::Uint32;

impl SpaceRenderer {
    /// TODO: Simplify callers by making it possible to create a SpaceRenderer without a space.
    /// Besides simpler initialization, this will also allow reusing allocated resources across a
    /// period of no space.
    pub fn new(
        space: URef<Space>,
        space_label: String,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        pipelines: &Pipelines,
    ) -> Result<Self, GraphicsResourceError> {
        let space_borrowed = space.borrow();

        let block_texture = AtlasAllocator::new(&space_label, device, queue)?;
        let light_texture = SpaceLightTexture::new(&space_label, device, space_borrowed.bounds());

        let space_bind_group = create_space_bind_group(
            &space_label,
            device,
            pipelines,
            &block_texture,
            &light_texture,
        );

        let camera_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some(&format!("{space_label} camera_buffer")),
            size: std::mem::size_of::<ShaderSpaceCamera>().try_into().unwrap(),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let camera_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &pipelines.camera_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: camera_buffer.as_entire_binding(),
            }],
            label: Some(&format!("{space_label} camera_bind_group")),
        });

        let todo = Arc::new(Mutex::new(SpaceRendererTodo::default()));
        space_borrowed.listen(TodoListener(Arc::downgrade(&todo)));

        Ok(SpaceRenderer {
            todo,
            render_pass_label: format!("{space_label} render_pass"),
            space_label,
            sky_color: space_borrowed.physics().sky_color,
            block_texture,
            light_texture,
            space_bind_group,
            camera_buffer,
            camera_bind_group,
            csm: ChunkedSpaceMesh::new(space),
        })
    }

    /// Replace the space being rendered, while preserving some of the resources used to render it.
    ///
    /// This is not a minimum-effort operation and should be thought of as an optimized
    /// variant of building a new [`SpaceRenderer`] from scratch. However, it does check if the
    /// given space is equal to the current space before doing anything.
    pub(crate) fn set_space(
        &mut self,
        device: &wgpu::Device,
        pipelines: &Pipelines,
        space: &URef<Space>,
    ) {
        if self.csm.space() == space {
            return;
        }

        // Destructuring to explicitly skip or handle each field.
        let SpaceRenderer {
            render_pass_label: _,
            space_label,
            todo,
            sky_color,
            block_texture,
            light_texture,
            camera_buffer: _,
            camera_bind_group: _,
            space_bind_group,
            csm,
        } = self;

        let space_borrowed = space.borrow();

        *todo = {
            let todo = Arc::new(Mutex::new(SpaceRendererTodo::default()));
            space_borrowed.listen(TodoListener(Arc::downgrade(&todo)));
            todo
        };
        // TODO: rescue ChunkChart and maybe block meshes from the old `csm`.
        *csm = ChunkedSpaceMesh::new(space.clone());
        *sky_color = space_borrowed.physics().sky_color;
        // TODO: don't replace light texture if the size is the same
        *light_texture = SpaceLightTexture::new(space_label, device, space_borrowed.bounds());
        // bind group must be recreated for new light texture
        *space_bind_group =
            create_space_bind_group(space_label, device, pipelines, block_texture, light_texture);
    }

    /// Update renderer internal state from the given [`Camera`] and referenced [`Space`],
    /// so that the next rendered meshes will be up to date (or as far up to date as the
    /// given [`deadline`] permits).
    pub(crate) fn update(
        &mut self,
        deadline: Instant,
        queue: &wgpu::Queue,
        camera: &Camera,
        bwp: BeltWritingParts<'_, '_>,
    ) -> Result<SpaceUpdateInfo, GraphicsResourceError> {
        let start_time = Instant::now();

        let mut todo = self.todo.lock().unwrap();

        let space = &*self
            .csm
            .space()
            .try_borrow()
            .expect("TODO: return a trivial result instead of panic.");

        // Update sky color (cheap so we don't bother todo-tracking it)
        self.sky_color = space.physics().sky_color;

        // Update light texture
        let start_light_update = Instant::now();
        let mut light_update_count = 0;
        if let Some(set) = &mut todo.light {
            // TODO: work in larger, ahem, chunks
            for cube in set.drain() {
                light_update_count += self.light_texture.update(
                    queue,
                    space,
                    GridAab::from_lower_size(cube, [1, 1, 1]),
                );
            }
        } else {
            light_update_count += self.light_texture.update_all(queue, space);
            todo.light = Some(HashSet::new());
        }
        let end_light_update = Instant::now();

        // TODO: kludge; refactor to avoid needing it
        let rcbwp = RefCell::new(bwp);

        // Update chunks
        let csm_info = self.csm.update_blocks_and_some_chunks(
            camera,
            &mut self.block_texture,
            deadline, // TODO: decrease deadline by some guess at texture writing time
            |u| {
                update_chunk_buffers(rcbwp.borrow_mut().reborrow(), u, &self.space_label);
            },
            |u| {
                if let Some(index_buf) = u.render_data.as_ref().and_then(|b| b.index_buf.get()) {
                    let index_buf_bytes = bytemuck::cast_slice::<u32, u8>(u.mesh.indices());
                    if let Some(len) = index_buf_bytes
                        .len()
                        .try_into()
                        .ok()
                        .and_then(wgpu::BufferSize::new)
                    {
                        rcbwp
                            .borrow_mut()
                            .write_buffer(index_buf, 0, len)
                            .copy_from_slice(index_buf_bytes);
                    }
                }
            },
        );

        // Flush all texture updates to GPU.
        // This must happen after `csm.update_blocks_and_some_chunks` so that the newly
        // generated meshes have the texels they expect.
        let texture_info = self.block_texture.flush(queue);

        // if graphics_options.debug_chunk_boxes {
        //     // TODO
        // } else {
        //     //self.debug_chunk_boxes_tess = None;
        // }

        let end_time = Instant::now();

        Ok(SpaceUpdateInfo {
            total_time: end_time.duration_since(start_time),
            light_update_time: end_light_update.duration_since(start_light_update),
            light_update_count,
            chunk_info: csm_info,
            texture_info,
        })
    }

    // TODO: needs error return or not?
    #[allow(clippy::too_many_arguments)]
    pub fn draw(
        &self,
        output_view: &wgpu::TextureView,
        depth_texture_view: &wgpu::TextureView,
        queue: &wgpu::Queue,
        encoder: &mut wgpu::CommandEncoder,
        pipelines: &Pipelines,
        camera: &Camera,
        color_load_op: wgpu::LoadOp<wgpu::Color>,
    ) -> Result<SpaceDrawInfo, GraphicsResourceError> {
        let start_time = Instant::now();

        let csm = &self.csm;
        let view_direction_mask = camera.view_direction_mask();
        let view_chunk = csm.view_chunk();

        queue.write_buffer(
            &self.camera_buffer,
            0,
            // The [] around the camera is needed for bytemuck, so that both input and output
            // are slices.
            bytemuck::cast_slice::<ShaderSpaceCamera, u8>(&[ShaderSpaceCamera::new(
                camera,
                self.sky_color,
                self.light_texture.light_lookup_offset(),
            )]),
        );

        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some(&self.render_pass_label),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: output_view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: color_load_op,
                    store: true,
                },
            })],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                view: depth_texture_view,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    // We don't need to store, because we won't be using this depth information
                    // in a future pass.
                    store: false,
                }),
                stencil_ops: None,
            }),
        });
        render_pass.set_bind_group(0, &self.camera_bind_group, &[]);
        render_pass.set_bind_group(1, &self.space_bind_group, &[]);

        // Opaque geometry first, in front-to-back order
        let start_opaque_draw_time = Instant::now();
        let mut chunks_drawn = 0;
        let mut squares_drawn = 0;
        render_pass.set_pipeline(&pipelines.opaque_render_pipeline);
        // TODO: ChunkedSpaceMesh should probably provide this chunk iterator itself
        // since every caller needs it.
        for p in csm.chunk_chart().chunks(view_chunk, view_direction_mask) {
            // Chunk existence lookup is faster than the frustum culling test, so we do that first.
            if let Some(chunk) = csm.chunk(p) {
                if cull(camera, p) {
                    continue;
                }
                chunks_drawn += 1;

                if let Some(buffers) = &chunk.render_data {
                    let range = chunk.mesh().opaque_range();
                    if !range.is_empty() {
                        set_buffers(&mut render_pass, buffers);
                        render_pass.draw_indexed(to_wgpu_index_range(range.clone()), 0, 0..1);
                        squares_drawn += range.len() / 6;
                    }
                }
            }
            // TODO: If the chunk is missing, draw a blocking shape, possibly?
        }

        // Transparent geometry after opaque geometry, in back-to-front order
        let start_draw_transparent_time = Instant::now();
        if camera.options().transparency.will_output_alpha() {
            render_pass.set_pipeline(&pipelines.transparent_render_pipeline);
            for p in csm
                .chunk_chart()
                .chunks(view_chunk, view_direction_mask)
                .rev()
            {
                // Chunk existence lookup is faster than the frustum culling test, so we do that first.
                if let Some(chunk) = csm.chunk(p) {
                    if cull(camera, p) {
                        continue;
                    }
                    if let Some(buffers) = &chunk.render_data {
                        let range = chunk.mesh().transparent_range(
                            // TODO: avoid adding and then subtracting view_chunk
                            DepthOrdering::from_view_direction(p.0 - view_chunk.0),
                        );
                        if !range.is_empty() {
                            set_buffers(&mut render_pass, buffers);
                            render_pass.draw_indexed(to_wgpu_index_range(range.clone()), 0, 0..1);
                            squares_drawn += range.len() / 6;
                        }
                    }
                }
            }
        }
        let end_time = Instant::now();

        Ok(SpaceDrawInfo {
            draw_init_time: start_opaque_draw_time.duration_since(start_time),
            draw_opaque_time: start_draw_transparent_time.duration_since(start_opaque_draw_time),
            draw_transparent_time: end_time.duration_since(start_draw_transparent_time),
            squares_drawn,
            chunks_drawn,
        })
    }

    /// Returns the camera, to allow additional drawing in the same coordinate system.
    pub(crate) fn camera_bind_group(&self) -> &wgpu::BindGroup {
        &self.camera_bind_group
    }

    /// Generate debug lines for the current state of the renderer, assuming
    /// draw() was just called.
    pub(crate) fn debug_lines(&self, camera: &Camera, v: &mut Vec<WgpuLinesVertex>) {
        if camera.options().debug_chunk_boxes {
            // TODO: remember view direction mask instead of rerequesting it?
            for chunk in self
                .csm
                .chunk_chart()
                .chunks(self.csm.view_chunk(), camera.view_direction_mask())
            {
                wireframe_vertices::<WgpuLinesVertex, _, _>(
                    v,
                    palette::DEBUG_CHUNK_MAJOR,
                    &Aab::from(chunk.bounds()),
                );
            }

            // Frame the nearest chunk in detail
            let chunk_origin = self
                .csm
                .view_chunk()
                .bounds()
                .lower_bounds()
                .map(FreeCoordinate::from);
            for face in Face6::ALL {
                let m = face.matrix(CHUNK_SIZE);
                for i in 1..CHUNK_SIZE {
                    let mut push = |p| {
                        v.push(WgpuLinesVertex::from_position_color(
                            m.transform_point(p).map(FreeCoordinate::from) + chunk_origin.to_vec(),
                            palette::DEBUG_CHUNK_MINOR,
                        ));
                    };
                    push(Point3::new(i, 0, 0));
                    push(Point3::new(i, CHUNK_SIZE, 0));
                    push(Point3::new(0, i, 0));
                    push(Point3::new(CHUNK_SIZE, i, 0));
                }
            }
        }
    }
}

fn create_space_bind_group(
    space_label: &str,
    device: &wgpu::Device,
    pipelines: &Pipelines,
    block_texture: &AtlasAllocator,
    light_texture: &SpaceLightTexture,
) -> wgpu::BindGroup {
    device.create_bind_group(&wgpu::BindGroupDescriptor {
        layout: &pipelines.space_texture_bind_group_layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&block_texture.texture_view),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Sampler(&block_texture.sampler),
            },
            wgpu::BindGroupEntry {
                binding: 2,
                resource: wgpu::BindingResource::TextureView(&light_texture.texture_view),
            },
        ],
        label: Some(&format!("{space_label} space_bind_group")),
    })
}

/// TODO: this probably should be a method on the camera
fn cull(camera: &Camera, chunk: ChunkPos<CHUNK_SIZE>) -> bool {
    camera.options().use_frustum_culling && !camera.aab_in_view(chunk.bounds().into())
}

fn set_buffers<'a>(render_pass: &mut wgpu::RenderPass<'a>, buffers: &'a ChunkBuffers) {
    render_pass.set_vertex_buffer(
        0,
        buffers
            .vertex_buf
            .get()
            .expect("missing vertex buffer")
            .slice(..),
    );
    render_pass.set_index_buffer(
        buffers
            .index_buf
            .get()
            .expect("missing index buffer")
            .slice(..),
        INDEX_FORMAT,
    );
}

/// Copy [`SpaceMesh`] data to GPU buffers.
fn update_chunk_buffers(
    mut bwp: BeltWritingParts<'_, '_>,
    update: ChunkMeshUpdate<'_, Option<ChunkBuffers>, WgpuBlockVertex, AtlasTile, CHUNK_SIZE>,
    space_label: &str,
) {
    if update.mesh.is_empty() {
        // No action needed
        return;
    }

    let new_vertices_data: &[u8] =
        bytemuck::cast_slice::<WgpuBlockVertex, u8>(update.mesh.vertices());
    // TODO: assert INDEX_FORMAT matches this type
    let new_indices_data: &[u8] = bytemuck::cast_slice::<u32, u8>(update.mesh.indices());

    let position: [GridCoordinate; 3] = update.position.0.into();
    let buffers = update.render_data.get_or_insert_with(ChunkBuffers::default);
    buffers.vertex_buf.write_with_resizing(
        bwp.reborrow(),
        &wgpu::util::BufferInitDescriptor {
            // TODO: Get the space's label and chunk coordinates here (cheaply)
            label: Some(&format!("{space_label} chunk vertex {position:?}")),
            contents: new_vertices_data,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        },
    );
    buffers.index_buf.write_with_resizing(
        bwp.reborrow(),
        &wgpu::util::BufferInitDescriptor {
            label: Some(&format!("{space_label} chunk index {position:?}")),
            contents: new_indices_data,
            usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
        },
    );
}

/// [`SpaceRenderer`]'s set of things that need recomputing.
#[derive(Debug, Default)]
struct SpaceRendererTodo {
    /// Blocks whose light texels should be updated.
    /// None means do a full space reupload.
    ///
    /// TODO: experiment with different granularities of light invalidation (chunks, dirty rects, etc.)
    light: Option<HashSet<GridPoint>>,
}

/// [`Listener`] adapter for [`SpaceRendererTodo`].
#[derive(Clone, Debug)]
struct TodoListener(Weak<Mutex<SpaceRendererTodo>>);

impl Listener<SpaceChange> for TodoListener {
    fn receive(&self, message: SpaceChange) {
        if let Some(cell) = self.0.upgrade() {
            if let Ok(mut todo) = cell.lock() {
                match message {
                    SpaceChange::EveryBlock => {
                        todo.light = None;
                    }
                    SpaceChange::Lighting(p) => {
                        // None means we're already at "update everything"
                        if let Some(set) = &mut todo.light {
                            set.insert(p);
                        }
                    }
                    SpaceChange::Block(..) => {}
                    SpaceChange::Number(..) => {}
                    SpaceChange::BlockValue(..) => {}
                }
            }
        }
    }

    fn alive(&self) -> bool {
        self.0.strong_count() > 0
    }
}

/// Keeps a 3D [`Texture`] up to date with the light data from a [`Space`].
///
/// The texels are in [`PackedLight`] form.
#[derive(Debug)]
struct SpaceLightTexture {
    texture: wgpu::Texture,
    texture_view: wgpu::TextureView,
    /// The region of cube coordinates for which there are valid texels.
    texture_bounds: GridAab,
}

impl SpaceLightTexture {
    /// Construct a new `SpaceLightTexture` for the specified size of [`Space`],
    /// with no data.
    pub fn new(label_prefix: &str, device: &wgpu::Device, bounds: GridAab) -> Self {
        // Boundary of 1 extra cube automatically captures sky light.
        let texture_bounds = bounds.expand(FaceMap {
            px: 1,
            py: 1,
            pz: 1,
            nx: 0,
            ny: 0,
            nz: 0,
            within: 0,
        });
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            size: size_vector_to_extent(texture_bounds.size()),
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D3,
            format: wgpu::TextureFormat::Rgba8Uint,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            label: Some(&format!("{label_prefix} space light")),
        });
        Self {
            texture_view: texture.create_view(&wgpu::TextureViewDescriptor::default()),
            texture,
            texture_bounds,
        }
    }

    /// Copy the specified region of light data.
    pub fn update(&mut self, queue: &wgpu::Queue, space: &Space, region: GridAab) -> usize {
        let mut data: Vec<[u8; 4]> = Vec::with_capacity(region.volume());
        // TODO: Enable circular operation and eliminate the need for the offset of the
        // coordinates (texture_bounds.lower_bounds() and light_offset in the shader)
        // by doing a coordinate wrap-around -- the shader and the Space will agree
        // on coordinates modulo the texture size, and this upload will need to be broken
        // into up to 8 pieces.
        for z in region.z_range() {
            for y in region.y_range() {
                for x in region.x_range() {
                    data.push(space.get_lighting([x, y, z]).as_texel());
                }
            }
        }

        write_texture_by_aab(
            queue,
            &self.texture,
            region.translate(self.light_lookup_offset()),
            &data,
        );

        region.volume()
    }

    pub fn update_all(&mut self, queue: &wgpu::Queue, space: &Space) -> usize {
        self.update(queue, space, self.texture_bounds);
        self.texture_bounds.volume()
    }

    fn light_lookup_offset(&self) -> Vector3<i32> {
        -self.texture_bounds.lower_bounds().to_vec()
    }
}
