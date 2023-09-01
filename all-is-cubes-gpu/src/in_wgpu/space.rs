//! Manages meshes for rendering a [`Space`].

use std::collections::HashSet;
use std::sync::{Arc, Mutex, Weak};

use instant::Instant;

use all_is_cubes::camera::{Camera, Flaws};
use all_is_cubes::cgmath::{EuclideanSpace, Point3, Vector3};
use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::content::palette;
use all_is_cubes::listen::{Listen as _, Listener};
use all_is_cubes::math::{Cube, Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, Rgb};
use all_is_cubes::space::{Space, SpaceChange};
use all_is_cubes::universe::URef;
use all_is_cubes_mesh::dynamic::{ChunkedSpaceMesh, RenderDataUpdate};
use all_is_cubes_mesh::{DepthOrdering, IndexSlice};

use crate::in_wgpu::frame_texture::FramebufferTextures;
use crate::in_wgpu::glue::{size_vector_to_extent, to_wgpu_index_format, write_texture_by_aab};
use crate::in_wgpu::pipelines::Pipelines;
use crate::in_wgpu::vertex::{WgpuInstanceData, WgpuLinesVertex};
use crate::in_wgpu::{
    block_texture::{AtlasAllocator, AtlasTile},
    camera::ShaderSpaceCamera,
    glue::{to_wgpu_index_range, BeltWritingParts, ResizingBuffer},
    vertex::WgpuBlockVertex,
};
use crate::{DebugLineVertex, GraphicsResourceError, Memo, SpaceDrawInfo, SpaceUpdateInfo};

const CHUNK_SIZE: GridCoordinate = 16;

/// Manages cached data and GPU resources for drawing a single [`Space`] and
/// following its changes.
#[derive(Debug)]
pub(crate) struct SpaceRenderer {
    space_label: String,
    /// A debugging label for the space's render pass.
    /// (Derived from constructor's space_label)
    render_pass_label: String,
    instance_buffer_label: String,

    /// Tracks information we need to update from the `Space`.
    /// Note that `self.csm` has its own todo listener too.
    todo: Arc<Mutex<SpaceRendererTodo>>,

    /// Cached copy of `space.physics.sky_color`.
    pub(crate) sky_color: Rgb,

    block_texture: AtlasAllocator,
    light_texture: SpaceLightTexture,

    /// Buffer containing the [`ShaderSpaceCamera`] configured for this Space.
    camera_buffer: SpaceCameraBuffer,

    /// Buffer for instance data.
    /// Rewritten every frame, but reused to save reallocation.
    instance_buffer: ResizingBuffer,

    /// Bind group containing our block texture and light texture,
    space_bind_group:
        Memo<(wgpu::Id<wgpu::TextureView>, wgpu::Id<wgpu::TextureView>), wgpu::BindGroup>,

    csm: ChunkedSpaceMesh<Option<ChunkBuffers>, WgpuBlockVertex, AtlasAllocator, CHUNK_SIZE>,

    interactive: bool,
}

#[derive(Debug, Default)]
struct ChunkBuffers {
    vertex_buf: ResizingBuffer,
    index_buf: ResizingBuffer,
    index_format: wgpu::IndexFormat,
}

impl SpaceRenderer {
    /// TODO: Simplify callers by making it possible to create a `SpaceRenderer` without a space.
    /// Besides simpler initialization, this will also allow reusing allocated resources across a
    /// period of no space.
    pub fn new(
        space: URef<Space>,
        space_label: String,
        device: &wgpu::Device,
        pipelines: &Pipelines,
        block_texture: AtlasAllocator,
        interactive: bool,
    ) -> Result<Self, GraphicsResourceError> {
        let space_borrowed = space.read().map_err(GraphicsResourceError::read_err)?;

        let light_texture = SpaceLightTexture::new(&space_label, device, space_borrowed.bounds());

        let camera_buffer = SpaceCameraBuffer::new(&space_label, device, pipelines);

        let todo = Arc::new(Mutex::new(SpaceRendererTodo::default()));
        space_borrowed.listen(TodoListener(Arc::downgrade(&todo)));

        Ok(SpaceRenderer {
            todo,
            render_pass_label: format!("{space_label} render_pass"),
            instance_buffer_label: format!("{space_label} instances"),
            space_label,
            sky_color: space_borrowed.physics().sky_color,
            block_texture,
            light_texture,
            space_bind_group: Memo::new(),
            camera_buffer,
            instance_buffer: ResizingBuffer::default(),
            csm: ChunkedSpaceMesh::new(space, interactive),
            interactive,
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
        _pipelines: &Pipelines,
        space: &URef<Space>,
    ) {
        if self.csm.space() == space {
            return;
        }

        // Destructuring to explicitly skip or handle each field.
        let SpaceRenderer {
            space_label,
            render_pass_label: _,
            instance_buffer_label: _,
            todo,
            sky_color,
            block_texture: _,
            light_texture,
            camera_buffer: _,
            instance_buffer: _,
            space_bind_group: _, // will be updated later
            csm,
            interactive,
        } = self;

        let space_borrowed = space.read().unwrap();

        *todo = {
            let todo = Arc::new(Mutex::new(SpaceRendererTodo::default()));
            space_borrowed.listen(TodoListener(Arc::downgrade(&todo)));
            todo
        };
        // TODO: rescue ChunkChart and maybe block meshes from the old `csm`.
        *csm = ChunkedSpaceMesh::new(space.clone(), *interactive);
        *sky_color = space_borrowed.physics().sky_color;
        // TODO: don't replace light texture if the size is the same
        *light_texture = SpaceLightTexture::new(space_label, device, space_borrowed.bounds());
    }

    /// Update renderer internal state from the given [`Camera`] and referenced [`Space`],
    /// so that the next rendered meshes will be up to date (or as far up to date as the
    /// given [`deadline`] permits).
    pub(crate) fn update(
        &mut self,
        deadline: Instant,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        pipelines: &Pipelines,
        camera: &Camera,
        mut bwp: BeltWritingParts<'_, '_>,
    ) -> Result<SpaceUpdateInfo, GraphicsResourceError> {
        let start_time = Instant::now();

        let mut todo = self.todo.lock().unwrap();

        let space = &*self
            .csm
            .space()
            .read()
            .map_err(GraphicsResourceError::read_err)?;

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

        // Update chunks
        let csm_info = self.csm.update_blocks_and_some_chunks(
            camera,
            &self.block_texture,
            deadline, // TODO: decrease deadline by some guess at texture writing time
            |u| {
                if u.indices_only {
                    if let Some(index_buf) = u.render_data.as_ref().and_then(|b| b.index_buf.get())
                    {
                        // It's OK to ignore which type the indices are because they will
                        // always be the same type as they were previously.
                        let index_buf_bytes = u.mesh.indices().as_bytes();
                        if let Some(len) = index_buf_bytes
                            .len()
                            .try_into()
                            .ok()
                            .and_then(wgpu::BufferSize::new)
                        {
                            bwp.write_buffer(index_buf, 0, len)
                                .copy_from_slice(index_buf_bytes);
                        }
                    }
                } else {
                    update_chunk_buffers(bwp.reborrow(), u, &self.space_label);
                }
            },
        );

        // Ensure instance buffer is big enough.
        self.instance_buffer.resize_at_least(
            bwp.device,
            &wgpu::BufferDescriptor {
                label: Some(&self.instance_buffer_label),
                size: u64::try_from(
                    self.csm.chunk_chart().count_all() * std::mem::size_of::<WgpuInstanceData>(),
                )
                .expect("instance buffer size overflow"),
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::VERTEX,
                mapped_at_creation: false,
            },
        );

        // Flush all texture updates to GPU.
        // This must happen after `csm.update_blocks_and_some_chunks` so that the newly
        // generated meshes have the texels they expect.
        let (block_texture_view, texture_info) = self.block_texture.flush(device, queue);

        // Update space bind group if needed.
        self.space_bind_group.get_or_insert(
            (
                block_texture_view.global_id(),
                self.light_texture.texture_view.global_id(),
            ),
            || {
                create_space_bind_group(
                    &self.space_label,
                    device,
                    pipelines,
                    &self.block_texture,
                    &self.light_texture,
                )
            },
        );

        let end_time = Instant::now();

        Ok(SpaceUpdateInfo {
            total_time: end_time.duration_since(start_time),
            light_update_time: end_light_update.duration_since(start_light_update),
            light_update_count,
            chunk_info: csm_info,
            texture_info,
        })
    }

    /// Draw the space as of the last [`Self::update`].
    ///
    /// Does not access the [`Space`] contents at all.
    // TODO: needs error return or not?
    #[allow(clippy::too_many_arguments)]
    pub fn draw(
        &self,
        fb: &FramebufferTextures,
        queue: &wgpu::Queue,
        encoder: &mut wgpu::CommandEncoder,
        pipelines: &Pipelines,
        camera: &Camera,
        color_load_op: wgpu::LoadOp<wgpu::Color>,
        store_depth: bool,
    ) -> Result<SpaceDrawInfo, GraphicsResourceError> {
        let start_time = Instant::now();

        let csm = &self.csm;
        let view_chunk = csm.view_chunk();

        let mut flaws = Flaws::empty();

        // Accumulates instance data for meshes, which we will then write as part of this
        // submission. (Draw commands always happen after buffer writes even if they
        // enter the command encoder first.)
        let mut instance_data: Vec<WgpuInstanceData> = Vec::with_capacity(
            self.instance_buffer
                .get()
                .map_or(0, |buffer| usize::try_from(buffer.size()).unwrap_or(0)),
        );

        queue.write_buffer(
            &self.camera_buffer.buffer,
            0,
            bytemuck::bytes_of(&ShaderSpaceCamera::new(
                camera,
                self.sky_color,
                self.light_texture.light_lookup_offset(),
            )),
        );

        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some(&self.render_pass_label),
            color_attachments: &[Some(fb.color_attachment_for_scene(color_load_op))],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                view: &fb.depth_texture_view,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    store: store_depth,
                }),
                stencil_ops: None,
            }),
        });
        render_pass.set_bind_group(0, &self.camera_buffer.bind_group, &[]);
        if let Some(space_bind_group) = self.space_bind_group.get() {
            render_pass.set_bind_group(1, space_bind_group, &[]);
        } else {
            // If there's no bind group then update() must not have been called, so there's
            // nothing to draw.
            flaws |= Flaws::UNFINISHED;
        }
        if let Some(buffer) = self.instance_buffer.get() {
            render_pass.set_vertex_buffer(1, buffer.slice(..));
        } else {
            // If there's no buffer then there must also be no instances; no action needed.
        }

        // Helper for the common logic of opaque + transparent drawing
        fn draw_instance<'pass>(
            range: std::ops::Range<usize>,
            render_pass: &mut wgpu::RenderPass<'pass>,
            buffers: &'pass ChunkBuffers,
            instance_data: &mut Vec<WgpuInstanceData>,
            p: ChunkPos<CHUNK_SIZE>,
            squares_drawn: &mut usize,
        ) {
            if !range.is_empty() {
                set_buffers(render_pass, buffers);
                let id = u32::try_from(instance_data.len()).unwrap();
                instance_data.push(WgpuInstanceData::new(p.bounds().lower_bounds().to_vec()));
                render_pass.draw_indexed(to_wgpu_index_range(range.clone()), 0, id..(id + 1));
                *squares_drawn += range.len() / 6;
            }
        }

        // Opaque geometry first, in front-to-back order
        let start_opaque_draw_time = Instant::now();
        let mut chunks_drawn = 0;
        let mut squares_drawn = 0;
        render_pass.set_pipeline(&pipelines.opaque_render_pipeline);
        for chunk in csm.iter_in_view(camera) {
            chunks_drawn += 1;

            if let Some(buffers) = &chunk.render_data {
                draw_instance(
                    chunk.mesh().opaque_range(),
                    &mut render_pass,
                    buffers,
                    &mut instance_data,
                    chunk.position(),
                    &mut squares_drawn,
                );
            } else {
                // TODO: If the chunk is missing, draw a blocking shape, possibly?
            }
            flaws |= chunk.mesh().flaws();
        }

        // Transparent geometry after opaque geometry, in back-to-front order
        let start_draw_transparent_time = Instant::now();
        if camera.options().transparency.will_output_alpha() {
            render_pass.set_pipeline(&pipelines.transparent_render_pipeline);
            for chunk in csm.iter_in_view(camera).rev() {
                if let Some(buffers) = &chunk.render_data {
                    draw_instance(
                        chunk
                            .mesh()
                            .transparent_range(DepthOrdering::from_view_direction(
                                chunk.position().0 - view_chunk.0,
                            )),
                        &mut render_pass,
                        buffers,
                        &mut instance_data,
                        chunk.position(),
                        &mut squares_drawn,
                    );
                }
                flaws |= chunk.mesh().flaws();
            }
        }

        queue.write_buffer(
            self.instance_buffer
                .get()
                .expect("SpaceRenderer::instance_buffer should have been created but wasn't"),
            0,
            bytemuck::cast_slice::<WgpuInstanceData, u8>(instance_data.as_slice()),
        );

        let end_time = Instant::now();

        Ok(SpaceDrawInfo {
            draw_init_time: start_opaque_draw_time.duration_since(start_time),
            draw_opaque_time: start_draw_transparent_time.duration_since(start_opaque_draw_time),
            draw_transparent_time: end_time.duration_since(start_draw_transparent_time),
            squares_drawn,
            chunks_drawn,
            flaws,
        })
    }

    /// Returns the camera, to allow additional drawing in the same coordinate system.
    pub(crate) fn camera_bind_group(&self) -> &wgpu::BindGroup {
        &self.camera_buffer.bind_group
    }

    /// Generate debug lines for the current state of the renderer, assuming
    /// draw() was just called.
    pub(crate) fn debug_lines(&self, camera: &Camera, v: &mut Vec<WgpuLinesVertex>) {
        if camera.options().debug_chunk_boxes {
            self.csm.chunk_debug_lines(
                camera,
                &mut crate::map_line_vertices::<WgpuLinesVertex>(v, palette::DEBUG_CHUNK_MAJOR),
            );

            // Frame the nearest chunk in detail
            let chunk_origin = self
                .csm
                .view_chunk()
                .bounds()
                .lower_bounds()
                .map(FreeCoordinate::from);
            for face in Face6::ALL {
                let ft = face.face_transform(CHUNK_SIZE);
                for i in 1..CHUNK_SIZE {
                    let mut push = |p| {
                        v.push(WgpuLinesVertex::from_position_color(
                            ft.transform_point(p).map(FreeCoordinate::from) + chunk_origin.to_vec(),
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

/// GPU resources for the camera uniform that [`BLOCKS_AND_LINES_SHADER`] expects,
/// matching [`Pipelines::camera_bind_group_layout`].
#[derive(Debug)]
pub(in crate::in_wgpu) struct SpaceCameraBuffer {
    /// Buffer containing a [`ShaderSpaceCamera`].
    ///
    /// Public for use in `shader_tests`.
    pub buffer: wgpu::Buffer,

    /// Bind group binding the buffer.
    ///
    /// Public for use in `shader_tests`.
    pub bind_group: wgpu::BindGroup,
}

impl SpaceCameraBuffer {
    pub fn new(space_label: &str, device: &wgpu::Device, pipelines: &Pipelines) -> Self {
        let buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some(&format!("{space_label} camera_buffer")),
            size: std::mem::size_of::<ShaderSpaceCamera>().try_into().unwrap(),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &pipelines.camera_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: buffer.as_entire_binding(),
            }],
            label: Some(&format!("{space_label} camera_bind_group")),
        });
        Self { buffer, bind_group }
    }
}

/// Create the bind group to be used with [`Pipelines::space_texture_bind_group_layout`].
///
/// Public for use in `shader_tests`.
/// Must be called after `block_texture.flush()`.
pub(in crate::in_wgpu) fn create_space_bind_group(
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
                // TODO: when resizing we will need to track rebuilding this
                resource: wgpu::BindingResource::TextureView(
                    &block_texture.current_texture_view().unwrap(),
                ),
            },
            wgpu::BindGroupEntry {
                binding: 2,
                resource: wgpu::BindingResource::TextureView(&light_texture.texture_view),
            },
        ],
        label: Some(&format!("{space_label} space_bind_group")),
    })
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
        buffers.index_format,
    );
}

/// Copy [`SpaceMesh`] data to GPU buffers.
fn update_chunk_buffers(
    mut bwp: BeltWritingParts<'_, '_>,
    update: RenderDataUpdate<'_, Option<ChunkBuffers>, WgpuBlockVertex, AtlasTile>,
    space_label: &str,
) {
    if update.mesh.is_empty() {
        // No action needed
        return;
    }

    let new_vertices_data: &[u8] =
        bytemuck::cast_slice::<WgpuBlockVertex, u8>(update.mesh.vertices());
    // TODO: assert INDEX_FORMAT matches this type
    let new_indices: IndexSlice<'_> = update.mesh.indices();

    let mesh_label = &update.mesh_label;
    let buffers = update.render_data.get_or_insert_with(ChunkBuffers::default);
    buffers.vertex_buf.write_with_resizing(
        bwp.reborrow(),
        &wgpu::util::BufferInitDescriptor {
            label: Some(&format!("{space_label} vertex {mesh_label:?}")),
            contents: new_vertices_data,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        },
    );
    buffers.index_buf.write_with_resizing(
        bwp.reborrow(),
        &wgpu::util::BufferInitDescriptor {
            label: Some(&format!("{space_label} index {mesh_label:?}")),
            contents: new_indices.as_bytes(),
            usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
        },
    );
    buffers.index_format = to_wgpu_index_format(new_indices);
}

/// [`SpaceRenderer`]'s set of things that need recomputing.
#[derive(Debug, Default)]
struct SpaceRendererTodo {
    /// Blocks whose light texels should be updated.
    /// None means do a full space reupload.
    ///
    /// TODO: experiment with different granularities of light invalidation (chunks, dirty rects, etc.)
    light: Option<HashSet<Cube>>,
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
pub(in crate::in_wgpu) struct SpaceLightTexture {
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
        });
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            size: size_vector_to_extent(texture_bounds.size()),
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D3,
            format: wgpu::TextureFormat::Rgba8Uint,
            view_formats: &[],
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
