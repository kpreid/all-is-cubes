//! Manages meshes for rendering a [`Space`].

use alloc::boxed::Box;
use alloc::format;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;
use core::mem;
use core::sync::atomic;
use core::time::Duration;
use std::sync::{Mutex, PoisonError, mpsc};

use hashbrown::HashSet;
use itertools::Itertools as _;

use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::content::palette;
use all_is_cubes::listen::{self, Listen as _, Listener};
use all_is_cubes::math::{
    Face6, FreeCoordinate, FreePoint, GridAab, GridCoordinate, GridPoint, GridSize, GridVector,
    Rgb, Rgba, Wireframe as _, ZeroOne, rgba_const,
};
use all_is_cubes::raycast::Ray;
#[cfg(feature = "rerun")]
use all_is_cubes::rerun_glue as rg;
use all_is_cubes::space::{Sky, Space, SpaceChange, SpaceFluff};
use all_is_cubes::time;
use all_is_cubes::universe::{Handle, HandleError, ReadTicket};
use all_is_cubes::util::Executor;
use all_is_cubes_mesh::dynamic::{self, ChunkedSpaceMesh, RenderDataUpdate};
use all_is_cubes_mesh::{DepthOrdering, IndexSlice};
use all_is_cubes_render::camera::Camera;
use all_is_cubes_render::{Flaws, RenderError};

use crate::in_wgpu::block_texture::BlockTextureViews;
use crate::in_wgpu::glue::{MapVec, buffer_size_of, to_wgpu_index_format};
use crate::in_wgpu::light_texture::LightChunk;
use crate::in_wgpu::pipelines::Pipelines;
use crate::in_wgpu::skybox;
use crate::in_wgpu::vertex::{WgpuInstanceData, WgpuLinesVertex};
use crate::in_wgpu::{LightTexture, WgpuMt};
use crate::in_wgpu::{
    block_texture::AtlasAllocator,
    camera::ShaderSpaceCamera,
    glue::{BeltWritingParts, ResizingBuffer, to_wgpu_index_range},
    vertex::WgpuBlockVertex,
};
use crate::{DebugLineVertex, Memo, Msw, SpaceDrawInfo, SpaceUpdateInfo};

// temporarily public for a lighting kludge
pub(super) const CHUNK_SIZE: GridCoordinate = 16;

const NO_WORLD_SKY: Sky = Sky::Uniform(palette::NO_WORLD_TO_SHOW.to_rgb());

/// Manages cached data and GPU resources for drawing a single [`Space`] and
/// following its changes.
///
/// Can be given a new [`Space`] or have none.
#[derive(Debug)]
pub(crate) struct SpaceRenderer {
    space_label: String,
    instance_buffer_label: String,

    /// Tracks information we need to update from the `Space`.
    /// Note that `self.csm` has its own todo listener too.
    todo: listen::StoreLock<SpaceRendererTodo>,

    /// Skybox texture.
    skybox: skybox::Skybox,

    block_texture: AtlasAllocator,
    light_texture: LightTexture,

    /// Buffer containing the [`ShaderSpaceCamera`] configured for this Space.
    camera_buffer: SpaceCameraBuffer,

    /// Buffer for instance data.
    /// Rewritten every frame, but reused to save reallocation.
    instance_buffer: ResizingBuffer,

    /// Temporary storage for organizing block instances to be drawn.
    ///
    /// Rewritten every frame, but reused to save reallocation; stored in a mutex to
    /// enable `&mut`-free usage (if the `Option` is empty we just allocate a new one).
    /// When a collector is present, it is always `clear()`ed.
    instance_collector: Mutex<Option<dynamic::InstanceCollector>>,

    /// Bind group containing our block texture and light texture,
    space_bind_group: Memo<[crate::Id<wgpu::TextureView>; 4], wgpu::BindGroup>,

    /// Mesh generator and updater.
    ///
    /// If [`None`], then we currently have no [`Space`].
    csm: Option<ChunkedSpaceMesh<WgpuMt, CHUNK_SIZE>>,

    /// The `interactive` parameter passed to `ChunkedSpaceMesh` construction.
    interactive: bool,

    /// Active [`Space::fluff()`], and in the future other particles, that we're drawing.
    /// Currently, it's all made of lines and thus invoked from the parent `EverythingRenderer`,
    /// but we do the tracking as time passes.
    particle_sets: Vec<ParticleSet>,

    /// Receiver whose sender end is held by a space listener, which feeds new particles into
    /// [`Self::particle_sets`].
    particle_rx: mpsc::Receiver<ParticleSet>,

    #[cfg(feature = "rerun")]
    rerun_destination: all_is_cubes::rerun_glue::Destination,
}

#[derive(Debug, Default)]
pub(super) struct ChunkBuffers {
    vertex_buf: ResizingBuffer,
    index_buf: ResizingBuffer,
    index_format: wgpu::IndexFormat,
}

impl SpaceRenderer {
    /// Constructs a new [`SpaceRenderer`] with no space to render yet.
    pub fn new(
        space_label: String,
        device: &wgpu::Device,
        pipelines: &Pipelines,
        block_texture: AtlasAllocator,
        interactive: bool,
    ) -> Self {
        let light_texture = LightTexture::new(
            &space_label,
            device,
            GridSize::splat(1),
            wgpu::TextureUsages::empty(),
        ); // dummy

        let camera_buffer = SpaceCameraBuffer::new(&space_label, device, pipelines);

        let todo = listen::StoreLock::new(SpaceRendererTodo::EVERYTHING);

        SpaceRenderer {
            todo,
            instance_buffer_label: format!("{space_label} instances"),
            skybox: skybox::Skybox::new(device, &space_label),
            block_texture,
            light_texture,
            space_bind_group: Memo::new(),
            camera_buffer,
            instance_buffer: ResizingBuffer::default(),
            instance_collector: Mutex::new(None),
            csm: None,
            interactive,
            space_label,
            particle_sets: Vec::new(),
            particle_rx: {
                let (_, rx) = mpsc::sync_channel(0);
                rx
            },
            #[cfg(feature = "rerun")]
            rerun_destination: Default::default(),
        }
    }

    pub(crate) fn space(&mut self) -> Option<&Handle<Space>> {
        self.csm.as_ref().map(|csm| csm.space())
    }

    /// Replace the space being rendered, while preserving some of the resources used to render it.
    ///
    /// This is not a minimum-effort operation and should be thought of as an optimized
    /// variant of building a new [`SpaceRenderer`] from scratch. However, it does check if the
    /// given space is equal to the current space before doing anything.
    ///
    /// Returns an error if reading the space fails. In that case, the state will be as if
    /// `set_space(..., None)` was called.
    pub(crate) fn set_space(
        &mut self,
        executor: &Arc<dyn Executor>,
        read_ticket: ReadTicket<'_>,
        space: Option<&Handle<Space>>,
    ) -> Result<(), HandleError> {
        if self.csm.as_ref().map(|csm| csm.space()) == space {
            // No change.
            return Ok(());
        }

        let Some(space) = space else {
            self.clear_space();
            return Ok(());
        };

        let space_borrowed = space.read(read_ticket)?;

        // Destructuring to explicitly skip or handle each field.
        let SpaceRenderer {
            space_label: _,
            instance_buffer_label: _,
            todo,
            skybox: _, // will be updated due to todo.sky = true
            block_texture: _,
            light_texture,
            camera_buffer: _,
            instance_buffer: _,
            instance_collector: _,
            space_bind_group: _, // will be updated later
            csm,
            interactive,
            particle_sets,
            particle_rx,
            #[cfg(feature = "rerun")]
                rerun_destination: _,
        } = self;

        *todo = {
            let new_todo = listen::StoreLock::new(SpaceRendererTodo::EVERYTHING);
            space_borrowed.listen(new_todo.listener());
            new_todo
        };

        #[allow(unused_mut)]
        let mut new_csm =
            ChunkedSpaceMesh::new(space.clone(), self.block_texture.clone(), *interactive);
        // TODO: rescue ChunkChart and maybe block meshes from the old `csm`.
        #[cfg(feature = "rerun")]
        {
            new_csm.log_to_rerun(self.rerun_destination.clone());
        }

        {
            let (particle_tx, new_particle_rx) = mpsc::sync_channel(400);
            space_borrowed
                .fluff()
                .listen(FluffListener::new(particle_tx));
            particle_sets.clear();
            *particle_rx = new_particle_rx;
        }

        // Spawn background mesh jobs
        executor.spawn_background(&mut || {
            let task = {
                let job_queue = new_csm.job_queue().clone();
                let executor = executor.clone();
                async move {
                    while let Some(job) = job_queue.next().await {
                        job.await;
                        executor.yield_now().await;
                    }
                }
            };

            // On wasm, wgpu is not Send, but for the same reason, we never use any other threads,
            // so a SendWrapper will make things work out.
            #[cfg(target_family = "wasm")]
            let task = send_wrapper::SendWrapper::new(task);

            Box::pin(task)
        });

        *csm = Some(new_csm);

        light_texture.forget_mapped();

        Ok(())
    }

    // Helper for set_space(), implementing the `space == None`` case.
    fn clear_space(&mut self) {
        let SpaceRenderer {
            space_label: _,
            instance_buffer_label: _,
            todo,
            skybox: _,
            block_texture: _,
            light_texture: _,
            camera_buffer: _,
            instance_buffer: _,
            instance_collector: _,
            space_bind_group: _,
            csm,
            interactive: _,
            particle_sets,
            particle_rx,
            #[cfg(feature = "rerun")]
                rerun_destination: _,
        } = self;

        // detach from space notifier, and also request rebuilding the skybox
        *todo = listen::StoreLock::new(SpaceRendererTodo::EVERYTHING);
        particle_sets.clear();
        (_, *particle_rx) = mpsc::sync_channel(0);
        *csm = None;
    }

    /// Update renderer internal state from the given [`Camera`] and referenced [`Space`],
    /// so that the next rendered meshes will be up to date (or as far up to date as the
    /// given [`deadline`] permits).
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn update(
        &mut self,
        deadline: time::Deadline,
        read_ticket: ReadTicket<'_>,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        pipelines: &Pipelines,
        camera: &Camera,
        mut bwp: BeltWritingParts<'_>,
    ) -> Result<SpaceUpdateInfo, RenderError> {
        let start_time = time::Instant::now();

        let todo = &mut self.todo.lock();

        let Some(csm) = &mut self.csm else {
            if mem::take(&mut todo.sky) {
                self.skybox.compute(device, queue, &NO_WORLD_SKY);
            }

            return Ok(SpaceUpdateInfo::default());
        };
        let space = &*csm.space().read(read_ticket).map_err(RenderError::Read)?;

        if mem::take(&mut todo.sky) {
            self.skybox.compute(device, queue, &space.physics().sky);
        }

        // Update light texture.
        let start_light_update = time::Instant::now();
        let mut light_update_count = 0;
        {
            // Check the size.
            let needed_size =
                LightTexture::choose_size(&device.limits(), space.bounds(), camera.view_distance());
            self.light_texture
                .ensure_as_big_as(&self.space_label, device, needed_size);

            // Handle individual changed cubes, or the space changing.
            if let Some(set) = &mut todo.light {
                // Update individual cubes.
                light_update_count +=
                    self.light_texture
                        .update_scatter(device, queue, space, set.drain());
            } else {
                self.light_texture.forget_mapped();
                todo.light = Some(HashSet::new());
            }

            // Ensure the texture covers the right region for the camera.
            light_update_count += self
                .light_texture
                .ensure_visible_is_mapped(queue, space, camera);
        }
        let end_light_update = time::Instant::now();

        // Update chunks.
        let csm_info = {
            let bwp_mutex = Mutex::new(Msw::new(bwp.reborrow()));
            csm.update(
                read_ticket,
                camera,
                deadline, // TODO: decrease deadline by some guess at texture writing time
                |u| {
                    #[expect(clippy::shadow_unrelated)]
                    let bwp = &mut *bwp_mutex.lock().unwrap();
                    if u.indices_only {
                        if let Some(index_buf) =
                            u.render_data.as_ref().and_then(|b| b.index_buf.get())
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
                                bwp.reborrow()
                                    .write_buffer(index_buf, 0, len)
                                    .copy_from_slice(index_buf_bytes);
                            }
                        }
                    } else {
                        update_chunk_buffers(bwp.reborrow(), u, &self.space_label);
                    }
                },
            )
        };

        // Update particle state.
        self.particle_sets.retain_mut(|pset| {
            // TODO: We need information about rate of time passing, to decide what the age
            // should be incremented by, but don't have it
            pset.age += 1;
            pset.age <= 10
        });
        while let Ok(pset) = self.particle_rx.try_recv() {
            self.particle_sets.push(pset);
        }

        // Ensure instance buffer is big enough.
        // This is an overallocation because it doesn't account for culling or empty chunks,
        // but it shouldn't be too much.
        let total_instance_count =
            csm.chunk_chart().count_all() + csm.count_block_instances(camera);
        self.instance_buffer.resize_at_least(
            bwp.device,
            &wgpu::BufferDescriptor {
                label: Some(&self.instance_buffer_label),
                size: u64::try_from(total_instance_count * size_of::<WgpuInstanceData>())
                    .expect("instance buffer size overflow"),
                usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::VERTEX,
                mapped_at_creation: false,
            },
        );

        // Flush all texture updates to GPU.
        // This must happen after `csm.update()` so that the newly
        // generated meshes have the texels they expect.
        let (block_texture_views, texture_info) = self.block_texture.flush(device, queue);

        // Update space bind group if needed.
        self.space_bind_group.get_or_insert(
            [
                // This needs one id from each block texture group
                block_texture_views.g0_reflectance.global_id(),
                block_texture_views.g1_reflectance.global_id(),
                self.light_texture.texture_view().global_id(),
                self.skybox.texture_view().global_id(),
            ],
            || {
                create_space_bind_group(
                    &self.space_label,
                    device,
                    pipelines,
                    &block_texture_views,
                    &self.light_texture,
                    self.skybox.texture_view(),
                )
            },
        );

        let end_time = time::Instant::now();

        let info = SpaceUpdateInfo {
            total_time: end_time.saturating_duration_since(start_time),
            light_update_time: end_light_update.saturating_duration_since(start_light_update),
            light_update_count,
            chunk_info: csm_info,
            texture_info,
        };

        #[cfg(feature = "rerun")]
        if self.rerun_destination.is_enabled() {
            info.write_to_rerun(&self.rerun_destination);
        }
        Ok(info)
    }

    /// Draw the space as of the last [`Self::update`].
    ///
    /// Does not access the [`Space`] contents at all.
    pub fn draw<'rpass>(
        &'rpass self,
        mut bwp: BeltWritingParts<'_>,
        render_pass: &mut wgpu::RenderPass<'rpass>,
        pipelines: &'rpass Pipelines,
        camera: &Camera,
        draw_sky: bool, // TODO: consider specifying this at update time to decide whether to calc
    ) -> SpaceDrawInfo {
        let start_time = time::Instant::now();
        let mut flaws = Flaws::empty();

        // Check if we actually have a space to render.
        let Some(csm) = &self.csm else {
            // If we have no space to render, then we only render the clear + skybox.

            return SpaceDrawInfo {
                draw_init_time: Duration::ZERO,
                draw_opaque_chunks_time: Duration::ZERO,
                draw_opaque_blocks_time: Duration::ZERO,
                draw_transparent_time: Duration::ZERO,
                finalize_time: Duration::ZERO,
                squares_drawn: 0,
                chunk_meshes_drawn: 0,
                chunks_with_instances_drawn: 0,
                blocks_drawn: 0,
                flaws,
            };
        };

        let view_chunk = csm.view_chunk();

        self.write_camera_only(bwp.reborrow(), camera);

        // If we have a instance buffer (which will have been previously allocated with sufficient
        // size), prepare to write to it via the staging belt.
        //
        // Note: If we find more uses for the belt during drawing, we will need to not have this
        // long-lived mapping, use multiple belts, or use a manually managed staging buffer for
        // instances (which might be a good idea itself).
        let mut instance_buffer_view: Option<wgpu::BufferViewMut<'_>> =
            self.instance_buffer.map_without_resizing(bwp);
        let mut instance_buffer_writer: MapVec<'_, WgpuInstanceData> = match instance_buffer_view {
            Some(ref mut view) => MapVec::new(view),
            None => MapVec::default(),
        };

        render_pass.set_bind_group(0, &self.camera_buffer.bind_group, &[]);
        render_pass.set_bind_group(2, &pipelines.blocks_static_bind_group, &[]);
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

        // Helper for the common logic of opaque + transparent drawing of a single instance
        // that's a chunk mesh (i.e. instance range is length 1).
        #[allow(clippy::too_many_arguments)]
        fn draw_chunk_instance<'pass>(
            range: std::ops::Range<usize>,
            render_pass: &mut wgpu::RenderPass<'pass>,
            buffers: &'pass ChunkBuffers,
            instance_buffer_writer: &mut MapVec<'_, WgpuInstanceData>,
            chunk_pos: ChunkPos<CHUNK_SIZE>,
            squares_drawn: &mut usize,
            flaws: &mut Flaws,
            in_world_debug_label: &'static str,
        ) {
            if !range.is_empty() {
                set_buffers(render_pass, buffers);
                let id = u32::try_from(instance_buffer_writer.len()).unwrap();

                let ok = instance_buffer_writer.push(&WgpuInstanceData::new(
                    chunk_pos.bounds().lower_bounds().to_vector(),
                    &format_args!("Ch{in_world_debug_label}"),
                ));
                if !ok {
                    if !flaws.contains(Flaws::UNFINISHED) {
                        *flaws |= Flaws::UNFINISHED;
                        log::warn!("instance buffer too small for {} instances", id + 1);
                    }
                    return;
                }

                render_pass.draw_indexed(to_wgpu_index_range(range.clone()), 0, id..(id + 1));
                *squares_drawn += range.len() / 6;
            }
        }

        if draw_sky && !camera.options().debug_pixel_cost {
            // Render skybox.
            // TODO: Ideally, we would do this after drawing other opaque geometry, but that requires
            // smarter depth test setup.
            render_pass.set_pipeline(&pipelines.skybox_render_pipeline);
            // No vertex buffer; shader generates a fullscreen triangle.
            render_pass.draw(0..3, 0..1);
        }

        if camera.options().debug_pixel_cost {
            // Draw everything, transparent or opaque, an extra time with no depth testing.
            // This visualizes the number of fragments which had to be checked against the
            // depth buffer, regardless of whether they passed;
            // it is the cost of rasterization as opposed to the cost of the depth buffer.
            //
            // Because this is a debug visualization, we skip updating any of the info numbers.
            //
            // This section of the code omits block instances; we'll do that inside the opaque
            // block instance section, instead.

            render_pass.set_pipeline(&pipelines.depthless_overdraw_render_pipeline);
            for dynamic::InViewChunkRef {
                chunk,
                mesh_in_view,
                ..
            } in csm.iter_in_view(camera)
            {
                if mesh_in_view && let Some(buffers) = &chunk.render_data {
                    draw_chunk_instance(
                        chunk.mesh().opaque_range(),
                        render_pass,
                        buffers,
                        &mut instance_buffer_writer,
                        chunk.position(),
                        &mut 0,
                        &mut flaws,
                        "",
                    );
                    draw_chunk_instance(
                        chunk.mesh().transparent_range(DepthOrdering::Any),
                        render_pass,
                        buffers,
                        &mut instance_buffer_writer,
                        chunk.position(),
                        &mut 0,
                        &mut flaws,
                        "",
                    );
                }
            }
        }

        // Opaque geometry before other geometry, in front-to-back order.
        // Also collect instances.
        let pipeline_for_opaque = if camera.options().debug_pixel_cost {
            &pipelines.opaque_overdraw_render_pipeline
        } else {
            &pipelines.opaque_render_pipeline
        };
        let start_opaque_chunk_draw_time = time::Instant::now();
        let mut chunk_meshes_drawn = 0;
        let mut chunks_with_instances_drawn = 0;
        let mut blocks_drawn = 0;
        let mut squares_drawn = 0;
        let mut block_instances: dynamic::InstanceCollector = self
            .instance_collector
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .take()
            .unwrap_or_default();
        render_pass.set_pipeline(pipeline_for_opaque);
        for dynamic::InViewChunkRef {
            chunk,
            mesh_in_view,
            instances_in_view,
            ..
        } in csm.iter_in_view(camera)
        {
            if mesh_in_view {
                chunk_meshes_drawn += 1;
                if let Some(buffers) = &chunk.render_data {
                    draw_chunk_instance(
                        chunk.mesh().opaque_range(),
                        render_pass,
                        buffers,
                        &mut instance_buffer_writer,
                        chunk.position(),
                        &mut squares_drawn,
                        &mut flaws,
                        "O",
                    );
                } else {
                    // TODO: If the chunk is missing, draw a blocking shape, possibly?
                }
                flaws |= chunk.mesh().flaws();
            }
            if instances_in_view {
                chunks_with_instances_drawn += 1;
                block_instances.extend(chunk.block_instances());
            }
        }

        // Render opaque block instances, which we just gathered from the chunks.
        // (Currently, we don't ever try to instance transparent meshes, to avoid sorting issues.)
        let start_opaque_instance_draw_time = time::Instant::now();
        for (block_index, cubes) in block_instances.iter() {
            // Set buffers for the mesh
            let Some(dynamic::InstanceMesh {
                meta,
                render_data: Some(buffers),
                ..
            }) = csm.block_instance_mesh(block_index)
            else {
                // TODO: this is an error that should be reported
                continue;
            };
            set_buffers(render_pass, buffers);

            let first_instance_index = u32::try_from(instance_buffer_writer.len()).unwrap();
            let mut count: u32 = 0;
            for cube in cubes {
                let ok = instance_buffer_writer.push(&WgpuInstanceData::new(
                    cube.lower_bounds().to_vector(),
                    &format_args!("IO{block_index}"),
                ));
                if ok {
                    count += 1;
                } else {
                    if !flaws.contains(Flaws::UNFINISHED) {
                        flaws |= Flaws::UNFINISHED;
                        log::warn!("instance buffer too small");
                    }
                    break;
                }
            }
            // Record draw command for all instances using this mesh
            let instance_range = first_instance_index..(first_instance_index + count);
            blocks_drawn += instance_range.len();
            squares_drawn += (meta.opaque_range().len() / 6) * instance_range.len();
            render_pass.draw_indexed(to_wgpu_index_range(meta.opaque_range()), 0, instance_range);

            // If we are doing overdraw visualization, run the depthless overdraw visualization.
            // We do this here so that we can take advantage of the state in this loop, even though
            // it forces a lot of pipeline switches, because `debug_pixel_cost` isn't about
            // *measuring performance* but about *cost*.
            if camera.options().debug_pixel_cost {
                render_pass.set_pipeline(&pipelines.depthless_overdraw_render_pipeline);
                render_pass.draw_indexed(
                    to_wgpu_index_range(meta.opaque_range()),
                    0,
                    first_instance_index..(first_instance_index + count),
                );
                // Restore previous pipeline
                render_pass.set_pipeline(pipeline_for_opaque);
            }
        }

        block_instances.clear();
        *self
            .instance_collector
            .lock()
            .unwrap_or_else(PoisonError::into_inner) = Some(block_instances);

        // Transparent geometry after opaque geometry, in back-to-front order
        let start_draw_transparent_time = time::Instant::now();
        if camera.options().transparency.will_output_alpha() {
            render_pass.set_pipeline(if camera.options().debug_pixel_cost {
                &pipelines.transparent_overdraw_render_pipeline
            } else {
                &pipelines.transparent_render_pipeline
            });
            for dynamic::InViewChunkRef {
                chunk,
                mesh_in_view,
                instances_in_view,
                ..
            } in csm.iter_in_view(camera).rev()
            {
                if mesh_in_view {
                    if let Some(buffers) = &chunk.render_data {
                        draw_chunk_instance(
                            chunk.mesh().transparent_range(depth_ordering_for_viewing(
                                chunk.position(),
                                view_chunk,
                            )),
                            render_pass,
                            buffers,
                            &mut instance_buffer_writer,
                            chunk.position(),
                            &mut squares_drawn,
                            &mut flaws,
                            "T",
                        );
                    }
                    flaws |= chunk.mesh().flaws();
                }
                // TODO: transparent instances support
                _ = instances_in_view;
            }
        }

        let end_time = time::Instant::now();

        SpaceDrawInfo {
            draw_init_time: start_opaque_chunk_draw_time.saturating_duration_since(start_time),
            draw_opaque_chunks_time: start_opaque_instance_draw_time
                .saturating_duration_since(start_opaque_chunk_draw_time),
            draw_opaque_blocks_time: start_draw_transparent_time
                .saturating_duration_since(start_opaque_instance_draw_time),
            draw_transparent_time: end_time.saturating_duration_since(start_draw_transparent_time),
            finalize_time: Duration::ZERO, // would be after draw_transparent_time if there was anything
            squares_drawn,
            chunk_meshes_drawn,
            chunks_with_instances_drawn,
            blocks_drawn,
            flaws,
        }
    }

    pub fn particle_lines(&self) -> impl Iterator<Item = WgpuLinesVertex> + '_ {
        self.particle_sets.iter().flat_map(|p| p.lines())
    }

    /// Updates the camera buffer in the same way [`Self::draw()`] does,
    /// so that [`Self::camera_bind_group()`] will be fresh even if `draw()` wasn’t called.
    pub fn write_camera_only(&self, bwp: BeltWritingParts<'_>, camera: &Camera) {
        bwp.write_buffer(
            &self.camera_buffer.buffer,
            0,
            const { buffer_size_of::<ShaderSpaceCamera>() },
        )
        .copy_from_slice(bytemuck::bytes_of(&ShaderSpaceCamera::new(camera)));
    }

    /// Returns the camera, to allow additional drawing in the same coordinate system.
    pub(crate) fn camera_bind_group(&self) -> &wgpu::BindGroup {
        &self.camera_buffer.bind_group
    }

    /// Generate debug lines for the current state of the renderer, assuming
    /// `draw()` was just called.
    pub(crate) fn debug_lines(&self, camera: &Camera, v: &mut Vec<WgpuLinesVertex>) {
        let Some(csm) = &self.csm else {
            return;
        };

        if camera.options().debug_reduce_view_frustum {
            // Draw the modified view frustum, which becomes a viewport box in screen space.
            // (Note the lines are at risk of being clipped, but in practice are sufficiently
            // visible but flickery.)
            camera
                .view_frustum_geometry()
                .wireframe_points(&mut crate::map_line_vertices::<WgpuLinesVertex>(
                    v,
                    Rgba::WHITE,
                ));
        }

        if camera.options().debug_chunk_boxes {
            let view_chunk = csm.view_chunk();

            csm.chunk_debug_lines(
                camera,
                &mut crate::map_line_vertices::<WgpuLinesVertex>(v, palette::DEBUG_CHUNK_MAJOR),
            );

            // Frame the nearest chunk in detail
            let chunk_origin = view_chunk.bounds().lower_bounds().map(FreeCoordinate::from);
            for face in Face6::ALL {
                let ft = face.face_transform(CHUNK_SIZE);
                for i in 1..CHUNK_SIZE {
                    let mut push = |p| {
                        v.push(WgpuLinesVertex::from_position_color(
                            ft.transform_point(p).map(FreeCoordinate::from)
                                + chunk_origin.to_vector(),
                            palette::DEBUG_CHUNK_MINOR,
                        ));
                    };
                    push(GridPoint::new(i, 0, 0));
                    push(GridPoint::new(i, CHUNK_SIZE, 0));
                    push(GridPoint::new(0, i, 0));
                    push(GridPoint::new(CHUNK_SIZE, i, 0));
                }
            }

            // Depth sorting order debug.
            // TODO: Make this more legible and shown in more distant chunks.
            if false {
                for near_chunk_pos in
                    GridAab::from_lower_upper([-1, -1, -1], [2, 2, 2]).interior_iter()
                {
                    let near_chunk_pos: ChunkPos<CHUNK_SIZE> =
                        ChunkPos(view_chunk.0 + near_chunk_pos.lower_bounds().to_vector());

                    let ordering = depth_ordering_for_viewing(near_chunk_pos, view_chunk);

                    let glyph: Vec<FreePoint> = match ordering {
                        DepthOrdering::Any => vec![
                            FreePoint::new(0.0, 0.0, 0.0),
                            FreePoint::new(0.5, 1.0, 0.5),
                            FreePoint::new(1.0, 0.0, 1.0),
                        ],
                        DepthOrdering::Within => vec![
                            FreePoint::new(0.0, 1.0, 0.0),
                            FreePoint::new(0.5, 0.0, 0.5),
                            FreePoint::new(1.0, 1.0, 1.0),
                        ],
                        DepthOrdering::Direction(d) => {
                            Ray {
                                origin: near_chunk_pos.bounds().center(),
                                direction: d.transform_vector(GridVector::new(-1, -2, -4)).to_f64()
                                    * 2.0,
                            }
                            .wireframe_points(
                                &mut crate::map_line_vertices::<WgpuLinesVertex>(
                                    v,
                                    rgba_const!(1.0, 0.0, 1.0, 1.0),
                                ),
                            );

                            continue;
                        }
                    };

                    let lb = near_chunk_pos.bounds().lower_bounds().to_f64();

                    let lines =
                        glyph
                            .iter()
                            .tuple_windows()
                            .flat_map(|(a, b)| [a, b])
                            .map(|&point| {
                                WgpuLinesVertex::from_position_color(
                                    lb + point.to_vector() * f64::from(CHUNK_SIZE),
                                    rgba_const!(1.0, 0.0, 1.0, 1.0),
                                )
                            });

                    v.extend(lines);
                }
            }
        }
    }

    /// Activate logging performance information to a Rerun stream.
    #[cfg(feature = "rerun")]
    pub fn log_to_rerun(&mut self, destination: rg::Destination) {
        if let Some(csm) = &mut self.csm {
            csm.log_to_rerun(destination.clone());
        }
        self.rerun_destination = destination;
    }

    /// This is separate so we can run it once for the shared allocator.
    #[cfg(feature = "rerun")]
    pub(crate) fn texture_allocator_log_to_rerun(&self, destination: rg::Destination) {
        self.block_texture.log_to_rerun(destination);
    }
}

fn depth_ordering_for_viewing(
    rendering_chunk: ChunkPos<CHUNK_SIZE>,
    view_chunk: ChunkPos<CHUNK_SIZE>,
) -> DepthOrdering {
    DepthOrdering::from_view_direction(rendering_chunk.0 - view_chunk.0)
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
            size: size_of::<ShaderSpaceCamera>().try_into().unwrap(),
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
    block_textures: &BlockTextureViews,
    light_texture: &LightTexture,
    skybox_texture: &wgpu::TextureView,
) -> wgpu::BindGroup {
    device.create_bind_group(&wgpu::BindGroupDescriptor {
        layout: &pipelines.space_texture_bind_group_layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(light_texture.texture_view()),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::TextureView(&block_textures.g0_reflectance),
            },
            wgpu::BindGroupEntry {
                binding: 2,
                resource: wgpu::BindingResource::TextureView(&block_textures.g1_reflectance),
            },
            wgpu::BindGroupEntry {
                binding: 3,
                resource: wgpu::BindingResource::TextureView(&block_textures.g1_emission),
            },
            wgpu::BindGroupEntry {
                binding: 4,
                resource: wgpu::BindingResource::TextureView(skybox_texture),
            },
            wgpu::BindGroupEntry {
                binding: 5,
                resource: wgpu::BindingResource::Sampler(&pipelines.skybox_sampler),
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
#[expect(
    clippy::needless_pass_by_value,
    reason = "https://github.com/rust-lang/rust-clippy/issues/12525"
)]
fn update_chunk_buffers(
    mut bwp: BeltWritingParts<'_>,
    update: RenderDataUpdate<'_, WgpuMt>,
    space_label: &str,
) {
    if update.mesh.is_empty() {
        // No action needed
        return;
    }

    let new_vertices_data: &[u8] =
        bytemuck::must_cast_slice::<WgpuBlockVertex, u8>(update.mesh.vertices());
    let new_indices: IndexSlice<'_> = update.mesh.indices();

    let mesh_id = &update.mesh_id;
    let buffers = update
        .render_data
        .get_or_insert_with(|| Msw::new(ChunkBuffers::default()));
    buffers.vertex_buf.write_with_resizing(
        bwp.reborrow(),
        &|| format!("{space_label} vertex {mesh_id:?}"),
        wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        [new_vertices_data],
    );
    buffers.index_buf.write_with_resizing(
        bwp.reborrow(),
        &|| format!("{space_label} index {mesh_id:?}"),
        wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
        [new_indices.as_bytes()],
    );
    buffers.index_format = to_wgpu_index_format(new_indices);
}

/// One or more particles derived from [`Fluff`] or similar.
#[derive(Debug)]
struct ParticleSet {
    fluff: SpaceFluff,
    age: u64,
}
impl ParticleSet {
    fn from_fluff(fluff: &SpaceFluff) -> Option<ParticleSet> {
        use all_is_cubes::fluff::Fluff;
        // Filter whether we want particles for this at all.
        // TODO: The initial age should be determined by time in the Space, so that we don't get
        // lingeringly visible hiccups any time a frame is skipped.
        match fluff.fluff {
            Fluff::BlockFault(_) => Some(Self {
                fluff: fluff.clone(),
                age: 0,
            }),
            Fluff::Beep
            | Fluff::Happened
            | Fluff::PlaceBlockGeneric
            | Fluff::BlockImpact { .. } => None,
            _ => todo!(),
        }
    }

    fn lines(&self) -> impl Iterator<Item = WgpuLinesVertex> + use<> {
        // TODO: this simple wireframe cube is a placeholder for more general mechanisms.
        // (But probably we also want to stop using lines, at some point, and use
        // specially-created block meshes instead.)
        let mut tmp: Vec<WgpuLinesVertex> = Vec::with_capacity(24); // TODO: inefficient allocation per object
        crate::wireframe_vertices::<WgpuLinesVertex, _, _>(
            &mut tmp,
            Rgb::ONE.with_alpha(
                ZeroOne::<f32>::try_from(0.9f32.powf(self.age as f32)).unwrap_or(ZeroOne::ZERO),
            ),
            &self.fluff.position.aab().expand(0.004 * (self.age as f64)),
        );
        tmp.into_iter()
    }
}

/// [`SpaceRenderer`]'s set of things that need recomputing.
#[derive(Debug)]
struct SpaceRendererTodo {
    /// Blocks whose light texels should be updated.
    /// None means do a full space reupload.
    ///
    /// TODO: experiment with different granularities of light invalidation (chunks, dirty rects, etc.)
    light: Option<HashSet<LightChunk>>,

    sky: bool,
}

impl SpaceRendererTodo {
    /// Initial value to use when we're initializing or re-initializing, which indicates to
    /// reinitialize/reupload everything.
    pub const EVERYTHING: Self = Self {
        light: None,
        sky: true,
    };
}

impl listen::Store<SpaceChange> for SpaceRendererTodo {
    fn receive(&mut self, messages: &[SpaceChange]) {
        for message in messages {
            match *message {
                SpaceChange::EveryBlock => {
                    self.light = None;
                }
                SpaceChange::CubeLight { cube } => {
                    // None means we're already at "update everything"
                    if let Some(set) = &mut self.light {
                        set.insert(LightChunk::new(cube));
                    }
                }
                SpaceChange::CubeBlock { .. } => {}
                SpaceChange::BlockIndex(..) => {}
                SpaceChange::BlockEvaluation(..) => {}
                SpaceChange::Physics => {
                    self.sky = true;
                }
            }
        }
    }
}

#[derive(Debug)]
struct FluffListener {
    particle_sender: mpsc::SyncSender<ParticleSet>,
    // We need this to be able to return false even when given an empty slice
    // of messages. TODO: Consider weakening receive()'s requirements to avoid that,
    // or switch to a different channel implementation which lets us query liveness
    // without sending anything.
    alive: atomic::AtomicBool,
}

impl FluffListener {
    fn new(sender: mpsc::SyncSender<ParticleSet>) -> Self {
        Self {
            particle_sender: sender,
            alive: atomic::AtomicBool::new(true),
        }
    }
}

impl Listener<SpaceFluff> for FluffListener {
    fn receive(&self, fluffs: &[SpaceFluff]) -> bool {
        if !self.alive.load(atomic::Ordering::Relaxed) {
            return false;
        }
        for fluff in fluffs {
            let Some(message) = ParticleSet::from_fluff(fluff) else {
                continue;
            };

            match self.particle_sender.try_send(message) {
                Ok(()) => {}
                Err(mpsc::TrySendError::Disconnected(_)) => {
                    self.alive.store(false, atomic::Ordering::Relaxed);
                    return false;
                }
                Err(mpsc::TrySendError::Full(_)) => {
                    // discard if we're not keeping upo
                }
            }
        }
        true
    }
}
