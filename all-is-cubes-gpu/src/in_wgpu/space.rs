// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Manages meshes for rendering a [`Space`].

use std::borrow::Cow;
use std::cell::RefCell;

use instant::Instant;
use once_cell::sync::Lazy;

use all_is_cubes::camera::Camera;
use all_is_cubes::chunking::ChunkPos;
use all_is_cubes::listen::DirtyFlag;
use all_is_cubes::math::{GridCoordinate, Rgb};
use all_is_cubes::mesh::chunked_mesh::ChunkedSpaceMesh;
use all_is_cubes::mesh::{DepthOrdering, SpaceMesh};
use all_is_cubes::space::Space;
use all_is_cubes::universe::URef;

use crate::in_wgpu::{
    block_texture::{AtlasAllocator, AtlasTile},
    camera::WgpuCamera,
    glue::{to_wgpu_color, to_wgpu_index_range, BeltWritingParts, ResizingBuffer},
    vertex::WgpuBlockVertex,
};
use crate::reloadable::{reloadable_str, Reloadable};
use crate::{GraphicsResourceError, SpaceRenderInfo};

const CHUNK_SIZE: GridCoordinate = 16;

/// Manages cached data and GPU resources for drawing a single [`Space`] and
/// following its changes.
#[derive(Debug)]
pub(crate) struct SpaceRenderer {
    /// A debugging label for the space's render pass.
    /// (Derived from space_label)
    render_pass_label: String,

    /// Note that `self.csm` has its own todo listener.
    /// todo: Arc<Mutex<SpaceRendererTodo>>,
    block_texture: AtlasAllocator,
    //TODO: light_texture: Option<SpaceLightTexture>,
    /// Buffer containing the [`WgpuCamera`] configured for this Space.
    camera_buffer: wgpu::Buffer,
    /// Bind group for camera_buffer.
    camera_bind_group: wgpu::BindGroup,

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
    pub fn new(
        space: URef<Space>,
        space_label: String,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        stuff: &BlockRenderStuff,
    ) -> Result<Self, GraphicsResourceError> {
        let block_texture = AtlasAllocator::new(device, queue)?;

        let space_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &stuff.space_texture_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&block_texture.texture_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&block_texture.sampler),
                },
            ],
            label: Some(&format!("{space_label} space_bind_group")),
        });

        let camera_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some(&format!("{space_label} camera_buffer")),
            size: std::mem::size_of::<WgpuCamera>().try_into().unwrap(),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let camera_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            layout: &stuff.camera_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: camera_buffer.as_entire_binding(),
            }],
            label: Some(&format!("{space_label} camera_bind_group")),
        });

        Ok(SpaceRenderer {
            render_pass_label: format!("{space_label} render_pass"),
            block_texture,
            space_bind_group,
            camera_buffer,
            camera_bind_group,
            csm: ChunkedSpaceMesh::new(space),
        })
    }

    pub fn space(&self) -> &URef<Space> {
        self.csm.space()
    }

    /// Prepare to draw a frame by updating as many chunks/buffers/textures as we're going to.
    pub(crate) fn prepare_frame<'a>(
        &'a mut self,
        queue: &wgpu::Queue,
        camera: &Camera,
        stuff: &'a BlockRenderStuff,
        bwp: BeltWritingParts<'_, '_>,
    ) -> Result<SpaceRendererOutput<'a>, GraphicsResourceError> {
        // let mut todo = self.todo.lock().unwrap();

        let space = &*self
            .csm
            .space()
            .try_borrow()
            .expect("TODO: return a trivial result instead of panic.");

        // if self.light_texture.is_none() {
        //     todo.light = None; // signal to update everything
        //     self.light_texture = Some(SpaceLightTexture::new(context, space.grid())?);
        // }
        // let light_texture = self.light_texture.as_mut().unwrap();

        // Update light texture
        let start_light_update = Instant::now();
        let light_update_count = 0;
        // if let Some(set) = &mut todo.light {
        //     // TODO: work in larger, ahem, chunks
        //     for cube in set.drain() {
        //         light_texture.update(space, Grid::new(cube, [1, 1, 1]))?;
        //     }
        // } else {
        //     light_texture.update_all(space)?;
        //     todo.light = Some(HashSet::new());
        // }
        let end_light_update = Instant::now();

        // TODO: kludge; refactor to avoid needing it
        let rcbwp = RefCell::new(bwp);

        // Update chunks
        let (csm_info, view_chunk) = self.csm.update_blocks_and_some_chunks(
            camera,
            &mut self.block_texture,
            |mesh, render_data| {
                update_chunk_buffers(rcbwp.borrow_mut().reborrow(), mesh, render_data);
            },
            |mesh, render_data| {
                if let Some(index_buf) = render_data.as_ref().and_then(|b| b.index_buf.get()) {
                    let index_buf_bytes = bytemuck::cast_slice::<u32, u8>(mesh.indices());
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
        // This must happen after `csm.update_blocks_and_some_chunks`.
        let texture_info = self.block_texture.flush(queue);

        // if graphics_options.debug_chunk_boxes {
        //     // TODO
        // } else {
        //     //self.debug_chunk_boxes_tess = None;
        // }

        Ok(SpaceRendererOutput {
            r: self,
            stuff,
            camera: camera.clone(),
            //debug_chunk_boxes_tess: &self.debug_chunk_boxes_tess,
            view_chunk,
            info: SpaceRenderInfo {
                light_update_time: end_light_update.duration_since(start_light_update),
                light_update_count,
                chunk_info: csm_info,
                texture_info,
                ..SpaceRenderInfo::default() // other fields filled later
            },
            sky_color: space.physics().sky_color,
            // block_texture: &mut block_texture_allocator.texture,
            // light_texture,
        })
    }
}

/// Ingredients to actually draw the [`Space`], produced by
/// [`SpaceRenderer::prepare_frame`].
pub(crate) struct SpaceRendererOutput<'a> {
    r: &'a SpaceRenderer,
    stuff: &'a BlockRenderStuff,

    pub(super) camera: Camera,
    // debug_chunk_boxes: &'a wgpu::Buffer,
    view_chunk: ChunkPos<CHUNK_SIZE>,
    info: SpaceRenderInfo,

    /// Space's sky color, to be used as background color (clear color / fog).
    ///
    /// Does not have camera exposure or tone mapping applied.
    pub(super) sky_color: Rgb,
}

impl<'a> SpaceRendererOutput<'a> {
    fn cull(&self, chunk: ChunkPos<CHUNK_SIZE>) -> bool {
        self.camera.options().use_frustum_culling && !self.camera.aab_in_view(chunk.grid().into())
    }

    // TODO: needs error return or not?
    #[allow(clippy::too_many_arguments)]
    pub fn draw(
        &self,
        output_view: &wgpu::TextureView,
        depth_texture_view: &wgpu::TextureView,
        queue: &wgpu::Queue,
        encoder: &mut wgpu::CommandEncoder,
        should_clear: bool,
    ) -> Result<SpaceRenderInfo, GraphicsResourceError> {
        let start_time = Instant::now();

        let mut final_info = self.info.clone();
        let csm = &self.r.csm;
        let view_direction_mask = self.camera.view_direction_mask();

        queue.write_buffer(
            &self.r.camera_buffer,
            0,
            // The [] around the camera is needed for bytemuck, so that both input and output
            // are slices.
            bytemuck::cast_slice::<WgpuCamera, u8>(&[WgpuCamera::new(
                &self.camera,
                self.sky_color,
            )]),
        );

        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some(&self.r.render_pass_label),
            color_attachments: &[wgpu::RenderPassColorAttachment {
                view: output_view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: if should_clear {
                        wgpu::LoadOp::Clear(to_wgpu_color(self.sky_color.with_alpha_one()))
                    } else {
                        wgpu::LoadOp::Load
                    },
                    store: true,
                },
            }],
            depth_stencil_attachment: Some(wgpu::RenderPassDepthStencilAttachment {
                view: depth_texture_view,
                depth_ops: Some(wgpu::Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    store: true,
                }),
                stencil_ops: None,
            }),
        });
        render_pass.set_bind_group(0, &self.r.camera_bind_group, &[]);
        render_pass.set_bind_group(1, &self.r.space_bind_group, &[]);

        // Opaque geometry first, in front-to-back order
        let start_opaque_draw_time = Instant::now();
        render_pass.set_pipeline(&self.stuff.opaque_render_pipeline);
        for p in csm
            .chunk_chart()
            .chunks(self.view_chunk, view_direction_mask)
        {
            // Chunk existence lookup is faster than the frustum culling test, so we do that first.
            if let Some(chunk) = csm.chunk(p) {
                if self.cull(p) {
                    continue;
                }
                final_info.chunks_drawn += 1;

                if let Some(buffers) = &chunk.render_data {
                    let range = chunk.mesh().opaque_range();
                    if !range.is_empty() {
                        set_buffers(&mut render_pass, buffers);
                        render_pass.draw_indexed(to_wgpu_index_range(range.clone()), 0, 0..1);
                        final_info.squares_drawn += range.len() / 6;
                    }
                }
            }
            // TODO: If the chunk is missing, draw a blocking shape, possibly?
        }

        // Transparent geometry after opaque geometry, in back-to-front order
        let start_draw_transparent_time = Instant::now();
        if self.camera.options().transparency.will_output_alpha() {
            render_pass.set_pipeline(&self.stuff.transparent_render_pipeline);
            for p in csm
                .chunk_chart()
                .chunks(self.view_chunk, view_direction_mask)
                .rev()
            {
                // Chunk existence lookup is faster than the frustum culling test, so we do that first.
                if let Some(chunk) = csm.chunk(p) {
                    if self.cull(p) {
                        continue;
                    }
                    if let Some(buffers) = &chunk.render_data {
                        let range = chunk.mesh().transparent_range(
                            // TODO: avoid adding and then subtracting view_chunk
                            DepthOrdering::from_view_direction(p.0 - self.view_chunk.0),
                        );
                        if !range.is_empty() {
                            set_buffers(&mut render_pass, buffers);
                            render_pass.draw_indexed(to_wgpu_index_range(range.clone()), 0, 0..1);
                            final_info.squares_drawn += range.len() / 6;
                        }
                    }
                }
            }
        }
        let end_time = Instant::now();

        Ok(SpaceRenderInfo {
            draw_init_time: start_opaque_draw_time.duration_since(start_time),
            draw_opaque_time: start_draw_transparent_time.duration_since(start_opaque_draw_time),
            draw_transparent_time: end_time.duration_since(start_draw_transparent_time),
            ..final_info
        })
    }
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
    new_mesh: &SpaceMesh<WgpuBlockVertex, AtlasTile>,
    buffers: &mut Option<ChunkBuffers>,
) {
    let new_vertices_data: &[u8] = bytemuck::cast_slice::<WgpuBlockVertex, u8>(new_mesh.vertices());
    // TODO: assert INDEX_FORMAT matches this type
    let new_indices_data: &[u8] = bytemuck::cast_slice::<u32, u8>(new_mesh.indices());

    if new_mesh.is_empty() {
        // No action needed
        return;
    }

    let buffers = buffers.get_or_insert_with(ChunkBuffers::default);
    buffers.vertex_buf.write_with_resizing(
        bwp.reborrow(),
        &wgpu::util::BufferInitDescriptor {
            // TODO: Get the space's label and chunk coordinates here (cheaply)
            label: Some("Chunk vertex buffer"),
            contents: new_vertices_data,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
        },
    );
    buffers.index_buf.write_with_resizing(
        bwp.reborrow(),
        &wgpu::util::BufferInitDescriptor {
            label: Some("Chunk index buffer"),
            contents: new_indices_data,
            usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
        },
    );
}

/// All the resources a [`SpaceRenderer`] needs that aren't actually specific to
/// a single [`Space`] and don't need to be modified (except when graphics options change).
/// Thus, this can be shared among all [`SpaceRenderer`]s.
///
/// TODO: better name
#[derive(Debug)]
pub(crate) struct BlockRenderStuff {
    shader_dirty: DirtyFlag,
    camera_bind_group_layout: wgpu::BindGroupLayout,
    space_texture_bind_group_layout: wgpu::BindGroupLayout,
    opaque_render_pipeline: wgpu::RenderPipeline,
    transparent_render_pipeline: wgpu::RenderPipeline,
}

impl BlockRenderStuff {
    // TODO: wants graphics options to configure shader?
    pub fn new(device: &wgpu::Device, surface_format: wgpu::TextureFormat) -> Self {
        let source = BLOCK_SHADER.as_source().snapshot();
        let shader = device.create_shader_module(&wgpu::ShaderModuleDescriptor {
            label: Some("BlockRenderStuff::shader"),
            source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(&*source)),
        });

        let space_texture_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            view_dimension: wgpu::TextureViewDimension::D3,
                            sample_type: wgpu::TextureSampleType::Float { filterable: false },
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::NonFiltering),
                        count: None,
                    },
                ],
                label: Some("BlockRenderStuff::space_texture_bind_group_layout"),
            });

        let camera_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                }],
                label: Some("BlockRenderStuff::camera_bind_group_layout"),
            });

        let render_pipeline_layout =
            device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("BlockRenderStuff::render_pipeline_layout"),
                bind_group_layouts: &[&camera_bind_group_layout, &space_texture_bind_group_layout],
                push_constant_ranges: &[],
            });

        // Parts of the render pipeline shared between opaque and transparent passes
        let primitive_state = wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Ccw,
            cull_mode: Some(wgpu::Face::Back),
            polygon_mode: wgpu::PolygonMode::Fill,
            unclipped_depth: false,
            conservative: false,
        };

        let opaque_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("BlockRenderStuff::opaque_render_pipeline"),
                layout: Some(&render_pipeline_layout),
                vertex: wgpu::VertexState {
                    module: &shader,
                    entry_point: "vs_main",
                    buffers: &[WgpuBlockVertex::desc()],
                },
                fragment: Some(wgpu::FragmentState {
                    module: &shader,
                    entry_point: "fs_main_opaque",
                    targets: &[wgpu::ColorTargetState {
                        format: surface_format,
                        blend: None,
                        write_mask: wgpu::ColorWrites::ALL,
                    }],
                }),
                primitive: primitive_state,
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: super::DEPTH_FORMAT,
                    depth_write_enabled: true,
                    depth_compare: wgpu::CompareFunction::Less,
                    stencil: wgpu::StencilState::default(),
                    bias: wgpu::DepthBiasState::default(),
                }),
                multisample: wgpu::MultisampleState::default(), // default = off
                multiview: None,
            });

        let transparent_render_pipeline =
            device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
                label: Some("BlockRenderStuff::transparent_render_pipeline"),
                layout: Some(&render_pipeline_layout),
                vertex: wgpu::VertexState {
                    module: &shader,
                    entry_point: "vs_main",
                    buffers: &[WgpuBlockVertex::desc()],
                },
                fragment: Some(wgpu::FragmentState {
                    module: &shader,
                    entry_point: "fs_main_transparent",
                    targets: &[wgpu::ColorTargetState {
                        format: surface_format,
                        // Note that this blending configuration is for premultiplied alpha.
                        // The fragment shader is responsible for producing premultiplied alpha outputs.
                        blend: Some(wgpu::BlendState {
                            color: wgpu::BlendComponent {
                                operation: wgpu::BlendOperation::Add,
                                src_factor: wgpu::BlendFactor::One,
                                dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                            },
                            // TODO: this probably isn't correct but it doesn't matter until such
                            // time as we get into transparent framebuffers
                            alpha: wgpu::BlendComponent::REPLACE,
                        }),
                        write_mask: wgpu::ColorWrites::ALL,
                    }],
                }),
                primitive: primitive_state,
                depth_stencil: Some(wgpu::DepthStencilState {
                    format: super::DEPTH_FORMAT,
                    depth_write_enabled: true,
                    depth_compare: wgpu::CompareFunction::Less,
                    stencil: wgpu::StencilState::default(),
                    bias: wgpu::DepthBiasState::default(),
                }),
                multisample: wgpu::MultisampleState::default(), // default = off
                multiview: None,
            });

        Self {
            shader_dirty: {
                // TODO: this is a common pattern which should get a helper method
                let flag = DirtyFlag::new(false);
                BLOCK_SHADER.as_source().listen(flag.listener());
                flag
            },
            camera_bind_group_layout,
            space_texture_bind_group_layout,
            opaque_render_pipeline,
            transparent_render_pipeline,
        }
    }

    pub(crate) fn recompile_if_changed(
        &mut self,
        device: &wgpu::Device,
        surface_format: wgpu::TextureFormat,
    ) {
        if self.shader_dirty.get_and_clear() {
            // TODO: slightly less efficient than it could be since it rebuilds the layouts too
            *self = Self::new(device, surface_format);
        }
    }
}

static BLOCK_SHADER: Lazy<Reloadable> =
    Lazy::new(|| reloadable_str!("src/in_wgpu/shaders/block.wgsl"));
