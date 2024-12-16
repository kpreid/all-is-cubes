//! Runs [`test_renderers::harness_main`] against glTF files produced by [`all_is_cubes_port`]
//! and rendered by [`rend3`].
//!
//! Note that in order to run this test, the `gltf` feature must be enabled.

use core::fmt;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser as _;
use futures_core::future::BoxFuture;
use wgpu_for_rend3 as wgpu;

use all_is_cubes::space::{Sky, Space};
use all_is_cubes::universe::Handle;
use all_is_cubes_content::palette;
use all_is_cubes_port as port;
use all_is_cubes_render::camera::StandardCameras;
use all_is_cubes_render::{Flaws, HeadlessRenderer, RenderError, Rendering};
use test_renderers::{RendererFactory, RendererId};

const TEXTURE_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8UnormSrgb;

#[tokio::main]
async fn main() -> test_renderers::HarnessResult {
    let args = test_renderers::HarnessArgs::parse();
    test_renderers::initialize_logging(&args);

    test_renderers::harness_main(
        &args,
        RendererId::Gltf,
        test_renderers::SuiteId::Renderers,
        test_renderers::test_cases::all_tests,
        get_factory,
        None,
    )
    .await
}

async fn get_factory() -> GltfFactory {
    match rend3::create_iad(None, None, None, None).await {
        Ok(iad) => GltfFactory { iad },
        Err(rend3::RendererInitializationError::MissingAdapter) => {
            // TODO: harness should support fallibility here
            eprintln!("Skipping rendering tests due to lack of wgpu::Adapter.");
            std::process::exit(0);
        }
        Err(e) => panic!("{e}", e = all_is_cubes::util::ErrorChain(&e)),
    }
}

#[derive(Clone)]
struct GltfFactory {
    iad: rend3::InstanceAdapterDevice,
}

impl fmt::Debug for GltfFactory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GltfFactory").finish_non_exhaustive()
    }
}

impl RendererFactory for GltfFactory {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send> {
        let renderer = rend3::Renderer::new(
            self.iad.clone(),
            rend3::types::Handedness::Right,
            Some(cameras.viewport().nominal_aspect_ratio() as f32),
        )
        .unwrap();

        // Initialize rend3 renderer state/features

        let mut spp = rend3::ShaderPreProcessor::new();
        rend3_routine::builtin_shaders(&mut spp);

        let base_rendergraph = rend3_routine::base::BaseRenderGraph::new(&renderer, &spp);
        let pbr_routine = rend3_routine::pbr::PbrRoutine::new(
            &renderer,
            &mut renderer.data_core.lock(),
            &spp,
            &base_rendergraph.interfaces,
            &base_rendergraph.gpu_culler.culling_buffer_map_handle,
        );
        let tonemapping_routine = rend3_routine::tonemapping::TonemappingRoutine::new(
            &renderer,
            &spp,
            &base_rendergraph.interfaces,
            TEXTURE_FORMAT,
        );

        Box::new(GltfRend3Renderer {
            gltf_dir: Arc::new(
                tempfile::tempdir().expect("failed to create temporary directory for glTF data"),
            ),
            gltf_output: None,
            renderer,
            cameras,
            sky: Sky::Uniform(palette::NO_WORLD_TO_SHOW.to_rgb()),
            base_rendergraph,
            pbr_routine,
            tonemapping_routine,
        })
    }

    fn id(&self) -> RendererId {
        RendererId::Gltf
    }
}

struct GltfRend3Renderer {
    gltf_dir: Arc<tempfile::TempDir>,
    /// Data passed from the last glTF generation by update()
    gltf_output: Option<GltfOutput>,

    cameras: StandardCameras,
    renderer: Arc<rend3::Renderer>,
    sky: Sky,

    base_rendergraph: rend3_routine::base::BaseRenderGraph,
    pbr_routine: rend3_routine::pbr::PbrRoutine,
    tonemapping_routine: rend3_routine::tonemapping::TonemappingRoutine,
}

struct GltfOutput {
    path: PathBuf,
    flaws: Flaws,
}

impl GltfRend3Renderer {
    async fn load_latest_gltf(
        &mut self,
    ) -> Result<
        (
            Option<(rend3_gltf::LoadedGltfScene, rend3_gltf::GltfSceneInstance)>,
            Flaws,
        ),
        RenderError,
    > {
        match &self.gltf_output {
            &Some(GltfOutput { ref path, flaws }) => {
                let gltf_json_data: Vec<u8> = tokio::fs::read(path)
                    .await
                    .expect("failed to read glTF JSON");
                let parent_dir = path.parent().unwrap().to_owned();

                match rend3_gltf::load_gltf(
                    &self.renderer,
                    &gltf_json_data,
                    &rend3_gltf::GltfLoadSettings::default(),
                    move |url| {
                        let parent_dir = parent_dir.clone();
                        async move {
                            rend3_gltf::filesystem_io_func(&parent_dir, url.as_str()).await
                        }
                    },
                )
                .await {
                    Ok(si) => Ok((Some(si), flaws)),
                    Err(rend3_gltf::GltfLoadError::GltfSingleSceneOnly) => {
                        Ok((None, Flaws::empty()))
                    }
                    Err(e) => panic!("{e:?}"), // TODO: convert to RenderError
                }
            }
            None => Ok((None, Flaws::UNFINISHED)),
        }
    }

    fn render_to_rend3(&mut self, frame_texture: &wgpu::Texture) {
        let renderer = &mut self.renderer;
        let size = glam::UVec2 {
            x: frame_texture.width(),
            y: frame_texture.height(),
        };

        renderer.swap_instruction_buffers();
        let mut eval_output = renderer.evaluate_instructions();

        let mut graph = rend3::graph::RenderGraph::new();

        let frame_handle = graph.add_imported_render_target(
            frame_texture,
            0..1,
            0..1,
            rend3::graph::ViewportRect::from_size(size),
        );
        self.base_rendergraph.add_to_graph(
            &mut graph,
            rend3_routine::base::BaseRenderGraphInputs {
                eval_output: &eval_output,
                routines: rend3_routine::base::BaseRenderGraphRoutines {
                    pbr: &self.pbr_routine,
                    skybox: None,
                    tonemapping: &self.tonemapping_routine,
                },
                target: rend3_routine::base::OutputRenderTarget {
                    handle: frame_handle,
                    resolution: size,
                    samples: rend3::types::SampleCount::One, // TODO: respect graphics options
                },
            },
            rend3_routine::base::BaseRenderGraphSettings {
                // TODO: We would like to export and then render baked lighting, but for now,
                // just use white ambient lighting to obtain LightingOption::None-like behavior
                // TODO: Put that in [`Flaws`].
                ambient_color: glam::Vec4::splat(1.0),
                // TODO: create skybox object
                clear_color: glam::Vec4::from(<[f32; 4]>::from(self.sky.mean().with_alpha_one())),
            },
        );

        graph.execute(renderer, &mut eval_output);
    }
}

impl HeadlessRenderer for GltfRend3Renderer {
    fn update<'a>(
        &'a mut self,
        _cursor: Option<&'a all_is_cubes::character::Cursor>,
    ) -> BoxFuture<'a, Result<(), RenderError>> {
        Box::pin(async {
            // Sync changes to what we should be drawing (exporting)
            self.cameras.update();

            let world_space: Option<Handle<Space>> = self.cameras.world_space().get();
            let gltf_path = self.gltf_dir.path().join("scene.gltf");

            self.sky = {
                match &world_space {
                    Some(sp) => sp.read().map_err(RenderError::Read)?.physics().sky.clone(),
                    None => Sky::Uniform(palette::NO_WORLD_TO_SHOW.to_rgb()),
                }
            };

            // TODO: delete old auxiliary files

            let progress = all_is_cubes::util::yield_progress_for_testing();
            port::export_to_path(
                progress,
                port::Format::Gltf,
                port::ExportSet::from_spaces(
                    world_space
                        .into_iter()
                        .map(|r| Handle::clone(&r))
                        .collect::<Vec<_>>(),
                ),
                gltf_path.clone(),
            )
            .await
            .unwrap(); // TODO: convert to RenderError

            self.gltf_output = Some(GltfOutput {
                path: gltf_path,
                flaws: Flaws::empty(), // TODO: hook up flaws from the export's mesh generation
            });
            Ok(())
        })
    }

    fn draw<'a>(&'a mut self, info_text: &'a str) -> BoxFuture<'a, Result<Rendering, RenderError>> {
        Box::pin(async {
            let aic_camera = &self.cameras.cameras().world;
            let viewport = aic_camera.viewport();

            if viewport.pixel_count() == Some(0) {
                // Empty viewport; must not try to use the GPU at all.
                return Ok(Rendering {
                    size: viewport.framebuffer_size,
                    data: Vec::new(),
                    flaws: Flaws::empty(),
                });
            }

            self.renderer.set_camera_data(convert_camera(aic_camera));

            let (_objects, mut flaws) = self.load_latest_gltf().await?;

            if !info_text.is_empty() {
                flaws |= Flaws::OTHER; // TODO: should have a flaw for this, or just actually implement it
            }

            // not bothering to reuse texture
            let frame_texture = self
                .renderer
                .device
                .create_texture(&wgpu::TextureDescriptor {
                    label: None,
                    size: wgpu::Extent3d {
                        width: viewport.framebuffer_size.width.max(1),
                        height: viewport.framebuffer_size.height.max(1),
                        depth_or_array_layers: 1,
                    },
                    mip_level_count: 1,
                    sample_count: 1,
                    dimension: wgpu::TextureDimension::D2,
                    format: TEXTURE_FORMAT,
                    view_formats: &[],
                    usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
                });

            self.render_to_rend3(&frame_texture);

            let rendering = old_wgpu_helpers::get_image_from_gpu(
                &self.renderer.device,
                &self.renderer.queue,
                &frame_texture,
                flaws,
            )
            .await;

            Ok(rendering)
        })
    }
}

fn convert_camera(aic_camera: &all_is_cubes_render::camera::Camera) -> rend3::types::Camera {
    // TODO: This conversion, or something else about the camera data path, is sometimes just mysteriously not working.

    // Convert translation
    let view_translation: mint::Vector3<f32> =
        (-aic_camera.view_position().to_f32().to_vector()).into();
    let translation_matrix = glam::Mat4::from_translation(view_translation.into());

    // Convert rotation
    let rot_quat = aic_camera.view_transform().rotation;
    let rotation_matrix = glam::Mat4::from_quat(
        glam::Quat::from_xyzw(
            rot_quat.i as f32,
            rot_quat.j as f32,
            rot_quat.k as f32,
            rot_quat.r as f32,
        )
        .normalize(),
    );

    rend3::types::Camera {
        // TODO: pass a converted projection matrix instead
        projection: rend3::types::CameraProjection::Perspective {
            vfov: aic_camera.options().fov_y.into_inner() as f32,
            near: 0.1,
        },
        view: rotation_matrix * translation_matrix,
    }
}

/// This is a copy and paste of all-is-cubes-gpu/src/in_wgpu/init.rs
/// except that it uses `rend3`'s version of wgpu instead. Yes
mod old_wgpu_helpers {
    use super::*;
    use all_is_cubes_render::camera;
    use std::future::Future;
    use std::sync::Arc;

    /// Copy the contents of a texture into a [`Rendering`],
    /// assuming that its byte layout is RGBA8.
    ///
    /// Panics if the pixel type or viewport size are incorrect.
    #[doc(hidden)]
    pub fn get_image_from_gpu(
        device: &Arc<wgpu::Device>,
        queue: &wgpu::Queue,
        texture: &wgpu::Texture,
        flaws: Flaws,
    ) -> impl Future<Output = Rendering> + 'static {
        // By making this an explicit `Future` return we avoid capturing the queue and texture
        // references.

        let size = camera::ImageSize::new(texture.width(), texture.height());
        let data_future = get_texels_from_gpu::<[u8; 4]>(device, queue, texture, 1);

        async move {
            Rendering {
                size,
                data: data_future.await,
                flaws,
            }
        }
    }

    /// Fetch the contents of a 2D texture, assuming that its byte layout is the same as that
    /// of `[C; components]` and returning a vector of length
    /// `dimensions.x * dimensions.y * components`.
    ///
    /// Panics if the provided sizes are incorrect.
    #[doc(hidden)]
    pub fn get_texels_from_gpu<C>(
        device: &Arc<wgpu::Device>,
        queue: &wgpu::Queue,
        texture: &wgpu::Texture,
        components: usize,
    ) -> impl Future<Output = Vec<C>> + 'static
    where
        C: bytemuck::AnyBitPattern,
    {
        // By making this an explicit `Future` return we avoid capturing the queue and texture
        // references.

        let dimensions = camera::ImageSize::new(texture.width(), texture.height());
        assert_eq!(texture.depth_or_array_layers(), 1);

        // Check that the format matches
        let format = texture.format();
        let size_of_texel = components * size_of::<C>();
        assert_eq!(
            (format.block_copy_size(None), format.block_dimensions()),
            (Some(size_of_texel as u32), (1, 1)),
            "Texture format does not match requested size",
        );

        let dense_bytes_per_row = dimensions.width * u32::try_from(size_of_texel).unwrap();
        let padded_bytes_per_row = dense_bytes_per_row.div_ceil(wgpu::COPY_BYTES_PER_ROW_ALIGNMENT)
            * wgpu::COPY_BYTES_PER_ROW_ALIGNMENT;

        let temp_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("GPU-to-CPU image copy buffer"),
            size: u64::from(padded_bytes_per_row) * u64::from(dimensions.height),
            usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
            mapped_at_creation: false,
        });

        {
            let mut encoder =
                device.create_command_encoder(&wgpu::CommandEncoderDescriptor::default());
            encoder.copy_texture_to_buffer(
                texture.as_image_copy(),
                wgpu::ImageCopyBuffer {
                    buffer: &temp_buffer,
                    layout: wgpu::ImageDataLayout {
                        offset: 0,
                        bytes_per_row: Some(padded_bytes_per_row),
                        rows_per_image: None,
                    },
                },
                texture.size(),
            );
            queue.submit(Some(encoder.finish()));
        }

        // Start the buffer mapping
        let (sender, receiver) = futures_channel::oneshot::channel();
        temp_buffer
            .slice(..)
            .map_async(wgpu::MapMode::Read, |result| {
                let _ = sender.send(result);
            });
        device.poll(wgpu::Maintain::Wait);

        // Await the buffer being available and build the image.
        async move {
            receiver
                .await
                .expect("communication failed")
                .expect("buffer reading failed");
            let mapped: &[u8] = &temp_buffer.slice(..).get_mapped_range();

            let element_count = camera::area_usize(dimensions).unwrap() * components;

            // Copy the mapped buffer data into a Rust vector, removing row padding if present
            // by copying it one row at a time.
            let mut texel_vector: Vec<C> = Vec::with_capacity(element_count);
            for row in 0..dimensions.height {
                let byte_start_of_row = padded_bytes_per_row * row;
                // TODO: this cast_slice() could fail if `C`’s alignment is higher than the buffer.
                texel_vector.extend(bytemuck::cast_slice::<u8, C>(
                    &mapped[byte_start_of_row as usize..][..dense_bytes_per_row as usize],
                ));
            }
            debug_assert_eq!(texel_vector.len(), element_count);

            temp_buffer.destroy();

            texel_vector
        }
    }
}
