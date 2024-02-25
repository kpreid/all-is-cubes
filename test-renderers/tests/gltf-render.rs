//! Runs [`test_renderers::harness_main`] against glTF files produced by [`all_is_cubes_port`]
//! and rendered by [`rend3`].
//!
//! Note that in order to run this test, the `gltf` feature must be enabled.

use core::fmt;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser as _;
use futures_core::future::BoxFuture;

use all_is_cubes::camera::{Flaws, HeadlessRenderer, RenderError, StandardCameras};
use all_is_cubes::space::{Sky, Space};
use all_is_cubes::universe::Handle;
use all_is_cubes_content::palette;
use all_is_cubes_port as port;
use test_renderers::{RendererFactory, RendererId};

const TEXTURE_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8UnormSrgb;

#[tokio::main]
async fn main() -> test_renderers::HarnessResult {
    test_renderers::initialize_logging();

    test_renderers::harness_main(
        test_renderers::HarnessArgs::parse(),
        RendererId::Gltf,
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

            let world_space: Option<Handle<Space>> = self.cameras.world_space().snapshot();
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
                port::ExportFormat::Gltf,
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

    fn draw<'a>(
        &'a mut self,
        info_text: &'a str,
    ) -> BoxFuture<'a, Result<all_is_cubes::camera::Rendering, RenderError>> {
        Box::pin(async {
            let aic_camera = &self.cameras.cameras().world;
            let viewport = aic_camera.viewport();

            if viewport.pixel_count() == Some(0) {
                // Empty viewport; must not try to use the GPU at all.
                return Ok(all_is_cubes::camera::Rendering {
                    size: viewport.framebuffer_size,
                    data: Vec::new(),
                    flaws: Flaws::empty(),
                });
            }

            self.renderer.set_camera_data(convert_camera(aic_camera));

            let (_objects, mut flaws) = self.load_latest_gltf().await?;

            if !info_text.is_empty() {
                flaws |= Flaws::MISSING_TEXTURES; // TODO: should have a flaw for this, or just actually implement it
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

            let rendering = all_is_cubes_gpu::in_wgpu::init::get_image_from_gpu(
                self.renderer.device.clone(),
                &self.renderer.queue,
                &frame_texture,
                flaws,
            )
            .await;

            Ok(rendering)
        })
    }
}

fn convert_camera(aic_camera: &all_is_cubes::camera::Camera) -> rend3::types::Camera {
    // TODO: This conversion, or something else about the camera data path, is sometimes just mysteriously not working.

    // Convert translation
    let view_translation: mint::Vector3<f32> =
        (-aic_camera.view_position().to_f32().to_vector()).into();
    let translation_matrix = glam::Mat4::from_translation(view_translation.into());

    // Convert rotation
    let rot_quat = aic_camera.get_view_transform().rotation;
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
