//! Runs [`test_renderers::harness_main`] against glTF files produced by [`all_is_cubes_port`].
//!
//! Or rather, it would if we had a glTF renderer.
//! The previous half-working implementation has been removed due to lack of maintenance;
//! this test will always fail.
//! TODO: Build a new implementation.
//!
//! Note that in order to run this test, the `gltf` feature must be enabled.

use core::fmt;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser as _;
use futures_core::future::BoxFuture;

use all_is_cubes::space::{Sky, Space};
use all_is_cubes::universe::Handle;
use all_is_cubes_content::palette;
use all_is_cubes_port as port;
use all_is_cubes_render::camera::StandardCameras;
use all_is_cubes_render::{Flaws, HeadlessRenderer, RenderError, Rendering};
use test_renderers::{RendererFactory, RendererId};

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

async fn get_factory(_label: String) -> GltfFactory {
    GltfFactory {}
}

#[derive(Clone)]
struct GltfFactory {}

impl fmt::Debug for GltfFactory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GltfFactory").finish_non_exhaustive()
    }
}

impl RendererFactory for GltfFactory {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send> {
        Box::new(GltfRend3Renderer {
            gltf_dir: Arc::new(
                tempfile::tempdir().expect("failed to create temporary directory for glTF data"),
            ),
            gltf_output: None,
            cameras,
            sky: Sky::Uniform(palette::NO_WORLD_TO_SHOW.to_rgb()),
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
    sky: Sky,
}

struct GltfOutput {
    path: PathBuf,
    flaws: Flaws,
}

impl GltfRend3Renderer {
    async fn load_latest_gltf(&mut self) -> Result<(Option<()>, Flaws), RenderError> {
        match &self.gltf_output {
            &Some(GltfOutput { ref path, flaws }) => {
                let gltf_json_data: Vec<u8> = tokio::fs::read(path)
                    .await
                    .expect("failed to read glTF JSON");
                let parent_dir = path.parent().unwrap().to_owned();

                // match rend3_gltf::load_gltf(
                //     &self.renderer,
                //     &gltf_json_data,
                //     &rend3_gltf::GltfLoadSettings::default(),
                //     move |url| {
                //         let parent_dir = parent_dir.clone();
                //         async move {
                //             rend3_gltf::filesystem_io_func(&parent_dir, url.as_str()).await
                //         }
                //     },
                // )
                // .await {
                //     Ok(si) => Ok((Some(si), flaws)),
                //     Err(rend3_gltf::GltfLoadError::GltfSingleSceneOnly) => {
                //         Ok((None, Flaws::empty()))
                //     }
                //     Err(e) => panic!("{e:?}"), // TODO: convert to RenderError
                // }
                Ok((None, Flaws::UNFINISHED))
            }
            None => Ok((None, Flaws::UNFINISHED)),
        }
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

            unimplemented!("gltf-render test has no renderer and is stubbed out");

            let rendering = todo!();
            // let rendering = old_wgpu_helpers::get_image_from_gpu(
            //     &self.renderer.device,
            //     &self.renderer.queue,
            //     &frame_texture,
            //     flaws,
            // )
            // .await;

            Ok(rendering)
        })
    }
}

// fn convert_camera(aic_camera: &all_is_cubes_render::camera::Camera) -> rend3::types::Camera {
//     // TODO: This conversion, or something else about the camera data path, is sometimes just mysteriously not working.
//
//     // Convert translation
//     let view_translation: mint::Vector3<f32> =
//         (-aic_camera.view_position().to_f32().to_vector()).into();
//     let translation_matrix = glam::Mat4::from_translation(view_translation.into());
//
//     // Convert rotation
//     let rot_quat = aic_camera.view_transform().rotation;
//     let rotation_matrix = glam::Mat4::from_quat(
//         glam::Quat::from_xyzw(
//             rot_quat.i as f32,
//             rot_quat.j as f32,
//             rot_quat.k as f32,
//             rot_quat.r as f32,
//         )
//         .normalize(),
//     );
//
//     rend3::types::Camera {
//         // TODO: pass a converted projection matrix instead
//         projection: rend3::types::CameraProjection::Perspective {
//             vfov: aic_camera.options().fov_y.into_inner() as f32,
//             near: 0.1,
//         },
//         view: rotation_matrix * translation_matrix,
//     }
// }
