//! [`GltfBevyRenderer`].

use std::sync::Arc;

use futures_core::future::BoxFuture;
use tokio::sync::{mpsc, oneshot};

use all_is_cubes::character::Cursor;
use all_is_cubes::math::{FaceMap, Vol};
use all_is_cubes::space::{Sky, Space};
use all_is_cubes::universe::{Handle, ReadTicket};
use all_is_cubes_content::palette;
use all_is_cubes_port as port;
use all_is_cubes_render::camera::{Layers, StandardCameras};
use all_is_cubes_render::{Flaws, HeadlessRenderer, RenderError, Rendering};

use crate::message::{RenderMsg, Transfer};

// -------------------------------------------------------------------------------------------------

/// [`HeadlessRenderer`] implementation that uses [`all_is_cubes_port`] to generate glTF files
/// and then uses [`bevy`] to render them.
pub(crate) struct GltfBevyRenderer {
    gltf_dir: Arc<tempfile::TempDir>,

    cameras: StandardCameras,
    tx: mpsc::UnboundedSender<RenderMsg>,
}

impl GltfBevyRenderer {
    pub fn new(factory: &crate::GltfFactory, cameras: StandardCameras) -> Self {
        // Bevy `App` is `!Send`, so we have to make an actor of it
        // (spawn a thread that owns it and receives commands).
        let (tx, rx) = mpsc::unbounded_channel();
        let handle = factory.handle.clone();
        let viewport = cameras.viewport();
        let label = factory.label.clone();
        std::thread::Builder::new()
            .name(label.clone())
            .spawn(move || handle.block_on(crate::bevy_app::bevy_app_actor(label, rx, viewport)))
            .expect("failed to spawn render App thread");

        GltfBevyRenderer {
            gltf_dir: Arc::new(
                tempfile::tempdir().expect("failed to create temporary directory for glTF data"),
            ),
            cameras,
            tx,
        }
    }
}

impl HeadlessRenderer for GltfBevyRenderer {
    fn update<'a>(
        &'a mut self,
        read_tickets: Layers<ReadTicket<'_>>,
        cursor: Option<&'a Cursor>,
    ) -> Result<(), RenderError> {
        // Sync changes to what we should be drawing (exporting)
        self.cameras.update(read_tickets);

        let world_space: Option<Handle<Space>> = self.cameras.world_space().get();
        let gltf_path = self.gltf_dir.path().join("scene.gltf");

        let sky = match &world_space {
            Some(sp) => {
                sp.read(read_tickets.world).map_err(RenderError::Read)?.physics().sky.clone()
            }
            None => Sky::Uniform(palette::NO_WORLD_TO_SHOW.to_rgb()),
        };

        // TODO: delete all files from the directory so there is no accidental statefulness

        let export_task = Box::pin(port::export_to_path(
            all_is_cubes::util::yield_progress_for_testing(),
            read_tickets.world,
            port::Format::Gltf,
            port::ExportSet::from_spaces(
                world_space.iter().cloned().collect::<Vec<Handle<Space>>>(),
            ),
            gltf_path.clone(),
        ));

        if false {
            // For debugging: report the temporary glTF file path and exit without deleting it.
            eprintln!("gltf: {}", gltf_path.display());
            std::process::exit(1);
        }

        // We don't need to await the results of this message, because all the world state
        // has already been captured in the glTF file or other parts of the `Transfer`.
        self.tx
            .send(RenderMsg::Update {
                export_task,
                transfer: Transfer {
                    gltf_path,
                    flaws: Flaws::empty(), // TODO: hook up flaws from the export's mesh generation
                    cursor: cursor.cloned(),
                    cameras: self.cameras.cameras().clone(),
                    has_scene: world_space.is_some(),
                    sky,
                    light_data: match world_space {
                        Some(world_space) => {
                            let world_space = world_space.read(read_tickets.world).unwrap();
                            Some(Vol::from_fn(
                                world_space.bounds().expand(FaceMap::splat(1)),
                                |cube| world_space.get_lighting(cube),
                            ))
                        }
                        None => None,
                    },
                },
            })
            .expect("render task failed");
        Ok(())
    }

    fn draw<'a>(&'a mut self, info_text: &'a str) -> BoxFuture<'a, Result<Rendering, RenderError>> {
        Box::pin(async {
            let (completion_tx, completion_rx) = oneshot::channel();
            self.tx
                .send(RenderMsg::Render {
                    info_text: info_text.to_owned(),
                    completed: completion_tx,
                })
                .expect("render task failed");
            completion_rx.await.expect("render task failed")
        })
    }
}
