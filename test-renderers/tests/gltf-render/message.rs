use std::fmt;
use std::path::PathBuf;

use futures_core::future::BoxFuture;
use tokio::sync::oneshot;

use all_is_cubes::character::Cursor;
use all_is_cubes::math::Vol;
use all_is_cubes::space::{PackedLight, Sky};
use all_is_cubes_render::camera::{Camera, Layers};
use all_is_cubes_render::{Flaws, RenderError, Rendering};

// -------------------------------------------------------------------------------------------------

/// glTF file location, and non-glTF data, transferred from the headless renderer object to
/// `bevy_app_actor` to specify a single scene/frame to be rendered.
#[derive(Debug)]
pub(crate) struct Transfer {
    /// Absolute path (within a temporary directory) of this scene's glTF file.
    pub gltf_path: PathBuf,

    /// Flaws reported by the glTF export.
    pub flaws: Flaws,

    /// Whether the glTF file is expected to contain a single scene or no scenes.
    pub has_scene: bool,

    /// Cursor data to be rendered.
    pub cursor: Option<Cursor>,

    /// Camera data.
    ///
    /// Currently, glTF *exports* do not include a glTF camera.
    /// Even if they did, we need several pieces of additional information to produce accurate
    /// renderings.
    ///
    /// In the long run, we should embed those in the glTF, but that is not a current priority;
    /// this module is implementing these rendering options not because they are part of the
    /// desired scope of glTF export but in order to be able to pass the renderer test suite.
    pub cameras: Layers<Camera>,

    pub sky: Sky,

    pub light_data: Option<Vol<Box<[PackedLight]>>>,
}

/// Messages from [`GltfBevyRenderer`] to [`bevy_app_actor()`].
pub(crate) enum RenderMsg {
    Update {
        /// Task performing the export.
        /// glTF file will not be ready until this future completes.
        export_task: BoxFuture<'static, Result<(), all_is_cubes_port::ExportError>>,

        /// Data to load into the world for rendering.
        transfer: Transfer,
    },
    Render {
        info_text: String,
        completed: oneshot::Sender<Result<Rendering, RenderError>>,
    },
}

impl fmt::Debug for RenderMsg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Update {
                export_task: _,
                transfer,
            } => f.debug_struct("Update").field("transfer", transfer).finish_non_exhaustive(),
            Self::Render {
                info_text,
                completed,
            } => f
                .debug_struct("Render")
                .field("info_text", info_text)
                .field("completed", completed)
                .finish(),
        }
    }
}
