//! Items not specific to a particular GPU API.

use std::error::Error;

use all_is_cubes::camera::RenderError;
use all_is_cubes::universe::RefError;
use instant::Duration;

use all_is_cubes::apps::Layers;

mod debug_lines;
pub(crate) use debug_lines::*;
mod draw_to_texture;
pub(crate) use draw_to_texture::*;
mod info;
pub use info::*;

#[doc(hidden)] // Exported only for use by fuzz_octree
pub mod octree_alloc;

pub(crate) mod reloadable;

/// Error arising when GPU/platform resources could not be obtained, or there is a bug
/// or incompatibility, and the requested graphics initialization or drawing could not be
/// completed.
///
/// Unless otherwise specified, these errors should be assumed to be recoverable
/// — invoking the renderer again in the same way next frame might or might not succeed,
/// but it should do no harm and should recover if possible (i.e. external reinitialization
/// should not be necessary).
///
/// TODO: Merge this with [`RenderError`], probably.
#[derive(Debug, thiserror::Error)]
#[error("graphics error (in {0})", context.as_ref().map(|s| s.as_ref()).unwrap_or("?"))]
pub struct GraphicsResourceError {
    context: Option<String>,
    #[source]
    source: Box<dyn Error + Send + Sync>,
}

impl GraphicsResourceError {
    pub(crate) fn new<E: Error + Send + Sync + 'static>(source: E) -> Self {
        GraphicsResourceError {
            context: None,
            source: Box::new(source),
        }
    }

    pub(crate) fn read_err(source: RefError) -> Self {
        GraphicsResourceError {
            context: Some(String::from("Unable to read scene data")),
            source: Box::new(source),
        }
    }

    /// TODO: make this not panic by expanding the functionality of [`RenderError`]
    pub fn into_render_error_or_panic(self) -> RenderError {
        if let Some(re) = self.source.downcast_ref::<RefError>() {
            RenderError::Read(re.clone())
        } else {
            // TODO: don't panic
            panic!("error updating renderer: {}", &self.source);
        }
    }
}

/// A plan for the maximum amount of time to use for each step of each frame of rendering.
///
/// Obeying the plan may consist of deferring work to the next frame, or (TODO: not
/// implemented yet) reducing the amount of content drawn, in detail or in distance.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FrameBudget {
    pub(crate) update_meshes: Layers<Duration>,
}

impl FrameBudget {
    #[doc(hidden)] // TODO: decide on good API
    pub const SIXTY_FPS: FrameBudget = FrameBudget {
        update_meshes: Layers {
            world: Duration::from_millis(5),
            ui: Duration::from_millis(3),
        },
    };

    /// No meaningful time restrictions; suitable for “offline” rendering that should
    /// always choose completeness over latency, and tests.
    pub const PRACTICALLY_INFINITE: FrameBudget = FrameBudget {
        update_meshes: Layers {
            world: VERY_LONG,
            ui: VERY_LONG,
        },
    };
}

/// A Duration long enough that it is not interesting in questions of rendering, but not
/// so long that adding a reasonable number of it to an [`Instant`] will overflow.
const VERY_LONG: Duration = Duration::from_secs(86400 * 7);

#[cfg(test)]
mod tests {
    use super::*;

    fn _test_graphics_resource_error_is_sync()
    where
        GraphicsResourceError: Send + Sync,
    {
    }
}
