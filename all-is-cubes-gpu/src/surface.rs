use alloc::string::String;
use alloc::sync::Arc;

use wgpu::TextureViewDescriptor;

use all_is_cubes::character::Cursor;
use all_is_cubes::listen;
use all_is_cubes::time;
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::util::Executor;
use all_is_cubes_render::camera::{Layers, StandardCameras};
use all_is_cubes_render::{Flaws, RenderError};

use crate::{EverythingRenderer, FrameBudget, RenderInfo, SpaceDrawInfo};

#[cfg(feature = "rerun")]
use {crate::RerunFilter, all_is_cubes::rerun_glue as rg};

// -------------------------------------------------------------------------------------------------

/// Entry point for [`wgpu`] rendering. Construct this and hand it the [`wgpu::Surface`]
/// to draw on.
///
/// If you wish to render to an image rather than a surface, use [`headless`][crate::headless]
/// instead.
#[derive(Debug)]
pub struct SurfaceRenderer {
    surface: wgpu::Surface<'static>,
    device: wgpu::Device,
    queue: wgpu::Queue,

    everything: EverythingRenderer,

    /// True if we need to reconfigure the surface.
    viewport_dirty: listen::Flag,
}

impl SurfaceRenderer {
    /// Constructs a renderer owning and operating on `surface`.
    ///
    /// This will create a dedicated [`wgpu::Device`] using the provided [`wgpu::Adapter`],
    /// and return an error if requesting the device fails.
    pub async fn new(
        cameras: StandardCameras,
        surface: wgpu::Surface<'static>,
        adapter: wgpu::Adapter,
        executor: Arc<dyn Executor>,
    ) -> Result<Self, wgpu::RequestDeviceError> {
        let request_device_future = adapter.request_device(&EverythingRenderer::device_descriptor(
            "SurfaceRenderer::device",
            adapter.limits(),
        ));
        let (device, queue) = request_device_future.await?;

        let viewport_source = cameras.viewport_source();
        let everything = EverythingRenderer::new(
            executor,
            device.clone(),
            &queue,
            cameras,
            choose_surface_format(&surface.get_capabilities(&adapter)),
            &adapter,
        );

        Ok(Self {
            viewport_dirty: listen::Flag::listening(true, &viewport_source),
            everything,
            surface,
            device,
            queue,
        })
    }

    /// Returns a clonable handle to the device this renderer owns.
    pub fn device(&self) -> &wgpu::Device {
        &self.device
    }

    /// Sync camera to character state. This is used so that cursor raycasts can be up-to-date
    /// to the same frame of input.
    ///
    /// TODO: This is a kludge which ought to be replaced with some architecture that
    /// doesn't require a very specific "do this before this"...
    #[doc(hidden)]
    pub fn update_world_camera(&mut self, read_tickets: Layers<ReadTicket<'_>>) {
        self.everything.cameras.update(read_tickets);
    }

    /// Returns the [`StandardCameras`] which control what is rendered by this renderer.
    pub fn cameras(&self) -> &StandardCameras {
        &self.everything.cameras
    }

    /// Renders one frame to the surface, using the current contents of [`Self::cameras()`] and
    /// the given cursor and overlay text.
    pub fn render_frame(
        &mut self,
        read_tickets: Layers<ReadTicket<'_>>,
        cursor_result: Option<&Cursor>,
        frame_budget: &FrameBudget,
        info_text_fn: impl FnOnce(&RenderInfo) -> String,
        about_to_present: impl FnOnce(),
    ) -> Result<RenderInfo, RenderError> {
        let update_info =
            self.everything.update(read_tickets, &self.queue, cursor_result, frame_budget)?;

        if self.viewport_dirty.get_and_clear() {
            // Test because wgpu insists on nonzero values -- we'd rather be inconsistent
            // than crash.
            let config = &self.everything.config();
            if config.width > 0 && config.height > 0 {
                self.surface.configure(&self.device, config);
            }
        }

        // If the GPU is busy, `get_current_texture()` blocks until a previous texture
        // is no longer in use (except on wasm, in which case submit() seems to take the time).
        let before_get = time::Instant::now();
        let output = match self.surface.get_current_texture() {
            Ok(t) => t,
            Err(e @ wgpu::SurfaceError::Timeout) => {
                // Nothing to do but try again next frame.
                log::error!(
                    "Error from wgpu::Surface::get_current_texture(): {e:?}. Skipping this frame."
                );
                return Ok(RenderInfo {
                    flaws: Flaws::UNFINISHED,
                    ..RenderInfo::default()
                });
            }
            Err(e @ (wgpu::SurfaceError::Outdated | wgpu::SurfaceError::Lost)) => {
                // Reconfigure and try again next frame.
                // NOTE: wgpu::SurfaceError::Outdated is very common with tiling window manager.
                log::error!(
                    "Error from wgpu::Surface::get_current_texture(): {e:?}. Skipping this frame."
                );
                self.surface.configure(&self.device, self.everything.config());
                return Ok(RenderInfo {
                    flaws: Flaws::UNFINISHED,
                    ..RenderInfo::default()
                });
            }
            Err(e) => {
                panic!(
                    "error from wgpu::Surface::get_current_texture(): {e:?}; \
                    error recovery not implemented"
                );
            }
        };
        let after_get = time::Instant::now();

        let draw_info = self.everything.draw_frame_linear(&self.queue);

        // Construct aggregated info.
        // TODO: the flaws combination logic is awkward. Should we combine them at printing
        // time only?
        let Layers {
            world: SpaceDrawInfo {
                flaws: world_flaws, ..
            },
            ui: SpaceDrawInfo {
                flaws: ui_flaws, ..
            },
        } = draw_info.space_info;
        let mut info = RenderInfo {
            waiting_for_gpu: after_get.saturating_duration_since(before_get),
            flaws: update_info.flaws | world_flaws | ui_flaws,
            update: update_info,
            draw: draw_info,
        };

        // Render info and postprocessing step.
        // TODO: We should record the amount of time this takes, then display that next frame.
        // TODO(efficiency): combine draw_frame_linear and postprocess into one submission.
        let (post_cmd, post_flaws) = self.everything.add_info_text_and_postprocess(
            &self.queue,
            &output.texture.create_view(&TextureViewDescriptor {
                format: Some(super::surface_view_format(self.everything.config().format)),
                ..Default::default()
            }),
            &info_text_fn(&info),
        );
        info.flaws |= post_flaws;
        self.queue.submit([post_cmd]);
        about_to_present();
        output.present();
        Ok(info)
    }

    /// Activate logging performance information state to a Rerun stream.
    #[cfg(feature = "rerun")]
    pub fn log_to_rerun(&mut self, destination: rg::Destination, filter: RerunFilter) {
        self.everything.log_to_rerun(destination, filter)
    }
}

// -------------------------------------------------------------------------------------------------

/// Choose the surface format we would prefer from among the supported formats.
fn choose_surface_format(capabilities: &wgpu::SurfaceCapabilities) -> wgpu::TextureFormat {
    /// A structure whose maximum [`Ord`] value corresponds to the texture format we'd rather use.
    #[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
    struct Rank {
        /// If we can have a float format that gives us (potential) HDR output, do that.
        is_float: bool,
        /// Known SRGB outputs are good
        is_srgb: bool,
        /// All else being equal, prefer the original preference order
        /// (earlier elements are better)
        negated_original_order: isize,
    }

    let (index, best) = capabilities
        .formats
        .iter()
        .copied()
        .enumerate()
        .max_by_key(|&(index, format)| {
            use wgpu::TextureFormat::*;
            Rank {
                is_srgb: format.is_srgb(),
                // Float output is somehow broken on wasm, so don't prefer it.
                // <https://github.com/kpreid/all-is-cubes/issues/310>
                // TODO: repro and report to wgpu, supposing that it is a wgpu bug
                is_float: matches!(format, Rgba16Float | Rgba32Float | Rgb9e5Ufloat)
                    && !cfg!(target_family = "wasm"),
                #[expect(clippy::cast_possible_wrap)]
                negated_original_order: -(index as isize),
            }
        })
        .expect("wgpu::Surface::get_supported_formats() was empty");
    log::debug!(
        "Chose surface format {best:?}, #{index} out of {formats:?}",
        index = index + 1,
        formats = capabilities.formats,
    );
    best
}
