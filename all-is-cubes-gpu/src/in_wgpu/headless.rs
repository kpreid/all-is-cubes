//! Implementation of [`HeadlessRenderer`] using [`wgpu`].

use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;

use futures_core::future::BoxFuture;

use all_is_cubes::character::Cursor;
use all_is_cubes::listen;
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::util::Executor;
use all_is_cubes_render::camera::{ImageSize, Layers, StandardCameras, Viewport};
use all_is_cubes_render::{Flaws, HeadlessRenderer, RenderError, Rendering};

use crate::common::FrameBudget;
use crate::in_wgpu::glue::size2d_to_extent;
use crate::in_wgpu::{self, init};

/// Builder for configuring a [headless](HeadlessRenderer) [`Renderer`].
///
/// The builder owns a `wgpu::Device`; all created renderers will share this device.
/// If the device is lost, a new `Builder` must be created.
#[derive(Clone, Debug)]
pub struct Builder {
    executor: Arc<dyn Executor>,
    pub(crate) adapter: wgpu::Adapter,
    device: wgpu::Device,
    queue: wgpu::Queue,
}

impl Builder {
    /// Create a [`Builder`] by obtaining a new [`wgpu::Device`] from the given adapter.
    pub async fn from_adapter(
        label: &str,
        adapter: wgpu::Adapter,
    ) -> Result<Self, wgpu::RequestDeviceError> {
        let (device, queue) = adapter
            .request_device(&in_wgpu::EverythingRenderer::device_descriptor(
                label,
                adapter.limits(),
            ))
            .await?;
        Ok(Self {
            device,
            queue,
            adapter,
            executor: Arc::new(()),
        })
    }

    /// Set the executor for parallel calculations.
    #[must_use]
    pub fn executor(mut self, executor: Arc<dyn Executor>) -> Self {
        self.executor = executor;
        self
    }

    /// Create a [`Renderer`] from the GPU connection in this builder and the given cameras.
    pub fn build(&self, cameras: StandardCameras) -> Renderer {
        let viewport_source = cameras.viewport_source();
        let everything = in_wgpu::EverythingRenderer::new(
            self.executor.clone(),
            self.device.clone(),
            &self.queue,
            cameras,
            wgpu::TextureFormat::Rgba8UnormSrgb,
            &self.adapter,
        );

        let viewport_dirty = listen::Flag::listening(false, &viewport_source);
        let viewport = viewport_source.get();
        let color_texture = create_color_texture(&self.device, viewport);

        Renderer::wrap(RendererImpl {
            adapter_info: self.adapter.get_info(),
            device: self.device.clone(),
            queue: self.queue.clone(),
            color_texture,
            everything,
            viewport_source,
            viewport_dirty,
            flaws: Flaws::UNFINISHED, // unfinished because no update() yet
        })
    }
}

/// Implementation of [`HeadlessRenderer`] using [`wgpu`].
///
/// This is constructed from a [`wgpu::Device`] and a [`StandardCameras`] using [`Builder`],
/// and may then be used once or repeatedly to produce images of what those cameras see.
#[derive(Debug)]
pub struct Renderer {
    /// `wgpu` is currently entirely `!Send` on web-wasm, but also there are no additional threads,
    /// that might use this renderer, so we can use `send_wrapper` to achieve `Send`.
    #[cfg(target_family = "wasm")]
    inner: send_wrapper::SendWrapper<RendererImpl>,
    #[cfg(not(target_family = "wasm"))]
    inner: RendererImpl,
}

/// Internals of [`Renderer`] to actually do the rendering.
#[derive(Debug)]
struct RendererImpl {
    adapter_info: wgpu::AdapterInfo,
    device: wgpu::Device,
    queue: wgpu::Queue,

    color_texture: wgpu::Texture,
    everything: super::EverythingRenderer,

    viewport_source: listen::DynSource<Viewport>,
    viewport_dirty: listen::Flag,

    /// Flaws from the last [`Self::update()`] call.
    flaws: Flaws,
}

impl Renderer {
    fn wrap(inner: RendererImpl) -> Renderer {
        Self {
            #[cfg(target_family = "wasm")]
            inner: send_wrapper::SendWrapper::new(inner),
            #[cfg(not(target_family = "wasm"))]
            inner,
        }
    }
}

impl HeadlessRenderer for Renderer {
    fn update(
        &mut self,
        read_tickets: Layers<ReadTicket<'_>>,
        cursor: Option<&Cursor>,
    ) -> Result<(), RenderError> {
        // Note: this delegation is the simplest ways to
        self.inner.update(read_tickets, cursor)
    }

    fn draw<'a>(&'a mut self, info_text: &'a str) -> BoxFuture<'a, Result<Rendering, RenderError>> {
        let future = async move { self.inner.draw(info_text).await };
        #[cfg(target_family = "wasm")]
        let future = send_wrapper::SendWrapper::new(future);
        Box::pin(future)
    }
}

impl RendererImpl {
    fn update(
        &mut self,
        read_tickets: Layers<ReadTicket<'_>>,
        cursor: Option<&Cursor>,
    ) -> Result<(), RenderError> {
        let info = self.everything.update(
            read_tickets,
            &self.queue,
            cursor,
            &FrameBudget::PRACTICALLY_INFINITE,
        )?;
        self.flaws = info.flaws();
        Ok(())
    }

    async fn draw(&mut self, info_text: &str) -> Result<Rendering, RenderError> {
        // TODO: refactor so that this viewport read is done synchronously, outside the RendererImpl
        let viewport = self.viewport_source.get();

        // If we are using the noop testing backend, then it is expected that the image will be
        // missing.
        let adapter_flaws = if self.adapter_info.backend == wgpu::Backend::Noop {
            Flaws::OTHER
        } else {
            Flaws::empty()
        };

        if viewport.is_empty() {
            // GPU doesn't accept zero size, so we have to short-circuit it at this layer or we will
            // get a placeholder at-least-1-pixel size that EverythingRenderer uses internally.
            return Ok(Rendering {
                size: viewport.framebuffer_size,
                data: Vec::new(),
                flaws: Flaws::empty(),
            });
        }

        if self.viewport_dirty.get_and_clear() {
            self.color_texture = create_color_texture(&self.device, viewport);
        }

        let draw_info = self.everything.draw_frame_linear(&self.queue);
        let (post_cmd, post_flaws) = self.everything.add_info_text_and_postprocess(
            &self.queue,
            &self
                .color_texture
                .create_view(&wgpu::TextureViewDescriptor::default()),
            info_text,
        );
        self.queue.submit([post_cmd]);
        let image = init::get_image_from_gpu(
            &self.device,
            &self.queue,
            &self.color_texture,
            self.flaws | draw_info.flaws() | post_flaws | adapter_flaws,
        )
        .await;
        debug_assert_eq!(viewport.framebuffer_size, image.size);
        Ok(image)
    }
}

fn create_color_texture(device: &wgpu::Device, viewport: Viewport) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some("headless::Renderer::color_texture"),
        size: size2d_to_extent(viewport.framebuffer_size.max(ImageSize::splat(1))),
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8UnormSrgb,
        view_formats: &[],
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
    })
}
