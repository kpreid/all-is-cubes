//! Implementation of [`HeadlessRenderer`] using [`wgpu`].

use std::sync::Arc;

use futures_channel::oneshot;
use futures_core::future::BoxFuture;

use all_is_cubes::camera::{self, Flaws, HeadlessRenderer, StandardCameras, Viewport};
use all_is_cubes::character::Cursor;
use all_is_cubes::listen::{DirtyFlag, ListenableSource};

use crate::common::{AdaptedInstant, FrameBudget, GraphicsResourceError};
use crate::in_wgpu::{self, init};

/// Builder for the headless [`Renderer`].
#[derive(Clone, Debug)]
pub struct Builder {
    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
    adapter: Arc<wgpu::Adapter>,
}

impl Builder {
    /// Create a [`Builder`] by obtaining a new [`wgpu::Device`] from the given adapter.
    #[cfg_attr(target_family = "wasm", allow(clippy::arc_with_non_send_sync))]
    pub async fn from_adapter(
        adapter: Arc<wgpu::Adapter>,
    ) -> Result<Self, wgpu::RequestDeviceError> {
        let (device, queue) = adapter
            .request_device(
                &in_wgpu::EverythingRenderer::<AdaptedInstant>::device_descriptor(adapter.limits()),
                None,
            )
            .await?;
        Ok(Self {
            device: Arc::new(device),
            queue: Arc::new(queue),
            adapter,
        })
    }

    /// Create a [`Renderer`] from the GPU connection in this builder and the given cameras.
    pub fn build(&self, cameras: StandardCameras) -> Renderer {
        let viewport_source = cameras.viewport_source();
        let everything = in_wgpu::EverythingRenderer::new(
            self.device.clone(),
            cameras,
            wgpu::TextureFormat::Rgba8UnormSrgb,
            &self.adapter,
        );

        let viewport_dirty = DirtyFlag::listening(false, &viewport_source);
        let viewport = viewport_source.snapshot();
        let color_texture = create_color_texture(&self.device, viewport);

        Renderer::wrap(RendererImpl {
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
    /// `wgpu` is currently entirely `!Send` on Wasm; use a channel and actor to handle that.
    #[cfg(target_family = "wasm")]
    inner: futures_channel::mpsc::Sender<RenderMsg>,
    #[cfg(not(target_family = "wasm"))]
    inner: RendererImpl,
}

/// Internals of [`Renderer`] to actually do the rendering.
#[derive(Debug)]
struct RendererImpl {
    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
    color_texture: wgpu::Texture,
    everything: super::EverythingRenderer<AdaptedInstant>,
    viewport_source: ListenableSource<Viewport>,
    viewport_dirty: DirtyFlag,
    flaws: Flaws,
}

/// Messages from [`Renderer`] to [`RendererImpl`].
pub(super) enum RenderMsg {
    Update(
        Option<Cursor>,
        oneshot::Sender<Result<(), camera::RenderError>>,
    ),
    Render(
        String,
        oneshot::Sender<Result<camera::Rendering, camera::RenderError>>,
    ),
}

impl Renderer {
    fn wrap(inner: RendererImpl) -> Renderer {
        Self {
            #[cfg(target_family = "wasm")]
            inner: {
                // On Wasm, wgpu objects are not Send. Therefore, spawn an actor which
                // explicitly runs on the main thread to own all of them.

                let (tx, mut rx) = futures_channel::mpsc::channel(1);
                wasm_bindgen_futures::spawn_local(async move {
                    use futures_util::stream::StreamExt as _;
                    let mut inner = inner;
                    while let Some(msg) = rx.next().await {
                        inner.handle(msg).await;
                    }
                });

                tx
            },
            #[cfg(not(target_family = "wasm"))]
            inner,
        }
    }

    async fn send_maybe_wait(&mut self, msg: RenderMsg) {
        #[cfg(target_family = "wasm")]
        {
            use futures_util::sink::SinkExt as _;
            self.inner
                .send(msg)
                .await
                .expect("Renderer actor unexpectedly disconnected");
        }
        #[cfg(not(target_family = "wasm"))]
        {
            self.inner.handle(msg).await;
        }
    }
}

impl HeadlessRenderer for Renderer {
    fn update<'a>(
        &'a mut self,
        cursor: Option<&'a Cursor>,
    ) -> BoxFuture<'a, Result<(), camera::RenderError>> {
        let (tx, rx) = oneshot::channel();
        Box::pin(async move {
            self.send_maybe_wait(RenderMsg::Update(cursor.cloned(), tx))
                .await;
            rx.await.unwrap()
        })
    }

    fn draw<'a>(
        &'a mut self,
        info_text: &'a str,
    ) -> BoxFuture<'a, Result<camera::Rendering, camera::RenderError>> {
        let (tx, rx) = oneshot::channel();
        Box::pin(async move {
            self.send_maybe_wait(RenderMsg::Render(info_text.to_owned(), tx))
                .await;
            rx.await.unwrap()
        })
    }
}

impl RendererImpl {
    async fn handle(&mut self, msg: RenderMsg) {
        match msg {
            RenderMsg::Update(cursor, reply) => {
                _ = reply.send(self.update(cursor.as_ref()));
            }
            RenderMsg::Render(info_text, reply) => {
                _ = reply.send(self.draw(&info_text).await);
            }
        }
    }

    fn update(&mut self, cursor: Option<&Cursor>) -> Result<(), camera::RenderError> {
        let info = self
            .everything
            .update(&self.queue, cursor, &FrameBudget::PRACTICALLY_INFINITE)
            .map_err(GraphicsResourceError::into_render_error_or_panic)?;
        self.flaws = info.flaws();
        Ok(())
    }

    async fn draw(&mut self, info_text: &str) -> Result<camera::Rendering, camera::RenderError> {
        // TODO: refactor so that this viewport read is done synchronously, outside the RendererImpl
        let viewport = self.viewport_source.snapshot();

        if viewport.is_empty() {
            // GPU doesn't accept zero size, so we have to short-circuit it at this layer or we will
            // get a placeholder at-least-1-pixel size that EverythingRenderer uses internally.
            return Ok(camera::Rendering {
                size: viewport.framebuffer_size,
                data: Vec::new(),
                flaws: Flaws::empty(),
            });
        }

        if self.viewport_dirty.get_and_clear() {
            self.color_texture = create_color_texture(&self.device, viewport);
        }

        let _draw_info = self.everything.draw_frame_linear(&self.queue).unwrap();
        self.everything
            .add_info_text_and_postprocess(&self.queue, &self.color_texture, info_text);
        let image = init::get_image_from_gpu(
            self.device.clone(),
            &self.queue,
            &self.color_texture,
            self.flaws,
        )
        .await;
        debug_assert_eq!(viewport.framebuffer_size, image.size);
        Ok(image)
    }
}

fn create_color_texture(device: &wgpu::Device, viewport: Viewport) -> wgpu::Texture {
    device.create_texture(&wgpu::TextureDescriptor {
        label: Some("headless::Renderer::color_texture"),
        size: wgpu::Extent3d {
            width: viewport.framebuffer_size.x.max(1),
            height: viewport.framebuffer_size.y.max(1),
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8UnormSrgb,
        view_formats: &[],
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
    })
}
