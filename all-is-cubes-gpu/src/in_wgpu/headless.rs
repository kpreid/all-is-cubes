use std::sync::Arc;

use futures_core::future::BoxFuture;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::camera::{self, Flaws, HeadlessRenderer, Viewport};
use all_is_cubes::character::Cursor;
use all_is_cubes::listen::{DirtyFlag, ListenableSource};

use crate::common::{FrameBudget, GraphicsResourceError};
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
    pub async fn from_adapter(
        adapter: Arc<wgpu::Adapter>,
    ) -> Result<Self, wgpu::RequestDeviceError> {
        let (device, queue) = adapter
            .request_device(&in_wgpu::EverythingRenderer::device_descriptor(), None)
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

        let viewport_dirty = DirtyFlag::listening(false, |l| viewport_source.listen(l));
        let viewport = viewport_source.snapshot();
        let color_texture = create_color_texture(&self.device, viewport);

        Renderer {
            device: self.device.clone(),
            queue: self.queue.clone(),
            color_texture,
            everything,
            viewport_source,
            viewport_dirty,
            flaws: Flaws::UNFINISHED, // unfinished because no update() yet
        }
    }
}

/// Implementation of [`HeadlessRenderer`] using [`wgpu`].
///
/// This is constructed from a [`wgpu::Device`] and a [`StandardCameras`] using [`Builder`],
/// and may then be used once or repeatedly to produce images of what those cameras see.
#[derive(Debug)]
pub struct Renderer {
    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
    color_texture: wgpu::Texture,
    everything: super::EverythingRenderer,
    viewport_source: ListenableSource<Viewport>,
    viewport_dirty: DirtyFlag,
    flaws: Flaws,
}

impl HeadlessRenderer for Renderer {
    fn update<'a>(
        &'a mut self,
        cursor: Option<&'a Cursor>,
    ) -> BoxFuture<'a, Result<(), camera::RenderError>> {
        Box::pin(async move {
            let info = self
                .everything
                .update(&self.queue, cursor, &FrameBudget::PRACTICALLY_INFINITE)
                .map_err(GraphicsResourceError::into_render_error_or_panic)?;
            self.flaws = info.flaws();
            Ok(())
        })
    }

    fn draw<'a>(
        &'a mut self,
        info_text: &'a str,
    ) -> BoxFuture<'a, Result<(image::RgbaImage, Flaws), camera::RenderError>> {
        let viewport = self.viewport_source.snapshot();
        if self.viewport_dirty.get_and_clear() {
            self.color_texture = create_color_texture(&self.device, viewport);
        }

        Box::pin(async move {
            let _draw_info = self.everything.draw_frame_linear(&self.queue).unwrap();
            self.everything.add_info_text_and_postprocess(
                &self.queue,
                &self.color_texture,
                info_text,
            );
            let image = init::get_image_from_gpu(
                &self.device,
                &self.queue,
                &self.color_texture,
                viewport.framebuffer_size,
            )
            .await;
            Ok((image, self.flaws))
        })
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
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
    })
}
