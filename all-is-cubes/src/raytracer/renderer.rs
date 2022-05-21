use futures_core::future::BoxFuture;
use image::RgbaImage;

use crate::apps::StandardCameras;
use crate::camera::{HeadlessRenderer, Overlays};
use crate::listen::ListenableSource;
use crate::math::Rgba;
use crate::raytracer::{ColorBuf, UpdatingSpaceRaytracer};

/// Builds upon [`UpdatingSpaceRaytracer`] to make a complete [`HeadlessRenderer`],
/// following the scene and camera information in a [`StandardCameras`].
#[derive(Debug)]
pub struct RtRenderer {
    cameras: StandardCameras,
    rt: UpdatingSpaceRaytracer<()>,
}

impl RtRenderer {
    pub fn new(cameras: StandardCameras) -> Self {
        let rt = UpdatingSpaceRaytracer::<()>::new(
            // TODO: We need to follow the cameras' character instead of snapshotting here
            cameras
                .world_space()
                .snapshot()
                .expect("No world space given!"),
            // TODO: StandardCameras should expose the options source
            ListenableSource::constant(cameras.graphics_options().clone()),
            ListenableSource::constant(()),
        );
        RtRenderer { cameras, rt }
    }
}

impl HeadlessRenderer for RtRenderer {
    fn render<'a>(&'a mut self, _overlays: Overlays<'a>) -> BoxFuture<'a, RgbaImage> {
        // TODO: implement drawing overlays
        Box::pin(async {
            let RtRenderer { cameras, rt } = self;
            let camera = cameras.cameras().world.clone();
            let (image, _info) = rt
                .get()
                .trace_scene_to_image::<ColorBuf, _, Rgba>(&camera, |pixel_buf| {
                    camera.post_process_color(Rgba::from(pixel_buf))
                });

            RgbaImage::from_raw(
                camera.viewport().framebuffer_size.x,
                camera.viewport().framebuffer_size.y,
                Vec::from(image)
                    .into_iter()
                    .flat_map(|color| color.to_srgb8())
                    .collect::<Vec<u8>>(),
            )
            .unwrap()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn _renderer_is_send_sync()
    where
        RtRenderer: Send + Sync + 'static,
    {
    }
}
