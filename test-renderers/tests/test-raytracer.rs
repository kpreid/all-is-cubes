// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use futures_core::future::BoxFuture;
use image::RgbaImage;
use test_renderers::{HeadlessRenderer, RendererFactory, RendererId};

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::math::Rgba;
use all_is_cubes::raytracer::{ColorBuf, UpdatingSpaceRaytracer};

#[allow(clippy::result_unit_err)]
#[cfg(test)]
#[tokio::main]
pub async fn main() -> Result<(), ()> {
    test_renderers::harness_main(
        RendererId::Raytracer,
        test_renderers::test_cases::all_tests,
        || std::future::ready(RtFactory),
    )
    .await
}

#[derive(Clone, Debug)]
struct RtFactory;

impl RendererFactory for RtFactory {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send> {
        let rt = UpdatingSpaceRaytracer::<()>::new(
            cameras
                .world_space()
                .snapshot()
                .expect("No world space given!"),
            // TODO: StandardCameras should expose the options source
            ListenableSource::constant(cameras.graphics_options().clone()),
            ListenableSource::constant(()),
        );
        Box::new(RtHeadless { cameras, rt })
    }

    fn id(&self) -> RendererId {
        RendererId::Raytracer
    }
}

struct RtHeadless {
    cameras: StandardCameras,
    rt: UpdatingSpaceRaytracer<()>,
}

impl HeadlessRenderer for RtHeadless {
    fn render(&mut self) -> BoxFuture<'_, RgbaImage> {
        Box::pin(async {
            let RtHeadless { cameras, rt } = self;
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
