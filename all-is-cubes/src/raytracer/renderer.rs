// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::fmt;

use cgmath::Vector2;
use futures_core::future::BoxFuture;
use image::RgbaImage;

use crate::apps::StandardCameras;
use crate::camera::{HeadlessRenderer, RenderError};
use crate::character::Cursor;
use crate::content::palette;
use crate::listen::ListenableSource;
use crate::math::Rgba;
use crate::raytracer::{ColorBuf, RtBlockData, UpdatingSpaceRaytracer};

/// Builds upon [`UpdatingSpaceRaytracer`] to make a complete [`HeadlessRenderer`],
/// following the scene and camera information in a [`StandardCameras`].
pub struct RtRenderer<D: RtBlockData = ()> {
    cameras: StandardCameras,
    rt: Option<UpdatingSpaceRaytracer<D>>,
}

impl RtRenderer {
    pub fn new(cameras: StandardCameras) -> Self {
        RtRenderer { cameras, rt: None }
    }
}

// manual impl avoids `D: Debug` bound
impl<D: RtBlockData> fmt::Debug for RtRenderer<D>
where
    D::Options: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("RtRenderer")
            .field("cameras", &self.cameras)
            .field("rt", &self.rt)
            .finish()
    }
}

impl HeadlessRenderer for RtRenderer<()> {
    fn update<'a>(
        &'a mut self,
        _cursor: Option<&'a Cursor>,
    ) -> BoxFuture<'a, Result<(), RenderError>> {
        // TODO: raytracer needs to implement drawing the cursor
        Box::pin(async {
            self.cameras.update();
            // TODO: this Option-synchronization pattern is recurring but also ugly ... look for ways to make it nicer
            let space = self.cameras.world_space().snapshot();
            if let Some(rt) = self
                .rt
                .as_mut()
                .filter(|rt| Some(rt.space()) == space.as_ref())
            {
                rt.update().map_err(RenderError::Read)?;
            } else {
                self.rt = if let Some(space) = space {
                    Some(UpdatingSpaceRaytracer::<()>::new(
                        space,
                        // TODO: StandardCameras should expose the options source
                        ListenableSource::constant(self.cameras.graphics_options().clone()),
                        ListenableSource::constant(()),
                    ))
                } else {
                    None
                }
            }

            Ok(())
        })
    }

    fn draw<'a>(
        &'a mut self,
        _info_text: &'a str,
    ) -> BoxFuture<'a, Result<RgbaImage, RenderError>> {
        // TODO: implement drawing info text (can use embedded_graphics for that)
        Box::pin(async {
            let camera = self.cameras.cameras().world.clone();
            let Vector2 {
                x: width,
                y: height,
            } = camera.viewport().framebuffer_size;

            let image: RgbaImage = match &mut self.rt {
                Some(rt) => {
                    let (image_vec, _info) = rt
                        .get()
                        .trace_scene_to_image::<ColorBuf, _, Rgba>(&camera, |pixel_buf| {
                            camera.post_process_color(Rgba::from(pixel_buf))
                        });

                    RgbaImage::from_raw(
                            width,
                            height,
                            Vec::from(image_vec)
                                .into_iter()
                                .flat_map(|color| color.to_srgb8())
                                .collect::<Vec<u8>>(),
                        )
                        .unwrap(/* can't happen: wrong dimensions */)
                }
                None => RgbaImage::from_pixel(
                    width,
                    height,
                    image::Rgba(palette::NO_WORLD_TO_SHOW.to_srgb8()),
                ),
            };

            Ok(image)
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
