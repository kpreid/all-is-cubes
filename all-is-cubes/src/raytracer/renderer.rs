// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::fmt;

use cgmath::Vector2;
use futures_core::future::BoxFuture;
use image::RgbaImage;

use crate::apps::StandardCameras;
use crate::camera::{HeadlessRenderer, RenderError, Viewport};
use crate::character::Cursor;
use crate::content::palette;
use crate::listen::ListenableSource;
use crate::math::Rgba;
use crate::raytracer::{
    ColorBuf, PixelBuf, RaytraceInfo, RtBlockData, RtOptionsRef, UpdatingSpaceRaytracer,
};

/// Builds upon [`UpdatingSpaceRaytracer`] to make a complete [`HeadlessRenderer`],
/// following the scene and camera information in a [`StandardCameras`].
pub struct RtRenderer<D: RtBlockData = ()> {
    rt: Option<UpdatingSpaceRaytracer<D>>,
    cameras: StandardCameras,
    /// Adjusts the `cameras` viewport to control how many pixels are actually traced.
    /// The output images will alway
    size_policy: Box<dyn Fn(Viewport) -> Viewport + Send + Sync>,
    custom_options: ListenableSource<D::Options>,
}

impl<D: RtBlockData> RtRenderer<D>
where
    D::Options: Clone + Sync + 'static,
{
    /// * `cameras`: Scene to draw.
    /// * `size_policy`: Modifier to the `cameras`' provided viewport to control how many
    ///    pixels are actually traced.
    /// * `custom_options`: The custom options for the `D` block data type; see
    ///   [`RtBlockData`].
    pub fn new(
        cameras: StandardCameras,
        size_policy: Box<dyn Fn(Viewport) -> Viewport + Send + Sync>,
        custom_options: ListenableSource<D::Options>,
    ) -> Self {
        RtRenderer {
            rt: None,
            cameras,
            size_policy,
            custom_options,
        }
    }

    /// Update the renderer's internal copy of the scene from the data sources
    /// (`URef<Character>` etc.) it is tracking.
    ///
    /// Returns [`RenderError::Read`] if said sources are in use.
    /// In that case, the renderer is still functional but will have stale data.
    ///
    /// This method is equivalent to [`HeadlessRenderer::update()`] except for
    /// fitting the raytracer's needs and capabilities (works with all types;
    /// not `async`).
    pub fn update(&mut self, _cursor: Option<&Cursor>) -> Result<(), RenderError> {
        // TODO: raytracer needs to implement drawing the cursor
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
                Some(UpdatingSpaceRaytracer::new(
                    space,
                    self.cameras.graphics_options_source(),
                    self.custom_options.clone(),
                ))
            } else {
                None
            }
        }

        Ok(())
    }

    /// Produce an image of the current state of the scene this renderer was created to
    /// track, as of the last call to [`Self::update()`], with the given overlaid text.
    ///
    /// The image's dimensions are determined by the previously supplied
    /// [`StandardCameras`]’ viewport value as of the last call to [`Self::update()`],
    /// as affected by the `size_policy`. The provided `output` buffer must have exactly
    /// that length.
    ///
    /// This operation does not attempt to access the scene objects and therefore may be
    /// called while the [`Universe`] is being stepped, etc.
    ///
    /// This method is equivalent to [`HeadlessRenderer::draw()`] except that it works
    /// with any [`PixelBuf`] instead of requiring [`ColorBuf`] and [`Rgba`] output,
    /// is not async, and does not require `&mut self`.
    ///
    /// [`Universe`]: crate::universe::Universe
    pub fn draw<P, E, O, IF>(&self, info_text_fn: IF, encoder: E, output: &mut [O]) -> RaytraceInfo
    where
        P: PixelBuf<BlockData = D>,
        E: Fn(P) -> O + Send + Sync,
        O: Clone + Send + Sync, // Clone is used in the no-data case
        IF: FnOnce(&RaytraceInfo) -> String,
    {
        // TODO: implement drawing info text (can use embedded_graphics for that)

        let mut camera = self.cameras.cameras().world.clone();
        let viewport = (self.size_policy)(camera.viewport());
        camera.set_viewport(viewport);

        let options = RtOptionsRef {
            graphics_options: self.cameras.graphics_options(),
            custom_options: &*self.custom_options.get(),
        };

        let info = match &self.rt {
            Some(rt) => rt
                .get()
                .trace_scene_to_image::<P, &E, O>(&camera, &encoder, output),
            None => {
                output.fill(encoder(P::paint(palette::NO_WORLD_TO_SHOW, options)));
                RaytraceInfo::default()
            }
        };

        let info_text: String = info_text_fn(&info);
        if !info_text.is_empty() && self.cameras.cameras().world.options().debug_info_text {
            eg::draw_info_text(
                output,
                viewport,
                [
                    encoder(P::paint(Rgba::BLACK, options)),
                    encoder(P::paint(Rgba::WHITE, options)),
                ],
                &info_text,
            );
        }

        info
    }

    /// Returns the [`StandardCameras`] this renderer contains.
    ///
    /// TODO: Should this be a standard part of HeadlessRenderer and/or other traits? It's likely
    /// to be useful for dealing with cursors and such matters, I think.
    pub fn cameras(&self) -> &StandardCameras {
        &self.cameras
    }

    /// Returns the [`Viewport`] as of the last [`Self::update()`] as modified by the
    /// `size_policy`. That is, this reports the size of images that will be actually
    /// drawn.
    pub fn modified_viewport(&self) -> Viewport {
        (self.size_policy)(self.cameras.viewport())
    }
}

impl RtRenderer<()> {
    /// As [`Self::draw()`], but the output is an [`RgbaImage`], and
    /// [`Camera::post_process_color()`] is applied to the pixels.
    ///
    ///  [`Camera::post_process_color()`]: crate::camera::Camera::post_process_color
    pub fn draw_rgba(
        &self,
        info_text_fn: impl FnOnce(&RaytraceInfo) -> String,
    ) -> (RgbaImage, RaytraceInfo) {
        let camera = self.cameras.cameras().world.clone();

        let Vector2 {
            x: width,
            y: height,
        } = self.modified_viewport().framebuffer_size;
        let mut image = RgbaImage::new(width, height);

        let info = self.draw::<ColorBuf, _, [u8; 4], _>(
            info_text_fn,
            |pixel_buf| camera.post_process_color(Rgba::from(pixel_buf)).to_srgb8(),
            bytemuck::cast_slice_mut::<u8, [u8; 4]>(image.as_mut()),
        );

        (image, info)
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
        cursor: Option<&'a Cursor>,
    ) -> BoxFuture<'a, Result<(), RenderError>> {
        Box::pin(async move { self.update(cursor) })
    }

    fn draw<'a>(&'a mut self, info_text: &'a str) -> BoxFuture<'a, Result<RgbaImage, RenderError>> {
        // TODO: implement drawing info text (can use embedded_graphics for that)
        Box::pin(async {
            let (image, _rt_info) = self.draw_rgba(|_| info_text.to_string());
            Ok(image)
        })
    }
}

mod eg {
    use super::*;
    use crate::camera::info_text_drawable;
    use embedded_graphics::draw_target::DrawTarget;
    use embedded_graphics::draw_target::DrawTargetExt;
    use embedded_graphics::pixelcolor::BinaryColor;
    use embedded_graphics::prelude::{OriginDimensions, Point, Size};
    use embedded_graphics::primitives::Rectangle;
    use embedded_graphics::Drawable;
    use embedded_graphics::Pixel;

    pub fn draw_info_text<T: Clone>(
        output: &mut [T],
        viewport: Viewport,
        paint: [T; 2],
        info_text: &str,
    ) {
        let target = &mut eg::EgImageTarget {
            data: output,
            paint,
            size: Size {
                width: viewport.framebuffer_size.x,
                height: viewport.framebuffer_size.y,
            },
        };
        let shadow = info_text_drawable(info_text, BinaryColor::Off);
        // TODO: use .into_ok() when stable for infallible drawing
        shadow
            .draw(&mut target.translated(Point::new(0, -1)))
            .unwrap();
        shadow
            .draw(&mut target.translated(Point::new(0, 1)))
            .unwrap();
        shadow
            .draw(&mut target.translated(Point::new(-1, 0)))
            .unwrap();
        shadow
            .draw(&mut target.translated(Point::new(1, 0)))
            .unwrap();
        info_text_drawable(info_text, BinaryColor::On)
            .draw(target)
            .unwrap();
    }

    /// Just enough [`DrawTarget`] to implement info text drawing.
    pub(crate) struct EgImageTarget<'a, T> {
        data: &'a mut [T],
        paint: [T; 2],
        size: Size,
    }

    impl<T: Clone> DrawTarget for EgImageTarget<'_, T> {
        type Color = BinaryColor;
        type Error = std::convert::Infallible;

        fn draw_iter<I>(&mut self, pixels: I) -> Result<(), Self::Error>
        where
            I: IntoIterator<Item = Pixel<Self::Color>>,
        {
            let bounds = Rectangle {
                top_left: Point::zero(),
                size: self.size,
            };
            for Pixel(point, color) in pixels.into_iter() {
                if bounds.contains(point) {
                    self.data[point.y as usize * self.size.width as usize + point.x as usize] =
                        match color {
                            BinaryColor::Off => &self.paint[0],
                            BinaryColor::On => &self.paint[1],
                        }
                        .clone();
                }
            }
            Ok(())
        }
    }

    impl<T> OriginDimensions for EgImageTarget<'_, T> {
        fn size(&self) -> Size {
            self.size
        }
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
