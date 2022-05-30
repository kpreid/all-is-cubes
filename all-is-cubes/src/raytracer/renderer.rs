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
    /// as affected by the `size_policy`.
    ///
    /// This operation does not attempt to access the scene objects and therefore may be
    /// called while the [`Universe`] is being stepped, etc.
    ///
    /// This method is equivalent to [`HeadlessRenderer::draw()`] except that it works
    /// with any [`PixelBuf`] instead of requiring [`ColorBuf`] and [`Rgba`] output,
    /// is not async, and does not require `&mut self`.
    ///
    /// [`Universe`]: crate::universe::Universe
    // TODO: this should take an info text *function*
    // TODO: This should take an image buffer to write into
    pub fn draw<P, E, O>(&self, _info_text: &str, encoder: E) -> (Viewport, Vec<O>, RaytraceInfo)
    where
        P: PixelBuf<BlockData = D>,
        E: Fn(P) -> O + Send + Sync,
        O: Clone + Send + Sync, // Clone is used in the no-data case
    {
        // TODO: implement drawing info text (can use embedded_graphics for that)

        let mut camera = self.cameras.cameras().world.clone();
        camera.set_viewport((self.size_policy)(camera.viewport()));
        let pixel_count = camera.viewport().pixel_count().unwrap(/* checked by caller */);

        let encoded_placeholder = {
            let options = RtOptionsRef {
                graphics_options: self.cameras.graphics_options(),
                custom_options: &*self.custom_options.get(),
            };
            let mut pixel_buf = P::default();
            pixel_buf.add(palette::NO_WORLD_TO_SHOW, &D::sky(options));
            encoder(pixel_buf)
        };

        let mut data = vec![encoded_placeholder; pixel_count];
        match &self.rt {
            Some(rt) => {
                let info = rt
                    .get()
                    .trace_scene_to_image::<P, E, O>(&camera, encoder, &mut data);
                (camera.viewport(), data, info)
            }
            None => (camera.viewport(), data, RaytraceInfo::default()),
        }
    }

    /// Returns the [`StandardCameras`] this renderer contains.
    ///
    /// TODO: Should this be a standard part of HeadlessRenderer and/or other traits? It's likely
    /// to be useful for dealing with cursors and such matters, I think.
    pub fn cameras(&self) -> &StandardCameras {
        &self.cameras
    }
}

impl RtRenderer<()> {
    /// As [`Self::draw()`], but the output is an [`RgbaImage`], and
    /// [`Camera::post_process_color()`] is applied to the pixels.
    ///
    ///  [`Camera::post_process_color()`]: crate::camera::Camera::post_process_color
    pub fn draw_rgba(&self, info_text: &str) -> (RgbaImage, RaytraceInfo) {
        let camera = self.cameras.cameras().world.clone();

        let (viewport, image_data, info) = self
            .draw::<ColorBuf, _, [u8; 4]>(info_text, |pixel_buf| {
                camera.post_process_color(Rgba::from(pixel_buf)).to_srgb8()
            });

        let Vector2 {
            x: width,
            y: height,
        } = viewport.framebuffer_size;
        let image = RgbaImage::from_raw(width, height, image_data.into_iter().flatten().collect())
            .expect("RtRenderer's given size_policy was inconsistent");

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
            let (image, _rt_info) = self.draw_rgba(info_text);
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
