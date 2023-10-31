use alloc::boxed::Box;
use alloc::string::{String, ToString};
use core::fmt;

use euclid::{point2, vec2};
use ordered_float::NotNan;

use crate::camera::{
    AntialiasingOption, Camera, Flaws, FogOption, GraphicsOptions, HeadlessRenderer, Layers,
    NdcPoint2, RenderError, Rendering, StandardCameras, Viewport,
};
use crate::character::Cursor;
use crate::content::palette;
use crate::listen::ListenableSource;
use crate::math::{Rgba, VectorOps};
use crate::raytracer::{
    Accumulate, ColorBuf, RaytraceInfo, RtBlockData, RtOptionsRef, SpaceRaytracer,
    UpdatingSpaceRaytracer,
};
use crate::space::Space;
use crate::universe::URef;
use crate::util::maybe_sync::BoxFuture;

/// Builds upon [`UpdatingSpaceRaytracer`] to make a complete [`HeadlessRenderer`],
/// following the scene and camera information in a [`StandardCameras`].
pub struct RtRenderer<D: RtBlockData = ()> {
    rts: Layers<Option<UpdatingSpaceRaytracer<D>>>,

    cameras: StandardCameras,

    /// Adjusts the `cameras` viewport to control how many pixels are actually traced.
    /// The output images will alway
    size_policy: Box<dyn Fn(Viewport) -> Viewport + Send + Sync>,

    custom_options: ListenableSource<D::Options>,

    /// Whether there was a [`Cursor`] to be drawn.
    /// Raytracing doesn't yet support cursors but we need to report that.
    had_cursor: bool,
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
            rts: Layers::<Option<_>>::default(),
            cameras,
            size_policy,
            custom_options,
            had_cursor: false,
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
    pub fn update(&mut self, cursor: Option<&Cursor>) -> Result<(), RenderError> {
        // TODO: raytracer needs to implement drawing the cursor
        self.had_cursor = cursor.is_some();
        self.cameras.update();

        fn sync_space<D: RtBlockData>(
            cached_rt: &mut Option<UpdatingSpaceRaytracer<D>>,
            optional_space: Option<&URef<Space>>,
            graphics_options_source: &ListenableSource<GraphicsOptions>,
            custom_options_source: &ListenableSource<D::Options>,
        ) -> Result<(), RenderError>
        where
            D::Options: Clone + Sync + 'static,
        {
            // TODO: this Option-synchronization pattern is recurring in renderers but also ugly ... look for ways to make it nicer

            // Check whether we need to replace the raytracer:
            match (optional_space, &mut *cached_rt) {
                // Matches already
                (Some(space), Some(rt)) if space == rt.space() => {}
                // Needs replacement
                (Some(space), rt) => {
                    *rt = Some(UpdatingSpaceRaytracer::new(
                        space.clone(),
                        graphics_options_source.clone(),
                        custom_options_source.clone(),
                    ))
                }
                // Space is None, so drop raytracer if any
                (None, c) => *c = None,
            }
            // Now that we have one if we should have one, update it.
            if let Some(rt) = cached_rt {
                rt.update().map_err(RenderError::Read)?;
            }
            Ok(())
        }
        let gs = self.cameras.graphics_options_source();
        sync_space(
            &mut self.rts.world,
            Option::as_ref(&self.cameras.world_space().get()),
            &gs,
            &self.custom_options,
        )?;
        sync_space(
            &mut self.rts.ui,
            self.cameras.ui_space(),
            &gs,
            &self.custom_options,
        )?;

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
    /// with any [`Accumulate`] instead of requiring [`ColorBuf`] and [`Rgba`] output,
    /// is not async, and does not require `&mut self`.
    ///
    /// [`Universe`]: crate::universe::Universe
    pub fn draw<P, E, O, IF>(&self, info_text_fn: IF, encoder: E, output: &mut [O]) -> RaytraceInfo
    where
        P: Accumulate<BlockData = D>,
        E: Fn(P) -> O + Send + Sync,
        O: Clone + Send + Sync, // Clone is used in the no-data case
        IF: FnOnce(&RaytraceInfo) -> String,
    {
        let mut cameras = self.cameras.cameras().clone();
        let viewport = (self.size_policy)(cameras.world.viewport());
        cameras.world.set_viewport(viewport);
        cameras.ui.set_viewport(viewport);
        assert_eq!(
            viewport.pixel_count(),
            Some(output.len()),
            "Viewport size does not match output buffer length",
        );

        let options = RtOptionsRef {
            graphics_options: self.cameras.graphics_options(),
            custom_options: &*self.custom_options.get(),
        };

        let scene = RtScene {
            rts: self
                .rts
                .as_refs()
                .map(|opt_urt| opt_urt.as_ref().map(|urt| urt.get())),
            cameras: &cameras,
            options,
        };

        let info = trace_image::trace_scene_to_image_impl(scene, &encoder, output);

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
    /// TODO: Should this be a standard part of [`HeadlessRenderer`] and/or other traits?
    /// It's likely to be useful for dealing with cursors and such matters, I think.
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
    /// As [`Self::draw()`], but the output is an [`Rendering`], and
    /// [`Camera::post_process_color()`] is applied to the pixels.
    ///
    ///  [`Camera::post_process_color()`]: crate::camera::Camera::post_process_color
    pub fn draw_rgba(
        &self,
        info_text_fn: impl FnOnce(&RaytraceInfo) -> String,
    ) -> (Rendering, RaytraceInfo) {
        let camera = self.cameras.cameras().world.clone();
        let size = self.modified_viewport().framebuffer_size;

        let mut data = vec![[0; 4]; usize::try_from(size.x * size.y).unwrap()];
        let info = self.draw::<ColorBuf, _, [u8; 4], _>(
            info_text_fn,
            |pixel_buf| camera.post_process_color(Rgba::from(pixel_buf)).to_srgb8(),
            &mut data,
        );

        let options = self.cameras.graphics_options();
        let mut flaws = Flaws::empty();
        if options.bloom_intensity != NotNan::from(0u8) {
            flaws |= Flaws::NO_BLOOM;
        }
        if self.had_cursor {
            flaws |= Flaws::NO_CURSOR;
        }
        if !matches!(options.fog, FogOption::None) {
            flaws |= Flaws::NO_FOG;
        }

        (Rendering { size, data, flaws }, info)
    }
}

// manual impl avoids `D: Debug` bound
impl<D: RtBlockData> fmt::Debug for RtRenderer<D>
where
    D::Options: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: missing fields
        f.debug_struct("RtRenderer")
            .field("cameras", &self.cameras)
            .field("rts", &self.rts)
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

    fn draw<'a>(&'a mut self, info_text: &'a str) -> BoxFuture<'a, Result<Rendering, RenderError>> {
        Box::pin(async {
            let (rendering, _rt_info) = self.draw_rgba(|_| info_text.to_string());

            Ok(rendering)
        })
    }
}

/// Bundle of references to the current scene data in a [`RtRenderer`],
/// used to implement tracing individual rays independent of how they
/// are assembled into an image. Differs from [`SpaceRaytracer::trace_ray`]
/// in that it includes the cameras (thus accepting screen-space coordinates
/// rather than a lay) and [`Layers`] rather than one space.
struct RtScene<'a, P: Accumulate> {
    rts: Layers<Option<&'a SpaceRaytracer<P::BlockData>>>,
    /// Cameras *with* size_policy applied.
    cameras: &'a Layers<Camera>,
    options: RtOptionsRef<'a, <P::BlockData as RtBlockData>::Options>,
}

impl<'a, P: Accumulate> Clone for RtScene<'a, P> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<P: Accumulate> Copy for RtScene<'_, P> {}

impl<P: Accumulate> RtScene<'_, P> {
    /// Called from threaded or non-threaded `trace_scene_to_image()` implementations
    /// to produce a single image pixel.
    #[inline]
    fn trace_patch(&self, patch: NdcRect) -> (P, RaytraceInfo) {
        if let Some(ui) = self.rts.ui {
            let (pixel, info): (P, RaytraceInfo) =
                trace_patch_in_one_space(ui, &self.cameras.ui, patch, false);
            if pixel.opaque() {
                // TODO: We should be doing alpha blending, but doing that requires
                // having control over the `Accumulate` that trace_ray starts with.
                return (pixel, info);
            }
        }
        if let Some(world) = self.rts.world {
            return trace_patch_in_one_space(world, &self.cameras.world, patch, true);
        }
        (
            P::paint(palette::NO_WORLD_TO_SHOW, self.options),
            RaytraceInfo::default(),
        )
    }
}

fn trace_patch_in_one_space<P: Accumulate>(
    space: &SpaceRaytracer<<P as Accumulate>::BlockData>,
    camera: &Camera,
    patch: NdcRect,
    include_sky: bool,
) -> (P, RaytraceInfo) {
    match camera.options().antialiasing {
        AntialiasingOption::None | AntialiasingOption::IfCheap => {
            space.trace_ray(camera.project_ndc_into_world(patch.center()), include_sky)
        }
        AntialiasingOption::Always => {
            const N: usize = 4;
            const SAMPLE_POINTS: [euclid::default::Vector2D<f64>; N] = [
                vec2(1. / 8., 5. / 8.),
                vec2(3. / 8., 1. / 8.),
                vec2(5. / 8., 7. / 8.),
                vec2(7. / 8., 3. / 8.),
            ];
            let mut info = RaytraceInfo::default();
            let samples: [P; N] = core::array::from_fn(|i| {
                let (p, i) = space.trace_ray(
                    camera.project_ndc_into_world(patch.point_within(SAMPLE_POINTS[i])),
                    include_sky,
                );
                info += i;
                p
            });
            (P::mean(samples), info)
        }
    }
}

/// A rectangle in normalized device coordinates (-1 to 1 is the viewport).
// TODO(euclid migration): use euclid::Box2D
#[derive(Clone, Copy, Debug, PartialEq)]
struct NdcRect {
    low: NdcPoint2,
    high: NdcPoint2,
}
impl NdcRect {
    fn center(self) -> NdcPoint2 {
        (self.low + self.high.to_vector()) / 2.
    }

    fn point_within(self, uv: euclid::default::Vector2D<f64>) -> NdcPoint2 {
        self.low + (self.high - self.low).component_mul(uv.cast_unit())
    }
}

/// Threaded and non-threaded implementations of generating a full image.
/// TODO: The design of this code (and its documentation) are slightly residual from
/// when `trace_scene_to_image()` was a public interface. Revisit them.
mod trace_image {
    use super::*;
    use crate::raytracer::{Accumulate, RaytraceInfo};

    /// Compute a full image, writing it into `output`.
    ///
    /// The produced data is in the usual left-right then top-bottom raster order;
    /// its dimensions are `camera.framebuffer_size`.
    ///
    /// `encoder` may be used to transform the output of the [`Accumulate`] into the stored
    /// representation.
    ///
    /// Panics if `output`'s length does not match the area of `camera.framebuffer_size`.
    ///
    /// TODO: Add a mechanism for incrementally rendering (not 100% of pixels) for
    /// interactive use.
    #[cfg(feature = "threads")]
    pub(super) fn trace_scene_to_image_impl<P, E, O>(
        scene: RtScene<'_, P>,
        encoder: E,
        output: &mut [O],
    ) -> RaytraceInfo
    where
        P: Accumulate,
        E: Fn(P) -> O + Send + Sync,
        O: Send + Sync,
    {
        use rayon::iter::{
            IndexedParallelIterator as _, IntoParallelIterator as _, ParallelIterator as _,
        };
        use rayon::slice::ParallelSliceMut as _;

        let viewport = scene.cameras.world.viewport();
        let viewport_size = viewport.framebuffer_size.map(|s| s as usize);
        let encoder = &encoder; // make shareable

        // x.max(1) is zero-sized-viewport protection; the chunk size will be wrong, but there
        // will be zero chunks anyway.
        let total_info = output
            .par_chunks_mut(viewport_size.x.max(1))
            .enumerate()
            .map(move |(ych, raster_row)| {
                let y0 = viewport.normalize_fb_y_edge(ych);
                let y1 = viewport.normalize_fb_y_edge(ych + 1);
                raster_row
                    .into_par_iter()
                    .enumerate()
                    .map(move |(xch, pixel_out)| {
                        let x0 = viewport.normalize_fb_x_edge(xch);
                        let x1 = viewport.normalize_fb_x_edge(xch + 1);
                        let (pixel, info) = scene.trace_patch(NdcRect {
                            low: point2(x0, y0),
                            high: point2(x1, y1),
                        });
                        *pixel_out = encoder(pixel);
                        info
                    })
            })
            .flatten()
            .sum();

        total_info
    }

    /// Compute a full image, writing it into `output`.
    ///
    /// The produced data is in the usual left-right then top-bottom raster order;
    /// its dimensions are `camera.framebuffer_size`.
    ///
    /// `encoder` may be used to transform the output of the [`Accumulate`] into the stored
    /// representation.
    ///
    /// Panics if `output`'s length does not match the area of `camera.framebuffer_size`.
    ///
    /// TODO: Add a mechanism for incrementally rendering (not 100% of pixels) for
    /// interactive use.
    #[cfg(not(feature = "threads"))]
    pub(super) fn trace_scene_to_image_impl<P, E, O>(
        scene: RtScene<'_, P>,
        encoder: E,
        output: &mut [O],
    ) -> RaytraceInfo
    where
        P: Accumulate,
        E: Fn(P) -> O + Send + Sync,
        O: Send + Sync,
    {
        let viewport = scene.cameras.world.viewport();
        let viewport_size = viewport.framebuffer_size.map(|s| s as usize);

        let mut total_info = RaytraceInfo::default();
        let mut index = 0;
        let mut y0 = viewport.normalize_fb_y_edge(0);
        for y_edge in 1..=viewport_size.y {
            let y1 = viewport.normalize_fb_y_edge(y_edge);
            let mut x0 = viewport.normalize_fb_x_edge(0);
            for x_edge in 1..=viewport_size.x {
                let x1 = viewport.normalize_fb_x_edge(x_edge);
                let (pixel, info) = scene.trace_patch(NdcRect {
                    low: point2(x0, y0),
                    high: point2(x1, y1),
                });
                output[index] = encoder(pixel);
                total_info += info;
                index += 1;
                x0 = x1;
            }
            y0 = y1;
        }

        total_info
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
        let target = &mut EgImageTarget {
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
        type Error = core::convert::Infallible;

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
    use crate::util::assert_conditional_send_sync;

    use super::*;

    #[test]
    fn renderer_is_send_sync() {
        assert_conditional_send_sync::<RtRenderer>()
    }
}
