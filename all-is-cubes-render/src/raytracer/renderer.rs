use alloc::boxed::Box;
use alloc::string::String;
use alloc::sync::Arc;
use core::fmt;

use all_is_cubes::character::Cursor;
use all_is_cubes::content::palette;
use all_is_cubes::euclid::{self, point2, vec2};
use all_is_cubes::listen::{self, Source as _};
use all_is_cubes::math::{Rgba, ZeroOne};
use all_is_cubes::space::Space;
use all_is_cubes::universe::Handle;

use crate::camera::{
    Camera, FogOption, GraphicsOptions, Layers, Ndc, NdcPoint2, StandardCameras, Viewport,
    area_usize,
};
use crate::raytracer::{
    Accumulate, ColorBuf, RaytraceInfo, RtBlockData, RtOptionsRef, SpaceRaytracer,
    UpdatingSpaceRaytracer,
};
use crate::{Flaws, RenderError, Rendering};

#[cfg(any(doc, feature = "std"))]
use crate::HeadlessRenderer;

/// Builds upon [`UpdatingSpaceRaytracer`] to make a complete [`HeadlessRenderer`],
/// following the scene and camera information in a [`StandardCameras`].
pub struct RtRenderer<D: RtBlockData = ()> {
    rts: Layers<Option<UpdatingSpaceRaytracer<D>>>,

    cameras: StandardCameras,

    /// Adjusts the `cameras` viewport to control how many pixels are actually traced.
    /// The output images will alway
    size_policy: Box<dyn Fn(Viewport) -> Viewport + Send + Sync>,

    // TODO: this oughta be just provided by `StandardCameras`
    ui_graphics_options: listen::DynSource<Arc<GraphicsOptions>>,

    custom_options: listen::DynSource<Arc<D::Options>>,
    /// Borrowable copy of the value in `custom_options`.
    custom_options_cache: Arc<D::Options>,

    /// Whether there was a [`Cursor`] to be drawn.
    /// Raytracing doesn't yet support cursors but we need to report that.
    had_cursor: bool,
}

impl<D> RtRenderer<D>
where
    D: RtBlockData<Options: Clone + Sync + 'static>,
{
    /// * `cameras`: Scene to draw.
    /// * `size_policy`: Modifier to the `cameras`' provided viewport to control how many
    ///   pixels are actually traced.
    /// * `custom_options`: The custom options for the `D` block data type; see
    ///   [`RtBlockData`].
    pub fn new(
        cameras: StandardCameras,
        size_policy: Box<dyn Fn(Viewport) -> Viewport + Send + Sync>,
        custom_options: listen::DynSource<Arc<D::Options>>,
    ) -> Self {
        RtRenderer {
            rts: Layers::<Option<_>>::default(),
            ui_graphics_options: Arc::new(
                cameras
                    .ui_view_source()
                    .map(|view| view.graphics_options.clone()),
            ),
            cameras,
            size_policy,
            custom_options_cache: custom_options.get(),
            custom_options,
            had_cursor: false,
        }
    }

    /// Update the renderer's internal copy of the scene from the data sources
    /// (`Handle<Character>` etc.) it is tracking.
    ///
    /// On success, returns whether any of the scene actually changed.
    ///
    /// Returns [`RenderError::Read`] if said sources are in use.
    /// In that case, the renderer is still functional but will have stale data.
    ///
    /// This method is equivalent to [`HeadlessRenderer::update()`] except for
    /// fitting the raytracer's needs and capabilities (works with all types;
    /// not `async`).
    pub fn update(&mut self, cursor: Option<&Cursor>) -> Result<bool, RenderError> {
        let mut anything_changed = false;

        // TODO: raytracer needs to implement drawing the cursor
        self.had_cursor = cursor.is_some();
        anything_changed |= self.cameras.update();
        self.custom_options_cache = self.custom_options.get();

        fn sync_space<D>(
            cached_rt: &mut Option<UpdatingSpaceRaytracer<D>>,
            optional_space: Option<&Handle<Space>>,
            graphics_options_source: listen::DynSource<Arc<GraphicsOptions>>,
            custom_options_source: &listen::DynSource<Arc<D::Options>>,
            anything_changed: &mut bool,
        ) -> Result<(), RenderError>
        where
            D: RtBlockData<Options: Clone + Sync + 'static>,
        {
            // TODO: this Option-synchronization pattern is recurring in renderers but also ugly ... look for ways to make it nicer

            // Check whether we need to replace the raytracer:
            match (optional_space, &mut *cached_rt) {
                // Matches already
                (Some(space), Some(rt)) if space == rt.space() => {}
                // Needs replacement
                (Some(space), rt) => {
                    *anything_changed = true;
                    *rt = Some(UpdatingSpaceRaytracer::new(
                        space.clone(),
                        graphics_options_source,
                        custom_options_source.clone(),
                    ));
                }
                // Space is None, so drop raytracer if any
                (None, c) => *c = None,
            }
            // Now that we have one if we should have one, update it.
            if let Some(rt) = cached_rt {
                *anything_changed |= rt.update().map_err(RenderError::Read)?;
            }
            Ok(())
        }
        sync_space(
            &mut self.rts.world,
            Option::as_ref(&self.cameras.world_space().get()),
            self.cameras.graphics_options_source(),
            &self.custom_options,
            &mut anything_changed,
        )?;
        sync_space(
            &mut self.rts.ui,
            self.cameras.ui_space(),
            self.ui_graphics_options.clone(),
            &self.custom_options,
            &mut anything_changed,
        )?;

        Ok(anything_changed)
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
    /// [`Universe`]: all_is_cubes::universe::Universe
    pub fn draw<P, E, O, IF>(&self, info_text_fn: IF, encoder: E, output: &mut [O]) -> RaytraceInfo
    where
        P: Accumulate<BlockData = D>,
        E: Fn(P) -> O + Send + Sync,
        O: Clone + Send + Sync, // Clone is used in the no-data case
        IF: FnOnce(&RaytraceInfo) -> String,
    {
        let scene = self.scene();
        let viewport = scene.cameras.world.viewport();

        assert_eq!(
            viewport.pixel_count(),
            Some(output.len()),
            "Viewport size does not match output buffer length",
        );

        let info = trace_image::trace_scene_to_image_impl(&scene, &encoder, output);

        let info_text: String = info_text_fn(&info);
        if !info_text.is_empty() && self.cameras.cameras().world.options().debug_info_text {
            eg::draw_info_text(
                output,
                viewport,
                [
                    encoder(P::paint(Rgba::BLACK, scene.options)),
                    encoder(P::paint(Rgba::WHITE, scene.options)),
                ],
                &info_text,
            );
        }

        info
    }

    /// Returns a [`RtScene`] which may be used to compute individual image pixels.
    ///
    /// This is the setup operation which [`RtRenderer::draw()`] is built upon;
    /// use it if you want to render partially or incrementally.
    pub fn scene<P>(&self) -> RtScene<'_, P>
    where
        P: Accumulate<BlockData = D>,
    {
        let mut cameras = self.cameras.cameras().clone();
        let viewport = (self.size_policy)(cameras.world.viewport());
        cameras.world.set_viewport(viewport);
        cameras.ui.set_viewport(viewport);

        let options =
            RtOptionsRef::_new_but_please_do_not_construct_this_if_you_are_not_all_is_cubes_itself(
                self.cameras.graphics_options(),
                &*self.custom_options_cache,
            );

        RtScene {
            rts: self
                .rts
                .as_refs()
                .map(|opt_urt| opt_urt.as_ref().map(|urt| urt.get())),
            cameras,
            options,
        }
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
    /// As [`Self::draw()`], but the output is a [`Rendering`], and
    /// [`Camera::post_process_color()`] is applied to the pixels.
    ///
    ///  [`Camera::post_process_color()`]: crate::camera::Camera::post_process_color
    pub fn draw_rgba(
        &self,
        info_text_fn: impl FnOnce(&RaytraceInfo) -> String,
    ) -> (Rendering, RaytraceInfo) {
        let camera = self.cameras.cameras().world.clone();
        let size = self.modified_viewport().framebuffer_size;

        let mut data = vec![[0; 4]; area_usize(size).unwrap()];
        let info = self.draw::<ColorBuf, _, [u8; 4], _>(
            info_text_fn,
            |pixel_buf| camera.post_process_color(Rgba::from(pixel_buf)).to_srgb8(),
            &mut data,
        );

        let options = self.cameras.graphics_options();
        let mut flaws = Flaws::empty();
        if options.bloom_intensity != ZeroOne::ZERO {
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
impl<D> fmt::Debug for RtRenderer<D>
where
    D: RtBlockData<Options: fmt::Debug>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            rts,
            cameras,
            size_policy: _,         // can't print a function
            ui_graphics_options: _, // derived
            custom_options,
            custom_options_cache: _, // not printed because its value is not meaningful when not in use
            had_cursor,
        } = self;
        // TODO: missing fields
        f.debug_struct("RtRenderer")
            .field("cameras", cameras)
            .field("rts", rts)
            .field("custom_options", custom_options)
            .field("had_cursor", had_cursor)
            .finish_non_exhaustive()
    }
}

/// This implementation is only available if the `std` feature is enabled.
#[cfg(feature = "std")] // can't provide `Sync` futures otherwise
impl HeadlessRenderer for RtRenderer<()> {
    fn update<'a>(
        &'a mut self,
        cursor: Option<&'a Cursor>,
    ) -> futures_core::future::BoxFuture<'a, Result<(), RenderError>> {
        Box::pin(async move {
            let _anything_changed = self.update(cursor)?;
            Ok(())
        })
    }

    fn draw<'a>(
        &'a mut self,
        info_text: &'a str,
    ) -> futures_core::future::BoxFuture<'a, Result<Rendering, RenderError>> {
        use alloc::string::ToString as _;

        Box::pin(async {
            let (rendering, _rt_info) = self.draw_rgba(|_| info_text.to_string());

            Ok(rendering)
        })
    }
}

/// Scene to be raytraced.
///
/// This may be obtained from [`RtRenderer::scene()`] and used to trace individual rays,
/// rather than an entire image.
///
/// Differs from [`SpaceRaytracer`]
/// in that it includes the [`Camera`]s (thus accepting screen-space coordinates
/// rather than a world-space ray) and [`Layers`] rather than one space.
///
/// Obtain this by calling [`RtRenderer::scene()`].
pub struct RtScene<'a, P: Accumulate> {
    rts: Layers<Option<&'a SpaceRaytracer<P::BlockData>>>,
    /// Cameras *with* `size_policy` applied.
    cameras: Layers<Camera>,
    options: RtOptionsRef<'a, <P::BlockData as RtBlockData>::Options>,
}

impl<P: Accumulate> fmt::Debug for RtScene<'_, P>
where
    <P::BlockData as RtBlockData>::Options: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            rts,
            cameras,
            options,
        } = self;
        f.debug_struct("RtScene")
            .field("rts", rts)
            .field("cameras", cameras)
            .field("options", options)
            .finish()
    }
}

impl<P: Accumulate> Clone for RtScene<'_, P> {
    fn clone(&self) -> Self {
        Self {
            rts: self.rts,
            cameras: self.cameras.clone(),
            options: self.options,
        }
    }
}

impl<P: Accumulate> RtScene<'_, P> {
    /// Given the `patch` which is the bounding box of a single image pixel in normalized device
    /// coordinates (range -1 to 1), produce the [`Accumulate`]d value of that pixel in this scene.
    #[inline]
    pub fn trace_patch(&self, patch: NdcRect) -> (P, RaytraceInfo) {
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
    if camera.options().antialiasing.is_strongly_enabled() {
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
                camera.project_ndc_into_world(point_within_patch(patch, SAMPLE_POINTS[i])),
                include_sky,
            );
            info += i;
            p
        });
        (P::mean(samples), info)
    } else {
        space.trace_ray(camera.project_ndc_into_world(patch.center()), include_sky)
    }
}

/// A rectangle in normalized device coordinates (-1 to 1 is the viewport).
type NdcRect = euclid::Box2D<f64, Ndc>;

fn point_within_patch(patch: NdcRect, uv: euclid::default::Vector2D<f64>) -> NdcPoint2 {
    patch.min + (patch.max - patch.min).component_mul(uv.cast_unit())
}

/// Threaded and non-threaded implementations of generating a full image.
/// TODO: The design of this code (and its documentation) are slightly residual from
/// when `trace_scene_to_image()` was a public interface. Revisit them.
mod trace_image {
    use super::*;

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
    #[cfg(feature = "auto-threads")]
    pub(super) fn trace_scene_to_image_impl<P, E, O>(
        scene: &RtScene<'_, P>,
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
        let viewport_size = viewport.framebuffer_size.to_usize();
        let encoder = &encoder; // make shareable

        // x.max(1) is zero-sized-viewport protection; the chunk size will be wrong, but there
        // will be zero chunks anyway.
        output
            .par_chunks_mut(viewport_size.width.max(1))
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
                            min: point2(x0, y0),
                            max: point2(x1, y1),
                        });
                        *pixel_out = encoder(pixel);
                        info
                    })
            })
            .flatten()
            .sum() // sum of info
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
    #[cfg(not(feature = "auto-threads"))]
    pub(super) fn trace_scene_to_image_impl<P, E, O>(
        scene: &RtScene<'_, P>,
        encoder: E,
        output: &mut [O],
    ) -> RaytraceInfo
    where
        P: Accumulate,
        E: Fn(P) -> O + Send + Sync,
        O: Send + Sync,
    {
        let viewport = scene.cameras.world.viewport();
        let viewport_size = viewport.framebuffer_size.to_usize();

        let mut total_info = RaytraceInfo::default();
        let mut index = 0;
        let mut y0 = viewport.normalize_fb_y_edge(0);
        for y_edge in 1..=viewport_size.height {
            let y1 = viewport.normalize_fb_y_edge(y_edge);
            let mut x0 = viewport.normalize_fb_x_edge(0);
            for x_edge in 1..=viewport_size.width {
                let x1 = viewport.normalize_fb_x_edge(x_edge);
                let (pixel, info) = scene.trace_patch(NdcRect {
                    min: point2(x0, y0),
                    max: point2(x1, y1),
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
    use crate::info_text_drawable;
    use embedded_graphics::Drawable;
    use embedded_graphics::Pixel;
    use embedded_graphics::draw_target::DrawTarget;
    use embedded_graphics::draw_target::DrawTargetExt;
    use embedded_graphics::pixelcolor::BinaryColor;
    use embedded_graphics::prelude::{OriginDimensions, Point, Size};
    use embedded_graphics::primitives::Rectangle;

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
                width: viewport.framebuffer_size.width,
                height: viewport.framebuffer_size.height,
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
            for Pixel(point, color) in pixels {
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
    use all_is_cubes::universe::Universe;
    use all_is_cubes::util::assert_conditional_send_sync;
    use core::convert::identity;

    #[test]
    fn renderer_is_send_sync() {
        assert_conditional_send_sync::<RtRenderer>()
    }

    #[test]
    fn custom_options_are_updated() {
        #[derive(Clone, Copy, Debug, Default, PartialEq)]
        struct CatchCustomOptions {
            custom_options: &'static str,
        }
        impl RtBlockData for CatchCustomOptions {
            type Options = &'static str;
            fn from_block(
                options: RtOptionsRef<'_, Self::Options>,
                _: &all_is_cubes::space::SpaceBlockData,
            ) -> Self {
                CatchCustomOptions {
                    custom_options: options.custom_options,
                }
            }
            fn error(options: RtOptionsRef<'_, Self::Options>) -> Self {
                CatchCustomOptions {
                    custom_options: options.custom_options,
                }
            }
            fn sky(options: RtOptionsRef<'_, Self::Options>) -> Self {
                CatchCustomOptions {
                    custom_options: options.custom_options,
                }
            }
        }
        impl Accumulate for CatchCustomOptions {
            type BlockData = CatchCustomOptions;
            fn opaque(&self) -> bool {
                self.custom_options.is_empty()
            }
            fn add(&mut self, _: ColorBuf, block_data: &Self::BlockData) {
                if self.custom_options.is_empty() {
                    *self = *block_data;
                }
            }
            fn mean<const N: usize>(_: [Self; N]) -> Self {
                unimplemented!()
            }
        }

        let universe = Universe::new();
        let cameras = StandardCameras::from_constant_for_test(
            GraphicsOptions::UNALTERED_COLORS,
            Viewport::with_scale(1.0, [1, 1]),
            &universe,
        );

        // Change the options after the renderer is created.
        let custom_options = listen::Cell::new(Arc::new("before"));
        let mut renderer = RtRenderer::new(cameras, Box::new(identity), custom_options.as_source());
        custom_options.set(Arc::new("after"));

        // See what options value is used.
        let mut result = [CatchCustomOptions::default()];
        renderer.update(None).unwrap();
        renderer.draw(|_| String::new(), identity, &mut result);

        assert_eq!(
            result,
            [CatchCustomOptions {
                custom_options: "after"
            }]
        )
    }
}
