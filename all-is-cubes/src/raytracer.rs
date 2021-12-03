// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Raytracer for [`Space`]s.
//!
//! ## Why?
//!
//! The original reason this exists is that I thought “we have [`raycast`](crate::raycast),
//! and that's nearly all the work, so why not?” Secondarily, it was written before
//! the [mesh-based renderer](crate::lum), and was useful as a cross-check since
//! it is much simpler. It continues to serve as a “reference implementation” and is used
//! by the terminal UI and in unit tests via [`print_space`].

use std::fmt;

use cgmath::{EuclideanSpace as _, InnerSpace as _, Point2, Vector2, Vector3};
use cgmath::{Point3, Vector4};
use ordered_float::NotNan;
#[cfg(feature = "rayon")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use crate::block::{Evoxel, Resolution, AIR};
use crate::camera::{Camera, GraphicsOptions, TransparencyOption};
use crate::math::{smoothstep, GridCoordinate};
use crate::math::{Face, FreeCoordinate, GridPoint, Rgb, Rgba};
use crate::raycast::Ray;
use crate::space::{BlockIndex, GridArray, PackedLight, Space, SpaceBlockData};

mod pixel_buf;
pub use pixel_buf::*;

mod surface;
use surface::{DepthIter, DepthStep, Span, Surface, SurfaceIter, TraceStep};
// TODO: pub use surface::*;

mod text;
pub use text::*;

/// Precomputed data for raytracing a single frame of a single [`Space`], and bearer of
/// the methods for actually performing raytracing.
pub struct SpaceRaytracer<P: PixelBuf> {
    blocks: Vec<TracingBlock<P::BlockData>>,
    cubes: GridArray<TracingCubeData>,

    options: GraphicsOptions,
    sky_color: Rgb,
    packed_sky_color: PackedLight,
}

impl<P: PixelBuf> SpaceRaytracer<P> {
    /// Snapshots the given [`Space`] to prepare for raytracing it.
    pub fn new(space: &Space, options: GraphicsOptions) -> Self {
        let sky_color = space.physics().sky_color;
        SpaceRaytracer {
            blocks: prepare_blocks::<P>(space),
            cubes: prepare_cubes(space),
            options,
            sky_color,
            packed_sky_color: sky_color.into(),
        }
    }

    /// Computes a single image pixel from the given ray.
    pub fn trace_ray(&self, ray: Ray) -> (P::Pixel, RaytraceInfo) {
        let t_to_absolute_distance = ray.direction.magnitude();
        let mut s: TracingState<P> = TracingState::default();
        let surface_iter = SurfaceIter::new(self, ray);

        // Use the more expensive volumetric tracing strategy only if we need it.
        match self.options.transparency {
            TransparencyOption::Volumetric => {
                for step in DepthIter::new(surface_iter) {
                    if s.count_step_should_stop() {
                        break;
                    }

                    match step {
                        DepthStep::Invisible => {
                            // Side effect: called count_step_should_stop.
                        }
                        DepthStep::Span(Span {
                            mut surface,
                            exit_t_distance,
                        }) => {
                            debug_assert!(!surface.diffuse_color.fully_transparent());

                            let thickness = ((exit_t_distance - surface.t_distance)
                                * t_to_absolute_distance)
                                as f32;

                            // Convert alpha to transmittance (light transmitted / light received).
                            let unit_transmittance =
                                1.0 - surface.diffuse_color.alpha().into_inner();
                            // Adjust transmittance for the thickness relative to an assumed 1.0 thickness.
                            let depth_transmittance = unit_transmittance.powf(thickness);
                            // Convert back to alpha.
                            // TODO: skip NaN check ... this may require refactoring Surface usage.
                            // We might also benefit from an "UncheckedRgba" concept.
                            surface.diffuse_color = surface
                                .diffuse_color
                                .to_rgb()
                                .with_alpha(NotNan::new(1.0 - depth_transmittance).unwrap());

                            s.trace_through_surface(surface, self);
                        }
                    }
                }
            }
            _ => {
                for step in surface_iter {
                    if s.count_step_should_stop() {
                        break;
                    }

                    use TraceStep::*;
                    match step {
                        Invisible { .. } | EnterBlock { .. } => {
                            // Side effect: called count_step_should_stop.
                        }
                        EnterSurface(surface) => {
                            debug_assert!(!surface.diffuse_color.fully_transparent());
                            s.trace_through_surface(surface, self);
                        }
                    }
                }
            }
        }
        s.finish(self.sky_color)
    }

    /// Compute a full image.
    ///
    /// The returned data is in the usual left-right then top-bottom raster order;
    /// its dimensions are `camera.framebuffer_size`.
    ///
    /// `encoder` may be used to transform the output of the `PixelBuf`.
    ///
    /// TODO: Add a mechanism for incrementally rendering into a mutable buffer instead of
    /// all-at-once into a newly allocated one, for interactive use.
    pub fn trace_scene_to_image<E, O>(
        &self,
        camera: &Camera,
        encoder: E,
    ) -> (Box<[O]>, RaytraceInfo)
    where
        E: Fn(P::Pixel) -> O + Send + Sync,
        O: Send + Sync,
    {
        // This wrapper function ensures that the two implementations have consistent
        // signatures.
        self.trace_scene_to_image_impl(camera, encoder)
    }

    #[cfg(feature = "rayon")]
    fn trace_scene_to_image_impl<E, O>(
        &self,
        camera: &Camera,
        encoder: E,
    ) -> (Box<[O]>, RaytraceInfo)
    where
        E: Fn(P::Pixel) -> O + Send + Sync,
        O: Send + Sync,
    {
        let viewport = camera.viewport();
        let viewport_size = viewport.framebuffer_size.map(|s| s as usize);
        let encoder = &encoder; // make shareable

        let output_iterator = (0..viewport_size.y)
            .into_par_iter()
            .map(move |ych| {
                let y = viewport.normalize_fb_y(ych);
                (0..viewport_size.x).into_par_iter().map(move |xch| {
                    let x = viewport.normalize_fb_x(xch);
                    let (pixel, info) =
                        self.trace_ray(camera.project_ndc_into_world(Point2::new(x, y)));
                    (encoder(pixel), info)
                })
            })
            .flatten();

        let (image, info_sum): (Vec<O>, rayon_helper::ParExtSum<RaytraceInfo>) =
            output_iterator.unzip();

        (image.into_boxed_slice(), info_sum.result())
    }

    #[cfg(not(feature = "rayon"))]
    fn trace_scene_to_image_impl<E, O>(
        &self,
        camera: &Camera,
        encoder: E,
    ) -> (Box<[O]>, RaytraceInfo)
    where
        E: Fn(P::Pixel) -> O + Send + Sync,
        O: Send + Sync,
    {
        let viewport = camera.viewport();
        let viewport_size = viewport.framebuffer_size.map(|s| s as usize);
        let mut image = Vec::with_capacity(viewport.pixel_count().expect("image too large"));

        let mut total_info = RaytraceInfo::default();
        for ych in 0..viewport_size.y {
            let y = viewport.normalize_fb_y(ych);
            for xch in 0..viewport_size.x {
                let x = viewport.normalize_fb_x(xch);
                let (pixel, info) =
                    self.trace_ray(camera.project_ndc_into_world(Point2::new(x, y)));
                total_info += info;
                image.push(encoder(pixel));
            }
        }

        (image.into_boxed_slice(), total_info)
    }

    #[inline]
    fn get_packed_light(&self, cube: GridPoint) -> PackedLight {
        self.cubes
            .get(cube)
            .map(|b| b.lighting)
            .unwrap_or(self.packed_sky_color)
    }

    #[inline]
    fn get_lighting(&self, cube: GridPoint) -> Rgb {
        self.cubes
            .get(cube)
            .map(|b| b.lighting.value())
            .unwrap_or(self.sky_color)
    }

    fn get_interpolated_light(&self, point: Point3<FreeCoordinate>, face: Face) -> Rgb {
        // This implementation is duplicated in GLSL at src/lum/shaders/fragment.glsl

        // About half the size of the smallest permissible voxel.
        let above_surface_epsilon = 0.5 / 256.0;

        // The position we should start with for light lookup and interpolation.
        let origin = point.to_vec() + face.normal_vector() * above_surface_epsilon;

        // Find linear interpolation coefficients based on where we are relative to
        // a half-cube-offset grid.
        let reference_frame = face.matrix(0).to_free();
        let mut mix_1 = (origin.dot(reference_frame.x.truncate()) - 0.5).rem_euclid(1.0);
        let mut mix_2 = (origin.dot(reference_frame.y.truncate()) - 0.5).rem_euclid(1.0);

        // Ensure that mix <= 0.5, i.e. the 'near' side below is the side we are on
        fn flip_mix(
            mix: &mut FreeCoordinate,
            dir: Vector4<FreeCoordinate>,
        ) -> Vector3<FreeCoordinate> {
            let dir = dir.truncate();
            if *mix > 0.5 {
                *mix = 1.0 - *mix;
                -dir
            } else {
                dir
            }
        }
        let dir_1 = flip_mix(&mut mix_1, reference_frame.x);
        let dir_2 = flip_mix(&mut mix_2, reference_frame.y);

        // Modify interpolation by smoothstep to change the visual impression towards
        // "blurred blocks" and away from the diamond-shaped gradients of linear interpolation
        // which, being so familiar, can give an unfortunate impression of "here is
        // a closeup of a really low-resolution texture".
        let mix_1 = smoothstep(mix_1);
        let mix_2 = smoothstep(mix_2);

        // Retrieve light data, again using the half-cube-offset grid (this way we won't have edge artifacts).
        let get_light = |p: Vector3<FreeCoordinate>| {
            self.get_packed_light(Point3::from_vec(
                (origin + p).map(|s| s.floor() as GridCoordinate),
            ))
        };
        let lin_lo = -0.5;
        let lin_hi = 0.5;
        let near12 = get_light(lin_lo * dir_1 + lin_lo * dir_2);
        let near1far2 = get_light(lin_lo * dir_1 + lin_hi * dir_2);
        let near2far1 = get_light(lin_hi * dir_1 + lin_lo * dir_2);
        let mut far12 = get_light(lin_hi * dir_1 + lin_hi * dir_2);

        if !near1far2.valid() && !near2far1.valid() {
            // The far corner is on the other side of a diagonal wall, so should be
            // ignored to prevent light leaks.
            far12 = near12;
        }

        // Apply ambient occlusion.
        let near12 = near12.value_with_ambient_occlusion();
        let near1far2 = near1far2.value_with_ambient_occlusion();
        let near2far1 = near2far1.value_with_ambient_occlusion();
        let far12 = far12.value_with_ambient_occlusion();

        // Perform bilinear interpolation.
        fn mix(x: Vector4<f32>, y: Vector4<f32>, a: FreeCoordinate) -> Vector4<f32> {
            // This should be replaced with https://doc.rust-lang.org/nightly/std/primitive.f32.html#method.lerp when that's stable
            let a = a as f32;
            x * (1. - a) + y * a
        }
        let v = mix(
            mix(near12, near1far2, mix_2),
            mix(near2far1, far12, mix_2),
            mix_1,
        );
        Rgb::try_from(v.truncate() / v.w.max(0.1)).unwrap()
    }
}

impl<P: PixelBuf<Pixel = String>> SpaceRaytracer<P> {
    /// Raytrace to text, using any [`PixelBuf`] whose output is [`String`].
    ///
    /// `F` is the function accepting the output, and `E` is the type of error it may
    /// produce. This function-based interface is intended to abstract over the
    /// inconvenient difference between [`std::io::Write`] and [`std::fmt::Write`].
    ///
    /// After each line (row) of the image, `write(line_ending)` will be called.
    pub fn trace_scene_to_text<F, E>(
        &self,
        camera: &Camera,
        line_ending: &str,
        write: F,
    ) -> Result<RaytraceInfo, E>
    where
        F: FnMut(&str) -> Result<(), E>,
    {
        // This wrapper function ensures that the two implementations have consistent
        // signatures.
        self.trace_scene_to_text_impl(camera, line_ending, write)
    }

    #[cfg(feature = "rayon")]
    fn trace_scene_to_text_impl<F, E>(
        &self,
        camera: &Camera,
        line_ending: &str,
        mut write: F,
    ) -> Result<RaytraceInfo, E>
    where
        F: FnMut(&str) -> Result<(), E>,
    {
        let viewport = camera.viewport();
        let viewport_size = viewport.framebuffer_size.map(|s| s as usize);
        let output_iterator = (0..viewport_size.y)
            .into_par_iter()
            .map(move |ych| {
                let y = viewport.normalize_fb_y(ych);
                (0..viewport_size.x)
                    .into_par_iter()
                    .map(move |xch| {
                        let x = viewport.normalize_fb_x(xch);
                        self.trace_ray(camera.project_ndc_into_world(Point2::new(x, y)))
                    })
                    .chain(Some((line_ending.to_owned(), RaytraceInfo::default())).into_par_iter())
            })
            .flatten();

        let (text, info_sum): (String, rayon_helper::ParExtSum<RaytraceInfo>) =
            output_iterator.unzip();
        write(text.as_ref())?;

        Ok(info_sum.result())
    }

    #[cfg(not(feature = "rayon"))]
    fn trace_scene_to_text_impl<F, E>(
        &self,
        camera: &Camera,
        line_ending: &str,
        mut write: F,
    ) -> Result<RaytraceInfo, E>
    where
        F: FnMut(&str) -> Result<(), E>,
    {
        let mut total_info = RaytraceInfo::default();

        let viewport = camera.viewport();
        let viewport_size = viewport.framebuffer_size.map(|s| s as usize);
        for ych in 0..viewport_size.y {
            let y = viewport.normalize_fb_y(ych);
            for xch in 0..viewport_size.x {
                let x = viewport.normalize_fb_x(xch);
                let (text, info) = self.trace_ray(camera.project_ndc_into_world(Point2::new(x, y)));
                total_info += info;
                write(text.as_ref())?;
            }
            write(line_ending)?;
        }

        Ok(total_info)
    }

    pub fn trace_scene_to_string(&self, camera: &Camera, line_ending: &str) -> String {
        let mut out = String::with_capacity(
            camera.viewport().framebuffer_size.dot(Vector2::new(1, 1)) as usize,
        );
        self.trace_scene_to_text(camera, line_ending, |s| {
            out.push_str(s);
            Ok::<(), std::convert::Infallible>(())
        })
        .unwrap();
        out
    }
}

impl<P: PixelBuf> fmt::Debug for SpaceRaytracer<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SpaceRaytracer")
            .field("blocks.len", &self.blocks.len())
            .field("cubes.grid", &self.cubes.grid())
            .field("options", &self.options)
            .field("sky_color", &self.sky_color)
            .finish_non_exhaustive()
    }
}

/// Performance info from a [`SpaceRaytracer`] operation.
///
/// The contents of this structure are subject to change; use [`Debug`] to view it.
/// The [`Default`] value is the zero value.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
pub struct RaytraceInfo {
    cubes_traced: usize,
}
impl std::ops::AddAssign<RaytraceInfo> for RaytraceInfo {
    fn add_assign(&mut self, other: Self) {
        self.cubes_traced += other.cubes_traced;
    }
}
impl std::iter::Sum for RaytraceInfo {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        let mut sum = Self::default();
        for part in iter {
            sum += part;
        }
        sum
    }
}

/// Copy all block data out of [`Space`].
#[inline]
fn prepare_blocks<P: PixelBuf>(space: &Space) -> Vec<TracingBlock<P::BlockData>> {
    space.block_data().iter().map(prepare_block::<P>).collect()
}

/// Copy one block's data out of [`Space`].
#[inline]
fn prepare_block<P: PixelBuf>(block_data: &SpaceBlockData) -> TracingBlock<P::BlockData> {
    let evaluated = block_data.evaluated();
    let pixel_block_data = P::compute_block_data(block_data);
    if let Some(ref voxels) = evaluated.voxels {
        TracingBlock::Recur(pixel_block_data, evaluated.resolution, voxels.clone())
    } else {
        TracingBlock::Atom(pixel_block_data, evaluated.color)
    }
}

/// Get cube data out of [`Space`].
#[inline]
fn prepare_cubes(space: &Space) -> GridArray<TracingCubeData> {
    space.extract(space.grid(), |index, block_data, lighting| {
        TracingCubeData {
            block_index: index.unwrap(),
            lighting,
            always_invisible: block_data.block() == &AIR,
        }
    })
}

#[derive(Clone, Debug)]
struct TracingCubeData {
    block_index: BlockIndex,
    lighting: PackedLight,
    /// True if the block is [`AIR`].
    ///
    /// This special information allows us to skip an indirect memory access in this
    /// extremely common case. We could generalize it to any block which is fully
    /// invisible, but only if *the block is not an indirection* since if it is, the
    /// block data could change without signaling a cube change, and currently we don't
    /// have a mechanism to obtain that information from the Space.
    always_invisible: bool,
}

#[derive(Clone, Debug)]
enum TracingBlock<B: 'static> {
    Atom(B, Rgba),
    Recur(B, Resolution, GridArray<Evoxel>),
}

#[derive(Clone, Debug, Default)]
struct TracingState<P: PixelBuf> {
    /// Number of cubes traced through -- controlled by the caller, so not necessarily
    /// equal to the number of calls to [`Self::trace_through_surface()`].
    cubes_traced: usize,
    pixel_buf: P,
}
impl<P: PixelBuf> TracingState<P> {
    #[inline]
    fn count_step_should_stop(&mut self) -> bool {
        self.cubes_traced += 1;
        if self.cubes_traced > 1000 {
            // Abort excessively long traces.
            self.pixel_buf = Default::default();
            self.pixel_buf
                .add(Rgba::new(1.0, 1.0, 1.0, 1.0), &P::error_block_data());
            true
        } else {
            self.pixel_buf.opaque()
        }
    }

    fn finish(mut self, sky_color: Rgb) -> (P::Pixel, RaytraceInfo) {
        if self.cubes_traced == 0 {
            // Didn't intersect the world at all. Draw these as plain background.
            // TODO: Switch to using the sky color, unless debugging options are set.
            self.pixel_buf.hit_nothing();
        }

        self.pixel_buf
            .add(sky_color.with_alpha_one(), &P::sky_block_data());

        // Debug visualization of number of raytracing steps.
        // TODO: Make this togglable and less of a kludge — we'd like to be able to mix with
        // the regular color view, but PixelBuf doesn't make that easy.
        if false {
            self.pixel_buf = Default::default();
            self.pixel_buf.add(
                (rgb_const!(0.02, 0.002, 0.0) * self.cubes_traced as f32).with_alpha_one(),
                &P::sky_block_data(),
            );
        }

        (
            self.pixel_buf.result(),
            RaytraceInfo {
                cubes_traced: self.cubes_traced,
            },
        )
    }

    /// Apply the effect of a given surface color.
    #[inline]
    fn trace_through_surface(
        &mut self,
        surface: Surface<'_, P::BlockData>,
        rt: &SpaceRaytracer<P>,
    ) {
        if let Some(color) = surface.to_lit_color(rt) {
            self.pixel_buf.add(color, surface.block_data);
        }
    }
}

pub use updating::*;
mod updating;

#[cfg(feature = "rayon")]
mod rayon_helper {
    use rayon::iter::{IntoParallelIterator, ParallelExtend, ParallelIterator as _};
    use std::iter::{empty, once, Sum};

    /// Implements [`ParallelExtend`] to just sum things, so that
    /// [`ParallelIterator::unzip`] can produce a sum.
    #[derive(Clone, Copy, Debug, Default)]
    pub(crate) struct ParExtSum<T>(Option<T>);

    impl<T: Sum> ParExtSum<T> {
        pub fn result(self) -> T {
            self.0.unwrap_or_else(|| empty().sum())
        }
    }

    impl<T: Sum + Send> ParallelExtend<T> for ParExtSum<T> {
        fn par_extend<I>(&mut self, par_iter: I)
        where
            I: IntoParallelIterator<Item = T>,
        {
            let new = par_iter.into_par_iter().sum();
            // The reason we use an `Option` at all is to make it possible to move the current
            // value.
            self.0 = Some(match self.0.take() {
                None => new,
                Some(previous) => once(previous).chain(once(new)).sum(),
            });
        }
    }
}
