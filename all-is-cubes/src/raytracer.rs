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

use std::convert::TryFrom as _;
use std::fmt;

use cgmath::{EuclideanSpace as _, InnerSpace as _, Point2, Vector3};
use cgmath::{Point3, Vector4};
use ouroboros::self_referencing;
#[cfg(feature = "rayon")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use crate::block::{recursive_ray, Evoxel, Resolution};
use crate::camera::{Camera, GraphicsOptions, LightingOption};
use crate::math::{smoothstep, GridCoordinate};
use crate::math::{Face, FreeCoordinate, GridPoint, Rgb, Rgba};
use crate::raycast::Ray;
use crate::space::{Grid, GridArray, PackedLight, Space};

mod pixel_buf;
pub use pixel_buf::*;

mod surface;
use surface::Surface;
// TODO: pub use surface::*;

mod text;
pub use text::*;

/// Precomputed data for raytracing a single frame of a single Space, and bearer of the
/// methods for actually performing raytracing.
pub struct SpaceRaytracer<P: PixelBuf>(SpaceRaytracerImpl<P>);

/// Helper struct for [`SpaceRaytracer`] so the details of [`ouroboros::self_referencing`]
/// aren't exposed.
#[self_referencing]
struct SpaceRaytracerImpl<P: PixelBuf> {
    blocks: Box<[TracingBlock<P::BlockData>]>,
    #[borrows(blocks)]
    #[covariant]
    cubes: GridArray<TracingCubeData<'this, P::BlockData>>,

    options: GraphicsOptions,
    sky_color: Rgb,
    packed_sky_color: PackedLight,
}

impl<P: PixelBuf> SpaceRaytracer<P> {
    /// Snapshots the given [`Space`] to prepare for raytracing it.
    pub fn new(space: &Space, options: GraphicsOptions) -> Self {
        let sky_color = space.physics().sky_color;
        #[allow(clippy::borrowed_box)]
        SpaceRaytracer(
            SpaceRaytracerImplBuilder {
                blocks: prepare_blocks::<P>(space),
                cubes_builder: |blocks: &Box<[TracingBlock<P::BlockData>]>| {
                    prepare_cubes::<P>(blocks, space)
                },
                options,
                sky_color,
                packed_sky_color: sky_color.into(),
            }
            .build(),
        )
    }

    /// Computes a single image pixel from the given ray.
    pub fn trace_ray(&self, ray: Ray) -> (P::Pixel, RaytraceInfo) {
        self.0.with(|impl_fields| {
            let cubes = impl_fields.cubes;
            let mut s: TracingState<P> = TracingState::default();
            for hit in ray.cast().within_grid(cubes.grid()) {
                if s.count_step_should_stop() {
                    break;
                }

                match &cubes[hit.cube_ahead()].block {
                    TracingBlock::Atom(pixel_block_data, color) => {
                        if color.fully_transparent() {
                            continue;
                        }
                        // TODO: To implement TransparencyOption::Volumetric we need to peek forward to the next change of color and find the distance between them, but only if the alpha is not 0 or 1. (Same here and in the recursive block case.)
                        s.trace_through_surface(
                            Surface {
                                block_data: pixel_block_data,
                                diffuse_color: *color,
                                illumination: match impl_fields.options.lighting_display {
                                    LightingOption::None => Rgb::ONE,
                                    LightingOption::Flat => self.get_lighting(hit.cube_behind()),
                                    LightingOption::Smooth => self.get_interpolated_light(
                                        hit.intersection_point(ray),
                                        hit.face(),
                                    ),
                                },
                                normal: hit.face(),
                            },
                            impl_fields.options,
                        );
                    }
                    TracingBlock::Recur(pixel_block_data, resolution, array) => {
                        let resolution = *resolution;
                        let sub_ray = recursive_ray(ray, hit.cube_ahead(), resolution);
                        let antiscale = FreeCoordinate::from(resolution).recip();
                        for subcube_hit in sub_ray.cast().within_grid(Grid::for_block(resolution)) {
                            if s.count_step_should_stop() {
                                break;
                            }
                            if let Some(voxel) = array.get(subcube_hit.cube_ahead()) {
                                s.trace_through_surface(
                                    Surface {
                                        block_data: pixel_block_data,
                                        diffuse_color: voxel.color,
                                        illumination: match impl_fields.options.lighting_display {
                                            LightingOption::None => Rgb::ONE,
                                            LightingOption::Flat => self.get_lighting(
                                                hit.cube_ahead()
                                                    + subcube_hit.face().normal_vector(),
                                            ),
                                            LightingOption::Smooth => self.get_interpolated_light(
                                                subcube_hit.intersection_point(sub_ray) * antiscale
                                                    + hit
                                                        .cube_ahead()
                                                        .map(FreeCoordinate::from)
                                                        .to_vec(),
                                                subcube_hit.face(),
                                            ),
                                        },
                                        normal: subcube_hit.face(),
                                    },
                                    impl_fields.options,
                                );
                            }
                        }
                    }
                }
            }
            s.finish(*impl_fields.sky_color)
        })
    }

    /// Compute a full image.
    ///
    /// The returned `[P::Pixel]` is in the usual left-right then top-bottom raster order;
    /// its dimensions are `camera.framebuffer_size`.
    ///
    /// TODO: Add a mechanism for incrementally rendering into a mutable buffer instead of
    /// all-at-once into a newly allocated one, for interactive use.
    pub fn trace_scene_to_image(&self, camera: &Camera) -> (Box<[P::Pixel]>, RaytraceInfo) {
        // This wrapper function ensures that the two implementations have consistent
        // signatures.
        self.trace_scene_to_image_impl(camera)
    }

    #[cfg(feature = "rayon")]
    fn trace_scene_to_image_impl(&self, camera: &Camera) -> (Box<[P::Pixel]>, RaytraceInfo) {
        let viewport = camera.viewport();
        let viewport_size = viewport.framebuffer_size.map(|s| s as usize);

        let output_iterator = (0..viewport_size.y)
            .into_par_iter()
            .map(move |ych| {
                let y = viewport.normalize_fb_y(ych);
                (0..viewport_size.x).into_par_iter().map(move |xch| {
                    let x = viewport.normalize_fb_x(xch);
                    self.trace_ray(camera.project_ndc_into_world(Point2::new(x, y)))
                })
            })
            .flatten();

        let (image, info_sum): (Vec<P::Pixel>, rayon_helper::ParExtSum<RaytraceInfo>) =
            output_iterator.unzip();

        (image.into_boxed_slice(), info_sum.result())
    }

    #[cfg(not(feature = "rayon"))]
    fn trace_scene_to_image_impl(&self, camera: &Camera) -> (Box<[P::Pixel]>, RaytraceInfo) {
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
                image.push(pixel);
            }
        }

        (image.into_boxed_slice(), total_info)
    }

    #[inline]
    fn get_packed_light(&self, cube: GridPoint) -> PackedLight {
        self.0.with(|impl_fields| {
            impl_fields
                .cubes
                .get(cube)
                .map(|b| b.lighting)
                .unwrap_or(*impl_fields.packed_sky_color)
        })
    }

    #[inline]
    fn get_lighting(&self, cube: GridPoint) -> Rgb {
        self.0.with(|impl_fields| {
            impl_fields
                .cubes
                .get(cube)
                .map(|b| b.lighting.value())
                .unwrap_or(*impl_fields.sky_color)
        })
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
}

impl<P: PixelBuf> fmt::Debug for SpaceRaytracer<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.with(|impl_fields| {
            f.debug_struct("SpaceRaytracer")
                .field("blocks.len", &impl_fields.blocks.len())
                .field("cubes.grid", &impl_fields.cubes.grid())
                .field("options", &impl_fields.options)
                .field("sky_color", &impl_fields.sky_color)
                .finish_non_exhaustive()
        })
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

/// Get block data out of [`Space`] (which is not [`Sync`], and not specialized for our
/// efficient use).
#[inline]
fn prepare_blocks<P: PixelBuf>(space: &Space) -> Box<[TracingBlock<P::BlockData>]> {
    space
        .block_data()
        .iter()
        .map(|block_data| {
            let evaluated = block_data.evaluated();
            let pixel_block_data = P::compute_block_data(block_data);
            if let Some(ref voxels) = evaluated.voxels {
                TracingBlock::Recur(pixel_block_data, evaluated.resolution, voxels.clone())
            } else {
                TracingBlock::Atom(pixel_block_data, evaluated.color)
            }
        })
        .collect()
}

/// Get cube data out of [`Space`] (which is not [`Sync`], and not specialized for our
/// efficient use).
#[inline]
#[allow(clippy::ptr_arg)] // no benefit
fn prepare_cubes<'a, P: PixelBuf>(
    indexed_block_data: &'a [TracingBlock<P::BlockData>],
    space: &Space,
) -> GridArray<TracingCubeData<'a, P::BlockData>> {
    space.extract(space.grid(), |index, _block, lighting| TracingCubeData {
        block: &indexed_block_data[index.unwrap() as usize],
        lighting,
    })
}

#[derive(Clone, Debug)]
struct TracingCubeData<'a, B: 'static> {
    block: &'a TracingBlock<B>,
    lighting: PackedLight,
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
        options: &GraphicsOptions,
    ) {
        if let Some(color) = surface.to_lit_color(options) {
            self.pixel_buf.add(color, surface.block_data);
        }
    }
}

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
