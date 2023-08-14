//! Raytracer for [`Space`]s.
//!
//! ## Why?
//!
//! The original reason this exists is that I thought “we have [`raycast`](crate::raycast),
//! and that's nearly all the work, so why not?” Secondarily, it was written before
//! the mesh-based renderer, and was useful as a cross-check since
//! it is much simpler. It continues to serve as a “reference implementation” and is used
//! by the terminal UI and in unit tests via [`print_space`].

use std::fmt;

use cgmath::{EuclideanSpace as _, InnerSpace as _, Point2, Vector2, Vector3, VectorSpace as _};
use cgmath::{Point3, Vector4};
use ordered_float::NotNan;
#[cfg(feature = "threads")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use crate::block::{Evoxels, Resolution, AIR};
use crate::camera::{Camera, GraphicsOptions, TransparencyOption};
use crate::math::{
    point_to_enclosing_cube, smoothstep, Face6, Face7, FreeCoordinate, GridAab, GridArray,
    GridMatrix, GridPoint, Rgb, Rgba,
};
use crate::raycast::Ray;
use crate::space::{BlockIndex, PackedLight, Space, SpaceBlockData};
use crate::util::{CustomFormat, StatusText};

mod pixel_buf;
pub use pixel_buf::*;
mod renderer;
pub use renderer::*;
mod surface;
use surface::{DepthIter, DepthStep, Span, Surface, SurfaceIter, TraceStep};
// TODO: pub use surface::*;
mod text;
pub use text::*;
pub use updating::*;
mod updating;

/// Precomputed data for raytracing a single frame of a single [`Space`], and bearer of
/// the methods for actually performing raytracing.
pub struct SpaceRaytracer<D: RtBlockData> {
    blocks: Vec<TracingBlock<D>>,
    cubes: GridArray<TracingCubeData>,

    graphics_options: GraphicsOptions,
    custom_options: D::Options,
    sky_color: Rgb,
    sky_data: D,
    packed_sky_color: PackedLight,
}

impl<D: RtBlockData> SpaceRaytracer<D> {
    /// Snapshots the given [`Space`] to prepare for raytracing it.
    pub fn new(
        space: &Space,
        graphics_options: GraphicsOptions,
        custom_options: D::Options,
    ) -> Self {
        let options = RtOptionsRef {
            graphics_options: &graphics_options,
            custom_options: &custom_options,
        };
        let sky_color = space.physics().sky_color;
        SpaceRaytracer {
            blocks: space
                .block_data()
                .iter()
                .map(|sbd| TracingBlock::<D>::from_block(options, sbd))
                .collect(),
            cubes: prepare_cubes(space),
            sky_color,
            sky_data: D::sky(options),
            packed_sky_color: sky_color.into(),

            graphics_options,
            custom_options,
        }
    }

    /// Construct a [`SpaceRaytracer`] with nothing to render.
    pub(crate) fn new_empty(
        sky_color: Rgb,
        graphics_options: GraphicsOptions,
        custom_options: D::Options,
    ) -> Self {
        let options = RtOptionsRef {
            graphics_options: &graphics_options,
            custom_options: &custom_options,
        };
        SpaceRaytracer {
            blocks: vec![],
            cubes: GridArray::from_elements(GridAab::from_lower_upper([0, 0, 0], [0, 0, 0]), [])
                .unwrap(),
            sky_color,
            sky_data: D::sky(options),
            packed_sky_color: sky_color.into(),

            graphics_options,
            custom_options,
        }
    }

    /// Computes a single image pixel from the given ray.
    pub fn trace_ray<P: PixelBuf<BlockData = D>>(
        &self,
        ray: Ray,
        include_sky: bool,
    ) -> (P, RaytraceInfo) {
        let options = RtOptionsRef {
            graphics_options: &self.graphics_options,
            custom_options: &self.custom_options,
        };

        let mut state: TracingState<P> = TracingState {
            t_to_absolute_distance: ray.direction.magnitude(),
            cubes_traced: 0,
            pixel_buf: P::default(),
        };
        let surface_iter = SurfaceIter::new(self, ray);

        // Use the more expensive volumetric tracing strategy only if we need it.
        match self.graphics_options.transparency {
            TransparencyOption::Volumetric => {
                for step in DepthIter::new(surface_iter) {
                    if state.count_step_should_stop(options) {
                        break;
                    }

                    match step {
                        DepthStep::Invisible => {
                            // Side effect: called count_step_should_stop.
                        }
                        DepthStep::Span(span) => {
                            debug_assert!(!span.surface.diffuse_color.fully_transparent());
                            state.trace_through_span(span, self);
                        }
                    }
                }
            }
            _ => {
                for step in surface_iter {
                    if state.count_step_should_stop(options) {
                        break;
                    }

                    use TraceStep::*;
                    match step {
                        Invisible { .. } | EnterBlock { .. } => {
                            // Side effect: called count_step_should_stop.
                        }
                        EnterSurface(surface) => {
                            debug_assert!(!surface.diffuse_color.fully_transparent());
                            state.trace_through_surface(surface, self);
                        }
                    }
                }
            }
        }
        state.finish(
            if include_sky {
                self.sky_color.with_alpha_one()
            } else {
                Rgba::TRANSPARENT
            },
            &self.sky_data,
        )
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

    fn get_interpolated_light(&self, point: Point3<FreeCoordinate>, face: Face7) -> Rgb {
        // This implementation is duplicated in WGSL in interpolated_space_light()
        // in all-is-cubes-gpu/src/in_wgpu/shaders/blocks-and-lines.wgsl.

        // About half the size of the smallest permissible voxel.
        let above_surface_epsilon = 0.5 / 256.0;

        // The position we should start with for light lookup and interpolation.
        let origin = point.to_vec() + face.normal_vector() * above_surface_epsilon;

        // Find linear interpolation coefficients based on where we are relative to
        // a half-cube-offset grid.
        let reference_frame = match Face6::try_from(face) {
            Ok(face) => face.face_transform(0).to_matrix(),
            Err(_) => GridMatrix::ZERO,
        }
        .to_free();
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
        let get_light = |p: Vector3<FreeCoordinate>| match point_to_enclosing_cube(
            Point3::from_vec(origin) + p,
        ) {
            Some(cube) => self.get_packed_light(cube),
            None => self.packed_sky_color,
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
        // TODO: most of the prior math for the mix values should be f32 already
        let v = Vector4::lerp(
            Vector4::lerp(near12, near1far2, mix_2 as f32),
            Vector4::lerp(near2far1, far12, mix_2 as f32),
            mix_1 as f32,
        );
        Rgb::try_from(v.truncate() / v.w.max(0.1)).unwrap()
    }
}

/// Text-specific methods.
impl<D: RtBlockData> SpaceRaytracer<D> {
    /// Raytrace to text, using any [`PixelBuf`] whose output can be [`String`].
    ///
    /// `F` is the function accepting the output, and `E` is the type of error it may
    /// produce. This function-based interface is intended to abstract over the
    /// inconvenient difference between [`std::io::Write`] and [`std::fmt::Write`].
    ///
    /// After each line (row) of the image, `write(line_ending)` will be called.
    pub fn trace_scene_to_text<P, F, E>(
        &self,
        camera: &Camera,
        line_ending: &str,
        write: F,
    ) -> Result<RaytraceInfo, E>
    where
        P: PixelBuf<BlockData = D> + Into<String>,
        F: FnMut(&str) -> Result<(), E>,
    {
        // This wrapper function ensures that the two implementations have consistent
        // signatures.
        self.trace_scene_to_text_impl::<P, F, E>(camera, line_ending, write)
    }

    #[cfg(feature = "threads")]
    fn trace_scene_to_text_impl<P, F, E>(
        &self,
        camera: &Camera,
        line_ending: &str,
        mut write: F,
    ) -> Result<RaytraceInfo, E>
    where
        P: PixelBuf<BlockData = D> + Into<String>,
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
                        let (buf, info) = self
                            .trace_ray::<P>(camera.project_ndc_into_world(Point2::new(x, y)), true);
                        (buf.into(), info)
                    })
                    .chain(Some((line_ending.to_owned(), RaytraceInfo::default())).into_par_iter())
            })
            .flatten();

        let (text, info_sum): (String, rayon_helper::ParExtSum<RaytraceInfo>) =
            output_iterator.unzip();
        write(text.as_str())?;

        Ok(info_sum.result())
    }

    #[cfg(not(feature = "threads"))]
    fn trace_scene_to_text_impl<P, F, E>(
        &self,
        camera: &Camera,
        line_ending: &str,
        mut write: F,
    ) -> Result<RaytraceInfo, E>
    where
        P: PixelBuf<BlockData = D> + Into<String>,
        F: FnMut(&str) -> Result<(), E>,
    {
        let mut total_info = RaytraceInfo::default();

        let viewport = camera.viewport();
        let viewport_size = viewport.framebuffer_size.map(|s| s as usize);
        for ych in 0..viewport_size.y {
            let y = viewport.normalize_fb_y(ych);
            for xch in 0..viewport_size.x {
                let x = viewport.normalize_fb_x(xch);
                let (buf, info) =
                    self.trace_ray::<P>(camera.project_ndc_into_world(Point2::new(x, y)), true);
                total_info += info;
                write(buf.into().as_str())?;
            }
            write(line_ending)?;
        }

        Ok(total_info)
    }

    /// As [`Self::trace_scene_to_text()`], but returning a string.
    pub fn trace_scene_to_string<P>(&self, camera: &Camera, line_ending: &str) -> String
    where
        P: PixelBuf<BlockData = D> + Into<String>,
    {
        let mut out = String::with_capacity(
            camera.viewport().framebuffer_size.dot(Vector2::new(1, 1)) as usize,
        );
        self.trace_scene_to_text::<P, _, _>(camera, line_ending, |s| {
            out.push_str(s);
            Ok::<(), std::convert::Infallible>(())
        })
        .unwrap();
        out
    }
}

// manual impl avoids `D: Debug` bound and avoids printing the entire grid
impl<D: RtBlockData> fmt::Debug for SpaceRaytracer<D>
where
    D::Options: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SpaceRaytracer")
            .field("blocks.len", &self.blocks.len())
            .field("cubes.bounds", &self.cubes.bounds())
            .field("graphics_options", &self.graphics_options)
            .field("custom_options", &self.custom_options)
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

impl CustomFormat<StatusText> for RaytraceInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _format_type: StatusText) -> fmt::Result {
        let &Self { cubes_traced } = self;
        write!(fmt, "Cubes traced: {cubes_traced}")
    }
}

/// Get cube data out of [`Space`].
#[inline]
fn prepare_cubes(space: &Space) -> GridArray<TracingCubeData> {
    space.extract(space.bounds(), |e| TracingCubeData {
        block_index: e.block_index(),
        lighting: e.light(),
        always_invisible: e.block_data().block() == &AIR,
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
struct TracingBlock<D> {
    block_data: D,
    // TODO: `Evoxels` carries more data than we actually need (color). Experiment with using a packed format.
    voxels: Evoxels,
}

impl<D: RtBlockData> TracingBlock<D> {
    fn from_block(
        options: RtOptionsRef<'_, D::Options>,
        space_block_data: &SpaceBlockData,
    ) -> Self {
        TracingBlock {
            block_data: D::from_block(options, space_block_data),
            voxels: space_block_data.evaluated().voxels.clone(),
        }
    }
}

/// Holds a [`PixelBuf`] and other per-ray state, and updates it
/// according to the things it encounters.
#[derive(Clone, Debug, Default)]
struct TracingState<P: PixelBuf> {
    /// Conversion factor from raycaster `t` values to “true” [`Space`] distance values
    /// where 1 unit = 1 block thickness.
    t_to_absolute_distance: f64,

    /// Number of cubes traced through -- controlled by the caller, so not necessarily
    /// equal to the number of calls to [`Self::trace_through_surface()`].
    cubes_traced: usize,

    pixel_buf: P,
}
impl<P: PixelBuf> TracingState<P> {
    #[inline]
    fn count_step_should_stop(
        &mut self,
        options: RtOptionsRef<'_, <P::BlockData as RtBlockData>::Options>,
    ) -> bool {
        self.cubes_traced += 1;
        if self.cubes_traced > 1000 {
            // Abort excessively long traces.
            self.pixel_buf = Default::default();
            self.pixel_buf
                .add(Rgba::new(1.0, 1.0, 1.0, 1.0), &P::BlockData::error(options));
            true
        } else {
            self.pixel_buf.opaque()
        }
    }

    fn finish(mut self, sky_color: Rgba, sky_data: &P::BlockData) -> (P, RaytraceInfo) {
        if self.cubes_traced == 0 {
            // Didn't intersect the world at all.
            // Inform the PixelBuf of this in case it wants to do something different.
            self.pixel_buf.hit_nothing();
        }

        self.pixel_buf.add(sky_color, sky_data);

        // Debug visualization of number of raytracing steps.
        // TODO: Make this togglable and less of a kludge — we'd like to be able to mix with
        // the regular color view, but PixelBuf doesn't make that easy.
        const DEBUG_STEPS: bool = false;
        if DEBUG_STEPS && self.pixel_buf.opaque() {
            self.pixel_buf = Default::default();
            self.pixel_buf.add(
                (rgb_const!(0.02, 0.002, 0.0) * self.cubes_traced as f32).with_alpha_one(),
                sky_data,
            );
        }

        (
            self.pixel_buf,
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
        rt: &SpaceRaytracer<P::BlockData>,
    ) {
        if let Some(color) = surface.to_lit_color(rt) {
            self.pixel_buf.add(color, surface.block_data);
        }
    }

    #[inline]
    fn trace_through_span(
        &mut self,
        span: Span<'_, P::BlockData>,
        rt: &SpaceRaytracer<P::BlockData>,
    ) {
        let Span {
            mut surface,
            exit_t_distance,
        } = span;

        let thickness =
            ((exit_t_distance - surface.t_distance) * self.t_to_absolute_distance) as f32;

        surface.diffuse_color = apply_transmittance(surface.diffuse_color, thickness);

        self.trace_through_surface(surface, rt);
    }
}

/// Given the alpha of a voxel color, and the thickness of that material passed through,
/// return the alpha that should be used for blending.
#[inline]
fn apply_transmittance(color: Rgba, thickness: f32) -> Rgba {
    // Convert alpha to transmittance (light transmitted / light received).
    let unit_transmittance = 1.0 - color.alpha().into_inner();
    // Adjust transmittance for the thickness relative to an assumed 1.0 thickness.
    let depth_transmittance = unit_transmittance.powf(thickness);
    // Convert back to alpha.
    // TODO: skip NaN check ... this may require refactoring Surface usage.
    // We might also benefit from an "UncheckedRgba" concept.
    let alpha = NotNan::new(1.0 - depth_transmittance).unwrap();

    color.to_rgb().with_alpha(alpha)
}

/// Minimal raytracing helper used by block evaluation to compute aggregate properties
/// of voxel blocks. Compared to the regular raytracer, it:
///
/// * Traces through `Evoxel`s instead of a `SpaceRaytracer`.
/// * Follows an axis-aligned ray only.
///
/// `origin` should be the first cube to trace through *within* the grid.
pub(crate) fn trace_axis_aligned<P: PixelBuf<BlockData = ()>>(
    voxels: &Evoxels,
    origin: GridPoint,
    direction: Face6,
    resolution: Resolution,
) -> P {
    let thickness = f32::from(resolution).recip();
    let step = direction.normal_vector();

    let mut cube = origin;
    let mut buf = P::default();

    while let Some(voxel) = voxels.get(cube) {
        buf.add(apply_transmittance(voxel.color, thickness), &());
        if buf.opaque() {
            break;
        }
        cube += step;
    }
    buf
}

#[cfg(feature = "threads")]
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
