//! Note: This module is hidden, and its contents re-exported as `all_is_cubes_render::raytracer`.
//! It is located in this crate so that it can be used by unit tests.

use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use core::marker::PhantomData;
use core::{fmt, mem};

use euclid::{Vector3D, vec3};
use manyfmt::Fmt;
/// Acts as polyfill for float methods
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use num_traits::float::Float as _;

#[cfg(feature = "auto-threads")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use crate::block::{AIR, Evoxels, Resolution};
use crate::camera::NdcPoint2;
use crate::camera::{Camera, GraphicsOptions, TransparencyOption};
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use crate::math::Euclid as _;
use crate::math::{
    Cube, Face6, Face7, FreeCoordinate, FreePoint, FreeVector, GridAab, GridMatrix, Intensity, Rgb,
    Rgba, Vol, ZeroOne, rgb_const, smoothstep,
};
use crate::raycast::{self, Ray, RayIsh};
use crate::space::{BlockIndex, BlockSky, PackedLight, Sky, Space, SpaceBlockData};
use crate::util::StatusText;

mod accum;
pub use accum::*;
mod surface;
use surface::{DepthIter, DepthStep, Span, Surface, SurfaceIter, TraceStep};
// TODO: pub use surface::*;
mod text;
pub use text::*;
pub use updating::*;
mod updating;

/// Precomputed data for raytracing a single frame of a single [`Space`], and bearer of
/// the methods for actually performing raytracing.
#[allow(clippy::module_name_repetitions, reason = "TODO: find better name")]
pub struct SpaceRaytracer<D: RtBlockData> {
    blocks: Vec<TracingBlock<D>>,
    cubes: Vol<Box<[TracingCubeData]>>,

    graphics_options: GraphicsOptions,
    custom_options: D::Options,
    sky: Sky,
    sky_data: D,
    block_sky: BlockSky,
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
        let sky = space.physics().sky.clone();
        SpaceRaytracer {
            blocks: space
                .block_data()
                .iter()
                .map(|sbd| TracingBlock::<D>::from_block(options, sbd))
                .collect(),
            cubes: prepare_cubes(space),
            block_sky: sky.for_blocks(),
            sky,
            sky_data: D::sky(options),

            graphics_options,
            custom_options,
        }
    }

    /// Construct a [`SpaceRaytracer`] with nothing to render.
    pub(crate) fn new_empty(
        sky: Sky,
        graphics_options: GraphicsOptions,
        custom_options: D::Options,
    ) -> Self {
        let options = RtOptionsRef {
            graphics_options: &graphics_options,
            custom_options: &custom_options,
        };
        SpaceRaytracer {
            blocks: Vec::new(),
            cubes: Vol::from_elements(GridAab::ORIGIN_EMPTY, []).unwrap(),
            block_sky: sky.for_blocks(),
            sky,
            sky_data: D::sky(options),

            graphics_options,
            custom_options,
        }
    }

    /// Computes a single image pixel from the given ray.
    pub fn trace_ray<P: Accumulate<BlockData = D>>(
        &self,
        ray: Ray,
        include_sky: bool,
    ) -> (P, RaytraceInfo) {
        self.trace_ray_impl::<P, Ray>(ray, include_sky)
    }

    /// Computes a single image pixel from the given ray.
    ///
    /// This is identical to [`Self::trace_ray()`] except that it can be more efficient.
    pub fn trace_axis_aligned_ray<P: Accumulate<BlockData = D>>(
        &self,
        ray: raycast::AaRay,
        include_sky: bool,
    ) -> (P, RaytraceInfo) {
        self.trace_ray_impl::<P, raycast::AaRay>(ray, include_sky)
    }

    fn trace_ray_impl<P: Accumulate<BlockData = D>, R: RayIsh>(
        &self,
        ray: R,
        include_sky: bool,
    ) -> (P, RaytraceInfo) {
        let options = RtOptionsRef {
            graphics_options: &self.graphics_options,
            custom_options: &self.custom_options,
        };

        let mut state: TracingState<P> = TracingState {
            t_to_absolute_distance: ray.direction().length(),
            cubes_traced: 0,
            accumulator: P::default(),
        };
        let surface_iter: SurfaceIter<'_, D, R::Caster> = SurfaceIter::new(self, ray);

        // Use the more expensive volumetric tracing strategy only if we need it.
        match self.graphics_options.transparency {
            TransparencyOption::Volumetric => {
                for step in DepthIter::new(surface_iter) {
                    if state.count_step_should_stop(options) {
                        break;
                    }

                    match step {
                        DepthStep::Invisible { .. } => {
                            // Side effect: called count_step_should_stop.
                        }
                        DepthStep::Span(span) => {
                            debug_assert!(span.surface.visible());
                            state.trace_through_span(span, self);
                        }
                        DepthStep::EnterBlock {
                            t_distance: _,
                            block_data,
                        } => state.accumulator.enter_block(block_data),
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
                        Invisible { .. } => {
                            // Side effect: called count_step_should_stop.
                        }
                        EnterBlock {
                            t_distance: _,
                            block_data,
                        } => state.accumulator.enter_block(block_data),
                        EnterSurface(surface) => {
                            debug_assert!(surface.visible());
                            state.trace_through_surface(&surface, self);
                        }
                    }
                }
            }
        }
        state.finish(
            if include_sky {
                self.sky.sample(ray.direction()).with_alpha_one()
            } else {
                Rgba::TRANSPARENT
            },
            &self.sky_data,
            self.graphics_options.debug_pixel_cost,
        )
    }

    #[inline]
    fn get_packed_light(&self, cube: Cube) -> PackedLight {
        match self.cubes.get(cube) {
            Some(b) => b.lighting,
            None => self.block_sky.light_outside(self.cubes.bounds(), cube),
        }
    }

    fn get_interpolated_light(&self, point: FreePoint, face: Face7) -> Rgb {
        // This implementation is duplicated in WGSL in interpolated_space_light()
        // in all-is-cubes-gpu/src/in_wgpu/shaders/blocks-and-lines.wgsl.

        // About half the size of the smallest permissible voxel.
        let above_surface_epsilon = 0.5 / 256.0;

        // The position we should start with for light lookup and interpolation.
        let origin = point.to_vector() + face.normal_vector() * above_surface_epsilon;

        // Find linear interpolation coefficients based on where we are relative to
        // a half-cube-offset grid.
        let reference_frame = match Face6::try_from(face) {
            Ok(face) => face.face_transform(0).to_matrix(),
            Err(_) => GridMatrix::ZERO,
        }
        .to_free();
        let reference_frame_x = reference_frame.transform_vector3d(vec3(1., 0., 0.));
        let reference_frame_y = reference_frame.transform_vector3d(vec3(0., 1., 0.));

        let mut mix_1 = (origin.dot(reference_frame_x) - 0.5).rem_euclid(1.0);
        let mut mix_2 = (origin.dot(reference_frame_y) - 0.5).rem_euclid(1.0);

        // Ensure that mix <= 0.5, i.e. the 'near' side below is the side we are on
        fn flip_mix(mix: &mut FreeCoordinate, dir: FreeVector) -> FreeVector {
            if *mix > 0.5 {
                *mix = 1.0 - *mix;
                -dir
            } else {
                dir
            }
        }
        let dir_1 = flip_mix(&mut mix_1, reference_frame_x);
        let dir_2 = flip_mix(&mut mix_2, reference_frame_y);

        // Modify interpolation by smoothstep to change the visual impression towards
        // "blurred blocks" and away from the diamond-shaped gradients of linear interpolation
        // which, being so familiar, can give an unfortunate impression of "here is
        // a closeup of a really low-resolution texture".
        let mix_1 = smoothstep(mix_1);
        let mix_2 = smoothstep(mix_2);

        // Retrieve light data, again using the half-cube-offset grid (this way we won't have edge artifacts).
        let get_light = |p: FreeVector| match Cube::containing(origin.to_point() + p) {
            Some(cube) => self.get_packed_light(cube),
            // Numerical overflow case -- shouldn't be terribly relevant.
            None => self.block_sky.mean(),
        };
        let lin_lo = -0.5;
        let lin_hi = 0.5;
        let near12 = get_light(dir_1 * lin_lo + dir_2 * lin_lo);
        let near1far2 = get_light(dir_1 * lin_lo + dir_2 * lin_hi);
        let near2far1 = get_light(dir_1 * lin_hi + dir_2 * lin_lo);
        let mut far12 = get_light(dir_1 * lin_hi + dir_2 * lin_hi);

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
        let [r, g, b, final_weight] = mix4(
            mix4(near12, near1far2, mix_2 as f32),
            mix4(near2far1, far12, mix_2 as f32),
            mix_1 as f32,
        );
        // Apply weight
        Rgb::try_from(Vector3D::from([r, g, b]) / final_weight.max(0.1)).unwrap()
    }
}

/// Text-specific methods.
impl<D: RtBlockData> SpaceRaytracer<D> {
    /// Raytrace to text, using any [`Accumulate`] whose output can be [`String`].
    ///
    /// After each line (row) of the image, including the last, `line_ending` will be inserted.
    pub fn to_text<'a, P>(
        &'a self,
        camera: &'a Camera,
        line_ending: &'a str,
    ) -> impl fmt::Display + 'a
    where
        P: Accumulate<BlockData = D> + Into<String> + 'a,
    {
        struct ToText<'a, D: RtBlockData, P> {
            rt: &'a SpaceRaytracer<D>,
            camera: &'a Camera,
            line_ending: &'a str,
            _p: PhantomData<fn() -> P>,
        }

        impl<D: RtBlockData, P: Accumulate<BlockData = D> + Into<String>> fmt::Display
            for ToText<'_, D, P>
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.rt
                    .trace_scene_to_text_impl::<P>(self.camera, self.line_ending, f)?;
                Ok(())
            }
        }

        ToText::<'a, D, P> {
            rt: self,
            camera,
            line_ending,
            _p: PhantomData,
        }
    }

    #[cfg(feature = "auto-threads")]
    fn trace_scene_to_text_impl<P>(
        &self,
        camera: &Camera,
        line_ending: &str,
        stream: &mut dyn fmt::Write,
    ) -> Result<RaytraceInfo, fmt::Error>
    where
        P: Accumulate<BlockData = D> + Into<String>,
    {
        let viewport = camera.viewport();
        let viewport_size = viewport.framebuffer_size.to_usize();
        let output_iterator = (0..viewport_size.height)
            .into_par_iter()
            .map(move |ych| {
                let y = viewport.normalize_fb_y(ych);
                (0..viewport_size.width)
                    .into_par_iter()
                    .map(move |xch| {
                        let x = viewport.normalize_fb_x(xch);
                        let (buf, info) = self.trace_ray::<P>(
                            camera.project_ndc_into_world(NdcPoint2::new(x, y)),
                            true,
                        );
                        (buf.into(), info)
                    })
                    .chain(
                        Some((String::from(line_ending), RaytraceInfo::default())).into_par_iter(),
                    )
            })
            .flatten();

        let (text, info_sum): (String, rayon_helper::ParExtSum<RaytraceInfo>) =
            output_iterator.unzip();
        stream.write_str(text.as_str())?;

        Ok(info_sum.result())
    }

    #[cfg(not(feature = "auto-threads"))]
    fn trace_scene_to_text_impl<P>(
        &self,
        camera: &Camera,
        line_ending: &str,
        stream: &mut dyn fmt::Write,
    ) -> Result<RaytraceInfo, fmt::Error>
    where
        P: Accumulate<BlockData = D> + Into<String>,
    {
        let mut total_info = RaytraceInfo::default();

        let viewport = camera.viewport();
        let viewport_size = viewport.framebuffer_size.to_usize();
        for ych in 0..viewport_size.height {
            let y = viewport.normalize_fb_y(ych);
            for xch in 0..viewport_size.width {
                let x = viewport.normalize_fb_x(xch);
                let (buf, info) =
                    self.trace_ray::<P>(camera.project_ndc_into_world(NdcPoint2::new(x, y)), true);
                total_info += info;
                stream.write_str(buf.into().as_str())?;
            }
            stream.write_str(line_ending)?;
        }

        Ok(total_info)
    }
}

// manual impl avoids `D: Debug` bound and avoids printing the entire grid
impl<D> fmt::Debug for SpaceRaytracer<D>
where
    D: RtBlockData<Options: fmt::Debug>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SpaceRaytracer")
            .field("blocks.len", &self.blocks.len())
            .field("cubes.bounds", &self.cubes.bounds())
            .field("graphics_options", &self.graphics_options)
            .field("custom_options", &self.custom_options)
            .field("sky", &self.sky)
            .finish_non_exhaustive()
    }
}

fn mix4(a: [f32; 4], b: [f32; 4], amount: f32) -> [f32; 4] {
    core::array::from_fn(|i| {
        let a = a[i];
        let b = b[i];
        a + (b - a) * amount
    })
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
impl core::ops::AddAssign<RaytraceInfo> for RaytraceInfo {
    fn add_assign(&mut self, other: Self) {
        self.cubes_traced += other.cubes_traced;
    }
}
impl core::iter::Sum for RaytraceInfo {
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

impl Fmt<StatusText> for RaytraceInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        let &Self { cubes_traced } = self;
        write!(fmt, "Cubes traced: {cubes_traced}")
    }
}

/// Get cube data out of [`Space`].
#[inline]
fn prepare_cubes(space: &Space) -> Vol<Box<[TracingCubeData]>> {
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
            voxels: space_block_data.evaluated().voxels().clone(),
        }
    }
}

/// Holds an [`Accumulate`] and other per-ray state, and updates it
/// according to the things it encounters.
#[derive(Clone, Debug, Default)]
struct TracingState<P: Accumulate> {
    /// Conversion factor from raycaster `t` values to “true” [`Space`] distance values
    /// where 1 unit = 1 block thickness.
    t_to_absolute_distance: f64,

    /// Number of cubes traced through -- controlled by the caller, so not necessarily
    /// equal to the number of calls to [`Self::trace_through_surface()`].
    cubes_traced: usize,

    accumulator: P,
}
impl<P: Accumulate> TracingState<P> {
    #[inline]
    fn count_step_should_stop(
        &mut self,
        options: RtOptionsRef<'_, <P::BlockData as RtBlockData>::Options>,
    ) -> bool {
        self.cubes_traced += 1;
        if self.cubes_traced > 1000 {
            // Abort excessively long traces.
            self.accumulator = Default::default();
            // TODO: Should there be a dedicated method for this like hit_nothing()?
            self.accumulator.add(Hit {
                surface: Rgba::WHITE.into(),
                block: &P::BlockData::error(options),
            });
            true
        } else {
            self.accumulator.opaque()
        }
    }

    fn finish(
        mut self,
        sky_color: Rgba,
        sky_data: &P::BlockData,
        debug_steps: bool,
    ) -> (P, RaytraceInfo) {
        if self.cubes_traced == 0 {
            // Didn't intersect the world at all.
            // Inform the accumulator of this in case it wants to do something different.
            self.accumulator.hit_nothing();
        }

        self.accumulator.add(Hit {
            surface: sky_color.into(),
            block: sky_data,
        });

        // Debug visualization of number of raytracing steps.
        // TODO: Make this less of a kludge — we'd like to be able to mix with
        // the regular color view, but Accumulate doesn't make that easy.
        if debug_steps {
            let original_accum: P = mem::take(&mut self.accumulator);
            #[allow(clippy::used_underscore_items)]
            let original_color: Rgba = original_accum
                ._unstable_get_original_color()
                .unwrap_or(Rgba::BLACK);

            self.accumulator.add(Hit {
                surface: (rgb_const!(0.02, 0.002, 0.0) * self.cubes_traced as f32
                    + rgb_const!(0.0, 0.0, 0.2) * original_color.luminance())
                .with_alpha_one()
                .into(),
                block: sky_data,
            });
        }

        (
            self.accumulator,
            RaytraceInfo {
                cubes_traced: self.cubes_traced,
            },
        )
    }

    /// Apply the effect of a given surface color.
    #[inline]
    fn trace_through_surface(
        &mut self,
        surface: &Surface<'_, P::BlockData>,
        rt: &SpaceRaytracer<P::BlockData>,
    ) {
        if let Some(light) = surface.to_light(rt) {
            self.accumulator.add(Hit {
                surface: light,
                block: surface.block_data,
            });
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

        // Adjust colors for the thickness
        let (adjusted_color, emission_coeff) =
            apply_transmittance(surface.diffuse_color, thickness);
        surface.diffuse_color = adjusted_color;
        surface.emission = surface.emission * emission_coeff;

        self.trace_through_surface(&surface, rt);
    }
}

/// Given an `Atom`/`Evoxel` color, and the thickness of that material passed through,
/// return the effective alpha that should replace the original, and the coefficient for
/// scaling the light emission.
#[inline]
fn apply_transmittance(color: Rgba, thickness: f32) -> (Rgba, f32) {
    // Distance calculations might produce small negative values; tolerate this.
    let thickness = thickness.max(0.0);

    // If the thickness is zero and the alpha is one, this is theoretically undefined.
    // In practice, thickness has error, so we want to count this as if it were a small
    // nonzero thickness.
    if thickness == 0.0 {
        return if color.fully_opaque() {
            (color, 1.0)
        } else {
            (Rgba::TRANSPARENT, 0.0)
        };
    }

    // Convert alpha to transmittance (light transmitted / light received).
    let unit_transmittance = 1.0 - color.clamp().alpha().into_inner();
    // Adjust transmittance for the thickness relative to an assumed 1.0 thickness.
    let depth_transmittance = unit_transmittance.powf(thickness);
    // Convert back to alpha.
    // TODO: skip NaN check ... this may require refactoring Surface usage.
    // We might also benefit from an "UncheckedRgba" concept.
    let alpha = ZeroOne::<f32>::new_clamped(1.0 - depth_transmittance);
    let modified_color = color.to_rgb().with_alpha(alpha);

    // Compute how the emission should be scaled to account for internal absorption and thickness.
    // Since voxel emission is defined as “emitted from the surface of a unit-thickness layer”,
    // the emission per length must be *greater* the more opaque the material is,
    // and yet also it is reduced the deeper we go.
    // This formula is the integral of that process.
    let emission_coeff = if unit_transmittance == 1.0 {
        // This is the integral
        //     ∫{0..thickness} unit_transmittance^x dx
        //   = ∫{0..thickness} 1 dx
        thickness
    } else {
        // This is the integral
        //     ∫{0..thickness} unit_transmittance^x dx
        // in the case where `unit_transmittance` is not equal to 1.
        (depth_transmittance - 1.) / (unit_transmittance - 1.)
    };

    (modified_color, emission_coeff.max(0.0))
}

/// Minimal raytracing helper used by block evaluation to compute aggregate properties
/// of voxel blocks. Compared to the regular raytracer, it:
///
/// * Traces through `Evoxel`s instead of a `SpaceRaytracer`.
/// * Follows an axis-aligned ray only.
///
/// `origin` should be the first cube to trace through *within* the grid.
pub(crate) fn trace_for_eval(
    voxels: &Evoxels,
    origin: Cube,
    direction: Face6,
    resolution: Resolution,
) -> EvalTrace {
    let thickness = f32::from(resolution).recip();
    let step = direction.normal_vector();

    let mut cube = origin;
    let mut color_buf = ColorBuf::default();
    let mut emission = Vector3D::zero();

    while let Some(voxel) = voxels.get(cube) {
        let (adjusted_color, emission_coeff) = apply_transmittance(voxel.color, thickness);
        emission += Vector3D::from(voxel.emission * emission_coeff) * color_buf.transmittance;
        color_buf.add(Hit {
            surface: adjusted_color.into(),
            block: &(),
        });

        if color_buf.opaque() {
            break;
        }
        cube += step;
    }
    EvalTrace {
        color: color_buf.into(),
        emission,
    }
}

#[derive(Clone, Copy)]
pub(crate) struct EvalTrace {
    pub color: Rgba,
    pub emission: Vector3D<f32, Intensity>,
}

#[cfg(feature = "auto-threads")]
mod rayon_helper {
    use core::iter::{Sum, empty, once};
    use rayon::iter::{IntoParallelIterator, ParallelExtend, ParallelIterator as _};

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::math::rgba_const;

    #[test]
    fn apply_transmittance_identity() {
        let color = rgba_const!(1.0, 0.5, 0.0, 0.5);
        assert_eq!(apply_transmittance(color, 1.0), (color, 1.0));
    }

    /// `apply_transmittance` + `ColorBuf` accumulation should add up to the identity function for
    /// any unit thickness (except for rounding error, which we are avoiding for this test case).
    ///
    /// TODO: test emission equivalence too
    #[test]
    fn apply_transmittance_equivalence() {
        fn case(color: Rgba, count: usize) {
            let (modified_color, _emission_coeff) =
                apply_transmittance(color, (count as f32).recip());
            let mut color_buf = ColorBuf::default();
            for _ in 0..count {
                color_buf.add(Hit {
                    surface: modified_color.into(),
                    block: &(),
                });
            }
            let actual = Rgba::from(color_buf);
            let error: Vec<f32> = <[f32; 4]>::from(actual)
                .into_iter()
                .zip(<[f32; 4]>::from(color))
                .map(|(a, b)| a - b)
                .collect();
            assert!(
                error.iter().sum::<f32>() < 0.00001,
                "count {count}, color {color:?}, actual {actual:?}, error {error:?}"
            );
        }

        let color = rgba_const!(1.0, 0.5, 0.0, 0.5);
        case(color, 1);
        case(color, 2);
        case(color, 8);
    }

    /// Regression test for numerical error actually encountered.
    #[test]
    fn apply_transmittance_negative_thickness_transparent() {
        assert_eq!(
            apply_transmittance(rgba_const!(1.0, 0.5, 0.0, 0.5), -0.125),
            (Rgba::TRANSPARENT, 0.0)
        );
    }
    #[test]
    fn apply_transmittance_negative_thickness_opaque() {
        let color = rgba_const!(1.0, 0.5, 0.0, 1.0);
        assert_eq!(apply_transmittance(color, -0.125), (color, 1.0));
    }

    #[test]
    fn apply_transmittance_zero_thickness_transparent() {
        assert_eq!(
            apply_transmittance(rgba_const!(1.0, 0.5, 0.0, 0.5), 0.0),
            (Rgba::TRANSPARENT, 0.0)
        );
    }
    #[test]
    fn apply_transmittance_zero_thickness_opaque() {
        let color = rgba_const!(1.0, 0.5, 0.0, 1.0);
        assert_eq!(apply_transmittance(color, 0.0), (color, 1.0));
    }
}
