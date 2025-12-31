//! [`SpaceRaytracer`] and friends.
//!
//! TODO: dissolve this module to other better-named places

use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use core::marker::PhantomData;

use manyfmt::Fmt;
/// Acts as polyfill for float methods
#[cfg(not(any(feature = "std", test)))]
#[allow(
    unused_imports,
    reason = "unclear why this warns even though it is needed"
)]
use num_traits::float::Float as _;
use rand::SeedableRng;

#[cfg(feature = "auto-threads")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use all_is_cubes::block::{AIR, Evoxels};
use all_is_cubes::camera::{Camera, GraphicsOptions, TransparencyOption};
use all_is_cubes::camera::{FogOption, NdcPoint2};
use all_is_cubes::euclid::{Vector3D, vec3};
use all_is_cubes::math::{
    Cube, Face6, Face7, FreeCoordinate, FreePoint, FreeVector, GridMatrix, Rgb, Rgba, Vol, ZeroOne,
    rgb_const,
};
use all_is_cubes::raycast::{self, Ray};
use all_is_cubes::raytracer_components::apply_transmittance;
use all_is_cubes::space::{self, BlockIndex, BlockSky, PackedLight, Sky, SpaceBlockData};
use all_is_cubes::util::StatusText;

#[cfg(doc)]
use all_is_cubes::space::Space;

use crate::raytracer::raycast_traits::RayIsh;
use crate::raytracer::surface::{DepthIter, DepthStep, Span, Surface, SurfaceIter, TraceStep};
use crate::raytracer::{
    Accumulate, BounceRng, Exception, Hit, Position, RtBlockData, RtOptionsRef,
};

// -------------------------------------------------------------------------------------------------

/// Precomputed data for raytracing a single frame of a single [`Space`], and bearer of
/// the methods for actually performing raytracing.
#[allow(clippy::module_name_repetitions, reason = "TODO: find better name")]
pub struct SpaceRaytracer<D: RtBlockData> {
    pub(in crate::raytracer) blocks: Vec<TracingBlock<D>>,
    pub(in crate::raytracer) cubes: Vol<Box<[TracingCubeData]>>,

    pub(in crate::raytracer) graphics_options: GraphicsOptions,
    pub(in crate::raytracer) custom_options: D::Options,
    pub(in crate::raytracer) sky: Sky,
    pub(in crate::raytracer) sky_data: D,
    pub(in crate::raytracer) block_sky: BlockSky,
}

impl<D: RtBlockData> SpaceRaytracer<D> {
    /// Snapshots the given [`Space`] to prepare for raytracing it.
    pub fn new(
        space: &space::Read<'_>,
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
            sky_data: D::exception(Exception::Sky, options),

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
            cubes: Vol::origin_empty(),
            block_sky: sky.for_blocks(),
            sky,
            sky_data: D::exception(Exception::Sky, options),

            graphics_options,
            custom_options,
        }
    }

    /// Computes a single image pixel from the given ray, adding it to `accumulator`.
    pub fn trace_ray<P: Accumulate<BlockData = D>>(
        &self,
        ray: Ray,
        accumulator: &mut P,
        include_sky: bool,
    ) -> RaytraceInfo {
        self.trace_ray_impl::<P, Ray>(ray, accumulator, include_sky, true)
    }

    /// Computes a single image pixel from the given ray.
    ///
    /// This is identical to [`Self::trace_ray()`] except that it can be more efficient
    /// by being restricted to a single axis.
    pub fn trace_axis_aligned_ray<P: Accumulate<BlockData = D>>(
        &self,
        ray: raycast::AaRay,
        accumulator: &mut P,
        include_sky: bool,
    ) -> RaytraceInfo {
        self.trace_ray_impl::<P, raycast::AaRay>(ray, accumulator, include_sky, true)
    }

    pub(crate) fn trace_ray_impl<P: Accumulate<BlockData = D>, R: RayIsh>(
        &self,
        ray: R,
        accumulator: &mut P,
        include_sky: bool,
        allow_ray_bounce: bool,
    ) -> RaytraceInfo {
        let options =
            RtOptionsRef::_new_but_please_do_not_construct_this_if_you_are_not_all_is_cubes_itself(
                &self.graphics_options,
                &self.custom_options,
            );
        let ray_direction = ray.direction();

        let sky_light = include_sky.then(|| self.sky.sample(ray.direction()));
        let t_to_absolute_distance = ray.direction().length();
        let mut state: TracingState<'_, P> = TracingState {
            t_to_absolute_distance,
            t_to_view_distance: (t_to_absolute_distance
                / self.graphics_options.view_distance.into_inner())
                as f32,
            distance_fog_light: match self.graphics_options.fog {
                FogOption::None => None,
                _ => sky_light,
            },
            distance_fog_blend: match self.graphics_options.fog {
                FogOption::Abrupt => 1.0,
                FogOption::Compromise => 0.5,
                FogOption::Physical => 0.0,
                /* FogOption::None | */ _ => 0.0,
            },
            primary_cubes_traced: 0,
            secondary_info: RaytraceInfo::default(),
            accumulator,
            ray_bounce_rng: allow_ray_bounce.then(|| {
                // Computing the random bounces from the ray direction makes the bounce pattern,
                // and thus the produced image, deterministic. This is useful for the current
                // experimentation, but will no longer be desirable in the event that we add
                // temporal accumulation. At that point we would want to either reuse a single
                // RNG for all rendering, or mix something like a frame number into this seed.
                rand::rngs::SmallRng::seed_from_u64(
                    ray_direction
                        .x
                        .to_bits()
                        .wrapping_add(ray_direction.y.to_bits())
                        .wrapping_add(ray_direction.z.to_bits()),
                )
            }),
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
            if let Some(sky_light) = sky_light {
                sky_light.with_alpha_one()
            } else {
                Rgba::TRANSPARENT
            },
            &self.sky_data,
            self.graphics_options.debug_pixel_cost,
            options,
        )
    }

    #[inline]
    pub(crate) fn get_packed_light(&self, cube: Cube) -> PackedLight {
        match self.cubes.get(cube) {
            Some(b) => b.lighting,
            None => self.block_sky.light_outside(self.cubes.bounds(), cube),
        }
    }

    pub(crate) fn get_interpolated_light(&self, point: FreePoint, face: Face7) -> Rgb {
        // This implementation is duplicated in WGSL in interpolated_space_light()
        // in all-is-cubes-gpu/src/in_wgpu/shaders/blocks-and-lines.wgsl.

        // About half the size of the smallest permissible voxel.
        let above_surface_epsilon = 0.5 / 256.0;

        // The position we should start with for light lookup and interpolation.
        let origin = point.to_vector() + face.vector(above_surface_epsilon);

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
        P: Accumulate<BlockData = D> + Into<String> + Default + 'a,
    {
        struct ToText<'a, D: RtBlockData, P> {
            rt: &'a SpaceRaytracer<D>,
            camera: &'a Camera,
            line_ending: &'a str,
            _p: PhantomData<fn() -> P>,
        }

        impl<D: RtBlockData, P: Accumulate<BlockData = D> + Into<String> + Default> fmt::Display
            for ToText<'_, D, P>
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.rt.trace_scene_to_text_impl::<P>(self.camera, self.line_ending, f)?;
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
        P: Accumulate<BlockData = D> + Into<String> + Default,
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
                        let mut buf = P::default();
                        let info = self.trace_ray::<P>(
                            camera.project_ndc_into_world(NdcPoint2::new(x, y)),
                            &mut buf,
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
        P: Accumulate<BlockData = D> + Into<String> + Default,
    {
        let mut total_info = RaytraceInfo::default();

        let viewport = camera.viewport();
        let viewport_size = viewport.framebuffer_size.to_usize();
        for ych in 0..viewport_size.height {
            let y = viewport.normalize_fb_y(ych);
            for xch in 0..viewport_size.width {
                let x = viewport.normalize_fb_x(xch);
                let mut buf = P::default();
                let info = self.trace_ray::<P>(
                    camera.project_ndc_into_world(NdcPoint2::new(x, y)),
                    &mut buf,
                    true,
                );
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

#[inline]
pub(crate) fn smoothstep(x: f64) -> f64 {
    let x = x.clamp(0.0, 1.0);
    3. * x.powi(2) - 2. * x.powi(3)
}

// -------------------------------------------------------------------------------------------------

/// Performance info from a [`SpaceRaytracer`] operation.
///
/// The contents of this structure are subject to change; use [`Debug`] to view it.
/// The [`Default`] value is the zero value.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
pub struct RaytraceInfo {
    cubes_traced: usize,
}
impl core::ops::Add for RaytraceInfo {
    type Output = Self;
    fn add(mut self, other: Self) -> Self {
        self += other;
        self
    }
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

// -------------------------------------------------------------------------------------------------

/// Get cube data out of [`Space`].
#[inline]
fn prepare_cubes(space: &space::Read<'_>) -> Vol<Box<[TracingCubeData]>> {
    space.extract(space.bounds(), |e| TracingCubeData {
        block_index: e.block_index(),
        lighting: e.light(),
        always_invisible: e.block_data().block() == &AIR,
    })
}

/// Data stored per space cube in a [`SpaceRaytracer`].
#[derive(Clone, Debug)]
pub(in crate::raytracer) struct TracingCubeData {
    pub block_index: BlockIndex,
    pub lighting: PackedLight,
    /// True if the block is [`AIR`].
    ///
    /// This special information allows us to skip an indirect memory access in this
    /// extremely common case. We could generalize it to any block which is fully
    /// invisible, but only if *the block is not an indirection* since if it is, the
    /// block data could change without signaling a cube change, and currently we don't
    /// have a mechanism to obtain that information from the Space.
    pub always_invisible: bool,
}

// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub(in crate::raytracer) struct TracingBlock<D> {
    pub block_data: D,
    // TODO: `Evoxels` carries more data than we actually need (color). Experiment with using a packed format.
    pub voxels: Evoxels,
}

impl<D: RtBlockData> TracingBlock<D> {
    pub(crate) fn from_block(
        options: RtOptionsRef<'_, D::Options>,
        space_block_data: &SpaceBlockData,
    ) -> Self {
        TracingBlock {
            block_data: D::from_block(options, space_block_data),
            voxels: space_block_data.evaluated().voxels().clone(),
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Holds an [`Accumulate`] and other per-ray state, and updates it
/// according to the things it encounters.
#[derive(Debug)]
pub(in crate::raytracer) struct TracingState<'a, P: Accumulate> {
    /// Conversion factor from raycaster `t` values to “true” [`Space`] distance values
    /// where 1 unit = 1 block thickness.
    t_to_absolute_distance: f64,

    /// Conversion factor from raycaster `t` values to fractions of
    /// `graphics_options.view_distance`.
    pub(in crate::raytracer) t_to_view_distance: f32,

    /// If fog is enabled, then this is what the light from the scene should be blended with.
    distance_fog_light: Option<Rgb>,

    /// Coefficient controlling fog density.
    distance_fog_blend: f32,

    /// Number of cubes traced through by primary rays -- controlled by the caller, so not
    /// necessarily equal to the number of calls to [`Self::trace_through_surface()`].
    primary_cubes_traced: usize,

    /// Diagnostic info from secondary rays.
    secondary_info: RaytraceInfo,

    accumulator: &'a mut P,

    /// *If* we are going to compute ray bounces, this RNG is used to decide which direction they
    /// bounce.
    pub(in crate::raytracer) ray_bounce_rng: Option<BounceRng>,
}
impl<P: Accumulate> TracingState<'_, P> {
    #[inline]
    fn count_step_should_stop(
        &mut self,
        options: RtOptionsRef<'_, <P::BlockData as RtBlockData>::Options>,
    ) -> bool {
        if self.primary_cubes_traced == 0 {
            self.accumulator.add(Hit {
                exception: Some(Exception::EnterSpace),
                surface: Rgba::TRANSPARENT.into(),
                t_distance: None,
                block: &P::BlockData::exception(Exception::EnterSpace, options),
                position: None,
            });
        }

        self.primary_cubes_traced += 1;

        // TODO: Ideally this constant would be configurable.
        // It can't simply be `view_distance` since it counts voxel steps.
        if self.primary_cubes_traced > 1000 {
            // Abort excessively long traces.
            self.accumulator.add(Hit {
                exception: Some(Exception::Incomplete),
                surface: Rgba::TRANSPARENT.into(),
                t_distance: None,
                block: &P::BlockData::exception(Exception::Incomplete, options),
                position: None,
            });
            true
        } else {
            self.accumulator.opaque()
        }
    }

    fn finish(
        self,
        sky_color: Rgba,
        sky_data: &P::BlockData,
        debug_steps: bool,
        options: RtOptionsRef<'_, <P::BlockData as RtBlockData>::Options>,
    ) -> RaytraceInfo {
        // TODO: The Accumulator should probably be allowed to tell the presence of sky.
        // Right now, since finish() (this function) is called regardless of whether
        // include_sky is true, we *always* give the accumulator this infinite hit even if
        // `sky_color` is transparent via `include_sky` being false.
        self.accumulator.add(Hit {
            exception: Some(Exception::Sky),
            surface: sky_color.into(),
            t_distance: Some(f64::INFINITY),
            block: sky_data,
            position: None,
        });

        // Debug visualization of number of raytracing steps.
        if debug_steps {
            self.accumulator.add(Hit {
                exception: Some(Exception::DebugOverrideRg),
                surface: (rgb_const!(0.02, 0.002, 0.0) * self.primary_cubes_traced as f32)
                    .with_alpha_one()
                    .into(),
                t_distance: None,
                block: &P::BlockData::exception(Exception::DebugOverrideRg, options),
                position: None,
            });
        }

        RaytraceInfo {
            cubes_traced: self.primary_cubes_traced,
        } + self.secondary_info
    }

    /// Apply the effect of a given surface color.
    #[inline]
    fn trace_through_surface(
        &mut self,
        surface: &Surface<'_, P::BlockData>,
        rt: &SpaceRaytracer<P::BlockData>,
    ) {
        if let Some((light, info)) = surface.to_light(rt, self) {
            self.accumulator.add(Hit {
                exception: None,
                surface: light,
                t_distance: Some(surface.t_distance),
                block: surface.block_data,
                position: Some(Position {
                    cube: surface.cube,
                    resolution: surface.voxel.0,
                    voxel: surface.voxel.1,
                    face: surface.normal,
                }),
            });
            self.secondary_info += info;
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

    /// Compute the density of the fog (0 = transparent, 1 = opaque) at a given *t*-distance from
    /// the view position.
    #[inline]
    pub(crate) fn distance_fog(&self, t_distance: f64) -> Option<(Rgb, ZeroOne<f32>)> {
        if let Some(fog_light) = self.distance_fog_light {
            let relative_distance = (t_distance as f32 * self.t_to_view_distance).clamp(0.0, 1.0);

            // This logic is also implemented in a shader in all-is-cubes-gpu
            // TODO: would it be cheaper to use an interpolated lookup table?
            let fog_exponential = 1.0 - (-1.6 * relative_distance).exp();
            let fog_exp_fudged = fog_exponential
                / (
                    // value of fog_exponential at relative_distance = 1.0
                    0.79810348
                );

            Some((
                fog_light,
                ZeroOne::<f32>::new_clamped(
                    fog_exp_fudged * (1.0 - self.distance_fog_blend)
                        + relative_distance.powi(4) * self.distance_fog_blend,
                ),
            ))
        } else {
            None
        }
    }
}

// -------------------------------------------------------------------------------------------------

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

// -------------------------------------------------------------------------------------------------

#[cfg(not(any(feature = "std", test)))]
#[allow(dead_code, reason = "unclear why this warns even though it is needed")]
/// Identical to [`num_traits::Euclid`] except that its signatures are compatible with
/// `std` versions.
///
/// Note: this code is duplicated among several crates so that it doesn't need to be public.
pub(crate) trait Euclid {
    fn div_euclid(self, rhs: Self) -> Self;
    fn rem_euclid(self, rhs: Self) -> Self;
}
#[cfg(not(any(feature = "std", test)))]
impl<T: num_traits::Euclid + Copy> Euclid for T {
    fn div_euclid(self, rhs: Self) -> Self {
        <T as num_traits::Euclid>::div_euclid(&self, &rhs)
    }
    fn rem_euclid(self, rhs: Self) -> Self {
        <T as num_traits::Euclid>::rem_euclid(&self, &rhs)
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoothstep_test() {
        assert_eq!(smoothstep(0.0), 0.0);
        assert_eq!(smoothstep(0.5), 0.5);
        assert_eq!(smoothstep(1.0), 1.0);
    }
}
