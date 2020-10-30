// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Raytracer for `Space`s.

use cgmath::{EuclideanSpace as _, Point3, Vector3, Zero as _};
#[cfg(feature = "rayon")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use std::convert::TryFrom;

use crate::camera::ProjectionHelper;
use crate::math::{Face, FreeCoordinate, RGB, RGBA};
use crate::raycast::Ray;
use crate::space::{GridArray, PackedLight, Space, SpaceBlockData};

// TODO: Reduce code duplication between parallel and non-parallel versions.

#[cfg(feature = "rayon")]
pub fn raytrace_space<P: PixelBuf>(
    projection: &ProjectionHelper,
    space: &Space,
) -> Vec<(usize, usize, P::Pixel, usize)> {
    let indexed_block_data = prepare_blocks::<P>(space);
    let space_data = prepare_cubes::<P>(&indexed_block_data, space);
    let sky = space.sky_color();

    // Construct iterator over pixel positions.
    // TODO: Make this pluggable so we can use incremental rendering strategies.
    let viewport = projection.viewport();
    let pixel_iterator = (0..viewport.y)
        .into_par_iter()
        .map(move |ych| {
            let y = projection.normalize_pixel_y(ych);
            (0..viewport.x).into_par_iter().map(move |xch| {
                let x = projection.normalize_pixel_x(xch);
                (xch, ych, x, y)
            })
        })
        .flatten();

    // Do the actual tracing.
    let output_iterator = pixel_iterator.map(move |(xch, ych, x, y)| {
        let ray = projection.project_ndc_into_world(x, y);
        let (pixel, count) = pixel_from_ray::<P>(ray, &space_data, sky);
        (xch, ych, pixel, count)
    });

    // Collect into a concrete, non-parallel result. TODO: This can probably be better API
    output_iterator.collect()
}

#[cfg(not(feature = "rayon"))]
pub fn raytrace_space<P: PixelBuf>(
    projection: &ProjectionHelper,
    space: &Space,
) -> Vec<(usize, usize, P::Pixel, usize)> {
    let indexed_block_data = prepare_blocks::<P>(space);
    let space_data = prepare_cubes::<P>(&indexed_block_data, space);
    let sky = space.sky_color();

    // Construct iterator over pixel positions.
    // TODO: Make this pluggable so we can use incremental rendering strategies.
    let viewport = projection.viewport();
    let pixel_iterator = (0..viewport.y)
        .map(move |ych| {
            let y = projection.normalize_pixel_y(ych);
            (0..viewport.x).map(move |xch| {
                let x = projection.normalize_pixel_x(xch);
                (xch, ych, x, y)
            })
        })
        .flatten();

    // Do the actual tracing.
    let output_iterator = pixel_iterator.map(move |(xch, ych, x, y)| {
        let ray = projection.project_ndc_into_world(x, y);
        let (pixel, count) = pixel_from_ray::<P>(ray, &space_data, sky);
        (xch, ych, pixel, count)
    });

    output_iterator.collect()
}

/// Get block data out of `Space` (which is not `Sync`, and not specialized for our efficient use).
#[inline]
fn prepare_blocks<P: PixelBuf>(space: &Space) -> Vec<TracingBlock<P::BlockData>> {
    space
        .distinct_blocks_unfiltered_iter()
        .map(|block_data| {
            let evaluated = block_data.evaluated();
            let pixel_block_data = P::compute_block_data(block_data);
            if let Some(ref voxels) = evaluated.voxels {
                TracingBlock::Recur(pixel_block_data, voxels.clone())
            } else {
                TracingBlock::Atom(pixel_block_data, block_data.evaluated().color)
            }
        })
        .collect()
}

/// Get cube data out of `Space` (which is not `Sync`, and not specialized for our efficient use).
#[inline]
#[allow(clippy::ptr_arg)] // no benefit
fn prepare_cubes<'a, P: PixelBuf>(
    indexed_block_data: &'a Vec<TracingBlock<P::BlockData>>,
    space: &Space,
) -> GridArray<TracingCubeData<'a, P::BlockData>> {
    space.extract(*space.grid(), |index, _block, lighting| TracingCubeData {
        block: &indexed_block_data[index as usize],
        lighting,
    })
}

#[inline]
fn pixel_from_ray<P: PixelBuf>(
    ray: Ray,
    space_data: &GridArray<TracingCubeData<P::BlockData>>,
    sky: RGB,
) -> (P::Pixel, usize) {
    let mut s: TracingState<P> = TracingState::default();
    for hit in ray.cast().within_grid(*space_data.grid()) {
        if s.count_step_should_stop() {
            break;
        }

        let cube_data = &space_data[hit.cube];
        match &cube_data.block {
            TracingBlock::Atom(pixel_block_data, color) => {
                if color.fully_transparent() {
                    // Skip lighting lookup
                    continue;
                }

                // Find lighting.
                let lighting: RGB = space_data
                    .get(hit.previous_cube())
                    .map(|b| b.lighting.into())
                    .unwrap_or(sky);

                s.trace_through_surface(pixel_block_data, *color, lighting, hit.face);
            }
            TracingBlock::Recur(pixel_block_data, array) => {
                // Find lighting.
                // TODO: duplicated code
                let lighting: RGB = space_data
                    .get(hit.previous_cube())
                    .map(|b| b.lighting.into())
                    .unwrap_or(sky);

                // Find where the origin in the space's coordinate system is.
                // TODO: Raycaster does not efficiently implement advancing from outside a
                // grid. Fix that to get way more performance.
                let adjusted_ray = Ray {
                    origin: Point3::from_vec(
                        (ray.origin - hit.cube.cast::<FreeCoordinate>().unwrap())
                            * FreeCoordinate::from(array.grid().size().x),
                    ),
                    ..ray
                };

                for subcube_hit in adjusted_ray.cast().within_grid(*array.grid()) {
                    if s.count_step_should_stop() {
                        break;
                    }
                    let color = array[subcube_hit.cube];
                    s.trace_through_surface(pixel_block_data, color, lighting, subcube_hit.face);
                }
            }
        }
    }
    s.finish(sky)
}

#[derive(Clone, Debug)]
struct TracingCubeData<'a, B: 'static> {
    block: &'a TracingBlock<B>,
    lighting: PackedLight,
}

#[derive(Clone, Debug)]
enum TracingBlock<B: 'static> {
    Atom(B, RGBA),
    Recur(B, GridArray<RGBA>),
}

#[derive(Clone, Debug, Default)]
struct TracingState<P: PixelBuf> {
    /// Number of cubes traced through -- controlled by the caller, so not necessarily
    /// equal to the number of calls to trace_through_surface().
    number_passed: usize,
    pixel_buf: P,
}
impl<P: PixelBuf> TracingState<P> {
    #[inline]
    fn count_step_should_stop(&mut self) -> bool {
        self.number_passed += 1;
        if self.number_passed > 1000 {
            // Abort excessively long traces.
            self.pixel_buf = Default::default();
            self.pixel_buf.add(
                RGBA::new(1.0, 1.0, 1.0, 1.0),
                &P::error_block_data(),
            );
            true
        } else {
            self.pixel_buf.opaque()
        }
    }

    fn finish(mut self, sky_color: RGB) -> (P::Pixel, usize) {
        if self.number_passed == 0 {
            // Didn't intersect the world at all. Draw these as plain background.
            // TODO: Switch to using the sky color, unless debugging options are set.
            self.pixel_buf.hit_nothing();
        }

        self.pixel_buf.add(
            sky_color.with_alpha_one(),
            &P::sky_block_data(),
        );

        (self.pixel_buf.result(), self.number_passed)
    }

    /// Apply the effect of a given surface color.
    ///
    /// Note this is not true volumetric ray tracing: we're considering each
    /// voxel surface to be discrete.
    #[inline]
    fn trace_through_surface(
        &mut self,
        block_data: &P::BlockData,
        surface: RGBA,
        lighting: RGB,
        face: Face,
    ) {
        if surface.fully_transparent() {
            return;
        }
        let adjusted_rgb = fake_lighting_adjustment(surface.to_rgb() * lighting, face);
        self.pixel_buf
            .add(adjusted_rgb.with_alpha(surface.alpha()), block_data);
    }
}

/// Representation of a single output pixel being computed.
///
/// This should be an efficiently updatable buffer able to accumulate partial values,
/// and it must represent the transparency so as to be able to signal when to stop
/// tracing.
///
/// The implementation of the `Default` trait must provide a suitable initial state,
/// i.e. fully transparent/no light accumulated.
pub trait PixelBuf: Default {
    /// Type of the pixels in the output image.
    type Pixel: Send + Sync + 'static;

    /// Type of the data precomputed for each distinct block.
    // Note: I tried letting BlockData contain references but I couldn't satisfy
    // the borrow checker.
    type BlockData: Send + Sync + 'static;

    fn compute_block_data(block: &SpaceBlockData) -> Self::BlockData;

    fn error_block_data() -> Self::BlockData;

    fn sky_block_data() -> Self::BlockData;

    /// Returns whether `self` has recorded an opaque surface and therefore will not
    /// be affected by future calls to `add`.
    fn opaque(&self) -> bool;

    /// Compute the final result.
    fn result(self) -> Self::Pixel;

    /// Adds the color of a surface to the buffer. The provided color should already
    /// have the effect of lighting applied.
    ///
    /// TODO: this interface might want even more information; generalize it to be
    /// more future-proof.
    fn add(&mut self, surface_color: RGBA, block_data: &Self::BlockData);

    /// Indicates that the trace did not intersect any space that could have contained
    /// anything to draw. May be used for special diagnostic drawing. If used, should
    /// disable future `add()` calls.
    fn hit_nothing(&mut self) {}
}

/// Implements `PixelBuf` in the straightforward fashion for RGB(A) color.
#[derive(Clone, Debug, PartialEq)]
pub struct ColorBuf {
    /// Color buffer.
    ///
    /// The value can be interpreted as being “premultiplied alpha” value where the alpha
    /// is `1.0 - self.ray_alpha`, or equivalently we can say that it is the color to
    /// display supposing that everything not already traced is black.
    ///
    /// Note: Not using the `RGB` type so as to skip NaN checks.
    color_accumulator: Vector3<f32>,

    /// Fraction of the color value that is to be determined by future, rather than past,
    /// tracing; starts at 1.0 and decreases as surfaces are encountered.
    ray_alpha: f32,
}

impl PixelBuf for ColorBuf {
    type Pixel = RGBA;
    type BlockData = ();

    fn compute_block_data(_: &SpaceBlockData) {}

    fn error_block_data() {}

    fn sky_block_data() {}

    #[inline]
    fn result(self) -> RGBA {
        if self.ray_alpha >= 1.0 {
            // Special case to avoid dividing by zero
            RGBA::TRANSPARENT
        } else {
            let color_alpha = 1.0 - self.ray_alpha;
            let non_premultiplied_color = self.color_accumulator / color_alpha;
            RGBA::try_from(non_premultiplied_color.extend(color_alpha))
                .unwrap_or_else(|_| RGBA::new(1.0, 0.0, 0.0, 1.0))
        }
    }

    #[inline]
    fn opaque(&self) -> bool {
        // Let's suppose that we don't care about differences that can't be represented
        // in 8-bit color...not considering gamma.
        self.ray_alpha < 1.0 / 256.0
    }

    #[inline]
    fn add(&mut self, surface_color: RGBA, _block_data: &Self::BlockData) {
        let color_vector: Vector3<f32> = surface_color.to_rgb().into();
        let surface_alpha = surface_color.alpha().into_inner();
        let alpha_for_add = surface_alpha * self.ray_alpha;
        self.ray_alpha *= 1.0 - surface_alpha;
        self.color_accumulator += color_vector * alpha_for_add;
    }
}

impl Default for ColorBuf {
    #[inline]
    fn default() -> Self {
        Self {
            color_accumulator: Vector3::zero(),
            ray_alpha: 1.0,
        }
    }
}

fn fake_lighting_adjustment(rgb: RGB, face: Face) -> RGB {
    // TODO: notion of "one step" is less coherent ...
    let one_step = 1.0 / 5.0;
    let modifier = match face {
        Face::PY => RGB::ONE * one_step * 2.0,
        Face::NY => RGB::ONE * one_step * -1.0,
        Face::NX | Face::PX => RGB::ONE * one_step * 1.0,
        _ => RGB::ONE * 0.0,
    };
    rgb + modifier
}

#[cfg(test)]
mod tests {
    use super::*;
    // use ordered_float::NotNan;

    #[test]
    fn color_buf() {
        let color_1 = RGBA::new(1.0, 0.0, 0.0, 0.75);
        let color_2 = RGBA::new(0.0, 1.0, 0.0, 0.5);
        let color_3 = RGBA::new(0.0, 0.0, 1.0, 1.0);

        let mut buf = ColorBuf::default();
        assert_eq!(buf.clone().result(), RGBA::TRANSPARENT);
        assert!(!buf.opaque());

        buf.add(color_1, &());
        assert_eq!(buf.clone().result(), color_1);
        assert!(!buf.opaque());

        buf.add(color_2, &());
        // TODO: this is not the right assertion because it's the premultiplied form.
        // assert_eq!(
        //     buf.result(),
        //     (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125)
        //         .with_alpha(NotNan::new(0.875).unwrap())
        // );
        assert!(!buf.opaque());

        buf.add(color_3, &());
        assert!(buf.clone().result().fully_opaque());
        //assert_eq!(
        //    buf.result(),
        //    (color_1.to_rgb() * 0.75 + color_2.to_rgb() * 0.125 + color_3.to_rgb() * 0.125)
        //        .with_alpha(NotNan::one())
        //);
        assert!(buf.opaque());
    }

    // TODO: test actual raytracer
}
