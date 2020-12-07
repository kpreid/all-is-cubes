// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Raytracer for [`Space`]s.
//!
//! ## Why?
//!
//! The original reason this exists is that I thought “we have `all_is_cubes::raycast`,
//! and that's nearly all the work, so why not?” Secondarily, it was written before
//! the mesh-based renderer `all_is_cubes::lum`, and was useful as a cross-check since
//! it is much simpler.
//!
//! In the future (or currently, if I forgot to update this comment), it will be used
//! as a means to display the state of `Space`s used for testing inline in test output.

use cgmath::{EuclideanSpace as _, Matrix4, Point3, Vector3, Zero as _};
use ouroboros::self_referencing;
#[cfg(feature = "rayon")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use std::borrow::Cow;
use std::convert::TryFrom;

use crate::camera::{eye_for_look_at, ProjectionHelper};
use crate::math::{Face, FreeCoordinate, GridPoint, RGB, RGBA};
use crate::raycast::Ray;
use crate::space::{GridArray, PackedLight, Space, SpaceBlockData};

/// Precomputed data for raytracing a single frame of a single Space, and bearer of the
/// methods for actually performing raytracing.
pub struct SpaceRaytracer<P: PixelBuf>(SpaceRaytracerImpl<P>);

/// Helper struct for [`SpaceRaytracer`] so the details of [`ouroboros::self_referencing`]
/// aren't exposed.
#[self_referencing]
struct SpaceRaytracerImpl<P: PixelBuf> {
    blocks: Box<[TracingBlock<P::BlockData>]>,
    #[borrows(blocks)]
    cubes: GridArray<TracingCubeData<'this, P::BlockData>>,
    sky_color: RGB,
}

impl<P: PixelBuf> SpaceRaytracer<P> {
    /// Snapshots the given [`Space`] to prepare for raytracing it.
    pub fn new(space: &Space) -> Self {
        SpaceRaytracer(
            SpaceRaytracerImplBuilder {
                blocks: prepare_blocks::<P>(space),
                cubes_builder: |blocks: &[TracingBlock<P::BlockData>]| {
                    prepare_cubes::<P>(blocks, space)
                },
                sky_color: space.sky_color(),
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
                        s.trace_through_surface(
                            pixel_block_data,
                            *color,
                            self.get_lighting(hit.cube_behind()),
                            hit.face(),
                        );
                    }
                    TracingBlock::Recur(pixel_block_data, array) => {
                        // Find where the origin in the space's coordinate system is.
                        // TODO: Raycaster does not efficiently implement advancing from outside a
                        // grid. Fix that to get way more performance.
                        let adjusted_ray = Ray {
                            origin: Point3::from_vec(
                                (ray.origin - hit.cube_ahead().cast::<FreeCoordinate>().unwrap())
                                    * FreeCoordinate::from(array.grid().size().x),
                            ),
                            ..ray
                        };
                        // TODO: not the right lighting for non-opaque blocks
                        let lighting = self.get_lighting(hit.cube_behind());
                        for subcube_hit in adjusted_ray.cast().within_grid(array.grid()) {
                            if s.count_step_should_stop() {
                                break;
                            }
                            let color = array[subcube_hit.cube_ahead()];
                            s.trace_through_surface(
                                pixel_block_data,
                                color,
                                lighting,
                                subcube_hit.face(),
                            );
                        }
                    }
                }
            }
            s.finish(*impl_fields.sky_color)
        })
    }

    #[inline]
    fn get_lighting(&self, cube: GridPoint) -> RGB {
        self.0.with(|impl_fields| {
            impl_fields
                .cubes
                .get(cube)
                .map(|b| b.lighting.into())
                .unwrap_or(*impl_fields.sky_color)
        })
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
        projection: &ProjectionHelper,
        line_ending: &str,
        write: F,
    ) -> Result<RaytraceInfo, E>
    where
        F: FnMut(&str) -> Result<(), E>,
    {
        // This wrapper function ensures that the two implementations have consistent
        // signatures.
        self.trace_scene_to_text_impl(projection, line_ending, write)
    }

    #[cfg(feature = "rayon")]
    fn trace_scene_to_text_impl<F, E>(
        &self,
        projection: &ProjectionHelper,
        line_ending: &str,
        mut write: F,
    ) -> Result<RaytraceInfo, E>
    where
        F: FnMut(&str) -> Result<(), E>,
    {
        let viewport = projection.viewport();
        let output_iterator = (0..viewport.y)
            .into_par_iter()
            .map(move |ych| {
                let y = projection.normalize_pixel_y(ych);
                (0..viewport.x)
                    .into_par_iter()
                    .map(move |xch| {
                        let x = projection.normalize_pixel_x(xch);
                        self.trace_ray(projection.project_ndc_into_world(x, y))
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
        projection: &ProjectionHelper,
        line_ending: &str,
        mut write: F,
    ) -> Result<RaytraceInfo, E>
    where
        F: FnMut(&str) -> Result<(), E>,
    {
        let mut total_info = RaytraceInfo::default();

        let viewport = projection.viewport();
        for ych in 0..viewport.y {
            let y = projection.normalize_pixel_y(ych);
            for xch in 0..viewport.x {
                let x = projection.normalize_pixel_x(xch);
                let (text, info) = self.trace_ray(projection.project_ndc_into_world(x, y));
                total_info += info;
                write(text.as_ref())?;
            }
            write(line_ending)?;
        }

        Ok(total_info)
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

/// Print an image of the given space as “ASCII art”.
///
/// Intended for use in tests, to visualize the results in case of failure.
/// Accordingly, it always writes to the same destination as [`print!`] (which is
/// redirected when tests are run).
///
/// `direction` specifies the direction from which the camera will be looking towards
/// the center of the space. The text output will be 80 columns wide.
pub fn print_space(space: &Space, direction: impl Into<Vector3<FreeCoordinate>>) {
    print_space_impl(space, direction, |s| {
        print!("{}", s);
    });
}

/// Version of `print_space` that takes a destination, for testing.
fn print_space_impl<F: FnMut(&str)>(
    space: &Space,
    direction: impl Into<Vector3<FreeCoordinate>>,
    mut write: F,
) -> RaytraceInfo {
    // TODO: optimize height (and thus aspect ratio) for the shape of the space
    let mut projection = ProjectionHelper::new(0.5, (80, 40));
    projection.set_view_matrix(Matrix4::look_at(
        eye_for_look_at(space.grid(), direction.into()),
        space.grid().center(),
        Vector3::new(0., 1., 0.),
    ));

    SpaceRaytracer::<CharacterBuf>::new(space)
        .trace_scene_to_text(&projection, &"\n", move |s| {
            write(s);
            let r: Result<(), ()> = Ok(());
            r
        })
        .unwrap()
}

/// Get block data out of [`Space`] (which is not [`Sync`], and not specialized for our
/// efficient use).
#[inline]
fn prepare_blocks<P: PixelBuf>(space: &Space) -> Box<[TracingBlock<P::BlockData>]> {
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
    Atom(B, RGBA),
    Recur(B, GridArray<RGBA>),
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
                .add(RGBA::new(1.0, 1.0, 1.0, 1.0), &P::error_block_data());
            true
        } else {
            self.pixel_buf.opaque()
        }
    }

    fn finish(mut self, sky_color: RGB) -> (P::Pixel, RaytraceInfo) {
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

/// Implementations of [`PixelBuf`] define output formats of the raytracer, by being
/// responsible for accumulating the color (and/or other information) for each image
/// pixel.
///
/// They should be an efficiently updatable buffer able to accumulate partial values,
/// and it must represent the transparency so as to be able to signal when to stop
/// tracing.
///
/// The implementation of the [`Default`] trait must provide a suitable initial state,
/// i.e. fully transparent/no light accumulated.
pub trait PixelBuf: Default {
    /// Type of the pixel value this [`PixelBuf`] produces; the value that will be
    /// returned by tracing a single ray.
    ///
    /// This trait does not define how multiple pixels are combined into an image.
    type Pixel: Send + Sync + 'static;

    /// Type of the data precomputed for each distinct block by
    /// [`Self::compute_block_data()`].
    ///
    /// If no data beyond color is needed, this may be `()`.
    // Note: I tried letting BlockData contain references but I couldn't satisfy
    // the borrow checker.
    type BlockData: Send + Sync + 'static;

    /// Computes whatever data this [`PixelBuf`] wishes to have available in
    /// [`Self::add`], for a given block.
    fn compute_block_data(block: &SpaceBlockData) -> Self::BlockData;

    /// Computes whatever value should be passed to [`Self::add`] when the raytracer
    /// encounters an error.
    fn error_block_data() -> Self::BlockData;

    /// Computes whatever value should be passed to [`Self::add`] when the raytracer
    /// encounters the sky (background behind all blocks).
    fn sky_block_data() -> Self::BlockData;

    /// Returns whether `self` has recorded an opaque surface and therefore will not
    /// be affected by future calls to [`Self::add`].
    fn opaque(&self) -> bool;

    /// Computes the value the raytracer should return for this pixel when tracing is
    /// complete.
    fn result(self) -> Self::Pixel;

    /// Adds the color of a surface to the buffer. The provided color should already
    /// have the effect of lighting applied.
    ///
    /// You should probably give this method the `#[inline]` attribute.
    ///
    /// TODO: this interface might want even more information; generalize it to be
    /// more future-proof.
    fn add(&mut self, surface_color: RGBA, block_data: &Self::BlockData);

    /// Indicates that the trace did not intersect any space that could have contained
    /// anything to draw. May be used for special diagnostic drawing. If used, should
    /// disable the effects of future [`Self::add`] calls.
    fn hit_nothing(&mut self) {}
}

/// Implements [`PixelBuf`] for RGB(A) color with [`f32`] components.
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

/// Implements [`PixelBuf`] for text output: captures the first characters of block names
/// rather than colors.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct CharacterBuf {
    /// Text to draw, if determined yet.
    hit_text: Option<String>,
}

impl PixelBuf for CharacterBuf {
    type Pixel = String;
    type BlockData = Cow<'static, str>;

    fn compute_block_data(s: &SpaceBlockData) -> Self::BlockData {
        // TODO: For more Unicode correctness, index by grapheme cluster...
        // ...and do something clever about double-width characters.
        s.evaluated()
            .attributes
            .display_name
            .chars()
            .next()
            .map(|c| Cow::Owned(c.to_string()))
            .unwrap_or(Cow::Borrowed(&" "))
    }

    fn error_block_data() -> Self::BlockData {
        Cow::Borrowed(&"X")
    }

    fn sky_block_data() -> Self::BlockData {
        Cow::Borrowed(&" ")
    }

    #[inline]
    fn opaque(&self) -> bool {
        self.hit_text.is_some()
    }

    #[inline]
    fn result(self) -> String {
        self.hit_text.unwrap_or_else(|| ".".to_owned())
    }

    #[inline]
    fn add(&mut self, _surface_color: RGBA, text: &Self::BlockData) {
        if self.hit_text.is_none() {
            self.hit_text = Some(text.to_owned().to_string());
        }
    }

    fn hit_nothing(&mut self) {
        self.hit_text = Some(".".to_owned());
    }
}

#[cfg(feature = "rayon")]
mod rayon_helper {
    use rayon::iter::{IntoParallelIterator, ParallelExtend, ParallelIterator as _};
    use std::iter::{empty, once, Sum};

    /// Implements [`ParallelExtend`] to just sum things, so that
    /// [`ParallelIterator::unzip`] can produce a sum.
    #[cfg(feature = "rayon")]
    #[derive(Clone, Copy, Debug, Default)]
    pub struct ParExtSum<T>(Option<T>);

    #[cfg(feature = "rayon")]
    impl<T: Sum> ParExtSum<T> {
        pub fn result(self) -> T {
            self.0.unwrap_or_else(|| empty().sum())
        }
    }

    #[cfg(feature = "rayon")]
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
    use crate::blockgen::make_some_blocks;
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
    // Particularly, test subcube/voxel rendering

    #[test]
    fn print_space_test() {
        let mut space = Space::empty_positive(3, 1, 1);
        let blocks = make_some_blocks(3);
        space.set((0, 0, 0), &blocks[0]).unwrap();
        space.set((1, 0, 0), &blocks[1]).unwrap();
        space.set((2, 0, 0), &blocks[2]).unwrap();

        let mut output = String::new();
        print_space_impl(&space, (1., 1., 1.), |s| output += s);
        print!("{}", output);
        assert_eq!(
            output,
            "\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ...........................0000000000...........................................\n\
            .......................0000000000000001111......................................\n\
            ........................000000001111111111111111................................\n\
            ........................00000011111111111111111112222...........................\n\
            .........................0000011111111111111122222222222222.....................\n\
            .........................000001111111111112222222222222222222222................\n\
            ...........................000011111111122222222222222222222222222..............\n\
            .............................001111111112222222222222222222222222...............\n\
            ...............................111111111222222222222222222222222................\n\
            ..................................11111122222222222222222222222.................\n\
            ....................................11112222222222222222222222..................\n\
            .......................................1222222222222222222222...................\n\
            .........................................2222222222222222222....................\n\
            ............................................222222222222222.....................\n\
            ..............................................22222222222.......................\n\
            ................................................22222222........................\n\
            ...................................................2222.........................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
            ................................................................................\n\
        "
        );
    }
}
