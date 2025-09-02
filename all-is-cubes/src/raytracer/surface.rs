use rand_distr::Distribution;

use crate::block::{Evoxel, Resolution};
use crate::camera::LightingOption;
use crate::math::{Cube, Face7, FaceMap, FreeCoordinate, FreePoint, FreeVector, Rgb, Rgba, Vol};
use crate::raycast::{Ray, RayIsh as _, RaycasterIsh};
use crate::raytracer::{
    BounceRng, ColorBuf, RaytraceInfo, RtBlockData, SpaceRaytracer, TracingBlock, TracingCubeData,
};

/// Description of a surface the ray passes through (or from the volumetric perspective,
/// a transition from one material to another).
// TODO: make public?
#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct Surface<'a, D> {
    pub block_data: &'a D,

    pub diffuse_color: Rgba,

    pub emission: Rgb,

    /// The cube of the [`Space`] which contains the block this surface belongs to.
    pub cube: Cube,

    pub voxel: (Resolution, Cube),

    /// The distance along the ray, in units of the ray's direction vector,
    /// where it intersected the surface.
    pub t_distance: FreeCoordinate,

    /// The point in the [`Space`]'s coordinate system where the ray intersected the surface.
    intersection_point: FreePoint,

    pub normal: Face7,
}

impl<D: RtBlockData> Surface<'_, D> {
    pub(crate) fn visible(&self) -> bool {
        let &Self {
            block_data: _,
            diffuse_color,
            emission,
            cube: _,
            voxel: _,
            t_distance: _,
            intersection_point: _,
            normal: _,
        } = self;
        !diffuse_color.fully_transparent() || emission != Rgb::ZERO
    }

    /// Combine the surface properties, lighting, and graphics options to produce
    /// a light intensity and a transmittance value for light arriving from behind the surface;
    /// or [`None`] if it is invisible.
    ///
    /// Note that the result is “premultiplied alpha”; the returned color should *not*
    /// be modified in any way by the returned transmittance.
    ///
    /// Note that this is completely unaware of volume/thickness; that is handled by
    /// `TracingState::trace_through_span()` tweaking the data before this is called.
    #[inline]
    pub(crate) fn to_light(
        &self,
        rt: &SpaceRaytracer<D>,
        ray_bounce_rng: Option<&mut BounceRng>,
    ) -> Option<(ColorBuf, RaytraceInfo)> {
        let diffuse_color = rt
            .graphics_options
            .transparency
            .limit_alpha(self.diffuse_color);
        if diffuse_color.fully_transparent() && self.emission == Rgb::ZERO {
            // Short-circuit if the surface has no effect.
            return None;
        }

        // Obtain the illumination of this surface.
        let (illumination, info) =
            self.compute_illumination(rt, ray_bounce_rng.filter(|_| diffuse_color.fully_opaque()));
        // Combine reflected and emitted light to produce the outgoing light.
        let outgoing_rgb = diffuse_color.reflect(illumination) + self.emission;

        Some((
            ColorBuf::from_light_and_transmittance(
                outgoing_rgb,
                1.0 - diffuse_color.alpha().into_inner(),
            ),
            info,
        ))
    }

    /// Compute the illumination on this surface from the space in `rt`.
    ///
    /// This will involve tracing further rays if the [`LightingOption`] in use allows and
    /// `bounce` is not [`None`]. `bounce` provides the RNG for randomly directing rays.
    /// The returned count of rays will be non-zero in that case.
    fn compute_illumination(
        &self,
        rt: &SpaceRaytracer<D>,
        bounce: Option<&mut BounceRng>,
    ) -> (Rgb, RaytraceInfo) {
        match (&rt.graphics_options.lighting_display, bounce) {
            (LightingOption::Bounce, Some(rng)) => {
                let mut multi_ray_accum: Rgb = Rgb::ZERO;
                let mut info_accum = RaytraceInfo::default();

                // Trace multiple pseudorandomly-directed secondary rays.
                let sample_count = 16u8;
                for _ in 0..sample_count {
                    // Choose a random reflection direction.
                    // This formula produces a distribution that obeys Lambert’s cosine law.
                    // TODO: Justify that claim. <https://fizzer.neocities.org/lambertnotangent>
                    // seems to agree but I don’t think it’s where I learned this trick and it isn’t
                    // as clear as I would like.
                    let lambertian_bounce_direction = self.normal.normal_vector()
                        + FreeVector::from(rand_distr::UnitSphere.sample(rng));

                    // If we wanted mirror reflection we would do it like this instead,
                    // but for now, voxels have no specularity parameters and are assumed to be
                    // always Lambertian.
                    //
                    // let mut mirror_direction = ray_direction.normalize();
                    // let Some(axis) = self.normal.axis() {
                    //     mirror_direction[axis] *= -1.0;
                    // } else { continue };

                    let ray = Ray::new(
                        // need some past-the-surface epsilon
                        self.intersection_point + self.normal.normal_vector() * 0.0001,
                        lambertian_bounce_direction,
                    );

                    // Compute the illumination from the reflected ray.
                    // Note that we pass allow_ray_bounce=false so that there will be no further
                    // bounces; the stored light data essentially completely suffices after one
                    // bounce. (This would not be true if we had any mirror reflections.)
                    let (light_accum_buf, ray_info) = rt
                        .trace_ray_impl::<super::IgnoreBlockData<D, ColorBuf>, Ray>(
                            ray, true, false,
                        );
                    multi_ray_accum += Rgba::from(light_accum_buf.inner).to_rgb();
                    info_accum += ray_info;
                }
                (
                    multi_ray_accum * f32::from(sample_count).recip(),
                    info_accum,
                )
            }

            // The non-raytraced options:
            (LightingOption::None, _) => (Rgb::ONE, RaytraceInfo::default()),

            // Note that if we've exceeded our bounce budget (which is always 1) we use Flat.
            // We don't combine Bounce and Smooth because the improvement of Smooth is negligible.
            (LightingOption::Flat | LightingOption::Bounce, _) => {
                let light = rt
                    .get_packed_light(self.cube + self.normal.normal_vector())
                    .value();
                (light, RaytraceInfo::default())
            }

            (LightingOption::Smooth, _) => {
                let light = rt.get_interpolated_light(self.intersection_point, self.normal);
                (light, RaytraceInfo::default())
            }
        }
    }
}

/// Builds on [`Surface`] to report the depth (length of ray through volume)
/// of a transparent surface.
#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct Span<'a, D> {
    pub surface: Surface<'a, D>,
    /// Distance along the ray at which the ray exits this volume.
    ///
    /// The `surface.t_distance` is the corresponding entry point.
    pub exit_t_distance: FreeCoordinate,
}

/// Output of [`SurfaceIter`], describing a single step of the raytracing process.
#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum TraceStep<'a, D> {
    /// A block or voxel surface as described was entered.
    ///
    /// Note that when the surface is invisible (alpha is zero or treated as zero),
    /// [`TraceStep::Invisible`] is returned.
    EnterSurface(Surface<'a, D>),

    /// A completely invisible surface was found.
    /// This is reported so that it may be counted in a decision to stop tracing.
    /// It is separate from [`TraceStep::EnterSurface`] to avoid computing lighting.
    Invisible { t_distance: FreeCoordinate },

    /// A recursive block was entered.
    ///
    /// This is reported so as to keep the iteration process O(1).
    /// In particular, otherwise an unbounded number of steps could be taken if the ray
    /// passes through a large number of recursive blocks with small bounds (such that no
    /// actual voxels exist to be visible or invisible).
    ///
    /// It also allows the recipient to note all `block_data` traversed even if none of the
    /// involved voxels are hit.
    EnterBlock {
        t_distance: FreeCoordinate,
        block_data: &'a D,
    },
}

/// An [`Iterator`] which reports each visible surface a [`Raycaster`] ray passes through.
#[derive(Clone, Debug)]
pub(crate) struct SurfaceIter<'a, D, R>
where
    R: RaycasterIsh,
{
    ray: R::Ray,
    block_raycaster: R,
    state: SurfaceIterState,
    // TODO: Should `current_block` become part of the state?
    current_block: Option<VoxelSurfaceIter<'a, D, R>>,
    blocks: &'a [TracingBlock<D>],
    array: Vol<&'a [TracingCubeData]>,
}

#[derive(Clone, Copy, Debug)]
enum SurfaceIterState {
    Initial,
    /// At least one raycast step within the space has been seen.
    EnteredSpace,
}

impl<'a, D: RtBlockData, R: RaycasterIsh> SurfaceIter<'a, D, R> {
    #[inline]
    pub(crate) fn new(rt: &'a SpaceRaytracer<D>, ray: R::Ray) -> Self {
        let mut block_raycaster = ray.cast();
        block_raycaster.add_bounds(rt.cubes.bounds());
        Self {
            ray,
            block_raycaster,
            state: SurfaceIterState::Initial,
            current_block: None,
            blocks: &rt.blocks,
            array: rt.cubes.as_ref(),
        }
    }
}

impl<'a, D, R> Iterator for SurfaceIter<'a, D, R>
where
    R: RaycasterIsh,
{
    type Item = TraceStep<'a, D>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(b) = &mut self.current_block
            && let Some(surface) = b.next()
        {
            return Some(surface);
        }
        // If we get here, self.current_block is either exhausted or already None
        self.current_block = None;

        let rc_step = self.block_raycaster.next()?;

        let cube_data: &TracingCubeData = match self.array.get(rc_step.cube_ahead()) {
            Some(cube_data) => {
                match &self.state {
                    SurfaceIterState::Initial => {
                        // If the ray entered the space, we also want to notice leaving.
                        // Do this by expanding the raycast bounds by one cube.
                        self.block_raycaster.remove_bounds();
                        self.block_raycaster
                            .add_bounds(self.array.bounds().expand(FaceMap::splat(1)));

                        self.state = SurfaceIterState::EnteredSpace;
                    }
                    SurfaceIterState::EnteredSpace => {}
                }

                cube_data
            }
            None => {
                // Just exiting the space — we previously set up a 1-block perimeter to ensure
                // that the exit's t_distance is reported rather than ignored.
                return Some(TraceStep::Invisible {
                    t_distance: rc_step.t_distance(),
                });
            }
        };
        if cube_data.always_invisible {
            // Early return that avoids indirecting through self.blocks
            return Some(TraceStep::Invisible {
                t_distance: rc_step.t_distance(),
            });
        }

        let tb: &TracingBlock<D> = &self.blocks[cube_data.block_index as usize];
        Some(match tb.voxels.single_voxel() {
            Some(Evoxel {
                color, emission, ..
            }) => {
                if color.fully_transparent() && emission == Rgb::ZERO {
                    // The caller could generically skip transparent, but if we do it then
                    // we can skip some math too.
                    TraceStep::Invisible {
                        t_distance: rc_step.t_distance(),
                    }
                } else {
                    TraceStep::EnterSurface(Surface {
                        block_data: &tb.block_data,
                        diffuse_color: color,
                        emission,
                        cube: rc_step.cube_ahead(),
                        voxel: (Resolution::R1, Cube::ORIGIN),
                        t_distance: rc_step.t_distance(),
                        intersection_point: rc_step.intersection_point(self.ray.into()),
                        normal: rc_step.face(),
                    })
                }
            }
            None => {
                let resolution = tb.voxels.resolution();
                let array = tb.voxels.as_vol_ref();
                let block_cube = rc_step.cube_ahead();
                let (sub_raycaster, sub_ray) =
                    R::recursive_raycast(rc_step, self.ray, resolution, array.bounds());
                let antiscale = FreeCoordinate::from(resolution).recip();

                self.current_block = Some(VoxelSurfaceIter {
                    voxel_ray: sub_ray,
                    voxel_raycaster: sub_raycaster,
                    block_data: &tb.block_data,
                    resolution,
                    antiscale,
                    array,
                    block_cube,
                });

                TraceStep::EnterBlock {
                    t_distance: rc_step.t_distance(),
                    block_data: &tb.block_data,
                }
            }
        })
    }

    // TODO: implement fold if it helps
}

/// Iterates over a [`Block`]'s voxels. Internal helper for [`SurfaceIter`].
#[derive(Clone, Debug)]
struct VoxelSurfaceIter<'a, D, R>
where
    R: RaycasterIsh,
{
    voxel_ray: R::Ray,
    voxel_raycaster: R,
    block_data: &'a D,
    resolution: Resolution,
    /// Reciprocal of resolution, for scaling back to outer world
    antiscale: FreeCoordinate,
    array: Vol<&'a [Evoxel]>,

    /// Cube these voxels are located in, for lighting lookups.
    block_cube: Cube,
}
impl<'a, D, R> VoxelSurfaceIter<'a, D, R>
where
    R: RaycasterIsh,
{
    /// This is not an  implementation of `Iterator` because it doesn't need to be — it's
    /// purely internal to [`SurfaceIter`].
    fn next(&mut self) -> Option<TraceStep<'a, D>> {
        // Fetch data and return None if out of range.
        let rc_step = self.voxel_raycaster.next()?;
        let voxel = self.array.get(rc_step.cube_ahead())?;

        // Note: The proper scaling here depends on the direction vector scale, that
        // recursive_raycast() _doesn't_ change.
        let t_distance = rc_step.t_distance() * self.antiscale;

        if voxel.color.fully_transparent() && voxel.emission == Rgb::ZERO {
            return Some(TraceStep::Invisible { t_distance });
        }

        Some(TraceStep::EnterSurface(Surface {
            block_data: self.block_data,
            diffuse_color: voxel.color,
            emission: voxel.emission,
            cube: self.block_cube,
            voxel: (self.resolution, rc_step.cube_ahead()),
            t_distance,
            intersection_point: rc_step.intersection_point(self.voxel_ray.into()) * self.antiscale
                + self
                    .block_cube
                    .lower_bounds()
                    .map(FreeCoordinate::from)
                    .to_vector(),
            normal: rc_step.face(),
        }))
    }
}

/// Builds on [`SurfaceIter`] to report spans of transparency along the ray.
pub(crate) struct DepthIter<'a, D, R>
where
    R: RaycasterIsh,
{
    surface_iter: SurfaceIter<'a, D, R>,

    /// Present if the last `EnterSurface` we discovered was transparent, or if
    /// we have another surface to report.
    last_surface: Option<Surface<'a, D>>,

    /// If present, the iterator returns this before doing anything else.
    /// Used to produce two items from one input.
    buffered_next: Option<DepthStep<'a, D>>,
}

impl<'a, D, R> DepthIter<'a, D, R>
where
    R: RaycasterIsh,
{
    #[inline]
    pub(crate) fn new(surface_iter: SurfaceIter<'a, D, R>) -> Self {
        Self {
            surface_iter,
            last_surface: None,
            buffered_next: None,
        }
    }

    fn flush_last_surface(&mut self, t_distance: f64) -> DepthStep<'a, D> {
        match self.last_surface.take() {
            Some(last_surface) => DepthStep::Span(Span {
                surface: last_surface,
                exit_t_distance: t_distance,
            }),
            None => DepthStep::Invisible,
        }
    }
}

impl<'a, D, R> Iterator for DepthIter<'a, D, R>
where
    R: RaycasterIsh,
{
    type Item = DepthStep<'a, D>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let ret @ Some(_) = self.buffered_next.take() {
            return ret;
        }

        // TODO: .next()? isn't quite right; we need to flush the last input, potentially? Or, no, that should never happen, but we should assert that.
        Some(match self.surface_iter.next()? {
            TraceStep::EnterSurface(this_surface) => {
                let exit_t_distance = this_surface.t_distance;
                match self.last_surface.replace(this_surface) {
                    Some(last_surface) => DepthStep::Span(Span {
                        surface: last_surface,
                        exit_t_distance,
                    }),
                    None => DepthStep::Invisible,
                }
            }
            TraceStep::Invisible { t_distance } => self.flush_last_surface(t_distance),
            TraceStep::EnterBlock {
                t_distance,
                block_data,
            } => {
                let item = self.flush_last_surface(t_distance);
                self.buffered_next = Some(DepthStep::EnterBlock {
                    t_distance,
                    block_data,
                });
                item
            }
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum DepthStep<'a, D> {
    Invisible,

    Span(Span<'a, D>),

    /// See [`TraceStep::EnterBlock`].
    EnterBlock {
        t_distance: FreeCoordinate,
        block_data: &'a D,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{AIR, Block, Resolution::*};
    use crate::camera::GraphicsOptions;
    use crate::content;
    use crate::math::{GridAab, rgba_const};
    use crate::raycast::{self, Ray};
    use crate::space::Space;
    use crate::universe::Universe;
    use TraceStep::{EnterBlock, EnterSurface, Invisible};
    use alloc::vec::Vec;
    use euclid::point3;
    use pretty_assertions::assert_eq;

    type SurfaceIterR<'a> = SurfaceIter<'a, (), raycast::Raycaster>;

    #[test]
    fn surface_and_depth_iter_basic() {
        let solid_test_color = rgba_const!(1., 0., 0., 1.);
        let slab_test_color = rgba_const!(1., 1., 0., 1.);
        let slab_test_color_block = Block::from(slab_test_color);

        let universe = &mut Universe::new();
        // This block has some AIR in it that the iterator will traverse
        let slab_with_extra_space = Block::builder()
            .voxels_fn(R4, |cube| {
                // the x part of the condition prevents voxels_fn() auto-bounds-shrinking
                if cube.y >= 2 && cube.x != 0 {
                    &AIR
                } else {
                    &slab_test_color_block
                }
            })
            .unwrap()
            .build_into(universe);
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 3, 1]))
            .read_ticket(universe.read_ticket())
            .build_and_mutate(|m| {
                m.set([0, 1, 0], Block::from(solid_test_color)).unwrap();
                m.set([0, 2, 0], slab_with_extra_space).unwrap();
                Ok(())
            })
            .unwrap();

        let rt = SpaceRaytracer::<()>::new(&space, GraphicsOptions::default(), ());
        let ray = Ray::new([0.25, -0.5, 0.25], [0., 1., 0.]);

        assert_eq!(
            SurfaceIterR::new(&rt, ray).collect::<Vec<TraceStep<'_, ()>>>(),
            vec![
                Invisible { t_distance: 0.5 }, // Cube [0, 0, 0] is empty
                EnterSurface(Surface {
                    block_data: &(),
                    diffuse_color: solid_test_color,
                    emission: Rgb::ZERO,
                    cube: Cube::new(0, 1, 0),
                    voxel: (R1, Cube::new(0, 0, 0)),
                    t_distance: 1.5, // half-block starting point + 1 empty block
                    intersection_point: point3(0.25, 1.0, 0.25),
                    normal: Face7::NY
                }),
                EnterBlock {
                    t_distance: 2.5,
                    block_data: &()
                },
                EnterSurface(Surface {
                    block_data: &(),
                    diffuse_color: slab_test_color,
                    emission: Rgb::ZERO,
                    cube: Cube::new(0, 2, 0),
                    voxel: (R4, Cube::new(1, 0, 1)),
                    t_distance: 2.5,
                    intersection_point: point3(0.25, 2.0, 0.25),
                    normal: Face7::NY
                }),
                // Second layer of slab.
                EnterSurface(Surface {
                    block_data: &(),
                    diffuse_color: slab_test_color,
                    emission: Rgb::ZERO,
                    cube: Cube::new(0, 2, 0),
                    voxel: (R4, Cube::new(1, 1, 1)),
                    t_distance: 2.75, // previous surface + 1/4 block of depth
                    intersection_point: point3(0.25, 2.25, 0.25),
                    normal: Face7::NY
                }),
                // Two top layers of slab.
                Invisible { t_distance: 3.0 },
                Invisible { t_distance: 3.25 },
                // "Back face" of final block, which we report so as to ensure that the
                // t_distance of the _exit_ point is known, for the benefit of volumetric
                // rendering.
                Invisible { t_distance: 3.5 },
            ]
        );

        // DepthIter is built on SurfaceIter, so it makes sense to test together and second
        assert_eq!(
            DepthIter::new(SurfaceIterR::new(&rt, ray)).collect::<Vec<DepthStep<'_, ()>>>(),
            vec![
                DepthStep::Invisible,
                DepthStep::Invisible,
                DepthStep::Span(Span {
                    surface: Surface {
                        block_data: &(),
                        diffuse_color: solid_test_color,
                        emission: Rgb::ZERO,
                        cube: Cube::new(0, 1, 0),
                        voxel: (R1, Cube::new(0, 0, 0)),
                        t_distance: 1.5,
                        intersection_point: point3(0.25, 1.0, 0.25),
                        normal: Face7::NY,
                    },
                    exit_t_distance: 2.5,
                }),
                DepthStep::EnterBlock {
                    t_distance: 2.5,
                    block_data: &(),
                },
                DepthStep::Invisible,
                DepthStep::Span(Span {
                    surface: Surface {
                        block_data: &(),
                        diffuse_color: rgba_const!(1., 1., 0., 1.),
                        emission: Rgb::ZERO,
                        cube: Cube::new(0, 2, 0),
                        voxel: (R4, Cube::new(1, 0, 1)),
                        t_distance: 2.5,
                        intersection_point: point3(0.25, 2.0, 0.25),
                        normal: Face7::NY,
                    },
                    exit_t_distance: 2.75,
                }),
                DepthStep::Span(Span {
                    surface: Surface {
                        block_data: &(),
                        diffuse_color: rgba_const!(1., 1., 0., 1.),
                        emission: Rgb::ZERO,
                        cube: Cube::new(0, 2, 0),
                        voxel: (R4, Cube::new(1, 1, 1)),
                        t_distance: 2.75,
                        intersection_point: point3(0.25, 2.25, 0.25),
                        normal: Face7::NY,
                    },
                    exit_t_distance: 3.0,
                }),
                DepthStep::Invisible,
                DepthStep::Invisible,
            ]
        );
    }

    /// Test that exiting a block at the edge of the space still reports the exit t-distance.
    #[test]
    fn surface_iter_exit_block_at_end_of_space() {
        let solid_test_color = rgba_const!(1., 0., 0., 1.);
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .filled_with(Block::from(solid_test_color))
            .build();

        let rt = SpaceRaytracer::<()>::new(&space, GraphicsOptions::default(), ());

        assert_eq!(
            SurfaceIterR::new(&rt, Ray::new([-0.5, 0.5, 0.5], [1., 0., 0.]))
                .collect::<Vec<TraceStep<'_, ()>>>(),
            vec![
                EnterSurface(Surface {
                    block_data: &(),
                    diffuse_color: solid_test_color,
                    emission: Rgb::ZERO,
                    cube: Cube::new(0, 0, 0),
                    voxel: (R1, Cube::ORIGIN),
                    t_distance: 0.5, // half-block starting point
                    intersection_point: point3(0.0, 0.5, 0.5),
                    normal: Face7::NX
                }),
                // Exit block -- this is the critical step that we're checking for.
                Invisible { t_distance: 1.5 },
            ]
        );
    }

    /// Test what happens, with both iterator types, when the ray passes through a block's cube but
    /// hits none of its voxel data.
    #[test]
    fn ray_misses_voxels() {
        let universe = &mut Universe::new();
        let slab = content::make_slab(universe, 1, R2);
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .read_ticket(universe.read_ticket())
            .filled_with(slab)
            .build();
        let rt = SpaceRaytracer::<()>::new(&space, GraphicsOptions::default(), ());
        let ray = Ray::new([-0.5, 0.75, 0.25], [1., 0., 0.]);

        assert_eq!(
            SurfaceIterR::new(&rt, ray).collect::<Vec<TraceStep<'_, ()>>>(),
            vec![
                EnterBlock {
                    t_distance: 0.5,
                    block_data: &()
                },
                Invisible { t_distance: 1.5 },
            ]
        );

        assert_eq!(
            DepthIter::new(SurfaceIterR::new(&rt, ray)).collect::<Vec<DepthStep<'_, ()>>>(),
            vec![
                // TODO: This step isn't, I think, really necessary for anything, and removing
                // it could increase performance.
                // But if we remove it we're making `EnterBlock` mean more. Think carefully.
                DepthStep::Invisible,
                DepthStep::EnterBlock {
                    t_distance: 0.5,
                    block_data: &()
                },
                DepthStep::Invisible,
            ]
        );
    }
}
