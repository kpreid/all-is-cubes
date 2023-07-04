//! Lighting algorithms for `Space`. This module is closely tied to `Space`
//! and separated out for readability, not modularity.

use std::cmp::Ordering;
use std::fmt;

use cgmath::{EuclideanSpace as _, InnerSpace as _, Point3, Vector3};
use instant::{Duration, Instant};
use once_cell::sync::Lazy;

use super::debug::LightComputeOutput;
use super::LightUpdateRequest;
use crate::block::EvaluatedBlock;
use crate::math::{Face6, FaceMap, FreeCoordinate, Geometry, GridPoint, NotNan, Rgb};
use crate::raycast::{Ray, RaycastStep};
use crate::space::light::{LightUpdateRayInfo, Priority};
use crate::space::{GridAab, LightPhysics, PackedLight, PackedLightScalar, Space, SpaceChange};
use crate::util::{CustomFormat, StatusText};

/// This parameter determines to what degree absorption of light due to a block surface's
/// color is taken into account. At zero, it is not (all surfaces are perfectly
/// reflective); at one, light values are simply multiplied by the surface color (e.g.
/// a red surface will reflect no green or blue light), which is the idealized physical
/// model.
const SURFACE_ABSORPTION: f32 = 0.75;

const RAY_DIRECTION_STEP: isize = 5;
const RAY_CUBE_EDGE: usize = (RAY_DIRECTION_STEP as usize) * 2 + 1;
const ALL_RAYS_COUNT: usize = RAY_CUBE_EDGE.pow(3) - (RAY_CUBE_EDGE - 2).pow(3);

#[derive(Debug)]
struct LightRayData {
    ray: Ray,
    face_cosines: FaceMap<f32>,
}

// TODO: Make multiple ray patterns that suit the maximum_distance parameter.
static LIGHT_RAYS: Lazy<[LightRayData; ALL_RAYS_COUNT]> = Lazy::new(|| {
    let mut rays: Vec<LightRayData> = Vec::new();
    let origin = Point3::new(0.5, 0.5, 0.5);

    // TODO: octahedron instead of cube
    for x in -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP {
        for y in -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP {
            for z in -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP {
                if x.abs() == RAY_DIRECTION_STEP
                    || y.abs() == RAY_DIRECTION_STEP
                    || z.abs() == RAY_DIRECTION_STEP
                {
                    let direction = Vector3::new(
                        x as FreeCoordinate,
                        y as FreeCoordinate,
                        z as FreeCoordinate,
                    )
                    .normalize();
                    rays.push(LightRayData {
                        ray: Ray { origin, direction },
                        face_cosines: FaceMap::from_fn(|face| {
                            face.dot(direction.map(|s| s as f32)).max(0.0)
                        }),
                    });
                }
            }
        }
    }
    rays.try_into().unwrap()
});

/// Methods on Space that specifically implement the lighting algorithm.
impl Space {
    pub(crate) fn light_needs_update(&mut self, cube: GridPoint, priority: Priority) {
        if self.physics.light == LightPhysics::None {
            return;
        }

        if self.bounds().contains_cube(cube) {
            self.light_update_queue
                .insert(LightUpdateRequest { priority, cube });
        }
    }

    pub(crate) fn in_light_update_queue(&self, cube: GridPoint) -> bool {
        self.light_update_queue.contains(cube)
    }

    /// Do some lighting updates.
    #[doc(hidden)] // TODO: eliminate calls outside the crate
    pub fn update_lighting_from_queue(&mut self, budget: Duration) -> LightUpdatesInfo {
        let mut light_update_count: usize = 0;
        self.last_light_updates.clear();
        let mut max_difference: PackedLightScalar = 0;

        if self.physics.light != LightPhysics::None && !budget.is_zero() {
            let t0 = Instant::now();
            let mut cost = 0;
            // We convert the time budget to an arbitrary cost value in order to avoid
            // the overhead of frequently making syscalls to check the clock.
            //
            // The unit of measure is one raycast step; other operations are arbitrarily assigned
            // higher cost values. (TODO: Profile to assign more consistent cost values.)
            //
            // TODO: Is this worthwhile?
            let max_cost = (budget.as_secs_f32() / self.light_cost_scale) as usize;

            while let Some(LightUpdateRequest { cube, .. }) = self.light_update_queue.pop() {
                if false {
                    // Log cubes that were updated for debug visualization.
                    self.last_light_updates.push(cube);
                }
                light_update_count += 1;
                let (difference, cube_cost) = self.update_lighting_now_on(cube);
                max_difference = max_difference.max(difference);
                cost += cube_cost;
                if cost >= max_cost {
                    break;
                }
            }

            let t1 = Instant::now();
            let cost_scale = (t1 - t0).as_secs_f32() / cost as f32;
            if cost_scale.is_finite() {
                // TODO(time-budget): don't let this grow or shrink too fast due to outliers
                self.light_cost_scale = 0.125 * cost_scale + 0.875 * self.light_cost_scale;
            }
        }

        LightUpdatesInfo {
            update_count: light_update_count,
            max_update_difference: max_difference,
            queue_count: self.light_update_queue.len(),
            max_queue_priority: self.light_update_queue.peek_priority(),
        }
    }

    #[inline]
    fn update_lighting_now_on(&mut self, cube: GridPoint) -> (PackedLightScalar, usize) {
        let (new_light_value, dependencies, mut cost, ()) = self.compute_lighting(cube);
        let old_light_value: PackedLight = self.get_lighting(cube);
        // Compare and set new value. Note that we MUST compare only the packed value so
        // that changes are detected in terms of that rounding, not float values.
        let difference_priority = new_light_value.difference_priority(old_light_value);
        if difference_priority > 0 {
            cost += 200;
            // TODO: compute index only once
            self.lighting[self.bounds().index(cube).unwrap()] = new_light_value;
            self.notifier.notify(SpaceChange::Lighting(cube));

            // The light algorithm, in its current form, can spend a very long time
            // evaluating 1-unit differences and possibly even loop infinitely. As a
            // pragmatic solution, don't bother queueing them at all. This means that
            // there may be 1-unit random differences lying around, but then, the
            // "reevaluate all the dependencies of the current evaluation" strategy
            // is also not perfect at updating everything that theoretically should be,
            // since the rays are not perfectly reciprocal.
            if difference_priority > 1 {
                let priority = Priority::from_difference(difference_priority);
                for cube in dependencies {
                    self.light_needs_update(cube, priority);
                }
            }
        }
        (difference_priority, cost)
    }

    /// Compute the new lighting value for a cube.
    ///
    /// The returned vector of points lists those cubes which the computed value depends on
    /// (imprecisely; empty cubes passed through are not listed).
    #[inline]
    #[doc(hidden)] // pub to be used by all-is-cubes-gpu for debugging
    pub fn compute_lighting<D>(&self, cube: GridPoint) -> (PackedLight, Vec<GridPoint>, usize, D)
    where
        D: LightComputeOutput,
    {
        let maximum_distance = match self.physics.light {
            LightPhysics::None => {
                panic!("Light is disabled; should not reach here");
            }
            LightPhysics::Rays { maximum_distance } => FreeCoordinate::from(maximum_distance),
        };

        let mut cube_buffer = LightBuffer::new();
        let mut info_rays = D::RayInfoBuffer::default();

        let ev_origin = self.get_evaluated(cube);
        let origin_is_opaque = ev_origin.opaque == FaceMap::repeat(true);
        if origin_is_opaque {
            // Opaque blocks are always dark inside — unless they are light sources.
            if !opaque_for_light_computation(ev_origin) {
                cube_buffer.add_weighted_light(ev_origin.attributes.light_emission, 1.0);
            }
        } else {
            let ev_neighbors =
                FaceMap::from_fn(|face| self.get_evaluated(cube + face.normal_vector()));
            let direction_weights = directions_to_seek_light(ev_origin, ev_neighbors);

            // TODO: Choose a ray pattern that suits the maximum_distance.
            for &LightRayData { ray, face_cosines } in &LIGHT_RAYS[..] {
                // TODO: Theoretically we should weight light rays by the cosine but that has caused poor behavior in the past.
                let ray_weight_by_faces = face_cosines
                    .zip(direction_weights, |_face, ray_cosine, reflects| {
                        ray_cosine * reflects
                    })
                    .into_values_iter()
                    .sum::<f32>();
                if ray_weight_by_faces <= 0.0 {
                    continue;
                }
                let mut ray_state = LightRayState::new(cube, ray, ray_weight_by_faces);
                let raycaster = ray_state.translated_ray.cast().within(self.bounds());

                'raycast: for hit in raycaster {
                    cube_buffer.cost += 1;
                    if hit.t_distance() > maximum_distance {
                        // Rays that didn't hit anything close enough will be treated
                        // as sky. TODO: We should have a better policy in case of large
                        // indoor spaces.
                        break 'raycast;
                    }
                    cube_buffer.traverse::<D>(&mut ray_state, &mut info_rays, self, hit);
                    if ray_state.alpha.partial_cmp(&0.0) != Some(Ordering::Greater) {
                        break;
                    }
                }
                cube_buffer.end_of_ray(&mut ray_state, self);
            }
        }

        let new_light_value = cube_buffer.finish(origin_is_opaque);

        (
            new_light_value,
            cube_buffer.dependencies,
            cube_buffer.cost,
            D::new(cube, new_light_value, info_rays),
        )
    }

    /// Clear and recompute light data and update queue, in a way which gets fast approximate
    /// results suitable for flat landscapes mostly lit from above (the +Y axis).
    ///
    /// TODO: Revisit whether this is a good public API.
    pub fn fast_evaluate_light(&mut self) {
        self.light_update_queue.clear(); // Going to refill it

        if self.physics.light == LightPhysics::None {
            return;
        }

        let bounds = self.bounds();
        for x in bounds.x_range() {
            for z in bounds.z_range() {
                let mut covered = false;
                for y in bounds.y_range().rev() {
                    let cube = GridPoint::new(x, y, z);
                    let index = bounds.index(cube).unwrap();

                    let this_cube_evaluated = &self.palette.entry(self.contents[index]).evaluated;
                    self.lighting[index] = if opaque_for_light_computation(this_cube_evaluated) {
                        covered = true;
                        PackedLight::OPAQUE
                    } else {
                        if this_cube_evaluated.visible_or_animated()
                            || Face6::ALL.into_iter().any(|face| {
                                self.get_evaluated(cube + face.normal_vector())
                                    .visible_or_animated()
                            })
                        {
                            // In this case (and only this case), we are guessing rather than being certain,
                            // so we need to schedule a proper update.
                            // (Bypassing `self.light_needs_update()` to skip bounds checks).
                            self.light_update_queue.insert(LightUpdateRequest {
                                priority: Priority::ESTIMATED,
                                cube,
                            });

                            if covered {
                                PackedLight::ZERO
                            } else {
                                self.packed_sky_color
                            }
                        } else {
                            PackedLight::NO_RAYS
                        }
                    };
                }
            }
        }
    }
}

impl LightPhysics {
    /// Generate the lighting data array that a newly created empty [`Space`] should have.
    pub(crate) fn initialize_lighting(&self, bounds: GridAab) -> Box<[PackedLight]> {
        match self {
            LightPhysics::None => Box::new([]),
            LightPhysics::Rays { .. } => {
                vec![PackedLight::NO_RAYS; bounds.volume()].into_boxed_slice()
            }
        }
    }
}

/// Given a block and its neighbors, which directions should we cast rays to find light
/// falling on it?
fn directions_to_seek_light(
    origin: &EvaluatedBlock,
    neighborhood: FaceMap<&EvaluatedBlock>,
) -> FaceMap<f32> {
    if origin.visible_or_animated() {
        // Non-opaque blocks should work the same as blocks which have all six adjacent faces present.
        FaceMap::repeat(1.0)
    } else {
        FaceMap::from_fn(|face| {
            // We want directions that either face away from visible faces, or towards light sources.
            if neighborhood[face.opposite()].visible_or_animated()
                || neighborhood[face].attributes.light_emission != Rgb::ZERO
            {
                // TODO: Once we have fancier block opacity precomputations, use them to
                // have weights besides 1.0
                1.0f32
            } else {
                0.0
            }
        })
    }
}

/// Accumulation buffer for the light falling on a single cube.
#[derive(Debug)]
struct LightBuffer {
    /// Accumulator of incoming light encountered.
    /// TODO: Make this a vector of f32 to save NaN checks?
    incoming_light: Rgb,
    /// Number of rays contributing to incoming_light.
    total_rays: usize,
    /// Number of rays, weighted by the ray angle versus local cube faces.
    total_ray_weight: f32,
    /// Cubes whose lighting value contributed to the incoming_light value.
    dependencies: Vec<GridPoint>,
    /// Approximation of CPU cost of doing the calculation, with one unit defined as
    /// one raycast step.
    cost: usize,
}

/// Companion to [`LightBuffer`] that tracks state for a single ray that makes part of
/// the sum.
struct LightRayState {
    /// Fraction of the light value that is to be determined by future, rather than past,
    /// tracing; starts at 1.0 and decreases as opaque surfaces are encountered.
    alpha: f32,
    /// Weighting factor for how much this ray contributes to the total light.
    /// If zero, this will not be counted as a ray at all.
    ray_weight_by_faces: f32,
    /// The cube we're lighting; remembered to check for loopbacks
    origin_cube: GridPoint,
    /// The ray we're casting; remembered for debugging only. (TODO: avoid this?)
    translated_ray: Ray,
}

impl LightRayState {
    /// * `origin_cube`: cube we are actually starting from
    /// * `abstract_ray`: ray as if we were lighting the [0, 0, 0] cube
    /// * `ray_weight_by_faces`: how much influence this ray should have on the
    ///   total illumination
    fn new(origin_cube: GridPoint, abstract_ray: Ray, ray_weight_by_faces: f32) -> Self {
        let translated_ray =
            abstract_ray.translate(origin_cube.cast::<FreeCoordinate>().unwrap().to_vec());
        LightRayState {
            alpha: 1.0,
            ray_weight_by_faces,
            origin_cube,
            translated_ray,
        }
    }
}

impl LightBuffer {
    fn new() -> Self {
        Self {
            incoming_light: Rgb::ZERO,
            total_rays: 0,
            total_ray_weight: 0.0,
            dependencies: Vec::new(),
            cost: 0,
        }
    }

    /// Process a ray intersecting a single cube.
    ///
    /// The caller should check `ray_state.alpha` to decide when to stop calling this.
    #[inline]
    #[allow(clippy::too_many_arguments)]
    fn traverse<D>(
        &mut self,
        ray_state: &mut LightRayState,
        info: &mut D::RayInfoBuffer,
        space: &Space,
        hit: RaycastStep,
    ) where
        D: LightComputeOutput,
    {
        let ev_hit = space.get_evaluated(hit.cube_ahead());
        if !ev_hit.visible_or_animated() {
            // Completely transparent block is passed through.
            return;
        }

        // Compute whether we hit an opaque face which should stop propagation.
        // TODO: Also count the opacity of the face we *exited* of the previous block,
        let hit_opaque_face = {
            let hit_opaque = ev_hit.opaque;
            match Face6::try_from(hit.face()) {
                Ok(face) => hit_opaque[face],
                Err(_) => hit_opaque == FaceMap::repeat(true),
            }
        };

        if hit_opaque_face {
            // On striking a fully opaque block face, we use the light value from its
            // adjacent cube as the light falling on that face.
            let light_cube = hit.cube_behind();
            if light_cube == hit.cube_ahead() {
                // Don't read the value we're trying to recalculate.
                // (And we hit an opaque block, so this ray is stopping.)

                // Setting the weight to 0 cancels its future effect,
                // and there were no past effects.
                ray_state.ray_weight_by_faces = 0.0;
                ray_state.alpha = 0.0;
                return;
            }
            let stored_light = space.get_lighting(light_cube);

            let surface_color = ev_hit.color.clamp().to_rgb() * SURFACE_ABSORPTION
                + Rgb::ONE * (1. - SURFACE_ABSORPTION);
            let light_from_struck_face =
                ev_hit.attributes.light_emission + stored_light.value() * surface_color;
            self.incoming_light +=
                light_from_struck_face * ray_state.alpha * ray_state.ray_weight_by_faces;
            self.dependencies.push(light_cube);
            self.cost += 10;
            // This terminates the raycast; we don't bounce rays
            // (diffuse reflections, not specular/mirror).
            ray_state.alpha = 0.0;

            // Diagnostics. TODO: Track transparency too.
            D::push_ray(
                info,
                LightUpdateRayInfo {
                    ray: Ray {
                        origin: ray_state.translated_ray.origin,
                        direction: hit.intersection_point(ray_state.translated_ray)
                            - ray_state.translated_ray.origin,
                    },
                    trigger_cube: hit.cube_ahead(),
                    value_cube: light_cube,
                    value: stored_light,
                },
            );
        } else {
            // Block is partly transparent and light should pass through.
            let light_cube = hit.cube_ahead();

            let stored_light = if light_cube == ray_state.origin_cube {
                // Don't read the value we're trying to recalculate.
                Rgb::ZERO
            } else {
                space.get_lighting(light_cube).value()
            };
            // 'coverage' is what fraction of the light ray we assume to hit this block,
            // as opposed to passing through it.
            // The block evaluation algorithm incidentally computes a suitable
            // approximation as an alpha value.
            let coverage = ev_hit.color.alpha().into_inner().clamp(0.0, 1.0);
            self.incoming_light += (ev_hit.attributes.light_emission + stored_light)
                * coverage
                * ray_state.alpha
                * ray_state.ray_weight_by_faces;
            self.cost += 10;
            ray_state.alpha *= 1.0 - coverage;

            self.dependencies.push(hit.cube_ahead());
            // We did not read hit.cube_behind(), but we want to trigger its updates
            // anyway, because otherwise, transparent blocks' neighbors will *never*
            // get their light updated except when the block is initially placed.
            self.dependencies.push(hit.cube_behind());
        }
    }

    /// The raycast exited the world or hit an opaque block; finish up by applying
    /// sky and incrementing the count.
    fn end_of_ray(&mut self, ray_state: &mut LightRayState, space: &Space) {
        // TODO: set *info even if we hit the sky

        // Note: this condition is key to allowing some cases to
        // not count this as a successful ray.
        // TODO: clarify signaling flow?
        if ray_state.ray_weight_by_faces > 0. {
            // Note that if ray_state.alpha has reached zero, the sky color has no effect.
            self.add_weighted_light(
                space.physics.sky_color * ray_state.alpha,
                ray_state.ray_weight_by_faces,
            );
        }
    }

    /// Add the given color to the sum counting it as having the given weight,
    /// as if it was an entire ray's contribution
    /// (that is, incrementing `self.total_rays`).
    fn add_weighted_light(&mut self, color: Rgb, weight: f32) {
        self.incoming_light += color * weight;
        self.total_rays += 1;
        self.total_ray_weight += weight;
    }

    /// Return the [`PackedLight`] value accumulated here
    fn finish(&self, origin_is_opaque: bool) -> PackedLight {
        // if total_rays is zero then incoming_light is zero so the result will be zero.
        // We just need to avoid dividing by zero.
        let scale = NotNan::new(1.0 / self.total_ray_weight.max(1.0)).unwrap();
        let new_light_value: PackedLight = if self.total_rays > 0 {
            PackedLight::some(self.incoming_light * scale)
        } else if origin_is_opaque {
            PackedLight::OPAQUE
        } else {
            PackedLight::NO_RAYS
        };
        new_light_value
    }
}

/// Performance data for bulk light updates.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct LightUpdatesInfo {
    /// Number of blocks whose light data updates are aggregated in this data.
    pub update_count: usize,
    /// The largest change in light value that occurred.
    pub max_update_difference: u8,
    /// Number of entries in the light update queue.
    pub queue_count: usize,
    /// The largest update priority in the queue (corresponds to the size of
    /// difference that caused the cube to be added).
    pub(crate) max_queue_priority: Priority,
}
impl std::ops::AddAssign<LightUpdatesInfo> for LightUpdatesInfo {
    fn add_assign(&mut self, other: Self) {
        self.update_count += other.update_count;
        self.max_update_difference = self.max_update_difference.max(other.max_update_difference);
        self.queue_count += other.queue_count;
        self.max_queue_priority = self.max_queue_priority.max(other.max_queue_priority);
    }
}
impl CustomFormat<StatusText> for LightUpdatesInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(
            fmt,
            "{:4} (max diff {:3}) of {:4} (max pri {:3?})",
            self.update_count,
            self.max_update_difference,
            self.queue_count,
            self.max_queue_priority
        )?;
        Ok(())
    }
}

/// A special definition of opacity for the lighting algorithm:
/// we want to treat opaque light-emitting blocks similarly to transparent blocks
/// *when deciding to compute light for them*, because this produces better results
/// for smooth (interpolated) lighting.
///
/// This function is fairly straightforward; it exists for purposes of *documenting
/// the places that care about this* rather than for code reduction.
pub(crate) fn opaque_for_light_computation(block: &EvaluatedBlock) -> bool {
    block.opaque == FaceMap::repeat(true) && block.attributes.light_emission == Rgb::ZERO
}
