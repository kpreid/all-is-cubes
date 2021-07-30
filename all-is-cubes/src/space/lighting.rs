// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Lighting algorithms for `Space`. This module is closely tied to `Space`
//! and separated out for readability, not modularity.

use std::convert::TryInto as _;
use std::fmt;

use cgmath::{EuclideanSpace as _, InnerSpace as _, Point3, Vector3};
use once_cell::sync::Lazy;

use crate::math::*;
use crate::raycast::Ray;
use crate::space::light_data::*;
use crate::space::*;
use crate::util::MapExtend;

/// This parameter determines to what degree absorption of light due to a block surface's
/// color is taken into account. At zero, it is not (all surfaces are perfectly
/// reflective); at one, light values are simply multiplied by the surface color (e.g.
/// a red surface will reflect no green or blue light), which is the idealized physical
/// model.
const SURFACE_ABSORPTION: f32 = 0.75;

/// Placeholder for better block opacity algorithms: any block which is partly transparent
/// is assumed to intercept this much of the ray passing through.
const TRANSPARENT_BLOCK_COVERAGE: f32 = 0.25;

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
                            direction
                                .map(|s| s as f32)
                                .dot(face.normal_vector())
                                .max(0.0)
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
    pub(crate) fn light_needs_update(&mut self, cube: GridPoint, priority: PackedLightScalar) {
        if self.physics.light == LightPhysics::None {
            return;
        }

        if self.grid().contains_cube(cube) {
            self.light_update_queue
                .insert(LightUpdateRequest { priority, cube });
        }
    }

    /// Do some lighting updates.
    pub(crate) fn update_lighting_from_queue(&mut self) -> LightUpdatesInfo {
        let mut light_update_count: usize = 0;
        self.last_light_updates.clear();
        let mut max_difference: PackedLightScalar = 0;
        let mut cost = 0;

        if self.physics.light != LightPhysics::None {
            while let Some(LightUpdateRequest { cube, .. }) = self.light_update_queue.pop() {
                if false {
                    // Log cubes that were updated for debug visualization.
                    self.last_light_updates.push(cube);
                }
                light_update_count += 1;
                // Note: For performance, it is key that this call site ignores the info value
                // and the functions are inlined. Thus, the info calculation can be
                // optimized away.
                let (difference, cube_cost, _) = self.update_lighting_now_on(cube);
                max_difference = max_difference.max(difference);
                cost += cube_cost;
                if cost >= 40000 {
                    break;
                }
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
    fn update_lighting_now_on(
        &mut self,
        cube: GridPoint,
    ) -> (PackedLightScalar, usize, LightUpdateCubeInfo) {
        let (new_light_value, dependencies, mut cost, info) = self.compute_lighting(cube);
        let old_light_value: PackedLight = self.get_lighting(cube);
        let difference_priority = new_light_value.difference_priority(old_light_value);
        if difference_priority > 0 {
            cost += 200;
            // TODO: compute index only once
            self.lighting[self.grid().index(cube).unwrap()] = new_light_value;
            self.notifier.notify(SpaceChange::Lighting(cube));
            for cube in dependencies {
                self.light_needs_update(cube, difference_priority);
            }
        }
        (difference_priority, cost, info)
    }

    /// Compute the new lighting value for a cube.
    ///
    /// The returned vector of points lists those cubes which the computed value depends on
    /// (imprecisely; empty cubes passed through are not listed).
    #[inline]
    pub(crate) fn compute_lighting(
        &self,
        cube: GridPoint,
    ) -> (PackedLight, Vec<GridPoint>, usize, LightUpdateCubeInfo) {
        let maximum_distance = match self.physics.light {
            LightPhysics::None => {
                panic!("Light is disabled; should not reach here");
            }
            LightPhysics::Rays { maximum_distance } => FreeCoordinate::from(maximum_distance),
        };

        // Accumulator of incoming light encountered.
        let mut incoming_light: Rgb = Rgb::ZERO;
        // Number of rays contributing to incoming_light.
        let mut total_rays = 0;
        // Number of rays, weighted by the ray angle versus local cube faces.
        let mut total_ray_weight = 0.0;
        // Cubes whose lighting value contributed to the incoming_light value.
        let mut dependencies: Vec<GridPoint> = Vec::new();
        // Approximation of CPU cost of doing the calculation, with one unit defined as
        // one raycast step.
        let mut cost = 0;
        // Diagnostics.
        let mut info_rays: [Option<LightUpdateRayInfo>; ALL_RAYS_COUNT] = [None; ALL_RAYS_COUNT];

        let ev_origin = self.get_evaluated(cube);
        if ev_origin.opaque {
            // Opaque blocks are always dark inside — unless they are light sources.
            if !opaque_for_light_computation(ev_origin) {
                incoming_light += ev_origin.attributes.light_emission;
                total_rays += 1;
                total_ray_weight += 1.0;
            }
        } else {
            let adjacent_faces = if ev_origin.visible {
                // Non-opaque blocks should work the same as blocks which have all six adjacent faces present.
                FaceMap::repeat(1.0)
            } else {
                FaceMap::from_fn(|face| {
                    // We want directions that either face away from visible faces, or towards light sources.
                    if self
                        .get_evaluated(cube + face.opposite().normal_vector())
                        .visible
                        || self
                            .get_evaluated(cube + face.normal_vector())
                            .attributes
                            .light_emission
                            != Rgb::ZERO
                    {
                        // TODO: Once we have fancier block opacity precomputations, use them to
                        // have weights besides 1.0
                        1.0f32
                    } else {
                        0.0
                    }
                })
            };

            // TODO: Choose a ray pattern that suits the maximum_distance.
            'each_ray: for LightRayData { ray, face_cosines } in &LIGHT_RAYS[..] {
                // TODO: Theoretically we should weight light rays by the cosine but that has caused poor behavior in the past.
                let ray_weight_by_faces = face_cosines
                    .zip(adjacent_faces, |_face, ray_cosine, reflects| {
                        ray_cosine * reflects
                    })
                    .into_values_iter()
                    .sum::<f32>();
                if ray_weight_by_faces <= 0.0 {
                    continue;
                }

                let translated_ray = ray.translate(cube.cast::<FreeCoordinate>().unwrap().to_vec());
                let raycaster = translated_ray.cast().within_grid(self.grid());

                // Fraction of the light value that is to be determined by future, rather than past,
                // tracing; starts at 1.0 and decreases as opaque surfaces are encountered.
                let mut ray_alpha = 1.0_f32;

                let info = &mut info_rays[total_rays];

                'raycast: for hit in raycaster {
                    cost += 1;
                    if hit.t_distance() > maximum_distance {
                        // TODO: arbitrary magic number in limit
                        // Don't count rays that didn't hit anything close enough.
                        break 'raycast;
                    }
                    let ev_hit = self.get_evaluated(hit.cube_ahead());
                    if !ev_hit.visible {
                        // Completely transparent block is passed through.
                        continue 'raycast;
                    }

                    // TODO: Implement blocks with some faces opaque.
                    if ev_hit.opaque {
                        // On striking a fully opaque block, we use the light value from its
                        // adjacent cube as the light falling on that face.
                        let light_cube = hit.cube_behind();
                        if light_cube == hit.cube_ahead() {
                            // Don't read the value we're trying to recalculate.
                            // We hit an opaque block, so this ray is stopping.
                            continue 'each_ray;
                        }
                        let stored_light = self.get_lighting(light_cube);

                        let surface_color = ev_hit.color.to_rgb() * SURFACE_ABSORPTION
                            + Rgb::ONE * (1. - SURFACE_ABSORPTION);
                        let light_from_struck_face =
                            ev_hit.attributes.light_emission + stored_light.value() * surface_color;
                        incoming_light += light_from_struck_face * ray_alpha * ray_weight_by_faces;
                        dependencies.push(light_cube);
                        cost += 10;
                        // This terminates the raycast; we don't bounce rays
                        // (diffuse reflections, not specular/mirror).
                        ray_alpha = 0.0;

                        // Diagnostics. TODO: Track transparency to some extent.
                        *info = Some(LightUpdateRayInfo {
                            ray: Ray {
                                origin: translated_ray.origin,
                                direction: hit.intersection_point(translated_ray)
                                    - translated_ray.origin,
                            },
                            trigger_cube: hit.cube_ahead(),
                            value_cube: light_cube,
                            value: stored_light,
                        });

                        break;
                    } else {
                        // Block is partly transparent and light should pass through.
                        let light_cube = hit.cube_ahead();

                        let stored_light = if light_cube == cube {
                            // Don't read the value we're trying to recalculate.
                            Rgb::ZERO
                        } else {
                            self.get_lighting(light_cube).value()
                        };
                        // 'coverage' is what fraction of the light ray we assume to hit this block,
                        // as opposed to passing through it.
                        // TODO: Compute coverage (and connectivity) in EvaluatedBlock.
                        let coverage = TRANSPARENT_BLOCK_COVERAGE;
                        incoming_light += (ev_hit.attributes.light_emission * ray_alpha
                            + stored_light)
                            * coverage
                            * ray_weight_by_faces;
                        cost += 10;
                        ray_alpha *= 1.0 - coverage;

                        dependencies.push(hit.cube_ahead());
                        // We did not read hit.cube_behind(), but we want to trigger its updates
                        // anyway, because otherwise, transparent blocks' neighbors will *never*
                        // get their light updated except when the block is initially placed.
                        dependencies.push(hit.cube_behind());
                    }
                }
                // TODO: set *info even if we hit the sky

                // Note that if ray_alpha has reached zero, the sky color has no effect.
                incoming_light += self.physics.sky_color * ray_alpha * ray_weight_by_faces;
                total_rays += 1;
                total_ray_weight += ray_weight_by_faces;
            }
        }

        // Compare and set new value. Note that we MUST compare the packed value so that
        // changes are detected in terms of the low-resolution values.

        // if total_rays is zero then incoming_light is zero so the result will be zero.
        // We just need to avoid dividing by zero.
        let scale = NotNan::new(1.0 / total_ray_weight.max(1.0)).unwrap();
        let new_light_value: PackedLight = if total_rays > 0 {
            PackedLight::some(incoming_light * scale)
        } else if ev_origin.opaque {
            PackedLight::OPAQUE
        } else {
            PackedLight::NO_RAYS
        };

        (
            new_light_value,
            dependencies,
            cost,
            LightUpdateCubeInfo {
                cube,
                result: new_light_value,
                rays: info_rays,
            },
        )
    }

    /// Clear and recompute light data and update queue, in a way which gets fast approximate
    /// results suitable for flat landscapes mostly lit from above (the +Y axis).
    pub(crate) fn fast_evaluate_light(&mut self) {
        self.light_update_queue.clear(); // Going to refill it

        if self.physics.light == LightPhysics::None {
            return;
        }

        let grid = self.grid();
        for x in grid.x_range() {
            for z in grid.z_range() {
                let mut covered = false;
                for y in grid.y_range().rev() {
                    let cube = GridPoint::new(x, y, z);
                    let index = grid.index(cube).unwrap();

                    let this_cube_evaluated =
                        &self.block_data[self.contents[index] as usize].evaluated;
                    self.lighting[index] = if opaque_for_light_computation(this_cube_evaluated) {
                        covered = true;
                        PackedLight::OPAQUE
                    } else {
                        if this_cube_evaluated.visible
                            || std::array::IntoIter::new(Face::ALL_SIX)
                                .any(|face| self.get_evaluated(cube + face.normal_vector()).visible)
                        {
                            // In this case (and only this case), we are guessing rather than being certain,
                            // so we need to schedule a proper update.
                            // (Bypassing `self.light_needs_update()` to skip bounds checks).
                            self.light_update_queue.insert(LightUpdateRequest {
                                priority: PackedLightScalar::MAX,
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
    pub(crate) fn initialize_lighting(&self, grid: Grid) -> Box<[PackedLight]> {
        match self {
            LightPhysics::None => Box::new([]),
            LightPhysics::Rays { .. } => {
                vec![PackedLight::NO_RAYS; grid.volume()].into_boxed_slice()
            }
        }
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
    pub max_queue_priority: u8,
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
            "{:4} (max diff {:3}) of {:4} (max pri {:3})",
            self.update_count,
            self.max_update_difference,
            self.queue_count,
            self.max_queue_priority
        )?;
        Ok(())
    }
}

/// Diagnostic data returned by lighting updates.
///
/// This is detailed information which is not computed except when requested.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct LightUpdateCubeInfo {
    cube: GridPoint,
    result: PackedLight,
    rays: [Option<LightUpdateRayInfo>; ALL_RAYS_COUNT],
}

impl Geometry for LightUpdateCubeInfo {
    type Coord = FreeCoordinate;

    fn translate(self, _offset: impl Into<Vector3<FreeCoordinate>>) -> Self {
        unimplemented!();
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<(Point3<FreeCoordinate>, Option<Rgba>)>,
    {
        // Draw output cube
        Aab::from_cube(self.cube)
            .enlarge(0.1)
            .wireframe_points(output);
        // Draw rays
        for ray_info in self.rays.iter().flatten() {
            ray_info.wireframe_points(output);
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct LightUpdateRayInfo {
    ray: Ray,
    trigger_cube: GridPoint,
    value_cube: GridPoint,
    value: PackedLight,
}

impl Geometry for LightUpdateRayInfo {
    type Coord = FreeCoordinate;

    fn translate(self, _offset: impl Into<Vector3<FreeCoordinate>>) -> Self {
        unimplemented!();
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<(Point3<FreeCoordinate>, Option<Rgba>)>,
    {
        Aab::from_cube(self.value_cube)
            .enlarge(0.01)
            .wireframe_points(output);
        self.ray.wireframe_points(&mut MapExtend::new(
            output,
            |(p, _): (Point3<FreeCoordinate>, Option<Rgba>)| {
                (p, Some(self.value.value().with_alpha_one()))
            },
        ))
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
    block.opaque && block.attributes.light_emission == Rgb::ZERO
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::listen::Sink;
    use crate::space::Space;

    #[test]
    fn initial_lighting_value() {
        let space = Space::empty_positive(1, 1, 1);
        assert_eq!(PackedLight::NO_RAYS, space.get_lighting((0, 0, 0)));
    }

    #[test]
    fn out_of_bounds_lighting_value() {
        let space = Space::empty_positive(1, 1, 1);
        assert_eq!(
            PackedLight::from(space.physics().sky_color),
            space.get_lighting((-1, 0, 0))
        );
    }

    #[test]
    fn step() {
        let mut space = Space::empty_positive(3, 1, 1);
        space.set_physics(SpacePhysics {
            sky_color: Rgb::new(1.0, 0.0, 0.0),
            ..SpacePhysics::default()
        });
        let sky_light = PackedLight::from(space.physics().sky_color);

        space.set((0, 0, 0), Rgb::ONE).unwrap();
        // Not changed yet... except for the now-opaque block
        assert_eq!(space.get_lighting((0, 0, 0)), PackedLight::OPAQUE);
        assert_eq!(space.get_lighting((1, 0, 0)), PackedLight::NO_RAYS);
        assert_eq!(space.get_lighting((2, 0, 0)), PackedLight::NO_RAYS);

        let (info, _) = space.step(None, Tick::arbitrary());
        assert_eq!(
            info.light,
            LightUpdatesInfo {
                update_count: 1,
                max_update_difference: sky_light.difference_priority(PackedLight::NO_RAYS),
                queue_count: 0,
                max_queue_priority: 0
            }
        );

        assert_eq!(space.get_lighting((0, 0, 0)), PackedLight::OPAQUE); // opaque
        assert_eq!(space.get_lighting((1, 0, 0)), sky_light); // updated
        assert_eq!(space.get_lighting((2, 0, 0)), PackedLight::NO_RAYS); // not updated/not relevant
    }

    #[test]
    fn evaluate_light() {
        let mut space = Space::empty_positive(3, 1, 1);
        assert_eq!(0, space.evaluate_light(0, |_| {}));
        space.set([1, 0, 0], Rgb::ONE).unwrap();
        assert_eq!(2, space.evaluate_light(0, |_| {}));
        assert_eq!(0, space.evaluate_light(0, |_| {}));
        // This is just a smoke test, "is it plausible that it's working".
        // Ideally we'd confirm identical results from repeated step() and single evaluate_light().
    }

    // TODO: test evaluate_light's epsilon parameter

    /// There's a special case for setting cubes to opaque. That case must do the usual
    /// light update and notification.
    #[test]
    fn set_cube_opaque_notification() {
        let mut space = Space::empty_positive(1, 1, 1);
        let mut sink = Sink::new();
        space.listen(
            sink.listener()
                .filter(|change| matches!(change, SpaceChange::Lighting(_)).then(|| change)),
        );
        // Self-test that the initial condition is not trivially the answer we're looking for
        assert_ne!(space.get_lighting([0, 0, 0]), PackedLight::OPAQUE);

        space.set([0, 0, 0], Block::from(Rgb::ONE)).unwrap();

        assert_eq!(space.get_lighting([0, 0, 0]), PackedLight::OPAQUE);
        assert_eq!(
            Some(SpaceChange::Lighting(GridPoint::new(0, 0, 0))),
            sink.next()
        );
    }

    fn light_source_test_space(block: Block) -> Space {
        let mut space = Space::empty_positive(3, 3, 3);
        space.set_physics(SpacePhysics {
            sky_color: Rgb::ZERO,
            ..Default::default()
        });
        space.set([1, 1, 1], block).unwrap();
        space.evaluate_light(0, |_| ());
        space
    }

    #[test]
    fn light_source_self_illumination_transparent() {
        let light = Rgb::new(0.5, 1.0, 2.0);
        let block = Block::builder()
            .light_emission(light)
            .color(Rgba::new(1.0, 0.0, 0.0, 0.33)) // irrelevant except for alpha
            .build();

        let space = light_source_test_space(block);
        // TODO: Arguably TRANSPARENT_BLOCK_COVERAGE shouldn't affect light emission.
        // Perhaps we should multiply the emission value by the coverage.
        assert_eq!(
            space.get_lighting([1, 1, 1]).value(),
            light * TRANSPARENT_BLOCK_COVERAGE
        );
    }

    #[test]
    fn light_source_self_illumination_opaque() {
        let light = Rgb::new(0.5, 1.0, 2.0);
        let block = Block::builder()
            .light_emission(light)
            .color(Rgba::new(1.0, 1.0, 1.0, 1.0)) // irrelevant except for alpha
            .build();

        let space = light_source_test_space(block);
        assert_eq!(space.get_lighting([1, 1, 1]), light.into());
        let adjacents = FaceMap::from_fn(|face| {
            space
                .get_lighting(GridPoint::new(1, 1, 1) + face.normal_vector())
                .value()
        });
        assert_eq!(
            adjacents,
            // TODO: make this test less fragile. The asymmetry isn't even wanted;
            // I think it's probably due to exactly diagonal rays.
            FaceMap {
                within: light,
                nx: Rgb::new(0.13053422, 0.26106843, 0.52213687),
                ny: Rgb::new(0.16210495, 0.3242099, 0.6484198),
                nz: Rgb::new(0.2102241, 0.4204482, 0.8408964),
                px: Rgb::new(0.13053422, 0.26106843, 0.52213687),
                py: Rgb::new(0.16210495, 0.3242099, 0.6484198),
                pz: Rgb::new(0.2102241, 0.4204482, 0.8408964)
            },
        );
    }

    /// Helper to construct a space with LightPhysics set to None
    fn space_with_disabled_light() -> Space {
        let mut space = Space::empty_positive(1, 1, 1);
        space.set_physics(SpacePhysics {
            light: LightPhysics::None,
            ..SpacePhysics::default()
        });
        space
    }

    #[test]
    fn disabled_lighting_returns_one_always() {
        assert_eq!(
            space_with_disabled_light().get_lighting([0, 0, 0]),
            PackedLight::ONE
        );
    }

    #[test]
    fn disabled_lighting_does_not_update() {
        let mut space = space_with_disabled_light();
        space.light_needs_update(GridPoint::new(0, 0, 0), u8::MAX);
        assert_eq!(
            space.step(None, Tick::arbitrary()).0.light,
            LightUpdatesInfo::default()
        );
    }

    // TODO: test sky lighting propagation onto blocks after quiescing

    // TODO: test a single semi-transparent block will receive and diffuse light
}
