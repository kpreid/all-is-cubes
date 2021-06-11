// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Lighting algorithms for `Space`. This module is closely tied to `Space`
//! and separated out for readability, not modularity.

use cgmath::InnerSpace;
use cgmath::{EuclideanSpace as _, Point3, Vector3};
use once_cell::sync::Lazy;
use std::convert::TryInto as _;
use std::fmt;

use crate::math::*;
use crate::raycast::Ray;
use crate::space::*;

/// This parameter determines to what degree absorption of light due to a block surface's
/// color is taken into account. At zero, it is not (all surfaces are perfectly
/// reflective); at one, light values are simply multiplied by the surface color (e.g.
/// a red surface will reflect no green or blue light), which is the idealized physical
/// model.
const SURFACE_ABSORPTION: f32 = 0.75;

/// Placeholder for better block opacity algorithms: any block which is partly transparent
/// is assumed to intercept this much of the ray passing through.
const TRANSPARENT_BLOCK_COVERAGE: f32 = 0.25;

/// One component of a `PackedLight`.
pub(crate) type PackedLightScalar = u8;

/// Lighting within a [`Space`]; an [`Rgb`] value stored with reduced precision and range.
///
/// TODO: This now stores additional information. Rename to 'SpaceLight' or some such.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PackedLight {
    // LightStatus being other than Visible is mutually exclusive with value being nonzero,
    // so we could in theory make this an enum, but that wouldn't actually compact the
    // representation, and this representation maps to 8-bit-per-component RGBA which is
    // what the shader expects.
    value: Vector3<PackedLightScalar>,
    status: LightStatus,
}
// TODO: Once we've built out the rest of the game, do some performance testing and
// decide whether having colored lighting is worth the compute and storage cost.
// If memory vs. bit depth is an issue, consider switching to something like YCbCr
// representation, or possibly something that GPUs specifically do well with.

/// Special reasons for a cube having zero light in it.
/// These may be used to help compute smoothed lighting across blocks.
///
/// The numeric value of this enum is used to transmit it to shaders by packing
/// it into an "RGBA" color value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum LightStatus {
    #[allow(unused)]
    Uninitialized = 0,
    /// The cube contains an opaque block and therefore does not have any light entering.
    Opaque = 1,
    /// The cube has no surfaces to catch light and therefore the light value is not tracked.
    NoRays = 2,
    /// No special situation: if it's black then it's just dark.
    Visible = 255,
}

impl PackedLight {
    const LOG_SCALE: f32 = 16.0;
    const LOG_OFFSET: f32 = 128.0;

    pub(crate) const OPAQUE: Self = Self::none(LightStatus::Opaque);
    pub(crate) const NO_RAYS: Self = Self::none(LightStatus::NoRays);
    pub(crate) const ONE: PackedLight = PackedLight {
        status: LightStatus::Visible,
        value: Vector3 {
            x: Self::LOG_OFFSET as PackedLightScalar,
            y: Self::LOG_OFFSET as PackedLightScalar,
            z: Self::LOG_OFFSET as PackedLightScalar,
        },
    };

    pub(crate) fn some(value: Rgb) -> Self {
        PackedLight {
            value: Vector3::new(
                Self::scalar_in(value.red()),
                Self::scalar_in(value.green()),
                Self::scalar_in(value.blue()),
            ),
            status: LightStatus::Visible,
        }
    }

    pub(crate) const fn none(status: LightStatus) -> Self {
        PackedLight {
            value: Vector3 { x: 0, y: 0, z: 0 },
            status,
        }
    }

    #[inline]
    pub fn value(&self) -> Rgb {
        Rgb::new_nn(
            Self::scalar_out_nn(self.value[0]),
            Self::scalar_out_nn(self.value[1]),
            Self::scalar_out_nn(self.value[2]),
        )
    }

    // TODO: Expose LightStatus once we are more confident in its API stability

    #[inline]
    pub(crate) fn as_texel(self) -> (u8, u8, u8, u8) {
        let Self {
            value: Vector3 { x, y, z },
            status,
        } = self;
        (x, y, z, status as u8)
    }

    #[inline]
    fn difference_magnitude(self, other: PackedLight) -> PackedLightScalar {
        if other.status != self.status {
            // A non-opaque block changing to an opaque one, or similar, changes the
            // results of the rest of the algorithm so should be counted as a difference
            // (and a high-priority one) even if it's still changing zero to zero.
            return PackedLightScalar::MAX;
        }

        fn dm(a: PackedLightScalar, b: PackedLightScalar) -> PackedLightScalar {
            a.max(b) - a.min(b)
        }
        dm(self.value[0], other.value[0])
            .max(dm(self.value[1], other.value[1]))
            .max(dm(self.value[2], other.value[2]))
    }

    fn scalar_in(value: impl Into<f32>) -> PackedLightScalar {
        // Note that `as` is a saturating cast.
        (value.into().log2() * Self::LOG_SCALE + Self::LOG_OFFSET) as PackedLightScalar
    }

    /// Convert a `PackedLightScalar` value to a linear color component value.
    /// This function is guaranteed (and tested) to only return finite floats.
    fn scalar_out(value: PackedLightScalar) -> f32 {
        // Special representation to ensure we don't "round" zero up to a small nonzero value.
        if value == 0 {
            0.0
        } else {
            ((f32::from(value) - Self::LOG_OFFSET) / Self::LOG_SCALE).exp2()
        }
    }

    fn scalar_out_nn(value: PackedLightScalar) -> NotNan<f32> {
        unsafe {
            // Safety: a test verifies that `scalar_out` can never return NaN.
            NotNan::new_unchecked(Self::scalar_out(value))
        }
    }
}

impl fmt::Debug for PackedLight {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "PackedLight({}, {}, {}, {:?})",
            self.value.x, self.value.y, self.value.z, self.status
        )
    }
}

impl From<Rgb> for PackedLight {
    #[inline]
    fn from(value: Rgb) -> Self {
        PackedLight::some(value)
    }
}

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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct LightUpdateRequest {
    priority: PackedLightScalar,
    cube: GridPoint,
}
impl LightUpdateRequest {
    /// A priority comparison for entries with equal specified priority:
    /// prefer cubes closer to the origin. (This is for prettier initial startup:
    /// assuming the viewpoint starts close to the origin it will see good nearby
    /// lighting sooner.)
    fn fallback_priority(&self) -> GridCoordinate {
        self.cube.map(|s| s.abs()).dot(Vector3::new(-1, -1, -1))
    }
}
impl Ord for LightUpdateRequest {
    fn cmp(&self, other: &LightUpdateRequest) -> std::cmp::Ordering {
        self.priority
            .cmp(&other.priority)
            .then_with(|| self.fallback_priority().cmp(&other.fallback_priority()))
            // To obey Ord's contract we must not return equal ordering when unequal by Eq,
            // so we must break all ties until only completely identical remains.
            .then_with(|| self.cube.x.cmp(&other.cube.x))
            .then_with(|| self.cube.y.cmp(&other.cube.y))
            .then_with(|| self.cube.z.cmp(&other.cube.z))
    }
}
impl PartialOrd for LightUpdateRequest {
    fn partial_cmp(&self, other: &LightUpdateRequest) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Space {
    pub(crate) fn light_needs_update(&mut self, cube: GridPoint, priority: PackedLightScalar) {
        if self.physics.light == LightPhysics::None {
            return;
        }

        if self.grid().contains_cube(cube) && !self.lighting_update_set.contains(&cube) {
            self.lighting_update_queue
                .push(LightUpdateRequest { priority, cube });
            self.lighting_update_set.insert(cube);
        }
    }

    /// Do some lighting updates.
    pub(crate) fn update_lighting_from_queue(&mut self) -> LightUpdatesInfo {
        let mut light_update_count: usize = 0;
        let mut max_difference: PackedLightScalar = 0;
        let mut cost = 0;

        if self.physics.light != LightPhysics::None {
            while let Some(LightUpdateRequest { cube, .. }) = self.lighting_update_queue.pop() {
                self.lighting_update_set.remove(&cube);
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
            queue_count: self.lighting_update_queue.len(),
            max_queue_priority: self
                .lighting_update_queue
                .peek()
                .map(|r| r.priority)
                .unwrap_or(0),
        }
    }

    #[inline]
    fn update_lighting_now_on(
        &mut self,
        cube: GridPoint,
    ) -> (PackedLightScalar, usize, LightUpdateCubeInfo) {
        let (new_light_value, dependencies, mut cost, info) = self.compute_lighting(cube);
        let old_light_value: PackedLight = self.get_lighting(cube);
        let difference_magnitude = new_light_value.difference_magnitude(old_light_value);
        if difference_magnitude > 0 {
            cost += 200;
            // TODO: compute index only once
            self.lighting[self.grid().index(cube).unwrap()] = new_light_value;
            self.notifier.notify(SpaceChange::Lighting(cube));
            for cube in dependencies {
                self.light_needs_update(cube, difference_magnitude);
            }
        }
        (difference_magnitude, cost, info)
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
            // Opaque blocks are always dark inside.
        } else {
            let adjacent_faces = if ev_origin.visible {
                // Non-opaque blocks should work the same as blocks which have all six adjacent faces present.
                FaceMap::repeat(1.0)
            } else {
                FaceMap::from_fn(|face| {
                    if self
                        .get_evaluated(cube + face.opposite().normal_vector())
                        .visible
                    {
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
                                direction: translated_ray.direction * 10.0, // TODO: translate hit position into ray
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
                        ray_alpha *= 1.0 - coverage;
                        dependencies.push(hit.cube_ahead());
                        cost += 10;
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
}

impl LightPhysics {
    /// Generate the lighting data array that a newly created empty [`Space`] should have.
    pub(crate) fn initialize_lighting(
        &self,
        grid: Grid,
        ambient_color: PackedLight,
    ) -> Box<[PackedLight]> {
        match self {
            LightPhysics::None => Box::new([]),
            LightPhysics::Rays { .. } => vec![ambient_color; grid.volume()].into_boxed_slice(),
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
        E: Extend<Point3<FreeCoordinate>>,
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
        E: Extend<Point3<FreeCoordinate>>,
    {
        // TODO: represent self.value somehoe
        Aab::from_cube(self.value_cube)
            .enlarge(0.01)
            .wireframe_points(output);
        self.ray.wireframe_points(output);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::space::Space;
    use std::iter::once;

    fn packed_light_test_values() -> impl Iterator<Item = PackedLight> {
        (PackedLightScalar::MIN..PackedLightScalar::MAX)
            .flat_map(|s| {
                vec![
                    PackedLight {
                        value: Vector3::new(s, 0, 0),
                        status: LightStatus::Visible,
                    },
                    PackedLight {
                        value: Vector3::new(0, s, 0),
                        status: LightStatus::Visible,
                    },
                    PackedLight {
                        value: Vector3::new(0, 0, s),
                        status: LightStatus::Visible,
                    },
                    PackedLight {
                        value: Vector3::new(s, 127, 255),
                        status: LightStatus::Visible,
                    },
                ]
                .into_iter()
            })
            .chain(once(PackedLight::OPAQUE))
            .chain(once(PackedLight::NO_RAYS))
    }

    /// Test that unpacking and packing doesn't shift the value, which could lead
    /// to runaway light values.
    #[test]
    fn packed_light_roundtrip() {
        for i in PackedLightScalar::MIN..PackedLightScalar::MAX {
            assert_eq!(i, PackedLight::scalar_in(PackedLight::scalar_out(i)));
        }
    }

    /// Safety test: we want to skip the NaN checks for constructing `Rgb`
    /// from `PackedLight`, so it had better not be NaN for any possible input.
    #[test]
    fn packed_light_always_finite() {
        for i in PackedLightScalar::MIN..PackedLightScalar::MAX {
            assert!(PackedLight::scalar_out(i).is_finite(), "{}", i);
        }
    }

    /// Test out-of-range floats.
    #[test]
    fn packed_light_clipping_in() {
        assert_eq!(
            [
                PackedLight::scalar_in(NotNan::new(-1.).unwrap()),
                PackedLight::scalar_in(NotNan::new(1e-30).unwrap()),
                PackedLight::scalar_in(NotNan::new(1e+30).unwrap()),
            ],
            [0, 0, 255],
        );
    }

    #[test]
    fn packed_light_is_packed() {
        // Technically this is not guaranteed by the compiler, but if it's false something probably went wrong.
        assert_eq!(std::mem::size_of::<PackedLight>(), 4);
    }

    /// Demonstrate what range and step sizes we get out of the encoding.
    #[test]
    fn packed_light_extreme_values_out() {
        assert_eq!(
            [
                PackedLight::scalar_out(0),
                PackedLight::scalar_out(1),
                PackedLight::scalar_out(2),
                PackedLight::scalar_out(254),
                PackedLight::scalar_out(255),
            ],
            [0.0, 0.0040791943, 0.004259796, 234.75304, 245.14644],
        );
    }

    #[test]
    fn packed_light_difference_vs_eq() {
        for v1 in packed_light_test_values() {
            for v2 in packed_light_test_values() {
                assert_eq!(
                    v1 == v2,
                    v1.difference_magnitude(v2) == 0,
                    "v1={:?} v2={:?}",
                    v1,
                    v2
                );
            }
        }
    }

    #[test]
    fn initial_lighting_value() {
        let space = Space::empty_positive(1, 1, 1);
        assert_eq!(
            PackedLight::from(space.physics().sky_color),
            space.get_lighting((0, 0, 0))
        );
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
        let former_sky_light = PackedLight::from(space.physics().sky_color);
        space.set_physics(SpacePhysics {
            sky_color: Rgb::new(1.0, 0.0, 0.0),
            ..SpacePhysics::default()
        });
        let new_sky_light = PackedLight::from(space.physics().sky_color);

        space.set((0, 0, 0), Rgb::ONE).unwrap();
        // Not changed yet... except for the now-opaque block
        assert_eq!(space.get_lighting((0, 0, 0)), PackedLight::OPAQUE);
        assert_eq!(space.get_lighting((1, 0, 0)), former_sky_light);
        assert_eq!(space.get_lighting((2, 0, 0)), former_sky_light);

        // Duration doesn't currently matter
        let info = space.step(Tick::arbitrary());
        assert_eq!(
            info.light,
            LightUpdatesInfo {
                update_count: 1,
                max_update_difference: new_sky_light.difference_magnitude(former_sky_light),
                queue_count: 0,
                max_queue_priority: 0
            }
        );

        assert_eq!(space.get_lighting((0, 0, 0)), PackedLight::OPAQUE); // opaque
        assert_eq!(space.get_lighting((1, 0, 0)), new_sky_light); // updated
        assert_eq!(space.get_lighting((2, 0, 0)), former_sky_light); // not updated
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

    #[test]
    fn lighting_queue_ordering() {
        let mut space = Space::empty_positive(99, 99, 99);
        space.light_needs_update(GridPoint::new(0, 0, 0), 0);
        space.light_needs_update(GridPoint::new(2, 0, 0), 0);
        space.light_needs_update(GridPoint::new(1, 0, 0), 0);
        space.light_needs_update(GridPoint::new(0, 0, 2), 200);
        space.light_needs_update(GridPoint::new(0, 0, 1), 100);
        assert_eq!(
            space.lighting_update_queue.pop(),
            Some(LightUpdateRequest {
                cube: GridPoint::new(0, 0, 2),
                priority: 200
            })
        );
        assert_eq!(
            space.lighting_update_queue.pop(),
            Some(LightUpdateRequest {
                cube: GridPoint::new(0, 0, 1),
                priority: 100
            })
        );
        assert_eq!(
            space.lighting_update_queue.pop(),
            Some(LightUpdateRequest {
                cube: GridPoint::new(0, 0, 0),
                priority: 0
            })
        );
        assert_eq!(
            space.lighting_update_queue.pop(),
            Some(LightUpdateRequest {
                cube: GridPoint::new(1, 0, 0),
                priority: 0
            })
        );
        assert_eq!(
            space.lighting_update_queue.pop(),
            Some(LightUpdateRequest {
                cube: GridPoint::new(2, 0, 0),
                priority: 0
            })
        );
        assert_eq!(space.lighting_update_queue.pop(), None);
    }

    #[test]
    fn light_source_self_illumination() {
        let light = Rgb::new(0.5, 1.0, 2.0);
        let block = Block::builder()
            .light_emission(light)
            .color(Rgba::new(1.0, 0.0, 0.0, 0.33)) // should be irrelevant
            .build();

        let mut space = Space::empty_positive(3, 3, 3);
        space.set_physics(SpacePhysics {
            sky_color: Rgb::ZERO,
            ..Default::default()
        });
        space.set([1, 1, 1], block).unwrap();
        space.evaluate_light(1, |_| ());
        // TODO: Arguably TRANSPARENT_BLOCK_COVERAGE shouldn't affect light emission.
        // Perhaps we should multiply the emission value by the coverage.
        assert_eq!(
            space.get_lighting([1, 1, 1]).value(),
            light * TRANSPARENT_BLOCK_COVERAGE
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
            space.step(Tick::arbitrary()).light,
            LightUpdatesInfo::default()
        );
    }

    // TODO: test sky lighting propagation onto blocks after quiescing

    // TODO: test a single semi-transparent block will receive and diffuse light
}
