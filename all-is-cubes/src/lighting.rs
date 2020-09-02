// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Lighting algorithms for `Space`. This module is closely tied to `Space`
//! and separated out for readability, not modularity.

use cgmath::{Transform as _, Vector3};
use lazy_static::lazy_static;
use std::convert::TryInto as _;

use crate::math::*;
use crate::raycast::Raycaster;
use crate::space::*;

pub(crate) type PackedLightScalar = u8;

/// RGB color for lighting from outside the space. Defined to be consistent with
/// `PackedLight::SKY`.
pub const SKY: RGB = RGB::ONE;

/// Lighting within a `Space`.
///
/// Each component is essentially a fixed-point value; `PackedLight::UNIT` is the
/// value to think of as "normal full brightness" and higher values represent
/// the bright light of light sources.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PackedLight(Vector3<PackedLightScalar>);
// TODO: Once we've built out the rest of the game, do some performance testing and
// decide whether having colored lighting is worth the compute and storage cost.
// If memory vs. bit depth is an issue, consider switching to something like YCbCr
// representation, or possibly something that GPUs specifically do well with.
//
// Also consider whether we should have gamma -- or even a logarithmic representation.

impl PackedLight {
    /// Unit value of these fixed-point color components.
    const UNIT: PackedLightScalar = 64;
    /// `UNIT` as a f32 value, for use in conversions in and out.
    const UNIT_F32: f32 = 64.0;

    pub const INITIAL: PackedLight = PackedLight(Vector3::new(
        PackedLight::UNIT,
        PackedLight::UNIT,
        PackedLight::UNIT,
    ));

    /// Light that is considered to exist in all directions outside the world.
    pub const SKY: PackedLight = PackedLight::INITIAL;

    fn difference_magnitude(self, other: PackedLight) -> PackedLightScalar {
        fn dm(a: PackedLightScalar, b: PackedLightScalar) -> PackedLightScalar {
            a.max(b) - a.min(b)
        }
        dm(self.0[0], other.0[0])
            .max(dm(self.0[1], other.0[1]))
            .max(dm(self.0[2], other.0[2]))
    }
}

impl From<RGB> for PackedLight {
    fn from(value: RGB) -> Self {
        PackedLight(Vector3::new(
            (value.red() * PackedLight::UNIT_F32) as PackedLightScalar,
            (value.green() * PackedLight::UNIT_F32) as PackedLightScalar,
            (value.blue() * PackedLight::UNIT_F32) as PackedLightScalar,
        ))
    }
}
impl From<PackedLight> for [f32; 3] {
    fn from(value: PackedLight) -> Self {
        [
            f32::from(value.0[0]) / PackedLight::UNIT_F32,
            f32::from(value.0[1]) / PackedLight::UNIT_F32,
            f32::from(value.0[2]) / PackedLight::UNIT_F32,
        ]
    }
}
impl From<PackedLight> for RGB {
    fn from(value: PackedLight) -> Self {
        RGB::new(
            f32::from(value.0[0]) / PackedLight::UNIT_F32,
            f32::from(value.0[1]) / PackedLight::UNIT_F32,
            f32::from(value.0[2]) / PackedLight::UNIT_F32,
        )
    }
}

/// Fixed configuration of light rays to use for light tracing.
#[derive(Clone, Copy, Debug)]
struct FaceRayData {
    reflect_face: Vector3<GridCoordinate>,
    rays: [LightRay; 3 * 3],
}
#[derive(Clone, Copy, Debug)]
struct LightRay {
    origin: Vector3<FreeCoordinate>,
    direction: Vector3<FreeCoordinate>,
}

lazy_static! {
    static ref LIGHT_RAYS: [FaceRayData; 6] = {
        let mut ray_data = Vec::new();
        for &face in Face::ALL_SIX {
            let origin = Vector3::new(0.5, 0.5, 0.5) + face.normal_vector() * -0.25;
            let reflect_face = Vector3::new(0, 0, 0) + face.normal_vector() * -1;
            let mut rays = Vec::new();
            for rayx in -1..=1 {
                for rayy in -1..=1 {
                    rays.push(LightRay {
                        origin,
                        direction: face.matrix().transform_vector(Vector3::new(
                            rayx.into(),
                            rayy.into(),
                            1.0,
                        )),
                    });
                }
            }
            ray_data.push(FaceRayData {
                reflect_face,
                rays: (*rays).try_into().unwrap(),
            });
        }
        (*ray_data).try_into().unwrap()
    };
}

pub(crate) fn initialize_lighting(grid: Grid) -> Box<[PackedLight]> {
    vec![PackedLight::INITIAL; grid.volume()].into_boxed_slice()
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct LightUpdateRequest {
    priority: PackedLightScalar,
    cube: GridPoint,
}
impl Ord for LightUpdateRequest {
    fn cmp(&self, other: &LightUpdateRequest) -> std::cmp::Ordering {
        self.priority
            .cmp(&other.priority)
            .then_with(|| self.cube[0].cmp(&other.cube[0]))
            .then_with(|| self.cube[1].cmp(&other.cube[1]))
            .then_with(|| self.cube[2].cmp(&other.cube[2]))
    }
}
impl PartialOrd for LightUpdateRequest {
    fn partial_cmp(&self, other: &LightUpdateRequest) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Space {
    pub(crate) fn light_needs_update(&mut self, cube: GridPoint, priority: PackedLightScalar) {
        if self.grid().contains_cube(cube) && !self.lighting_update_set.contains(&cube) {
            self.lighting_update_queue
                .push(LightUpdateRequest { priority, cube });
            self.lighting_update_set.insert(cube);
        }
    }

    /// Do some lighting updates.
    pub(crate) fn update_lighting_from_queue(&mut self) -> SpaceStepInfo {
        // Do a finite number of updates.
        let mut light_update_count: usize = 0;
        let mut max_difference: PackedLightScalar = 0;
        while let Some(LightUpdateRequest { cube, .. }) = self.lighting_update_queue.pop() {
            self.lighting_update_set.remove(&cube);
            light_update_count += 1;
            max_difference = max_difference.max(self.update_lighting_now_on(cube));
            if light_update_count >= 120 {
                break;
            }
        }
        SpaceStepInfo {
            light_update_count,
            light_queue_count: self.lighting_update_queue.len(),
            max_light_update_difference: max_difference,
        }
    }

    fn update_lighting_now_on(&mut self, cube: GridPoint) -> PackedLightScalar {
        let mut total_rays = 0;
        let mut incoming_light: RGB = RGB::ZERO;
        let mut dependencies: Vec<GridPoint> = Vec::new(); // TODO: reuse buffer instead of allocating every time

        if self[cube].opaque_to_light() {
            // Opaque blocks are always dark inside
            total_rays = 1;
        } else {
            for face_ray_data in &*LIGHT_RAYS {
                // TODO port over the empty space test here

                for ray in &face_ray_data.rays {
                    // TODO this is wrong it is not the nested algorithm
                    total_rays += 1;
                    let raycaster = Raycaster::new(
                        cube.cast::<FreeCoordinate>().unwrap() + ray.origin,
                        ray.direction,
                    )
                    .within_grid(*self.grid());
                    // TODO tracing variables ...
                    let mut found = false;
                    for hit in raycaster {
                        let block = &self[hit.cube];
                        if !block.opaque_to_light() { // TODO wrong test?
                             // Do nothing for now. TODO: Implement passing through transparency and transparent light sources
                        } else {
                            let light_cube = hit.previous_cube();
                            let light_from_struck_face = block.attributes().light_emission
                                + self.get_lighting(light_cube).into();
                            incoming_light += light_from_struck_face;
                            dependencies.push(light_cube);
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        incoming_light += PackedLight::SKY.into(); // TODO silly conversion
                    }
                }
            }
        }

        // Compare and set new value. Note that we MUST compare the packed value so that
        // changes are detected in terms of the low-resolution values.
        let new_light_value: PackedLight = (incoming_light / total_rays as f32).into();
        let old_light_value: PackedLight = self.get_lighting(cube);
        let difference_magnitude = new_light_value.difference_magnitude(old_light_value);
        if difference_magnitude > 0 {
            // TODO: compute index only once
            self.lighting[self.grid().index(cube).unwrap()] = new_light_value;
            self.mutation_counter += 1;
            // TODO: push ray block hits onto lighting update queue for recursive relighting
            for cube in dependencies {
                self.light_needs_update(cube, difference_magnitude);
            }
        }
        difference_magnitude
    }
}
