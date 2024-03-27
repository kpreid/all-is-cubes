//! Types and data pertaining to the pattern of rays that are cast from a block to potential
//! light sources. Used by the algorithms in [`crate::space::light::updater`].

use alloc::vec::Vec;

use euclid::{Point3D, Vector3D};

use crate::math::{CubeFace, FaceMap};
use crate::raycast::Ray;
use crate::space::LightPhysics;

#[derive(Debug)]
struct LightRayData {
    ray: Ray,
    face_cosines: FaceMap<f32>,
}

/// Derived from [`LightRayData`], but with a pre-calculated sequence of cubes instead of a ray
///  for maximum performance in the lighting calculation.
#[derive(Debug)]
pub(in crate::space) struct LightRayCubes {
    /// For diagnostics only
    pub ray: Ray,
    pub relative_cube_sequence: Vec<LightRayStep>,
    pub face_cosines: FaceMap<f32>,
}

/// A raycast step pre-adapted.
#[derive(Debug)]
pub(in crate::space) struct LightRayStep {
    /// Cube we just hit.
    pub relative_cube_face: CubeFace,
    /// Ray segment from the origin to the point where it struck the cube.
    /// Used only for diagnostic purposes ("where did the rays go?").
    pub relative_ray_to_here: Ray,
}

// Build script generates the declaration:
// static LIGHT_RAYS: &[LightRayData] = &[...
include!(concat!(env!("OUT_DIR"), "/light_ray_pattern.rs"));

/// Convert [`LIGHT_RAYS`] containing [`LightRayData`] into [`LightRayCubes`].
pub(in crate::space) fn calculate_propagation_table(physics: &LightPhysics) -> Vec<LightRayCubes> {
    match *physics {
        LightPhysics::None => vec![],
        // TODO: Instead of having a constant ray pattern, choose one that suits the
        // maximum_distance.
        LightPhysics::Rays { maximum_distance } => {
            let maximum_distance = f64::from(maximum_distance);
            LIGHT_RAYS
                .iter()
                .map(|&LightRayData { ray, face_cosines }| LightRayCubes {
                    relative_cube_sequence: ray
                        .cast()
                        .take_while(|step| step.t_distance() <= maximum_distance)
                        .map(|step| LightRayStep {
                            relative_cube_face: step.cube_face(),
                            relative_ray_to_here: Ray {
                                origin: ray.origin,
                                direction: step.intersection_point(ray) - ray.origin,
                            },
                        })
                        .collect(),
                    ray,
                    face_cosines,
                })
                .collect()
        }
    }
}
