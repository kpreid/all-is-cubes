//! Types and data pertaining to the pattern of rays that are cast from a block to potential
//! light sources. Used by the algorithms in [`crate::space::light::updater`].

use alloc::vec::Vec;

use crate::math::{CubeFace, FaceMap};
use crate::raycast::Ray;
use crate::space::light::chart_schema::OneRay;
use crate::space::LightPhysics;

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
    /// Cube we just hit, relative to the origin of rays.
    pub relative_cube_face: CubeFace,
    /// Ray segment from the origin to the point where it struck the cube.
    /// Used only for diagnostic purposes ("where did the rays go?").
    pub relative_ray_to_here: Ray,
}

/// `bytemuck::cast_slice()` can't be const, so we have to write a function,
/// but this should all compile to a noop.
fn light_rays_data() -> &'static [OneRay] {
    const LIGHT_RAYS_BYTES_LEN: usize =
        include_bytes!(concat!(env!("OUT_DIR"), "/light_ray_pattern.bin")).len();

    // Ensure the data is sufficiently aligned
    #[repr(C)]
    struct Align {
        _aligner: [OneRay; 0],
        data: [u8; LIGHT_RAYS_BYTES_LEN],
    }

    static LIGHT_RAYS_BYTES: Align = Align {
        _aligner: [],
        data: *include_bytes!(concat!(env!("OUT_DIR"), "/light_ray_pattern.bin")),
    };

    bytemuck::cast_slice::<u8, OneRay>(&LIGHT_RAYS_BYTES.data)
}

/// Convert [`LIGHT_RAYS`] containing [`LightRayData`] into [`LightRayCubes`].
#[inline(never)] // cold code shouldn't be duplicated
pub(in crate::space) fn calculate_propagation_table(physics: &LightPhysics) -> Vec<LightRayCubes> {
    // TODO: Save memory for the table by adding mirroring support, like ChunkChart does

    match *physics {
        LightPhysics::None => vec![],
        // TODO: Instead of having a constant ray pattern, choose one that suits the
        // maximum_distance.
        LightPhysics::Rays { maximum_distance } => {
            let maximum_distance = f64::from(maximum_distance);
            light_rays_data()
                .iter()
                .map(|&ray_data| {
                    let ray = ray_data.ray();
                    LightRayCubes {
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
                        face_cosines: ray_data.face_cosines(),
                    }
                })
                .collect()
        }
    }
}
