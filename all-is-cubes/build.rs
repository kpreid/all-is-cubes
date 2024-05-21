//! Build script for all-is-cubes.
//!
//! Does not do any native compilation; this is just precomputation and code-generation
//! more convenient than a proc macro.

extern crate alloc;

use std::path::{Path, PathBuf};
use std::{env, fs};

use all_is_cubes_base::math::{self, Face6, FaceMap, FreePoint, FreeVector};
use all_is_cubes_base::raycast::Ray;

fn main() {
    println!("cargo:rerun-if-changed=src/space/light/chart/shared.rs");

    let rays = generate_light_ray_pattern();
    let chart = generate_light_propagation_chart(&rays);
    write_light_propagation_chart(chart);
}

const RAY_DIRECTION_STEP: isize = 5;

// TODO: Use morerays once we have a more efficient chart format that
// deduplicates work of near-parallel rays.
fn generate_light_ray_pattern() -> Vec<OneRay> {
    let origin = FreePoint::new(0.5, 0.5, 0.5);

    let mut rays = Vec::new();

    // TODO: octahedron instead of cube
    for x in -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP {
        for y in -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP {
            for z in -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP {
                if x.abs() == RAY_DIRECTION_STEP
                    || y.abs() == RAY_DIRECTION_STEP
                    || z.abs() == RAY_DIRECTION_STEP
                {
                    let direction = FreeVector::new(x as f64, y as f64, z as f64).normalize();

                    let mut cosines = FaceMap::repeat(0.0f32);
                    for face in Face6::ALL {
                        let unit_vector: FreeVector = face.normal_vector();
                        let cosine = unit_vector.to_f32().dot(direction.to_f32()).max(0.0);
                        cosines[face] = cosine;
                    }

                    rays.push(OneRay::new(Ray::new(origin, direction), cosines))
                }
            }
        }
    }

    rays
}

/// Convert rays into their steps (sequence of cube intersections).
fn generate_light_propagation_chart(rays: &[OneRay]) -> Vec<chart_schema::Steps> {
    let maximum_distance = 127.0;
    rays.iter()
        .map(|&info| {
            let ray: Ray = info.ray.into();
            chart_schema::Steps {
                info,
                relative_cube_sequence: ray
                    .cast()
                    .take_while(|step| step.t_distance() <= maximum_distance)
                    .map(|step| chart_schema::Step {
                        relative_cube: step
                            .cube_ahead()
                            .lower_bounds()
                            .map(|coord| {
                                TargetEndian::from(i8::try_from(coord).expect("coordinate too big"))
                            })
                            .into(),
                        face: step.face().into(),
                        distance: step.t_distance().ceil() as u8,
                    })
                    .collect(),
            }
        })
        .collect()
}

fn write_light_propagation_chart(chart: Vec<chart_schema::Steps>) {
    // Repack data into two vectors instead of a vector of vectors.
    let mut offset = 0;
    let info: Vec<chart_schema::IndirectSteps> = chart
        .iter()
        .map(|steps| {
            let len = u32::try_from(steps.relative_cube_sequence.len()).unwrap();
            let start = offset;
            let end = offset + len;
            offset += len;
            chart_schema::IndirectSteps {
                info: steps.info,
                relative_cube_sequence: [start, end].map(TargetEndian::from),
            }
        })
        .collect();
    let all_steps_concat: Vec<chart_schema::Step> = chart
        .into_iter()
        .flat_map(|steps| steps.relative_cube_sequence)
        .collect();

    writemuck(Path::new("light_chart_info.bin"), info.as_slice());
    writemuck(
        Path::new("light_chart_steps.bin"),
        all_steps_concat.as_slice(),
    );
}

/// Write the bytes of the given data to the given path within `OUT_DIR`.
fn writemuck<T: bytemuck::NoUninit>(out_relative_path: &Path, data: &[T]) {
    assert!(out_relative_path.is_relative());
    let path = PathBuf::from(env::var_os("OUT_DIR").unwrap()).join(out_relative_path);
    if let Err(e) = fs::write(&path, bytemuck::cast_slice::<T, u8>(data)) {
        panic!(
            "failed to write generated data to {path}: {e}",
            path = path.display()
        )
    }
}

use chart_schema::{OneRay, TargetEndian};

// If this module path changes, update the rerun-if-changed directive above!
#[path = "src/space/light/chart/"]
mod chart_schema {
    use crate::math::FaceMap;
    use all_is_cubes_base::raycast::Ray;
    use core::fmt;
    use num_traits::{FromBytes, ToBytes};
    use std::env;

    // If this module name changes, update the rerun-if-changed directive above!
    mod shared;
    pub(crate) use shared::{IndirectSteps, OneRay, Step, Steps};

    impl OneRay {
        pub fn new(ray: Ray, face_cosines: FaceMap<f32>) -> Self {
            let face_cosines = face_cosines.map(|_, c| TargetEndian::from(c));
            Self {
                ray: ray.into(),
                face_cosines: [
                    face_cosines.nx,
                    face_cosines.ny,
                    face_cosines.nz,
                    face_cosines.px,
                    face_cosines.py,
                    face_cosines.pz,
                ],
            }
        }
    }

    impl From<Ray> for shared::Ray {
        fn from(value: Ray) -> Self {
            Self {
                origin: value.origin.map(TargetEndian::from).into(),
                direction: value.direction.map(TargetEndian::from).into(),
            }
        }
    }

    /// Used as `super::TargetEndian` by `shared`.
    #[derive(Copy, Clone, Debug, bytemuck::Pod, bytemuck::Zeroable)]
    #[repr(C, packed)]
    pub(crate) struct TargetEndian<T>(<T as ToBytes>::Bytes)
    where
        T: ToBytes,
        <T as ToBytes>::Bytes: Copy + Clone + fmt::Debug + bytemuck::Pod + bytemuck::Zeroable;

    impl<T: ToBytes> From<T> for TargetEndian<T>
    where
        <T as ToBytes>::Bytes: Copy + Clone + fmt::Debug + bytemuck::Pod + bytemuck::Zeroable,
    {
        fn from(value: T) -> Self {
            Self(
                match env::var("CARGO_CFG_TARGET_ENDIAN").unwrap().as_str() {
                    "big" => T::to_be_bytes(&value),
                    "little" => T::to_le_bytes(&value),
                    e => panic!("unknown endianness: {e}"),
                },
            )
        }
    }

    // Orphan rules don't allow this as a generic impl directly
    impl<T> TargetEndian<T>
    where
        <T as ToBytes>::Bytes: Copy + Clone + bytemuck::Pod + bytemuck::Zeroable,
        T: FromBytes<Bytes = <T as ToBytes>::Bytes> + ToBytes + bytemuck::Pod + bytemuck::Zeroable,
    {
        fn into_value(self) -> T {
            let bytes = self.0;
            match env::var("CARGO_CFG_TARGET_ENDIAN").unwrap().as_str() {
                "big" => T::from_be_bytes(&bytes),
                "little" => T::from_le_bytes(&bytes),
                e => panic!("unknown endianness: {e}"),
            }
        }
    }
    impl From<TargetEndian<f32>> for f32 {
        fn from(value: TargetEndian<f32>) -> Self {
            value.into_value()
        }
    }
    impl From<TargetEndian<f64>> for f64 {
        fn from(value: TargetEndian<f64>) -> Self {
            value.into_value()
        }
    }
}
