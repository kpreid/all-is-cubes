//! Build script for all-is-cubes.
//!
//! Does not do any native compilation; this is just precomputation and code-generation
//! more convenient than a proc macro.

use std::path::PathBuf;
use std::{env, fs};

use all_is_cubes_base::math::{self, Face6, FaceMap, FreePoint, FreeVector};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/space/light/chart_data.rs");

    let rays = generate_light_ray_pattern();

    fs::write(
        PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("light_ray_pattern.bin"),
        bytemuck::cast_slice::<OneRay, u8>(rays.as_slice()),
    )
    .expect("failed to write light_ray_pattern");
}

const RAY_DIRECTION_STEP: isize = 5;

// TODO: Make multiple ray patterns that suit the maximum_distance parameter.
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

                    rays.push(OneRay::new(origin, direction, cosines))
                }
            }
        }
    }

    rays
}

use chart_schema::OneRay;
#[path = "src/space/light/"]
mod chart_schema {
    use crate::math::{FaceMap, FreePoint, FreeVector, VectorOps as _};
    use core::fmt;
    use num_traits::ToBytes;
    use std::env;

    mod chart_schema_shared;
    pub(crate) use chart_schema_shared::OneRay;

    impl OneRay {
        pub fn new(origin: FreePoint, direction: FreeVector, face_cosines: FaceMap<f32>) -> Self {
            let face_cosines = face_cosines.map(|_, c| TargetEndian::from(c));
            Self {
                origin: origin.map(TargetEndian::from).into(),
                direction: direction.map(TargetEndian::from).into(),
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
}
