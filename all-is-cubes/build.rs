//! Build script for all-is-cubes.
//!
//! Does not do any native compilation; this is just precomputation and code-generation
//! more convenient than a proc macro.

use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::{env, fs};

use euclid::default::{Point3D, Vector3D};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    generate_light_ray_pattern(
        &PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("light_ray_pattern.rs"),
    );
}

const RAY_DIRECTION_STEP: isize = 5;

// TODO: Make multiple ray patterns that suit the maximum_distance parameter.
// TODO: Consider replacing this manual formatting with https://docs.rs/uneval/latest
fn generate_light_ray_pattern(path: &Path) {
    let mut file = fs::File::create(path).expect("failed to create light ray file");

    let origin = Point3D::new(0.5, 0.5, 0.5);

    writeln!(file, "static LIGHT_RAYS: &[LightRayData] = &[").unwrap();

    // TODO: octahedron instead of cube
    for x in -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP {
        for y in -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP {
            for z in -RAY_DIRECTION_STEP..=RAY_DIRECTION_STEP {
                if x.abs() == RAY_DIRECTION_STEP
                    || y.abs() == RAY_DIRECTION_STEP
                    || z.abs() == RAY_DIRECTION_STEP
                {
                    let direction = Vector3D::new(x as f64, y as f64, z as f64).normalize();

                    writeln!(file, "LightRayData {{").unwrap();
                    writeln!(file,
                        "    ray: Ray {{ origin: Point3D::new({origin}), direction: Vector3D::new({direction}) }},\n    face_cosines: FaceMap {{",
                        origin = vecfields(origin),
                        direction = vecfields(direction),
                    ).unwrap();

                    for (name, unit_vector) in [
                        ("nx", Vector3D::new(-1, 0, 0)),
                        ("ny", Vector3D::new(0, -1, 0)),
                        ("nz", Vector3D::new(0, 0, -1)),
                        ("px", Vector3D::new(1, 0, 0)),
                        ("py", Vector3D::new(0, 1, 0)),
                        ("pz", Vector3D::new(0, 0, 1)),
                    ] {
                        let cosine = unit_vector.to_f32().dot(direction.to_f32()).max(0.0);
                        writeln!(file, "        {name}: {cosine:?},").unwrap();
                    }

                    // close braces for `FaceMap` and `LightRayData` structs
                    writeln!(file, "}} }},").unwrap();
                }
            }
        }
    }

    // end of LIGHT_RAYS
    writeln!(file, "];").unwrap();

    file.flush().unwrap();
}

fn vecfields(value: impl Into<[f64; 3]>) -> String {
    let [x, y, z] = value.into();
    format!("{x:?}, {y:?}, {z:?},")
}
