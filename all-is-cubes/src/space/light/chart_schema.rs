use euclid::Point3D;

use all_is_cubes_base::math::{Cube, CubeFace};

#[path = "chart_schema_shared.rs"]
mod chart_schema_shared;
pub(crate) use chart_schema_shared::*;

impl OneRay {
    pub fn face_cosines(&self) -> crate::math::FaceMap<f32> {
        let [nx, ny, nz, px, py, pz] = self.face_cosines;
        crate::math::FaceMap {
            nx,
            ny,
            nz,
            px,
            py,
            pz,
        }
    }
}

impl Step {
    pub fn relative_cube_face(self) -> CubeFace {
        CubeFace {
            cube: Cube::from(Point3D::from(self.relative_cube).to_i32()),
            face: self.face.into(),
        }
    }
}

/// Used by `chart_data` type declarations to have compatible behavior when cross-compiling.
type TargetEndian<T> = T;
