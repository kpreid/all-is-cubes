use euclid::Point3D;

use all_is_cubes_base::math::{Cube, CubeFace, FaceMap};

use crate::space::light::chart;

impl chart::OneRay {
    pub fn face_cosines(&self) -> FaceMap<f32> {
        let [nx, ny, nz, px, py, pz] = self.face_cosines;
        FaceMap {
            nx,
            ny,
            nz,
            px,
            py,
            pz,
        }
    }
}

impl chart::Step {
    pub fn relative_cube_face(self) -> CubeFace {
        CubeFace {
            cube: Cube::from(Point3D::from(self.relative_cube).to_i32()),
            face: self.face.into(),
        }
    }
}

/// Used by `chart` type declarations to have compatible behavior when cross-compiling.
pub(super) type TargetEndian<T> = T;
