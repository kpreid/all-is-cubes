use euclid::Point3D;

use all_is_cubes_base::math::{Cube, CubeFace, FaceMap};

use crate::space::light::chart;

impl chart::OneRay {
    pub fn face_cosines(&self) -> FaceMap<f32> {
        FaceMap::from(self.face_cosines).map(|_, byte| f32::from(byte) / 255.0f32)
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
