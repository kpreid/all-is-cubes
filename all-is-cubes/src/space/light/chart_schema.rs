use crate::raycast::Ray;

#[path = "chart_schema_shared.rs"]
mod chart_schema_shared;
pub(crate) use chart_schema_shared::OneRay;

impl OneRay {
    pub fn ray(&self) -> Ray {
        Ray::new(self.origin, self.direction)
    }

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

/// Used by `chart_data` type declarations to have compatible behavior when cross-compiling.
type TargetEndian<T> = T;
