// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Basic camera. TODO: This will eventually become 'character', probably, but for now we have static worlds and want to move a viewpoint around.

use cgmath::{Basis2, Deg, EuclideanSpace, Matrix4, Ortho, Point3, Rotation, Rotation2, Vector2, Vector3};

use crate::math::{FreeCoordinate};
use crate::space::{Grid};

type M = Matrix4<FreeCoordinate>;

pub struct Camera {
    projection: M,
    center: Point3<FreeCoordinate>,
    pub yaw: FreeCoordinate,
    pub pitch: FreeCoordinate,
}

impl Camera {
    pub fn for_grid(aspect_ratio: FreeCoordinate, grid: &Grid) -> Self {
        // TODO: Support dynamic aspect ratio.
        Self {
            projection: cgmath::perspective(
                /* fovy: */ Deg(90.0),
                aspect_ratio,
                /* near: */ 0.1,
                /* far: */ 2000.0,
            ).into(),
            // TODO: this starting point is pretty arbitrary, but we'll be replacing it with persistent character position tied into worldgen.
            center: ((grid.lower_bounds() + grid.upper_bounds().to_vec()) / 2)
                .map(|x| x as FreeCoordinate) + Vector3::new(-3.0, 3.0, -3.0),
            yaw: 90.0,
            pitch: 15.0,
        }
    }

    pub fn projection(&self) -> M {
        self.projection
    }

    pub fn view(&self) -> M {
        Matrix4::from_angle_x(Deg(self.pitch))
            * Matrix4::from_angle_y(Deg(self.yaw))
            * Matrix4::from_translation(-(self.center.to_vec()))
    }

    pub fn combined_matrix(&self) -> M {
        self.projection() * self.view()
    }

    pub fn walk(&mut self, x: FreeCoordinate, z: FreeCoordinate) {
        let rotation :Basis2<FreeCoordinate> = Rotation2::from_angle(Deg(self.yaw));
        let dir = Vector2::new(x, z);
        let dir = rotation.rotate_vector(dir);
        self.center += Vector3::new(dir.x, 0.0, dir.y);
    }
}