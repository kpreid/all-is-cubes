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
        // TODO: ew, complicated and also wrongish (need euclidean distance to corner), but this is all prototype stuff anyway.
        let grid_view_radius = *(grid.size()[..].into_iter().max().unwrap()) as FreeCoordinate * 0.8;

        Self {
            projection: Ortho {
                left: -grid_view_radius * aspect_ratio,
                right: grid_view_radius * aspect_ratio,
                bottom: -grid_view_radius,
                top: grid_view_radius,
                // TODO: far & near should be longer...
                far: grid_view_radius * 1.0,
                near: -grid_view_radius * 1.0,
            }.into(),
            center: ((grid.lower_bounds() + grid.upper_bounds().to_vec()) / 2)
                .map(|x| x as FreeCoordinate),
            yaw: 0.0,
            pitch: -15.0,
        }
    }

    pub fn matrix(&self) -> M {
        // TODO: For performance, we ought to do the inverse to the projection once
        self.projection
            * Matrix4::from_angle_x(Deg(self.pitch))
            * Matrix4::from_angle_y(Deg(self.yaw))
            * Matrix4::from_translation(-(self.center.to_vec()))
    }
    
    pub fn walk(&mut self, x: FreeCoordinate, z: FreeCoordinate) {
        let rotation :Basis2<FreeCoordinate> = Rotation2::from_angle(Deg(self.yaw));
        let dir = Vector2::new(x, z);
        let dir = rotation.rotate_vector(dir);
        self.center += Vector3::new(dir.x, 0.0, dir.y);
    }
}