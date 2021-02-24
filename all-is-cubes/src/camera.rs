// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Projection and view matrices, viewport and aspect ratio, visibility,
//! raycasting into the scene, etc.

use cgmath::{
    Deg, EuclideanSpace as _, InnerSpace as _, Matrix as _, Matrix4, Point2, Point3, SquareMatrix,
    Transform, Vector2, Vector3, Vector4,
};

use crate::math::FreeCoordinate;
use crate::raycast::Ray;
use crate::space::Grid;

type M = Matrix4<FreeCoordinate>;

/// Tool for setting up and using a projection matrix. Stores viewport dimensions
/// and aspect ratio, and derives a projection matrix.
///
/// Also stores an externally-provided view matrix (world space to camera space).
/// If not needed, can be ignored.
#[derive(Clone, Debug)]
pub struct ProjectionHelper {
    // Caller-provided data
    viewport: Viewport,
    fov_y: Deg<FreeCoordinate>,
    view_distance: FreeCoordinate,
    view_matrix: M,

    // Derived data
    projection: M,
    view_position: Point3<FreeCoordinate>,
    inverse_projection_view: M,

    /// Position of mouse pointer or other input device in normalized device coordinates
    /// (range -1 to 1 upward and rightward). If there is no such input device, zero may
    /// be used to designate the center of the screen.
    pub cursor_ndc_position: Option<Vector2<FreeCoordinate>>,
}

#[allow(clippy::cast_lossless)]
impl ProjectionHelper {
    pub fn new(viewport: Viewport) -> Self {
        let mut new_self = Self {
            viewport,
            // TODO: user preferences for FOV and distance
            fov_y: Deg(90.0),
            view_distance: 200.0,
            view_matrix: M::identity(),
            cursor_ndc_position: None,

            // Overwritten immediately by compute_matrices
            projection: M::identity(),
            view_position: Point3::origin(),
            inverse_projection_view: M::identity(),
        };
        new_self.compute_matrices();
        new_self
    }

    /// Returns the viewport value last provided.
    pub fn viewport(&self) -> Viewport {
        self.viewport
    }

    /// Sets the contained viewport value, and recalculates matrices to be suitable for
    /// the new viewport's aspect ratio.
    pub fn set_viewport(&mut self, viewport: Viewport) {
        if viewport != self.viewport {
            self.viewport = viewport;
            self.compute_matrices();
        }
    }

    /// Returns the field of view, expressed in degrees on the vertical axis (that is, the
    /// horizontal field of view depends on the viewport's aspect ratio).
    pub fn fov_y(&self) -> Deg<FreeCoordinate> {
        self.fov_y
    }

    /// Sets the field of view, in degrees on the vertical axis, and recalculates matrices
    /// to be suitable for the new projection.
    pub fn set_fov_y(&mut self, fov_y: Deg<FreeCoordinate>) {
        if fov_y.0.is_nan() {
            return; // TODO: panic? reset to default?
        }
        self.fov_y = Deg(fov_y.0.min(179.).max(1.));
        self.compute_matrices();
    }

    /// Returns the view distance; the far plane of the projection matrix, or the distance
    /// at which rendering may be truncated.
    pub fn view_distance(&self) -> FreeCoordinate {
        self.view_distance
    }

    /// Set the current cursor position. In the same pixel units as `set_viewport`.
    pub fn set_cursor_position(&mut self, position: Option<Point2<usize>>) {
        self.cursor_ndc_position = position.map(|p| self.viewport.normalize_nominal_point(p));
    }

    /// Sets the view matrix.
    ///
    /// This matrix is used to determine world coordinates for purposes of
    /// [`view_position`](Self::view_position),
    /// [`project_ndc_into_world`](Self::project_ndc_into_world),
    /// and [`project_cursor_into_world`](Self::project_cursor_into_world).
    /// to determine what world coordinates are.
    pub fn set_view_matrix(&mut self, view_matrix: M) {
        if view_matrix != self.view_matrix {
            self.view_matrix = view_matrix;
            self.compute_matrices();
        }
    }

    /// Returns a projection matrix suitable for OpenGL use.
    pub fn projection(&self) -> M {
        self.projection
    }

    /// Returns a view matrix suitable for OpenGL use.
    pub fn view_matrix(&self) -> M {
        self.view_matrix
    }

    /// Returns the eye position in world coordinates, as set by [`Self::set_view_matrix()`].
    pub fn view_position(&self) -> Point3<FreeCoordinate> {
        self.view_position
    }

    /// Converts a screen position in normalized device coordinates (as produced by
    /// [`Viewport::normalize_nominal_point`]) into a ray in world space.
    /// Uses the view transformation given by [`set_view_matrix`](Self::set_view_matrix).
    pub fn project_ndc_into_world(&self, ndc_x: FreeCoordinate, ndc_y: FreeCoordinate) -> Ray {
        let ndc_near = Vector4::new(ndc_x, ndc_y, -1.0, 1.0);
        let ndc_far = Vector4::new(ndc_x, ndc_y, 1.0, 1.0);
        // World-space endpoints of the ray.
        let world_near = Point3::from_homogeneous(self.inverse_projection_view * ndc_near);
        let world_far = Point3::from_homogeneous(self.inverse_projection_view * ndc_far);
        let direction = world_far - world_near;
        Ray {
            origin: world_near,
            direction,
        }
    }

    /// Converts the cursor position into a ray in world space.
    /// Uses the view transformation given by [`set_view_matrix`](Self::set_view_matrix).
    pub fn project_cursor_into_world(&self) -> Option<Ray> {
        self.cursor_ndc_position
            .map(|pos| self.project_ndc_into_world(pos.x, pos.y))
    }

    fn compute_matrices(&mut self) {
        self.projection = cgmath::perspective(
            self.fov_y(),
            self.viewport.nominal_aspect_ratio(),
            /* near: */ 1. / 32., // half a voxel at resolution=16
            /* far: */ self.view_distance,
        );
        self.view_position = Point3::from_vec(
            self.view_matrix
                .inverse_transform()
                .expect("view matrix was not invertible")
                .transpose()
                .row(3)
                .truncate(),
        );
        self.inverse_projection_view = (self.projection * self.view_matrix)
            .inverse_transform()
            .expect("projection and view matrix was not invertible");
    }
}

/// Viewport dimensions for rendering and UI layout with the correct resolution and
/// aspect ratio.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Viewport {
    /// Viewport dimensions to use for determining aspect ratio and interpreting
    /// pointer events.
    pub nominal_size: Vector2<FreeCoordinate>,
    /// Viewport dimensions to use for framebuffer configuration.
    /// This aspect ratio may differ to represent non-square pixels.
    pub framebuffer_size: Vector2<u32>,
}

impl Viewport {
    #![allow(clippy::cast_lossless)] // lossiness depends on size of usize

    /// Calculates the aspect ratio (width divided by height) of the `nominal_size` of this
    /// viewport.
    #[inline]
    pub fn nominal_aspect_ratio(&self) -> FreeCoordinate {
        self.nominal_size.x / self.nominal_size.y
    }

    /// Convert an *x* coordinate from the range `0..self.framebuffer_size.x` (upper exclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers).
    #[inline]
    pub fn normalize_fb_x(&self, x: usize) -> FreeCoordinate {
        (x as FreeCoordinate + 0.5) / FreeCoordinate::from(self.framebuffer_size.x) * 2.0 - 1.0
    }

    /// Convert a *y* coordinate from the range `0..self.framebuffer_size.y` (upper exclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers) and flipped.
    #[inline]
    pub fn normalize_fb_y(&self, y: usize) -> FreeCoordinate {
        -((y as FreeCoordinate + 0.5) / FreeCoordinate::from(self.framebuffer_size.y) * 2.0 - 1.0)
    }

    /// Convert a point in the `self.nominal_size` coordinate system to
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers) with Y flipped.
    ///
    /// TODO: this should take float input, probably
    #[inline]
    pub fn normalize_nominal_point(&self, nominal_point: Point2<usize>) -> Vector2<FreeCoordinate> {
        Vector2::new(
            (nominal_point.x as FreeCoordinate + 0.5) / self.nominal_size.x * 2.0 - 1.0,
            -((nominal_point.y as FreeCoordinate + 0.5) / self.nominal_size.y * 2.0 - 1.0),
        )
    }

    // TODO: Maybe have a validate() that checks if the data is not fit for producing an
    // invertible transform.
}

/// Calculate an “eye position” (camera position) to view the entire given `grid`.
///
/// `direction` points in the direction the camera should be relative to the space.
///
/// TODO: This function does not yet consider the effects of field-of-view,
/// and it will need additional parameters to do so.
pub fn eye_for_look_at(grid: Grid, direction: Vector3<FreeCoordinate>) -> Point3<FreeCoordinate> {
    let mut space_radius: FreeCoordinate = 0.0;
    for axis in 0..3 {
        space_radius = space_radius.max(grid.size()[axis].into());
    }
    grid.center() + direction.normalize() * space_radius // TODO: allow for camera FoV
}

#[cfg(test)]
mod tests {
    use super::*;

    const DUMMY_VIEWPORT: Viewport = Viewport {
        nominal_size: Vector2::new(2.0, 2.0),
        framebuffer_size: Vector2::new(2, 2),
    };

    #[test]
    fn projection_helper_bad_viewport_doesnt_panic() {
        ProjectionHelper::new(Viewport {
            nominal_size: Vector2::new(0.0, 0.0),
            framebuffer_size: Vector2::new(0, 0),
        });
    }

    #[test]
    fn projection_helper_view_position() {
        let mut ph = ProjectionHelper::new(DUMMY_VIEWPORT);
        let pos = Point3::new(1.0, 2.0, 3.0);
        ph.set_view_matrix(
            Matrix4::from_angle_x(Deg(45.0)) * Matrix4::from_translation(-pos.to_vec()),
        );
        assert_eq!(ph.view_position(), pos);
    }
}
