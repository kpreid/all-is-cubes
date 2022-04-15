// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Projection and view matrices, viewport and aspect ratio, visibility,
//! raycasting into the scene, etc.

use cgmath::{
    Basis3, Decomposed, Deg, EuclideanSpace as _, InnerSpace as _, Matrix4, One as _, Point2,
    Point3, SquareMatrix, Transform, Vector2, Vector3,
};
use itertools::Itertools as _;
use ordered_float::NotNan;

use crate::chunking::OctantMask;
use crate::math::{Aab, FreeCoordinate, Rgba};
use crate::raycast::Ray;
use crate::space::Grid;

mod graphics_options;
pub use graphics_options::*;

type M = Matrix4<FreeCoordinate>;

/// Representation of a camera viewpoint and orientation, using [`cgmath`] types.
///
/// Note that this is treated as a transform **from** the origin looking in the &minus;Z
/// direction **to** the camera position in the world. This is done so that the
/// [`Decomposed::disp`] vector is equal to the world position, rather than needing to
/// be rotated by the view direction.
pub type ViewTransform = Decomposed<Vector3<FreeCoordinate>, Basis3<FreeCoordinate>>;

/// Defines a viewpoint in/of the world: a viewport (aspect ratio), projection matrix,
/// and view matrix.
#[derive(Clone, Debug)]
pub struct Camera {
    /// Caller-provided options. Always validated by [`GraphicsOptions::repair`].
    options: GraphicsOptions,

    /// Scale factor for scene brightness.
    pub exposure: NotNan<f32>,

    /// Caller-provided viewport.
    viewport: Viewport,

    /// Caller-provided view transform.
    eye_to_world_transform: ViewTransform,

    /// Inverse of `eye_to_world_transform` as a matrix.
    /// Might also be called "view matrix".
    /// Calculated by [`Self::compute_matrices`].
    world_to_eye_matrix: M,

    /// Projection matrix derived from viewport and options.
    /// Calculated by [`Self::compute_matrices`].
    projection: M,

    /// View point derived from view matrix.
    /// Calculated by [`Self::compute_matrices`].
    view_position: Point3<FreeCoordinate>,

    /// Inverse of `projection * world_to_eye_matrix`.
    /// Calculated by [`Self::compute_matrices`].
    inverse_projection_view: M,

    /// Bounds of the visible area in world space.
    /// Calculated by [`Self::compute_matrices`].
    view_frustum: FrustumPoints,
}

#[allow(clippy::cast_lossless)]
impl Camera {
    pub fn new(options: GraphicsOptions, viewport: Viewport) -> Self {
        let mut new_self = Self {
            options: options.repair(),
            exposure: NotNan::one(),
            viewport,
            eye_to_world_transform: ViewTransform::one(),

            // Overwritten immediately by compute_matrices
            world_to_eye_matrix: M::identity(),
            projection: M::identity(),
            view_position: Point3::origin(),
            inverse_projection_view: M::identity(),
            view_frustum: Default::default(),
        };
        new_self.compute_matrices();
        new_self
    }

    /// Returns the viewport value last provided.
    pub fn viewport(&self) -> Viewport {
        self.viewport
    }

    /// Returns the [`GraphicsOptions`] value last provided (possibly with adjusted values).
    pub fn options(&self) -> &GraphicsOptions {
        &self.options
    }

    pub fn set_options(&mut self, options: GraphicsOptions) {
        self.options = options.repair();
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
    /// This differs from the value in [`GraphicsOptions`] by being clamped to valid values.
    pub fn fov_y(&self) -> Deg<FreeCoordinate> {
        Deg(self.options.fov_y.into_inner())
    }

    /// Returns the view distance; the far plane of the projection matrix, or the distance
    /// at which rendering may be truncated.
    pub fn view_distance(&self) -> FreeCoordinate {
        self.options.view_distance.into_inner()
    }

    /// Sets the view transform.
    ///
    /// Besides controlling rendering, this is used to determine world coordinates for purposes
    /// of [`view_position`](Self::view_position) and
    /// [`project_ndc_into_world`](Self::project_ndc_into_world).
    ///
    /// The scale currently must be 1.
    #[track_caller]
    #[allow(clippy::float_cmp)]
    pub fn set_view_transform(&mut self, eye_to_world_transform: ViewTransform) {
        if eye_to_world_transform == self.eye_to_world_transform {
            return;
        }
        assert_eq!(
            eye_to_world_transform.scale, 1.0,
            "eye_to_world_transform.scale must be equal to 1"
        );

        self.eye_to_world_transform = eye_to_world_transform;
        self.compute_matrices();
    }

    /// Returns a projection matrix suitable for OpenGL use.
    pub fn projection(&self) -> M {
        self.projection
    }

    /// Returns a view matrix suitable for OpenGL use.
    pub fn view_matrix(&self) -> M {
        self.world_to_eye_matrix
    }

    /// Returns the eye position in world coordinates, as set by [`Camera::set_view_transform()`].
    pub fn view_position(&self) -> Point3<FreeCoordinate> {
        self.view_position
    }

    /// Returns an [`OctantMask`] including all directions this camera's field of view includes.
    pub fn view_direction_mask(&self) -> OctantMask {
        #[rustfmt::skip]
        let FrustumPoints { lbf, rbf, ltf, rtf, lbn, rbn, ltn, rtn } = self.view_frustum;

        let mut mask = OctantMask::NONE;
        // Fill the mask with 9 representative rays from the camera.
        // Nine should be sufficient because the FOV cannot exceed 180°.
        // (TODO: Wait, that's 2D reasoning...)
        // Corner points
        let lb = lbf - lbn;
        let lt = ltf - ltn;
        let rb = rbf - rbn;
        let rt = rtf - rtn;
        mask.set_octant_of(lb);
        mask.set_octant_of(lt);
        mask.set_octant_of(rb);
        mask.set_octant_of(rt);
        // Midpoints
        let lmid = lb + lt;
        let rmid = rb + rt;
        mask.set_octant_of(lmid);
        mask.set_octant_of(rmid);
        mask.set_octant_of(lt + rt);
        mask.set_octant_of(lb + rb);
        // Center line
        mask.set_octant_of(lmid + rmid);

        mask
    }

    /// Converts a screen position in normalized device coordinates (as produced by
    /// [`Viewport::normalize_nominal_point`]) into a ray in world space.
    /// Uses the view transformation given by [`set_view_transform`](Self::set_view_transform).
    pub fn project_ndc_into_world(&self, ndc: Point2<FreeCoordinate>) -> Ray {
        let ndc_near = ndc.to_vec().extend(-1.0).extend(1.0);
        let ndc_far = ndc.to_vec().extend(1.0).extend(1.0);
        // World-space endpoints of the ray.
        let world_near = Point3::from_homogeneous(self.inverse_projection_view * ndc_near);
        let world_far = Point3::from_homogeneous(self.inverse_projection_view * ndc_far);
        let direction = world_far - world_near;
        Ray {
            origin: world_near,
            direction,
        }
    }

    fn project_point_into_world(&self, p: Point3<FreeCoordinate>) -> Point3<FreeCoordinate> {
        Point3::from_homogeneous(self.inverse_projection_view * p.to_homogeneous())
    }

    /// Determine whether the given `Aab` is visible in this projection+view.
    pub fn aab_in_view(&self, aab: Aab) -> bool {
        // This algorithm uses the separating axis theorem, which states that for two
        // convex objects (here, an AAB and a frustum), if there is some axis for which
        // projecting the objects' points onto that axis produces non-overlapping ranges,
        // the objects do not intersect.
        //
        // The separation axes which we test are the face normals of each object.
        // This is technically not sufficient (see e.g.
        //     https://gamedev.stackexchange.com/a/44501/9825
        // ), but false intersections are okay here since we're trying to do view culling,
        // not collision detection.

        // Test the view frustum's face normals.
        // (Benchmarking has shown testing this first to be better, though not shown that
        // it is better for all possible view orientations.)
        //
        // Note that testing against the near plane (lbn, ltn, rtn) is not necessary
        // since it is always parallel to the far plane, and we are testing ranges on
        // the axis, not “is this object on the far side of this plane”.
        #[rustfmt::skip]
        let FrustumPoints { lbf, rbf, ltf, rtf, lbn, rbn, ltn, rtn } = self.view_frustum;
        for &(p1, p2, p3) in &[
            (lbn, lbf, ltf), // left
            (rtn, rtf, rbf), // right
            (ltn, ltf, rtf), // top
            (rbn, rbf, lbf), // bottom
            (lbf, rbf, ltf), // far
        ] {
            let normal = (p2 - p1).cross(p3 - p1);
            if Self::separated_along(self.view_frustum.iter(), aab.corner_points(), normal) {
                return false;
            }
        }

        // Test the AAB's face normals (i.e. the coordinate axes).
        // We only need 3 tests, not 6, since there are only 3 axes.
        for axis in [Vector3::unit_x(), Vector3::unit_y(), Vector3::unit_z()] {
            if Self::separated_along(self.view_frustum.iter(), aab.corner_points(), axis) {
                return false;
            }
        }

        true
    }

    /// Helper for aab_in_view; finds if two sets of points' projections onto a line intersect.
    ///
    /// Note: NOT #[inline] because profiling shows that to have a negative effect.
    fn separated_along(
        points1: impl IntoIterator<Item = Point3<FreeCoordinate>>,
        points2: impl IntoIterator<Item = Point3<FreeCoordinate>>,
        axis: Vector3<FreeCoordinate>,
    ) -> bool {
        let (min1, max1) = Self::projected_range(points1, axis);
        let (min2, max2) = Self::projected_range(points2, axis);
        let intersection_min = min1.max(min2);
        let intersection_max = max1.min(max2);
        intersection_max < intersection_min
    }

    /// Helper for aab_in_view; projects a set of points onto a line.
    #[inline(always)]
    fn projected_range(
        points: impl IntoIterator<Item = Point3<FreeCoordinate>>,
        axis: Vector3<FreeCoordinate>,
    ) -> (FreeCoordinate, FreeCoordinate) {
        points
            .into_iter()
            .map(|p| p.to_vec().dot(axis))
            .minmax()
            .into_option()
            .unwrap()
    }

    /// Apply postprocessing steps determined by this camera to convert a HDR “scene”
    /// color into a LDR “image” color. Specifically:
    ///
    /// 1. Multiply the input by this camera's exposure value.
    /// 2. Apply the tone mapping operator specified in [`Camera::options()`].
    pub fn post_process_color(&self, color: Rgba) -> Rgba {
        color.map_rgb(|rgb| self.options.tone_mapping.apply(rgb * self.exposure))
    }

    fn compute_matrices(&mut self) {
        self.projection = cgmath::perspective(
            self.fov_y(),
            self.viewport.nominal_aspect_ratio(),
            /* near: */ 1. / 32., // half a voxel at resolution=16
            /* far: */ self.view_distance(),
        );

        self.world_to_eye_matrix = self.eye_to_world_transform
            .inverse_transform()
            .unwrap(/* Inverting cannot fail as long as scale is nonzero */)
            .into();

        self.view_position = Point3::from_vec(self.eye_to_world_transform.disp);

        self.inverse_projection_view = (self.projection * self.world_to_eye_matrix)
            .inverse_transform()
            .expect("projection and view matrix was not invertible");

        // Compute the view frustum's corner points,
        // by unprojecting the corners of clip space.
        self.view_frustum = FrustumPoints {
            lbn: self.project_point_into_world(Point3::new(-1., -1., -1.)),
            rbn: self.project_point_into_world(Point3::new(1., -1., -1.)),
            ltn: self.project_point_into_world(Point3::new(-1., 1., -1.)),
            rtn: self.project_point_into_world(Point3::new(1., 1., -1.)),
            lbf: self.project_point_into_world(Point3::new(-1., -1., 1.)),
            rbf: self.project_point_into_world(Point3::new(1., -1., 1.)),
            ltf: self.project_point_into_world(Point3::new(-1., 1., 1.)),
            rtf: self.project_point_into_world(Point3::new(1., 1., 1.)),
        };
    }
}

/// Viewport dimensions for rendering and UI layout with the correct resolution and
/// aspect ratio.
#[allow(clippy::exhaustive_structs)]
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

    /// Construct a Viewport from a pixel count and a scale factor.
    ///
    /// The `nominal_size` will be the given `framebuffer_size` divided by the given
    /// `scale_factor`.
    pub fn with_scale(scale_factor: f64, framebuffer_size: Vector2<u32>) -> Self {
        Self {
            framebuffer_size,
            nominal_size: framebuffer_size.map(f64::from) / scale_factor,
        }
    }

    /// A meaningless but valid [`Viewport`] value for use in tests which require one
    /// but do not care about its effects.
    #[cfg(test)]
    pub(crate) const ARBITRARY: Viewport = Viewport {
        nominal_size: Vector2::new(2.0, 2.0),
        framebuffer_size: Vector2::new(2, 2),
    };

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

    /// Convert a point in the [`Self::nominal_size`] coordinate system to
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers) with Y flipped.
    ///
    /// TODO: Some windowing APIs providing float input might have different ideas of pixel centers.
    #[inline]
    pub fn normalize_nominal_point(&self, nominal_point: Point2<f64>) -> Vector2<FreeCoordinate> {
        Vector2::new(
            (nominal_point.x + 0.5) / self.nominal_size.x * 2.0 - 1.0,
            -((nominal_point.y + 0.5) / self.nominal_size.y * 2.0 - 1.0),
        )
    }

    /// Computes the number of pixels in the framebuffer.
    /// Returns [`None`] if that number does not fit in a [`usize`].
    pub fn pixel_count(&self) -> Option<usize> {
        let w: usize = self.framebuffer_size.x.try_into().ok()?;
        let h: usize = self.framebuffer_size.y.try_into().ok()?;
        w.checked_mul(h)
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

/// A view frustum, represented by its corner points.
/// This is an underconstrained representation, but one that is useful to precompute.
#[derive(Clone, Copy, Debug, PartialEq)]
struct FrustumPoints {
    lbf: Point3<FreeCoordinate>,
    rbf: Point3<FreeCoordinate>,
    ltf: Point3<FreeCoordinate>,
    rtf: Point3<FreeCoordinate>,
    lbn: Point3<FreeCoordinate>,
    rbn: Point3<FreeCoordinate>,
    ltn: Point3<FreeCoordinate>,
    rtn: Point3<FreeCoordinate>,
}

impl Default for FrustumPoints {
    fn default() -> Self {
        Self {
            lbf: Point3::origin(),
            rbf: Point3::origin(),
            ltf: Point3::origin(),
            rtf: Point3::origin(),
            lbn: Point3::origin(),
            rbn: Point3::origin(),
            ltn: Point3::origin(),
            rtn: Point3::origin(),
        }
    }
}

impl FrustumPoints {
    fn iter(self) -> impl Iterator<Item = Point3<FreeCoordinate>> {
        [
            self.lbf, self.rbf, self.ltf, self.rtf, self.lbn, self.rbn, self.ltn, self.rtn,
        ]
        .into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn camera_bad_viewport_doesnt_panic() {
        Camera::new(
            GraphicsOptions::default(),
            Viewport {
                nominal_size: Vector2::new(0.0, 0.0),
                framebuffer_size: Vector2::new(0, 0),
            },
        );
    }

    #[test]
    fn camera_view_position() {
        // This test used to be less trivial when the transform was taken as a matrix
        let mut camera = Camera::new(GraphicsOptions::default(), Viewport::ARBITRARY);
        let pos = Point3::new(1.0, 2.0, 3.0);
        camera.set_view_transform(Decomposed {
            scale: 1.0,
            rot: Basis3::one(),
            disp: pos.to_vec(),
        });
        assert_eq!(camera.view_position(), pos);
    }

    #[test]
    fn view_frustum() {
        let camera = Camera::new(
            GraphicsOptions {
                view_distance: NotNan::new(10.0_f64.powi(2)).unwrap(),
                fov_y: notnan!(90.0),
                ..GraphicsOptions::default()
            },
            Viewport::with_scale(1.0, Vector2::new(10, 5)),
        );
        // TODO: approximate comparison instead of equals
        let x_near = 0.062499999999999986;
        let y_near = 0.031249999999999993;
        let z_near = -0.03125;
        let x_far = 199.99999999996868;
        let y_far = 99.99999999998434;
        let z_far = -99.99999999998437;
        assert_eq!(
            camera.view_frustum,
            FrustumPoints {
                lbn: Point3::new(-x_near, -y_near, z_near),
                ltn: Point3::new(-x_near, y_near, z_near),
                rbn: Point3::new(x_near, -y_near, z_near),
                rtn: Point3::new(x_near, y_near, z_near),
                lbf: Point3::new(-x_far, -y_far, z_far),
                ltf: Point3::new(-x_far, y_far, z_far),
                rbf: Point3::new(x_far, -y_far, z_far),
                rtf: Point3::new(x_far, y_far, z_far),
            }
        );
    }

    #[test]
    fn post_process() {
        let mut camera = Camera::new(GraphicsOptions::default(), Viewport::ARBITRARY);

        // A camera with all default options should pass colors unchanged.
        let color = rgba_const!(0.1, 0.2, 0.3, 0.4);
        assert_eq!(camera.post_process_color(color), color);

        // Try exposure
        camera.exposure = notnan!(0.5);
        assert_eq!(
            camera.post_process_color(color),
            color.map_rgb(|rgb| rgb * 0.5)
        );
    }
}
