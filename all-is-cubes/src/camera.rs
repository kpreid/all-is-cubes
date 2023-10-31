//! Projection and view matrices, viewport and aspect ratio, visibility,
//! raycasting into the scene, etc.

use euclid::{
    point3, vec3, Angle, Point2D, Point3D, RigidTransform3D, Rotation3D, Transform3D, Vector2D,
};
use itertools::Itertools as _;
use num_traits::One;
use ordered_float::NotNan;

#[cfg(not(feature = "std"))]
/// Acts as polyfill for float methods
use num_traits::float::Float as _;

use crate::chunking::OctantMask;
use crate::math::{
    Aab, Axis, Cube, FreeCoordinate, FreePoint, FreeVector, GridAab, Rgba, VectorOps,
};
use crate::raycast::Ray;

mod flaws;
pub use flaws::*;

mod graphics_options;
pub use graphics_options::*;

mod renderer;
pub use renderer::*;

mod stdcam;
pub use stdcam::*;

#[cfg(test)]
mod tests;

/// Representation of a camera viewpoint and orientation.
///
/// Note that this is treated as a transform **from** the origin looking in the &minus;Z
/// direction (eye space) **to** the camera position and orientation (world space).
/// This is done so that the [`RigidTransform3D::translation`] vector is equal to the
/// world position, rather than needing to be rotated by the view direction.
pub type ViewTransform = RigidTransform3D<FreeCoordinate, Eye, Cube>;

/// Defines a viewpoint in/of the world: a viewport (aspect ratio), projection matrix,
/// and view matrix.
///
/// See also [`StandardCameras`], which adds self-updating from a character’s viewport,
/// among other features.
#[derive(Clone, Debug)]
pub struct Camera {
    /// Caller-provided options. Always validated by [`GraphicsOptions::repair`].
    options: GraphicsOptions,

    /// Caller-provided viewport.
    viewport: Viewport,

    /// Caller-provided view transform.
    eye_to_world_transform: ViewTransform,

    /// Inverse of `eye_to_world_transform` as a matrix.
    /// Might also be called "view matrix".
    /// Calculated by [`Self::compute_matrices`].
    world_to_eye_matrix: Transform3D<FreeCoordinate, Cube, Eye>,

    /// Projection matrix derived from viewport and options.
    /// Calculated by [`Self::compute_matrices`].
    projection: Transform3D<FreeCoordinate, Eye, Ndc>,

    /// View point derived from view matrix.
    /// Calculated by [`Self::compute_matrices`].
    view_position: FreePoint,

    /// Inverse of `projection * world_to_eye_matrix`.
    /// Calculated by [`Self::compute_matrices`].
    inverse_projection_view: Transform3D<FreeCoordinate, Ndc, Cube>,

    /// Bounds of the visible area in world space.
    /// Calculated by [`Self::compute_matrices`].
    view_frustum: FrustumPoints,

    /// Scale factor for scene brightness.
    /// Calculated from `options.exposure` by [`Self::set_options`].
    exposure_value: NotNan<f32>,
}

#[allow(clippy::cast_lossless)]
impl Camera {
    /// Create a camera which has
    ///
    /// * `options` and `viewport` as given,
    /// * a `view_transform` of [`ViewTransform::identity()`], and
    /// * an `exposure` determined based on the graphics options.
    pub fn new(options: GraphicsOptions, viewport: Viewport) -> Self {
        let options = options.repair();
        let mut new_self = Self {
            viewport,
            eye_to_world_transform: ViewTransform::identity(),

            // Overwritten immediately by compute_matrices
            world_to_eye_matrix: Transform3D::identity(),
            projection: Transform3D::identity(),
            view_position: Point3D::origin(),
            inverse_projection_view: Transform3D::identity(),
            view_frustum: Default::default(),

            exposure_value: options.exposure.initial(),

            options,
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

    /// Replace the [`GraphicsOptions`] stored in this camera with
    /// [`options.repair()`](GraphicsOptions::repair).
    pub fn set_options(&mut self, options: GraphicsOptions) {
        let options = options.repair();
        self.exposure_value = options.exposure.initial();
        self.options = options;
        // TODO: we only *need* to recompute if fov_y changed (currently)
        self.compute_matrices();
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
    pub fn fov_y(&self) -> FreeCoordinate {
        self.options.fov_y.into_inner()
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
    #[allow(clippy::float_cmp)]
    pub fn set_view_transform(&mut self, eye_to_world_transform: ViewTransform) {
        if eye_to_world_transform.to_untyped() == self.eye_to_world_transform.to_untyped() {
            return;
        }

        self.eye_to_world_transform = eye_to_world_transform;
        self.compute_matrices();
    }

    /// Sets the view transform like [`Self::set_view_transform()`], but in “look at” fashion.
    pub fn look_at_y_up(&mut self, eye: FreePoint, target: FreePoint) {
        self.set_view_transform(look_at_y_up(eye, target))
    }

    /// Gets the last eye-to-world transform set by [`Self::set_view_transform()`].
    pub fn get_view_transform(&self) -> ViewTransform {
        self.eye_to_world_transform
    }

    /// Returns a projection matrix suitable for OpenGL use.
    pub fn projection(&self) -> Transform3D<FreeCoordinate, Eye, Ndc> {
        self.projection
    }

    /// Returns a view matrix suitable for OpenGL use.
    pub fn view_matrix(&self) -> Transform3D<FreeCoordinate, Cube, Eye> {
        self.world_to_eye_matrix
    }

    /// Returns the eye position in world coordinates, as set by [`Camera::set_view_transform()`].
    pub fn view_position(&self) -> FreePoint {
        self.view_position
    }

    /// Returns an [`OctantMask`] including all directions this camera's field of view includes.
    pub fn view_direction_mask(&self) -> OctantMask {
        #[rustfmt::skip]
        let FrustumPoints { lbf, rbf, ltf, rtf, lbn, rbn, ltn, rtn, .. } = self.view_frustum;

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
    pub fn project_ndc_into_world(&self, ndc: NdcPoint2) -> Ray {
        let ndc_near = ndc.extend(-1.0);
        let ndc_far = ndc.extend(1.0);

        // World-space endpoints of the ray.
        // TODO(euclid migration): don't unwrap, do what instead?
        let world_near = self
            .inverse_projection_view
            .transform_point3d(ndc_near)
            .unwrap();
        let world_far = self
            .inverse_projection_view
            .transform_point3d(ndc_far)
            .unwrap();

        let direction = world_far - world_near;
        Ray {
            origin: world_near,
            direction,
        }
    }

    fn project_point_into_world(&self, p: NdcPoint3) -> FreePoint {
        // TODO(euclid migration): don't unwrap, do what instead?
        self.inverse_projection_view.transform_point3d(p).unwrap()
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

        // Test the AAB's face normals (i.e. the coordinate axes).
        // To save some arithmetic, we've precomputed the frustum's axis-aligned bounding
        // box, so we can use that instead of the general separated_along().
        if !aab.intersects(self.view_frustum.bounds) {
            return false;
        }

        // Test the view frustum's face normals.
        // (Benchmarking has shown testing this first to be better, though not shown that
        // it is better for all possible view orientations.)
        //
        // Note that testing against the near plane (lbn, ltn, rtn) is not necessary
        // since it is always parallel to the far plane, and we are testing ranges on
        // the axis, not “is this object on the far side of this plane”.
        #[rustfmt::skip]
        let FrustumPoints { lbf, rbf, ltf, rtf, lbn, rbn, ltn, rtn, .. } = self.view_frustum;
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

        true
    }

    /// Helper for [`aab_in_view`]; finds if two sets of points' projections onto a line intersect.
    ///
    /// Note: NOT `#[inline]` because profiling shows that to have a negative effect.
    fn separated_along(
        points1: impl IntoIterator<Item = FreePoint>,
        points2: impl IntoIterator<Item = FreePoint>,
        axis: FreeVector,
    ) -> bool {
        let (min1, max1) = projected_range(points1, axis);
        let (min2, max2) = projected_range(points2, axis);
        let intersection_min = min1.max(min2);
        let intersection_max = max1.min(max2);
        intersection_max < intersection_min
    }

    /// Apply postprocessing steps determined by this camera to convert a HDR “scene”
    /// color into a LDR “image” color. Specifically:
    ///
    /// 1. Multiply the input by this camera's exposure value.
    /// 2. Apply the tone mapping operator specified in [`Camera::options()`].
    pub fn post_process_color(&self, color: Rgba) -> Rgba {
        color.map_rgb(|rgb| self.options.tone_mapping.apply(rgb * self.exposure()))
    }

    /// Returns the current exposure value for scaling luminance.
    ///
    /// Renderers should use this value.
    pub fn exposure(&self) -> NotNan<f32> {
        self.exposure_value
    }

    /// Set the exposure value determined by average scene brightness.
    /// This may or may not affect [`Self::exposure()`] depending on the current
    /// graphics options.
    pub fn set_measured_exposure(&mut self, value: f32) {
        if let Ok(value) = NotNan::new(value) {
            match (&self.options.exposure, &self.options.lighting_display) {
                (ExposureOption::Fixed(_), _) => { /* nothing to do */ }
                (ExposureOption::Automatic, LightingOption::None) => {
                    self.exposure_value = NotNan::one();
                }
                (ExposureOption::Automatic, _) => {
                    self.exposure_value = value;
                }
            }
        }
    }

    fn compute_matrices(&mut self) {
        let fov_cot = (self.fov_y() / 2.).to_radians().tan().recip();
        let aspect = self.viewport.nominal_aspect_ratio();

        let near = 1. / 32.; // half a voxel at resolution=16
        let far = self.view_distance();

        // Rationale for this particular matrix formula: "that's what `cgmath` does".
        //
        // Note that this is an OpenGL—style projection matrix — that is, the depth range
        // is -1 to 1, not 0 to 1. TODO: Change that, and update the documentation.
        #[rustfmt::skip]
        let projection = Transform3D::new(
            fov_cot / aspect, 0.0, 0.0, 0.0,
            0.0, fov_cot, 0.0, 0.0,
            0.0, 0.0, (far + near) / (near - far), -1.0,
            0.0, 0.0, (2. * far * near) / (near - far), 0.0,
        );
        self.projection = projection;

        self.world_to_eye_matrix = self.eye_to_world_transform.inverse().to_transform();

        self.view_position = self.eye_to_world_transform.translation.to_point();

        self.inverse_projection_view = self
            .world_to_eye_matrix
            .then(&self.projection)
            .inverse()
            .expect("projection and view matrix was not invertible");

        // Compute the view frustum's corner points,
        // by unprojecting the corners of clip space.
        self.view_frustum = FrustumPoints {
            lbn: self.project_point_into_world(point3(-1., -1., -1.)),
            rbn: self.project_point_into_world(point3(1., -1., -1.)),
            ltn: self.project_point_into_world(point3(-1., 1., -1.)),
            rtn: self.project_point_into_world(point3(1., 1., -1.)),
            lbf: self.project_point_into_world(point3(-1., -1., 1.)),
            rbf: self.project_point_into_world(point3(1., -1., 1.)),
            ltf: self.project_point_into_world(point3(-1., 1., 1.)),
            rtf: self.project_point_into_world(point3(1., 1., 1.)),
            bounds: Aab::ZERO,
        };
        self.view_frustum.compute_bounds();
    }
}

/// Unit-of-measure/coordinate-system type for points/vectors in “eye space”,
/// the space of camera-relative coordinates that are *not* perspective-projected.
///
/// +X is right, +Y is up, +Z is towards-the-viewer (right-handed coordinates).
#[allow(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum Eye {}

/// Unit-of-measure type for vectors representing the on-screen dimensions of a [`Viewport`],
/// which may be different from the “physical” pixels of the image rendered to it.
#[allow(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum NominalPixel {}

/// Unit-of-measure type for vectors representing the width and height of an image.
///
/// Used in [`Viewport::framebuffer_size`].
#[allow(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum ImagePixel {}

/// Unit-of-measure type for points/vectors in “normalized device coordinates”
/// (where screen-space x and y have the range -1 to 1, zero is the center of the
/// screen, and z is image depth rather than an equivalent third spatial axis).
#[allow(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum Ndc {}

/// Screen-space point in [normalized device coordinates](Ndc), with depth.
pub type NdcPoint2 = Point2D<f64, Ndc>;
/// Screen-space point in [normalized device coordinates](Ndc), with depth.
pub type NdcPoint3 = Point3D<f64, Ndc>;

/// Width and height of an image, framebuffer, or window, as measured in actual distinct
/// image pixels.
///
/// For sizes that are in nominal, or “logical” pixel units that have become separated from
/// actual image or display resolution, use `Vector2D<T, NominalPixel>`; there is no type
/// alias for that.
///
/// TODO: euclid has a `Size2D` type. Try switching to that.
pub type ImageSize = Vector2D<u32, ImagePixel>;

/// Viewport dimensions for rendering and UI layout with the correct resolution and
/// aspect ratio.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Viewport {
    /// Viewport dimensions to use for determining aspect ratio and interpreting
    /// pointer events.
    pub nominal_size: Vector2D<FreeCoordinate, NominalPixel>,
    /// Viewport dimensions to use for framebuffer configuration.
    /// This aspect ratio may differ to represent non-square pixels.
    pub framebuffer_size: ImageSize,
}

impl Viewport {
    #![allow(clippy::cast_lossless)] // lossiness depends on size of usize

    /// Construct a Viewport from a pixel count and a scale factor.
    ///
    /// The `nominal_size` will be the given `framebuffer_size` divided by the given
    /// `scale_factor`.
    pub fn with_scale(
        scale_factor: f64,
        framebuffer_size: impl Into<Vector2D<u32, ImagePixel>>,
    ) -> Self {
        let framebuffer_size = framebuffer_size.into();
        Self {
            framebuffer_size,
            nominal_size: framebuffer_size.map(f64::from).cast_unit() / scale_factor,
        }
    }

    /// A meaningless but valid [`Viewport`] value for use in tests which require one
    /// but do not care about its effects.
    #[doc(hidden)]
    pub const ARBITRARY: Viewport = Viewport {
        nominal_size: Vector2D::new(2.0, 2.0),
        framebuffer_size: Vector2D::new(2, 2),
    };

    /// Calculates the aspect ratio (width divided by height) of the `nominal_size` of this
    /// viewport.
    ///
    /// If the result would naturally be infinite or undefined then it is reported as 1
    /// instead. This is intended to aid in robust handling of degenerate viewports which
    /// contain no pixels.
    #[inline]
    pub fn nominal_aspect_ratio(&self) -> FreeCoordinate {
        let ratio = self.nominal_size.x / self.nominal_size.y;
        if ratio.is_finite() {
            ratio
        } else {
            1.0
        }
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

    /// Convert an *x* coordinate from the range `0..=self.framebuffer_size.x` (inclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel *edges*).
    #[inline]
    pub fn normalize_fb_x_edge(&self, x: usize) -> FreeCoordinate {
        (x as FreeCoordinate) / FreeCoordinate::from(self.framebuffer_size.x) * 2.0 - 1.0
    }

    /// Convert a *y* coordinate from the range `0..=self.framebuffer_size.y` (inclusive)
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel *edges*) and flipped.
    #[inline]
    pub fn normalize_fb_y_edge(&self, y: usize) -> FreeCoordinate {
        -((y as FreeCoordinate) / FreeCoordinate::from(self.framebuffer_size.y) * 2.0 - 1.0)
    }

    /// Convert a point in the [`Self::nominal_size`] coordinate system to
    /// to OpenGL normalized device coordinates, range -1 to 1 (at pixel centers) with Y flipped.
    ///
    /// TODO: Some windowing APIs providing float input might have different ideas of pixel centers.
    #[inline]
    pub fn normalize_nominal_point(&self, nominal_point: Point2D<f64, NominalPixel>) -> NdcPoint2 {
        Point2D::new(
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

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a> arbitrary::Arbitrary<'a> for Viewport {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(Viewport {
            nominal_size: Vector2D::new(u.arbitrary()?, u.arbitrary()?),
            framebuffer_size: Vector2D::new(u.arbitrary()?, u.arbitrary()?),
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        arbitrary::size_hint::and(
            <[u32; 2]>::size_hint(depth),
            <[FreeCoordinate; 2]>::size_hint(depth),
        )
    }
}

/// Calculate an “eye position” (camera position) to view the entire given `bounds`.
///
/// `direction` points in the direction the camera should be relative to the space.
///
/// TODO: This function does not yet consider the effects of field-of-view,
/// and it will need additional parameters to do so.
pub fn eye_for_look_at(bounds: GridAab, direction: FreeVector) -> FreePoint {
    let mut space_radius: FreeCoordinate = 0.0;
    for axis in Axis::ALL {
        space_radius = space_radius.max(bounds.size()[axis].into());
    }
    bounds.center() + direction.normalize() * space_radius // TODO: allow for camera FoV
}

/// Look-at implementation broken out for testing
fn look_at_y_up(eye: FreePoint, target: FreePoint) -> ViewTransform {
    let look_direction = target - eye;
    let yaw = Angle {
        radians: look_direction.x.atan2(-look_direction.z),
    };
    let pitch = Angle {
        radians: (-look_direction.y).atan2(look_direction.xz().length()),
    };
    ViewTransform {
        rotation: Rotation3D::<_, Eye, Cube>::around_x(-pitch).then(&Rotation3D::around_y(-yaw)),
        translation: eye.to_vector(),
    }
}

/// A view frustum, represented by its corner points.
/// This is an underconstrained representation, but one that is useful to precompute.
#[derive(Clone, Copy, Debug, PartialEq)]
struct FrustumPoints {
    lbf: FreePoint,
    rbf: FreePoint,
    ltf: FreePoint,
    rtf: FreePoint,
    lbn: FreePoint,
    rbn: FreePoint,
    ltn: FreePoint,
    rtn: FreePoint,
    bounds: Aab,
}

impl Default for FrustumPoints {
    fn default() -> Self {
        Self {
            lbf: Point3D::origin(),
            rbf: Point3D::origin(),
            ltf: Point3D::origin(),
            rtf: Point3D::origin(),
            lbn: Point3D::origin(),
            rbn: Point3D::origin(),
            ltn: Point3D::origin(),
            rtn: Point3D::origin(),
            bounds: Aab::new(0., 0., 0., 0., 0., 0.),
        }
    }
}

impl FrustumPoints {
    fn iter(self) -> impl Iterator<Item = FreePoint> {
        [
            self.lbf, self.rbf, self.ltf, self.rtf, self.lbn, self.rbn, self.ltn, self.rtn,
        ]
        .into_iter()
    }

    fn compute_bounds(&mut self) {
        let (xl, xh) = projected_range(self.iter(), vec3(1., 0., 0.));
        let (yl, yh) = projected_range(self.iter(), vec3(0., 1., 0.));
        let (zl, zh) = projected_range(self.iter(), vec3(0., 0., 1.));
        self.bounds = Aab::from_lower_upper([xl, yl, zl], [xh, yh, zh]);
    }
}

/// Projects a set of points onto an axis and returns the least and greatest dot product
/// with the axis vector.
#[inline(always)]
fn projected_range(
    points: impl IntoIterator<Item = FreePoint>,
    axis: FreeVector,
) -> (FreeCoordinate, FreeCoordinate) {
    points
        .into_iter()
        .map(|p| p.to_vector().dot(axis))
        .minmax()
        .into_option()
        .unwrap()
}
