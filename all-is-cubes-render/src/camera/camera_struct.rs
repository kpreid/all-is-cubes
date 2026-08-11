use itertools::Itertools as _;
use num_traits::ConstOne as _;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use all_is_cubes::euclid::{Angle, Point2D, Point3D, Rotation3D, Transform3D, point3, vec3};
use all_is_cubes::math::{
    Aab, Cube, FreeCoordinate, FreePoint, FreeVector, Octant, OctantMask, PositiveSign, Rgba,
    lines, ps64,
};
use all_is_cubes::raycast::Ray;

use crate::camera::{
    ExposureOption, Eye, GraphicsOptions, LightingOption, ViewTransform, Viewport,
};

#[cfg(doc)]
use {crate::camera::StandardCameras, all_is_cubes::space::Space};

// -------------------------------------------------------------------------------------------------

/// Defines a perspective view in/of the world.
///
/// A [`Camera`] has the following independently controllable properties.
///
/// * A [`ViewTransform`], which specifies the viewpoint (eye position) and
///   direction.
/// * A [`Viewport`], whose aspect ratio is used in the projection matrix.
/// * A [`GraphicsOptions`], whose `fov_y` is used in the projection matrix.
/// * A “measured exposure” (luminance scale factor) value which should be provided by examining
///   the world, and which is carried through to [the output](Camera::exposure).
///
/// From this information, it derives [view](Camera::view_matrix) and
/// [projection](Camera::projection_matrix) matrices.
///
/// It does not specify *what* [`Space`] is to be viewed; more broadly, it is
/// plain data structure that does some calculations, and knows nothing outside of that.
/// See [`StandardCameras`] for going further.
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
    pub(in crate::camera) view_frustum: FrustumPoints,

    /// Scale factor for scene brightness.
    /// Calculated from `options.exposure` by [`Self::set_options`].
    exposure_value: PositiveSign<f32>,
}

/// Basic creation and mutation.
impl Camera {
    /// Create a camera which has
    ///
    /// * options and viewport as given,
    /// * a view transform of [`ViewTransform::identity()`], and
    /// * an exposure determined based on the graphics options.
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

    /// Replace the [`GraphicsOptions`] stored in this camera with
    /// [`options.repair()`](GraphicsOptions::repair).
    pub fn set_options(&mut self, options: GraphicsOptions) {
        let options = options.repair();
        self.exposure_value = options.exposure.initial();
        self.options = options;
        // TODO: we only *need* to recompute if fov_y changed (currently)
        self.compute_matrices();
    }

    /// Returns the [`GraphicsOptions`] value last provided to
    /// [`Camera::new()`] or [`Camera::set_options()`] (possibly with
    /// [adjusted](GraphicsOptions::repair) values).
    pub fn options(&self) -> &GraphicsOptions {
        &self.options
    }

    /// Sets the contained viewport value, and recalculates matrices to be suitable for
    /// the new viewport's aspect ratio.
    pub fn set_viewport(&mut self, viewport: Viewport) {
        if viewport != self.viewport {
            self.viewport = viewport;
            // TODO: What happens if the viewport is negative sized?
            self.compute_matrices();
        }
    }

    /// Returns the viewport last provided to [`Camera::new()`] or [`Camera::set_viewport()`].
    pub fn viewport(&self) -> Viewport {
        self.viewport
    }

    /// Sets the view transform and recalculates matrices appropriately.
    ///
    /// Note that this is specified as the eye-to-world transform; that is, the given transform’s
    /// translation should be equal to the view point in world coordinates.
    ///
    /// Besides controlling rendering, this is used to determine world coordinates for purposes
    /// of [`view_position()`](Self::view_position) and
    /// [`project_ndc_into_world()`](Self::project_ndc_into_world).
    pub fn set_view_transform(&mut self, eye_to_world_transform: ViewTransform) {
        if eye_to_world_transform.to_untyped() == self.eye_to_world_transform.to_untyped() {
            return;
        }

        self.eye_to_world_transform = eye_to_world_transform;
        self.compute_matrices();
    }

    /// Sets the view transform like [`Camera::set_view_transform()`], but in “look at” fashion.
    pub fn look_at_y_up(&mut self, eye: FreePoint, target: FreePoint) {
        self.set_view_transform(look_at_y_up(eye, target))
    }

    /// Returns the last eye-to-world transform set by [`Camera::set_view_transform()`].
    pub fn view_transform(&self) -> ViewTransform {
        self.eye_to_world_transform
    }

    /// Sets the exposure value that should have been determined by average scene brightness.
    /// This may or may not affect [`Self::exposure()`] depending on the current
    /// graphics options.
    pub fn set_measured_exposure(&mut self, value: f32) {
        if let Ok(value) = PositiveSign::<f32>::try_from(value) {
            match (&self.options.exposure, &self.options.lighting_display) {
                (ExposureOption::Fixed(_), _) => { /* nothing to do */ }
                (ExposureOption::Automatic, LightingOption::None) => {
                    self.exposure_value = PositiveSign::ONE;
                }
                (ExposureOption::Automatic, _) => {
                    self.exposure_value = value;
                }
            }
        }
    }
}

/// Values derived from the basic parameters, and
/// functions for applying this camera’s characteristics to points and colors.
impl Camera {
    /// Returns the field of view, expressed in degrees on the vertical axis (that is, the
    /// horizontal field of view depends on the viewport's aspect ratio).
    pub fn fov_y(&self) -> FreeCoordinate {
        self.options.fov_y.into_inner()
    }

    /// Returns the view distance; the far plane of the view frustum, or the distance
    /// at which rendering may be truncated.
    pub fn view_distance(&self) -> PositiveSign<FreeCoordinate> {
        self.options.view_distance
    }

    /// Returns the position of the near plane of the view frustum.
    /// This is not currently configurable.
    #[allow(clippy::unused_self)]
    pub fn near_plane_distance(&self) -> PositiveSign<FreeCoordinate> {
        // half a voxel at resolution=16
        ps64((32.0f64).recip())
    }

    /// Returns a perspective projection matrix based on the configured FOV and view distance,
    ///
    /// It maps coordinates in “eye” space into the Normalized Device Cooordinate space whose range
    /// is from -1 to 1 in X and Y, and 0 to 1 in Z. (This is DirectX and WebGPU style NDC, rather
    /// than OpenGL style which has a range of -1 to 1 in Z.)
    pub fn projection_matrix(&self) -> Transform3D<FreeCoordinate, Eye, Ndc> {
        self.projection
    }

    /// Returns a matrix which maps coordinates in world space to coordinates in eye space.
    /// It is the inverse of the current [`Camera::view_transform()`].
    pub fn view_matrix(&self) -> Transform3D<FreeCoordinate, Cube, Eye> {
        self.world_to_eye_matrix
    }

    /// Returns the eye position in world coordinates, as set by [`Camera::set_view_transform()`].
    pub fn view_position(&self) -> FreePoint {
        self.view_position
    }

    /// Converts a screen position in normalized device coordinates (as produced by
    /// [`Viewport::normalize_nominal_point()`]) into a ray in world space.
    /// Uses the view transformation given by [`set_view_transform`](Self::set_view_transform).
    ///
    /// The input coordinates should be within the range -1 to 1, inclusive.
    /// If they are not, the result may be a ray whose components are NaN.
    ///
    /// The ray’s origin’s distance along the view direction axis is equal to
    /// [`Camera::near_plane_distance()`].
    /// The ray's [`Ray::unit_endpoint()`] along the view direction axis is equal to
    /// [`Camera::view_distance()`].
    pub fn project_ndc_into_world(&self, ndc: NdcPoint2) -> Ray {
        let ndc_near = ndc.extend(0.0);
        let ndc_far = ndc.extend(1.0);

        // World-space endpoints of the ray.
        let world_near = self.project_ndc3_into_world(ndc_near);
        let world_far = self.project_ndc3_into_world(ndc_far);

        let direction = world_far - world_near;
        Ray {
            origin: world_near,
            direction,
        }
    }

    fn project_ndc3_into_world(&self, p: NdcPoint3) -> FreePoint {
        self.inverse_projection_view
            .transform_point3d(p)
            .unwrap_or(FreePoint::splat(FreeCoordinate::NAN))
    }

    /// Returns an [`OctantMask`] which includes all directions (in world space) visible in
    /// images rendered as specified by this camera.
    ///
    /// This information may be used as a fast initial culling step, avoiding iterating over
    /// content behind the camera.
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
        mask.set(Octant::from_vector(lb));
        mask.set(Octant::from_vector(lt));
        mask.set(Octant::from_vector(rb));
        mask.set(Octant::from_vector(rt));
        // Midpoints
        let lmid = lb + lt;
        let rmid = rb + rt;
        mask.set(Octant::from_vector(lmid));
        mask.set(Octant::from_vector(rmid));
        mask.set(Octant::from_vector(lt + rt));
        mask.set(Octant::from_vector(lb + rb));
        // Center line
        mask.set(Octant::from_vector(lmid + rmid));

        mask
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

    #[doc(hidden)] // used in other crates debugging; not stable API
    pub fn view_frustum_geometry(&self) -> &(impl lines::Wireframe + '_) {
        &self.view_frustum
    }

    /// Returns the current exposure value for scaling luminance.
    ///
    /// Renderers should use this value, not the fixed exposure value in the [`GraphicsOptions`].
    /// It may or may not be equal to the last
    /// [`set_measured_exposure()`](Self::set_measured_exposure),
    /// depending on the graphics options.
    pub fn exposure(&self) -> PositiveSign<f32> {
        self.exposure_value
    }

    /// Apply postprocessing steps determined by this camera to convert a [HDR] “scene”
    /// color into a SDR “image” color. Specifically:
    ///
    /// 1. Multiply the input by this camera's [`exposure()`](Camera::exposure) value.
    /// 2. Apply the tone mapping operator specified in [`Camera::options()`].
    ///
    /// [HDR]: https://en.wikipedia.org/wiki/High_dynamic_range
    pub fn post_process_color(&self, color: Rgba) -> Rgba {
        color.map_rgb(|rgb| {
            self.options
                .tone_mapping
                .apply(self.options.maximum_intensity, rgb * self.exposure())
        })
    }
}

// Internals
impl Camera {
    fn compute_matrices(&mut self) {
        let fov_cot = (self.fov_y() / 2.).to_radians().tan().recip();
        let aspect = self.viewport.nominal_aspect_ratio();

        let PositiveSign(near) = self.near_plane_distance();
        let PositiveSign(far) = self.view_distance();

        // Rationale for this particular matrix formula: "that's what `cgmath` does",
        // and we used to use `cgmath`.
        //
        // Note that this is an DirectX—style projection matrix — that is, the depth range
        // is 0 to 1, not -1 to 1.
        #[rustfmt::skip]
        let projection = Transform3D::new(
            fov_cot / aspect, 0.0, 0.0, 0.0,
            0.0, fov_cot, 0.0, 0.0,
            0.0, 0.0, far / (near - far), -1.0,
            0.0, 0.0, (far * near) / (near - far), 0.0,
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
        let xy_limit = if self.options.debug_reduce_view_frustum {
            0.5
        } else {
            1.
        };
        self.view_frustum = FrustumPoints {
            lbn: self.project_ndc3_into_world(point3(-xy_limit, -xy_limit, 0.)),
            rbn: self.project_ndc3_into_world(point3(xy_limit, -xy_limit, 0.)),
            ltn: self.project_ndc3_into_world(point3(-xy_limit, xy_limit, 0.)),
            rtn: self.project_ndc3_into_world(point3(xy_limit, xy_limit, 0.)),
            lbf: self.project_ndc3_into_world(point3(-xy_limit, -xy_limit, 1.)),
            rbf: self.project_ndc3_into_world(point3(xy_limit, -xy_limit, 1.)),
            ltf: self.project_ndc3_into_world(point3(-xy_limit, xy_limit, 1.)),
            rtf: self.project_ndc3_into_world(point3(xy_limit, xy_limit, 1.)),
            bounds: Aab::ZERO,
        };
        self.view_frustum.compute_bounds();
    }
}

/// Unit-of-measure type for points/vectors in “normalized device coordinates”.
///
/// In this coordinate system,
/// screen-space <var>x</var> and <var>y</var> have the range -1 to 1;
/// zero is the center of the screen;
/// and <var>z</var> has the range 0 (nearest) to 1 (farthest),
/// and is image depth rather than an equivalent third spatial axis.
#[expect(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum Ndc {}

/// Screen-space point in [normalized device coordinates](Ndc), with depth.
pub type NdcPoint2 = Point2D<f64, Ndc>;
/// Screen-space point in [normalized device coordinates](Ndc), with depth.
pub type NdcPoint3 = Point3D<f64, Ndc>;

// -------------------------------------------------------------------------------------------------

/// Look-at implementation broken out for testing
pub(crate) fn look_at_y_up(eye: FreePoint, target: FreePoint) -> ViewTransform {
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
pub(in crate::camera) struct FrustumPoints {
    // visibility is for testing -- TODO: consider moving those tests here
    pub(in crate::camera) lbf: FreePoint,
    pub(in crate::camera) rbf: FreePoint,
    pub(in crate::camera) ltf: FreePoint,
    pub(in crate::camera) rtf: FreePoint,
    pub(in crate::camera) lbn: FreePoint,
    pub(in crate::camera) rbn: FreePoint,
    pub(in crate::camera) ltn: FreePoint,
    pub(in crate::camera) rtn: FreePoint,
    pub(in crate::camera) bounds: Aab,
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
            bounds: Aab::ZERO,
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

impl lines::Wireframe for FrustumPoints {
    fn wireframe_points<E: Extend<[lines::Vertex; 2]>>(&self, output: &mut E) {
        output.extend(
            [
                // far plane box
                [self.lbf, self.rbf],
                [self.rbf, self.rtf],
                [self.rtf, self.ltf],
                [self.ltf, self.lbf],
                // near plane box
                [self.lbn, self.rbn],
                [self.rbn, self.rtn],
                [self.rtn, self.ltn],
                [self.ltn, self.lbn],
                // far-near joining lines
                [self.lbf, self.lbn],
                [self.rbf, self.rbn],
                [self.rtf, self.rtn],
                [self.ltf, self.ltn],
            ]
            .into_iter()
            .map(|line| line.map(lines::Vertex::from)),
        );
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
        .expect("iterator should be non-empty")
}
