use core::f64::consts::TAU;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use crate::math::{self, FreeCoordinate, FreePoint, FreeVector, LineVertex};

use super::Raycaster;

/// A ray; a half-infinite line segment (sometimes used as finite by the length of the
/// direction vector).
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Ray {
    // TODO(euclid migration): should we expose the coordinate unit generic?
    /// The sole endpoint of the ray.
    pub origin: FreePoint,

    /// The direction in which the ray extends infinitely.
    ///
    /// The meaning, if any, of the magnitude of this vector depends on context;
    /// considered as a geometric object it is a parameter.
    pub direction: FreeVector,
}

impl Ray {
    /// Constructs a [`Ray`] from convertible types (e.g. tuples or 3-element arrays).
    /// Other than the use of [`Into`], this is equivalent to a struct literal.
    ///
    /// ```
    /// # use all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::euclid::{point3, vec3};
    /// use all_is_cubes::raycast::Ray;
    ///
    /// assert_eq!(
    ///     Ray::new([1., 2., 3.], [4., 5., 6.]),
    ///     Ray {
    ///         origin: point3(1., 2., 3.),
    ///         direction: vec3(4., 5., 6.),
    ///     }
    /// );
    /// ```
    #[allow(clippy::missing_inline_in_public_items)] // is generic already
    pub fn new(origin: impl Into<FreePoint>, direction: impl Into<FreeVector>) -> Self {
        Self {
            origin: origin.into(),
            direction: direction.into(),
        }
    }

    /// Prepares a [`Raycaster`] that will iterate over cubes intersected by this ray.
    #[must_use]
    #[allow(clippy::missing_inline_in_public_items)]
    pub fn cast(&self) -> Raycaster {
        Raycaster::new(self.origin, self.direction)
    }

    /// Scale the ray's coordinates by the given factor.
    #[must_use]
    #[inline]
    pub fn scale_all(self, scale: FreeCoordinate) -> Self {
        Self {
            origin: self.origin * scale,
            direction: self.direction * scale,
        }
    }

    /// Scale the ray's direction vector by the given factor.
    #[must_use]
    #[inline]
    pub fn scale_direction(self, scale: FreeCoordinate) -> Self {
        Self {
            origin: self.origin,
            direction: self.direction * scale,
        }
    }

    /// Return `self.origin + self.direction`, the “far end” of the ray.
    ///
    /// This only makes sense in contexts which are specifically using the length of the
    /// direction vector as a distance, or for visualization as a line segment.
    #[must_use]
    #[inline]
    pub fn unit_endpoint(self) -> FreePoint {
        self.origin + self.direction
    }

    pub(crate) fn advance(self, t: FreeCoordinate) -> Self {
        Self {
            origin: self.origin + self.direction * t,
            direction: self.direction,
        }
    }
}

impl math::Geometry for Ray {
    type Coord = FreeCoordinate;

    #[inline]
    fn translate(self, offset: FreeVector) -> Self {
        Self {
            origin: self.origin + offset,
            ..self
        }
    }

    #[allow(clippy::missing_inline_in_public_items)]
    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>,
    {
        // Draw line
        let tip = self.unit_endpoint();
        output.extend([self.origin.into(), tip.into()]);

        // If the length is nonzero, draw arrowhead
        let length = self.direction.length();
        if length.partial_cmp(&0.0) != Some(core::cmp::Ordering::Greater) {
            return;
        }
        let norm_dir = self.direction / length;

        // Pick a size of arrowhead
        let head_length = (length * 0.25).min(0.125);
        let head_width = head_length * 0.25;
        let head_base_point = tip - norm_dir * head_length;

        // Pick a set of perpendicular axes
        let mut perp1 = norm_dir.cross(FreeVector::new(0., 1., 0.));
        if (perp1.length() - 1.0).abs() > 1e-2 {
            // handle parallel-to-up vectors
            perp1 = norm_dir.cross(FreeVector::new(1., 0., 0.));
        }
        let perp2 = norm_dir.cross(perp1);

        // Generate a wireframe cone
        fn ang(step: i32) -> f64 {
            f64::from(step) * (TAU / 8.0)
        }
        for step in 0..8 {
            let circle_point = head_base_point
                + perp1 * head_width * ang(step).sin()
                + perp2 * head_width * ang(step).cos();
            let adj_circle_point = head_base_point
                + perp1 * head_width * ang(step + 1).sin()
                + perp2 * head_width * ang(step + 1).cos();
            output.extend([
                circle_point.into(),
                tip.into(),
                circle_point.into(),
                adj_circle_point.into(),
            ]);
        }
    }
}
