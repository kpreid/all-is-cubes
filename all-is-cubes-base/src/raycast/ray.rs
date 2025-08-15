use core::f64::consts::TAU;

use euclid::Point3D;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use crate::math::{
    self, Cube, Face7, FreeCoordinate, FreePoint, FreeVector, GridCoordinate, GridVector, lines,
};
use crate::raycast::{AxisAlignedRaycaster, Raycaster};
use crate::resolution::Resolution;

#[cfg(doc)]
use crate::math::GridAab;

/// A ray; a half-infinite line segment (sometimes used as finite by the length of the
/// direction vector).
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Ray {
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
    #[allow(clippy::missing_inline_in_public_items, reason = "is generic already")]
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

    /// Add `offset` to the origin of this ray.
    #[inline]
    #[must_use]
    pub fn translate(self, offset: FreeVector) -> Self {
        Self {
            origin: self.origin + offset,
            ..self
        }
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

impl lines::Wireframe for Ray {
    #[allow(clippy::missing_inline_in_public_items)]
    fn wireframe_points<E: Extend<[lines::Vertex; 2]>>(&self, output: &mut E) {
        // Draw line
        let tip = self.unit_endpoint();
        output.extend([[self.origin.into(), tip.into()]]);

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
                [circle_point.into(), tip.into()],
                [circle_point.into(), adj_circle_point.into()],
            ]);
        }
    }
}

/// An axis-aligned, grid-aligned version of [`Ray`].
#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub struct AaRay {
    /// Cube within which the ray starts.
    pub(in crate::raycast) origin: Cube,
    /// Axis-aligned direction in which the ray extends.
    pub(in crate::raycast) direction: Face7,
    /// Further offset within the `origin` cube.
    /// Components are always in the range `0.0..1.0`.
    //---
    // TODO: switch this to f16 when that is supported — we really don't need more precision,
    // because the goal is to hit individual voxels, not any finer subdivision than that.
    // (In fact, fixed-point u8 would really suffice.)
    pub(in crate::raycast) sub_origin: Point3D<f32, Cube>,
}

impl AaRay {
    /// Constructs an [`AaRay`] from its origin cube and direction.
    ///
    /// The ray's exact origin is considered to be in the center of the given cube.
    ///
    /// # Example
    ///
    /// ```
    /// # use all_is_cubes_base as all_is_cubes;
    /// use all_is_cubes::math::{Cube, Face7};
    /// use all_is_cubes::raycast::{AaRay, Ray};
    ///
    /// let aa_ray = AaRay::new(Cube::new(1, 2, 3), Face7::PX);
    ///
    /// assert_eq!(Ray::from(aa_ray), Ray::new([1.5, 2.5, 3.5], [1., 0., 0.]));
    /// ```
    #[inline]
    #[must_use]
    pub fn new(origin: Cube, direction: Face7) -> Self {
        Self {
            origin,
            direction,
            sub_origin: Point3D::splat(0.5),
        }
    }

    /// Returns the cube which this ray’s origin point is located in.
    #[inline]
    #[must_use]
    pub fn origin_cube(&self) -> Cube {
        self.origin
    }

    /// Scale and translate this ray’s origin to occupy the given cube;
    /// its prior origin now lies within that cube.
    ///
    /// Panics if `self`’s position is outside of the bounds of
    /// [`GridAab::for_block(resolution)`](GridAab::for_block).
    ///
    /// # Example
    ///
    /// ```
    /// # mod all_is_cubes {
    /// #   pub mod block { pub use all_is_cubes_base::resolution::Resolution; }
    /// #   pub use all_is_cubes_base::{math, raycast};
    /// # }
    /// use all_is_cubes::block::Resolution;
    /// use all_is_cubes::math::{Cube, Face7};
    /// use all_is_cubes::raycast::{AaRay, Ray};
    ///
    /// let aa_ray =
    ///     AaRay::new(Cube::new(1, 2, 0), Face7::PX)
    ///         .zoom_out(Cube::new(100, 100, 100), Resolution::R4);
    ///
    /// assert_eq!(Ray::from(aa_ray), Ray::new([100.375, 100.625, 100.125], [1., 0., 0.]));
    /// ```
    #[inline]
    #[must_use]
    #[track_caller]
    pub fn zoom_out(self, cube: Cube, resolution: Resolution) -> Self {
        assert!(
            math::GridAab::for_block(resolution).contains_cube(self.origin),
            "ray origin {o:?} is out of bounds for within_cube({resolution:?}",
            o = self.origin
        );

        let sub_origin = (self.origin.lower_bounds().to_f32() + self.sub_origin.to_vector())
            / f32::from(resolution);

        Self {
            origin: cube,
            direction: self.direction,
            sub_origin,
        }
    }

    /// Create a new ray which crosses a grid of resolution `resolution`
    /// within the bounds of `cube`, along the same path this ray takes through that cube.
    ///
    /// This is not a perfect inverse of [`AaRay::zoom_in()`], because it does not
    /// preserve the origin’s position along the path of the ray.
    #[inline]
    #[must_use]
    pub fn zoom_in(self, cube: Cube, resolution: Resolution) -> Self {
        let resolution_g = GridCoordinate::from(resolution);
        let mut transformed_origin = self
            .origin
            .lower_bounds()
            .zip(cube.lower_bounds(), GridCoordinate::saturating_sub)
            .map(|coord| coord.saturating_mul(resolution_g))
            .to_point();

        // The origin coordinates may have saturated, but only along the axis of the ray, because
        // the others must be within the cube bounds.
        // Replace that coordinate with one which is just outside the bounds of the cube,
        // but only if not starting within that cube.
        if let Some(axis) = self.direction.axis()
            && !(0..resolution_g).contains(&transformed_origin[axis])
        {
            transformed_origin[axis] = if self.direction.is_positive() {
                -1
            } else {
                resolution_g
            };
        }

        // Translation vector for the cube of the high-resolution grid that the ray is within
        // according to `sub_origin`.
        let sub_cube_containing_sub_origin: GridVector =
            Cube::containing(self.sub_origin.to_f64() * FreeCoordinate::from(resolution))
                .unwrap() // can't fail because it's in the range 0..Resolution::MAX
                .lower_bounds()
                .to_vector();

        Self {
            origin: Cube::from(transformed_origin + sub_cube_containing_sub_origin),
            direction: self.direction,
            sub_origin: self.sub_origin * f32::from(resolution)
                - sub_cube_containing_sub_origin.to_f32(),
        }
    }

    /// Prepares an [`AxisAlignedRaycaster`] that will iterate over cubes intersected by this ray.
    #[inline]
    pub fn cast(self) -> AxisAlignedRaycaster {
        AxisAlignedRaycaster::new(self)
    }
}

impl From<AaRay> for Ray {
    #[inline]
    fn from(ray: AaRay) -> Self {
        Ray {
            origin: ray.origin.lower_bounds().to_f64() + (ray.sub_origin.to_f64().to_vector()),
            direction: ray.direction.normal_vector(),
        }
    }
}

impl TryFrom<Ray> for AaRay {
    // TODO: proper error type
    type Error = ();

    /// Converts the given arbitrary ray into an axis-aligned ray.
    ///
    /// Currently, its length is discarded.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    ///
    /// * The ray’s origin lies outside of the bounds permitted by [`Cube::containing()`].
    /// * The ray’s direction is not axis-aligned.
    /// * The ray has NaN components.
    #[inline]
    fn try_from(ray: Ray) -> Result<Self, Self::Error> {
        use core::cmp::Ordering::*;

        // Find which axis is nonzero.
        // Reject all cases where more than one axis is nonzero, or any axis is NaN.
        let direction = match <[_; 3]>::from(ray.direction.map(|coord| coord.partial_cmp(&0.0))) {
            [Some(Less), Some(Equal), Some(Equal)] => Face7::NX,
            [Some(Equal), Some(Less), Some(Equal)] => Face7::NY,
            [Some(Equal), Some(Equal), Some(Less)] => Face7::NZ,
            [Some(Greater), Some(Equal), Some(Equal)] => Face7::PX,
            [Some(Equal), Some(Greater), Some(Equal)] => Face7::PY,
            [Some(Equal), Some(Equal), Some(Greater)] => Face7::PZ,
            [Some(Equal), Some(Equal), Some(Equal)] => Face7::Within,
            _ => return Err(()),
        };

        let origin = Cube::containing(ray.origin).ok_or(())?;
        Ok(AaRay {
            origin,
            direction,
            sub_origin: (ray.origin - origin.lower_bounds().to_vector().to_f64()).to_f32(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aa_ray_round_trip() {
        assert_eq!(
            Ray::from(AaRay::try_from(Ray::new([1., 2., 3.], [0., 0., 0.])).unwrap()),
            Ray::new([1., 2., 3.], [0., 0., 0.]),
        );
        assert_eq!(
            Ray::from(AaRay::try_from(Ray::new([1., 2., 3.], [4., 0., 0.])).unwrap()),
            Ray::new([1., 2., 3.], [1., 0., 0.]), // note length got reset
        );
    }
}
