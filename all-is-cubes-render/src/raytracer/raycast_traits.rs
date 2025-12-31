//! Traits used by the raytracer to abstract over raycasters.
//!
//! Arguably this belongs in [`all_is_cubes::raycast`], but I’m skeptical the traits are designed
//! well, so I’m keeping them `pub(crate)` for now.

use all_is_cubes::block::Resolution;
use all_is_cubes::math::{FreeVector, GridAab};
use all_is_cubes::raycast::{AaRay, AxisAlignedRaycaster, Ray, RaycastStep, Raycaster};

/// Trait for types that can be used for raycasting.
///
/// Additional requirement: The length of the direction vector returned through [`Into<Ray>`]
/// is equal to the grid distance represented by one “t” unit
/// in the [`RaycastStep`]s produced by [`Self::cast()`].
///
/// TODO: rename this trait once we figure out a good name.
/// (Not doing that for now because it's unclear how broadly useful the whole “axis-aligned
/// raycasting” subsystem is going to be.)
pub(crate) trait RayIsh: Copy + Into<Ray> {
    type Caster: RaycasterIsh<Ray = Self>;

    fn cast_within(self, bounds: GridAab, include_exit: bool) -> Self::Caster;

    fn direction(&self) -> FreeVector {
        <Self as Into<Ray>>::into(*self).direction
    }
}

impl RayIsh for Ray {
    type Caster = Raycaster;

    fn cast_within(self, bounds: GridAab, include_exit: bool) -> Self::Caster {
        Ray::cast(&self).within(bounds, include_exit)
    }
}

impl RayIsh for AaRay {
    type Caster = AxisAlignedRaycaster;

    fn cast_within(self, bounds: GridAab, include_exit: bool) -> Self::Caster {
        self.cast().within(bounds, include_exit)
    }
}

/// Operations that can be performed on a rqycaster while it is in progress.
/// Used just by the raytracer for now.
///
/// TODO: rename this trait once we figure out a good name.
/// (Not doing that for now because it's unclear how broadly useful the whole “axis-aligned
/// raycasting” subsystem is going to be.)
pub(crate) trait RaycasterIsh: Iterator<Item = RaycastStep> {
    type Ray: RayIsh<Caster = Self>;

    fn recursive_raycast(
        within_step: RaycastStep,
        ray: Self::Ray,
        resolution: Resolution,
        bounds: GridAab,
    ) -> (Self, Self::Ray)
    where
        Self: Sized;
}

impl RaycasterIsh for Raycaster {
    type Ray = Ray;

    #[inline]
    fn recursive_raycast(
        step: RaycastStep,
        ray: Ray,
        resolution: Resolution,
        bounds: GridAab,
    ) -> (Self, Ray) {
        step.recursive_raycast(ray, resolution, bounds)
    }
}

impl RaycasterIsh for AxisAlignedRaycaster {
    type Ray = AaRay;

    #[inline]
    fn recursive_raycast(
        step: RaycastStep,
        ray: AaRay,
        resolution: Resolution,
        bounds: GridAab,
    ) -> (Self, AaRay) {
        let sub_ray = ray.zoom_in(step.cube_ahead(), resolution);
        (sub_ray.cast().within(bounds, true), sub_ray)
    }
}
