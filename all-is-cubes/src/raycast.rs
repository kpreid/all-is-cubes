//! Algorithm for raycasting through voxel grids.
//!
//! This deals purely with the question “which cubes does this ray intersect”,
//! and does not concern itself with what might occupy those cubes. If you’re
//! looking for *raytracing*, forming an image from many rays, that’s
//! `all_is_cubes_render::raytracer`.

use crate::block::Resolution;
use crate::math::{FreeVector, GridAab};

// This module is primarily a re-export.
pub use all_is_cubes_base::raycast::*;

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

    fn cast(self) -> Self::Caster;

    fn direction(&self) -> FreeVector {
        <Self as Into<Ray>>::into(*self).direction
    }
}

impl RayIsh for Ray {
    type Caster = Raycaster;

    fn cast(self) -> Self::Caster {
        Ray::cast(&self)
    }
}

impl RayIsh for AaRay {
    type Caster = AxisAlignedRaycaster;

    fn cast(self) -> Self::Caster {
        self.cast()
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

    fn add_bounds(&mut self, bounds: GridAab, include_exit: bool);

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
    fn add_bounds(&mut self, bounds: GridAab, include_exit: bool) {
        Raycaster::add_bounds(self, bounds, include_exit);
    }

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
    fn add_bounds(&mut self, bounds: GridAab, include_exit: bool) {
        AxisAlignedRaycaster::add_bounds(self, bounds, include_exit);
    }

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
