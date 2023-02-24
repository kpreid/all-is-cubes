//! Data structures and helper traits for getting detailed debug info
//! out of the lighting algorithm.
//!
//! Note that this entire module is `doc(hidden)`; pub items inside it
//! are for intra-project use only.

use cgmath::Vector3;

use crate::math::{Aab, FreeCoordinate, Geometry, GridPoint};
use crate::mesh::LineVertex;
use crate::raycast::Ray;
use crate::space::PackedLight;
use crate::util::MapExtend;

/// Trait used to encourage the generation of with-debug-info and without-info versions
/// of the lighting algorithm.
pub trait LightComputeOutput {
    type RayInfoBuffer: Default;
    fn new(cube: GridPoint, result: PackedLight, rays: Self::RayInfoBuffer) -> Self;
    fn push_ray(buffer: &mut Self::RayInfoBuffer, ray_info: LightUpdateRayInfo);
}

impl LightComputeOutput for () {
    type RayInfoBuffer = ();
    #[inline(always)]
    fn new(_: GridPoint, _: PackedLight, _: Self::RayInfoBuffer) {}
    #[inline(always)]
    fn push_ray(_: &mut Self::RayInfoBuffer, _: LightUpdateRayInfo) {}
}

impl LightComputeOutput for LightUpdateCubeInfo {
    type RayInfoBuffer = Vec<LightUpdateRayInfo>;
    fn new(cube: GridPoint, result: PackedLight, rays: Self::RayInfoBuffer) -> Self {
        Self { cube, result, rays }
    }
    fn push_ray(buffer: &mut Self::RayInfoBuffer, ray_info: LightUpdateRayInfo) {
        buffer.push(ray_info)
    }
}

/// Diagnostic data describing the details of the light calculation for one cube.
#[derive(Clone, Debug)]
#[non_exhaustive]
#[allow(dead_code)] // fields used for Debug printing
pub struct LightUpdateCubeInfo {
    pub(crate) cube: GridPoint,
    pub(crate) result: PackedLight,
    pub(crate) rays: Vec<LightUpdateRayInfo>,
}

impl Geometry for LightUpdateCubeInfo {
    type Coord = FreeCoordinate;

    fn translate(self, _offset: Vector3<FreeCoordinate>) -> Self {
        unimplemented!();
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>,
    {
        // Draw output cube
        Aab::from_cube(self.cube)
            .expand(0.1)
            .wireframe_points(output);
        // Draw rays
        for ray_info in self.rays.iter() {
            ray_info.wireframe_points(output);
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct LightUpdateRayInfo {
    pub(crate) ray: Ray,
    #[allow(dead_code)] // field used for Debug printing but not visualized yet
    pub(crate) trigger_cube: GridPoint,
    pub(crate) value_cube: GridPoint,
    pub(crate) value: PackedLight,
}

impl Geometry for LightUpdateRayInfo {
    type Coord = FreeCoordinate;

    fn translate(self, _offset: Vector3<FreeCoordinate>) -> Self {
        unimplemented!();
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>,
    {
        Aab::from_cube(self.value_cube)
            .expand(0.01)
            .wireframe_points(output);
        self.ray
            .wireframe_points(&mut MapExtend::new(output, |mut v: LineVertex| {
                v.color = Some(self.value.value().with_alpha_one());
                v
            }))
    }
}
