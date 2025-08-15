//! Data structures and helper traits for getting detailed debug info
//! out of the lighting algorithm.
//!
//! Note that this entire module is `doc(hidden)`; pub items inside it
//! are for intra-project use only.

use alloc::vec::Vec;

use crate::math::{Cube, Rgb, lines, lines::Wireframe as _};
use crate::raycast::Ray;
use crate::space::PackedLight;

/// Trait used to encourage the generation of with-debug-info and without-info versions
/// of the lighting algorithm.
#[expect(unnameable_types, reason = "sealed trait")]
pub trait LightComputeOutput {
    type RayInfoBuffer: Default;
    fn new(cube: Cube, result: PackedLight, rays: Self::RayInfoBuffer) -> Self;
    fn push_ray(buffer: &mut Self::RayInfoBuffer, ray_info: impl FnOnce() -> LightUpdateRayInfo);
}

impl LightComputeOutput for () {
    type RayInfoBuffer = ();
    #[inline(always)]
    fn new(_: Cube, _: PackedLight, (): Self::RayInfoBuffer) {}
    #[inline(always)]
    fn push_ray((): &mut Self::RayInfoBuffer, _: impl FnOnce() -> LightUpdateRayInfo) {}
}

impl LightComputeOutput for LightUpdateCubeInfo {
    type RayInfoBuffer = Vec<LightUpdateRayInfo>;
    fn new(cube: Cube, result: PackedLight, rays: Self::RayInfoBuffer) -> Self {
        Self { cube, result, rays }
    }
    fn push_ray(buffer: &mut Self::RayInfoBuffer, ray_info: impl FnOnce() -> LightUpdateRayInfo) {
        buffer.push(ray_info())
    }
}

/// Diagnostic data describing the details of the light calculation for one cube.
#[derive(Clone, Debug)]
#[non_exhaustive]
#[expect(dead_code, reason = "fields used for Debug printing")]
pub struct LightUpdateCubeInfo {
    pub(crate) cube: Cube,
    pub(crate) result: PackedLight,
    pub(crate) rays: Vec<LightUpdateRayInfo>,
}

impl lines::Wireframe for LightUpdateCubeInfo {
    fn wireframe_points<E: Extend<[lines::Vertex; 2]>>(&self, output: &mut E) {
        // Draw output cube
        self.cube.aab().expand(0.1).wireframe_points(output);
        // Draw rays
        for ray_info in self.rays.iter() {
            ray_info.wireframe_points(self.cube, output);
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[expect(unnameable_types)] //
pub struct LightUpdateRayInfo {
    pub(crate) trigger_cube: Cube,

    pub(crate) value_cube: Cube,

    /// Value *stored* in the cube we're reading data from.
    #[expect(
        dead_code,
        reason = "field used for Debug printing but not visualized yet"
    )]
    pub(crate) value: PackedLight,

    /// Value *considered as outgoing from* the cube we're reading data from.
    /// This differs from `value` in that it includes surface color.
    pub(crate) light_from_struck_face: Rgb,
}

impl LightUpdateRayInfo {
    fn wireframe_points(&self, lit_cube: Cube, output: &mut impl Extend<[lines::Vertex; 2]>) {
        // TODO: Visualize trigger_cube and value.

        // We used to have precise rays, but we don't have them any more.
        // Approximate them using the involved cubes.
        let hit_point = self
            .trigger_cube
            .midpoint()
            .lerp(self.value_cube.midpoint(), 0.5);
        let ray = Ray::new(lit_cube.midpoint(), hit_point - lit_cube.midpoint());

        self.value_cube.aab().expand(0.01).wireframe_points(output);
        ray.wireframe_points(&mut lines::colorize(
            output,
            self.light_from_struck_face.with_alpha_one(),
        ))
    }
}
