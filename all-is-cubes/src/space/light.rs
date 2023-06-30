mod data;
pub use data::PackedLight;
pub(crate) use data::{LightStatus, LightUpdateQueue, LightUpdateRequest, PackedLightScalar};

mod debug;
#[doc(hidden)] // pub only for visualization by all-is-cubes-gpu
pub use debug::{LightUpdateCubeInfo, LightUpdateRayInfo};

mod updater;
pub(crate) use updater::opaque_for_light_computation;
pub use updater::LightUpdatesInfo;

#[cfg(test)]
mod tests;
