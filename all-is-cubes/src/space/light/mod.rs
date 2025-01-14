mod data;
pub use data::PackedLight;
pub(crate) use data::{LightStatus, PackedLightScalar};

mod debug;
pub(crate) use debug::LightComputeOutput;
#[doc(hidden)] // pub only for visualization by all-is-cubes-gpu
pub use debug::LightUpdateCubeInfo;

mod queue;
pub(crate) use queue::{LightUpdateQueue, LightUpdateRequest, Priority};

#[cfg(feature = "rerun")]
pub mod chart;
#[cfg(not(feature = "rerun"))]
mod chart;

mod updater;
pub use updater::LightUpdatesInfo;
pub(in crate::space) use updater::{ComputedLight, LightStorage, UpdateCtx};

#[cfg(test)]
mod tests;
