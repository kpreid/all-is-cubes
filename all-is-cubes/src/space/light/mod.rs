#![cfg_attr(
    feature = "_special_testing",
    expect(
        missing_debug_implementations,
        clippy::len_without_is_empty,
        clippy::module_name_repetitions,
        clippy::new_without_default,
        reason = "module is conditionally public for testing only"
    )
)]

mod data;
pub use data::PackedLight;
pub(crate) use data::{LightStatus, PackedLightScalar};

mod debug;
pub(crate) use debug::LightComputeOutput;
#[doc(hidden)] // pub only for visualization by all-is-cubes-gpu
pub use debug::LightUpdateCubeInfo;

mod queue;
#[cfg_attr(feature = "_special_testing", visibility::make(pub))]
#[doc(hidden)]
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
