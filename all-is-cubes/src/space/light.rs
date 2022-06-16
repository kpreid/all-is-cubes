// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

mod data;
pub use data::PackedLight;
pub(crate) use data::{LightUpdateQueue, LightUpdateRequest, PackedLightScalar};

mod debug;
#[doc(hidden)] // pub only for visualization by all-is-cubes-gpu
pub use debug::{LightUpdateCubeInfo, LightUpdateRayInfo};

mod updater;
pub(crate) use updater::opaque_for_light_computation;
pub use updater::LightUpdatesInfo;

#[cfg(test)]
mod tests;
