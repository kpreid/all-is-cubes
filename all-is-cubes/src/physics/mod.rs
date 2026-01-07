//! Continuously moving objects and collision.

use crate::math::FreeCoordinate;

mod body;
pub use body::*;
mod collision;
pub use collision::*;

pub(crate) mod step;
pub(crate) use step::VELOCITY_MAGNITUDE_LIMIT_SQUARED;
#[doc(hidden)] // pub to be used by all-is-cubes-gpu and fuzz_physics
#[allow(clippy::module_name_repetitions)]
pub use step::{BodyStepDetails, PhysicsOutputs};

#[cfg(test)]
mod tests;

/// Close-but-not-intersecting objects are set to this separation.
pub(crate) const POSITION_EPSILON: FreeCoordinate = 1e-6 * 1e-6;

/// Unit-of-measure type for vectors that are velocity in cubes/s.
#[expect(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum Velocity {}

/// Unit-of-measure type for vectors that are acceleration in cubes/s².
#[expect(clippy::exhaustive_enums)]
#[derive(Debug, Eq, PartialEq)]
pub enum Acceleration {}
