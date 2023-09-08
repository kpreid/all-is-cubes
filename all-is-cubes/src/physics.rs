//! Continuously moving objects and collision.

use crate::math::FreeCoordinate;

mod body;
pub use body::*;
mod collision;
pub use collision::*;

#[cfg(test)]
mod tests;

/// Close-but-not-intersecting objects are set to this separation.
pub(crate) const POSITION_EPSILON: FreeCoordinate = 1e-6 * 1e-6;
