//! Items relating to the block evaluation process,
//! which produces an [`EvaluatedBlock`] from a [`Block`].
//!
//! This module contains types and algorithms that are applicable to all block evaluation;
//! it does not contain the elements that are evaluated
//! ([`crate::block::Primitive`] and [`crate::block::Modifier`])

mod control;
#[cfg(test)]
pub(crate) use control::ErrorKind;
pub(crate) use control::{Budget, EvalFilter};
pub use control::{Cost, EvalBlockError};
pub(in crate::block) use control::{InEvalError, finish_evaluation};

mod derived;
#[cfg(test)]
pub(super) use derived::Derived;
pub use derived::VoxelOpacityMask;
use derived::compute_derived;

mod evaluated;
pub(crate) use evaluated::AIR_EVALUATED_REF;
pub use evaluated::{AIR_EVALUATED, EvKey, EvaluatedBlock};
pub(in crate::block) use evaluated::{AIR_EVALUATED_MIN, MinEval};

mod voxel_storage;
pub use voxel_storage::{Evoxel, Evoxels};

#[cfg(test)]
mod tests;
