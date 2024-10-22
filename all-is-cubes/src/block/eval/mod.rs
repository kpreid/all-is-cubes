//! Items relating to the block evaluation process,
//! which produces an [`EvaluatedBlock`] from a [`Block`].
//!
//! This module contains types and algorithms that are applicable to all block evaluation;
//! it does not contain the elements that are evaluated
//! ([`crate::block::Primitive`] and [`crate::block::Modifier`])

mod control;
#[cfg(test)]
pub(crate) use control::ErrorKind;
pub(in crate::block) use control::{finish_evaluation, InEvalError};
pub(crate) use control::{Budget, EvalFilter};
pub use control::{Cost, EvalBlockError};

mod derived;
use derived::compute_derived;
#[cfg(test)]
pub(super) use derived::Derived;
pub use derived::VoxelOpacityMask;

mod evaluated;
pub(crate) use evaluated::AIR_EVALUATED_REF;
pub use evaluated::{EvKey, EvaluatedBlock, AIR_EVALUATED};
pub(in crate::block) use evaluated::{MinEval, AIR_EVALUATED_MIN};

mod voxel_storage;
pub use voxel_storage::{Evoxel, Evoxels};

#[cfg(test)]
mod tests;
