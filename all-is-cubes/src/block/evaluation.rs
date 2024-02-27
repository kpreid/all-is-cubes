//! Components of the process of block evaluation
//! (excluding the original input and final output)

use core::cell::Cell;

#[cfg(doc)]
use crate::block::Block;
use crate::block::{
    self, BlockAttributes, BlockChange, EvaluatedBlock, Evoxel, Evoxels, Resolution,
};
use crate::content::palette;
use crate::listen;
use crate::math::{GridAab, Rgba, Vol};
#[cfg(doc)]
use crate::universe::Handle;
use crate::universe::HandleError;

/// Parameters to [`Block::evaluate2()`] to choose which information to compute.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Debug)]
pub(crate) struct EvalFilter {
    /// If true, don't actually evaluate, but return a placeholder value and do listen.
    ///
    /// TODO: All of the use cases where this is useful should actually be replaced with
    /// combined eval+listen, but we will also want to have a "evaluate only this region"
    /// mode which will be somewhat analogous.
    pub skip_eval: bool,

    /// A [`Listener`] which will be notified of changes in all data sources that might
    /// affect the evaluation result.
    ///
    /// Note that this does not listen for mutations of the [`Block`] value itself, in the
    /// sense that none of the methods on [`Block`] will cause this listener to fire.
    /// Rather, it listens for changes in by-reference-to-interior-mutable-data sources
    /// such as the [`Space`] referred to by a [`Primitive::Recur`] or the [`BlockDef`]
    /// referred to by a [`Primitive::Indirect`].
    pub listener: Option<listen::DynListener<BlockChange>>,

    /// How much computation may be spent on performing the evaluation.
    ///
    /// If the budget is exhausted, evaluation returns [`EvalBlockError::StackOverflow`].
    ///
    /// Outside of special circumstances, use [`Budget::default()`] here.
    pub budget: Cell<Budget>,
}

impl Default for EvalFilter {
    /// Returns a default `EvalFilter` which requests a complete result and installs no
    /// listener.
    fn default() -> Self {
        Self {
            skip_eval: Default::default(),
            listener: Default::default(),
            budget: Default::default(),
        }
    }
}

/// Computation budget for block evaluations.
///
/// This is used inside an [`EvalFilter`].
///
/// In principle, what we want is a time budget, but in order to offer determinism and
/// comprehensibility, it is instead made up of multiple discrete quantities.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct Budget {
    /// Number of [`Primitive`]s and [`Modifier`]s.
    pub(super) components: u32,

    /// Number of individual voxels produced (e.g. by a [`Primitive::Recur`]) or altered
    /// (e.g. by a [`Modifier::Composite`]).
    voxels: u32,

    /// Number of levels of evaluation recursion permitted.
    ///
    /// Recursion occurs when a primitive or modifier which itself contains a [`Block`] is
    /// evaluated; currently, these are [`Primitive::Text`] and [`Modifier::Composite`].
    ///
    /// This must be set low enough to avoid Rust stack overflows which cannot be recovered from.
    /// Unlike the other budget parameters, this is not cumulative over the entire evaluation.
    recursion: u8,

    /// Number of recursion levels actually used by the evaluation.
    /// This is tracked separately so that it can be reported afterward,
    /// whereas the other counters are only ever decremented and so a subtraction suffices.
    recursion_used: u8,
}

impl Budget {
    pub(in crate::block) fn decrement_components(cell: &Cell<Budget>) -> Result<(), InEvalError> {
        let mut budget = cell.get();
        match budget.components.checked_sub(1) {
            Some(updated) => budget.components = updated,
            None => return Err(InEvalError::BudgetExceeded),
        }
        cell.set(budget);
        Ok(())
    }

    pub(in crate::block) fn decrement_voxels(
        cell: &Cell<Budget>,
        amount: usize,
    ) -> Result<(), InEvalError> {
        let mut budget = cell.get();
        match u32::try_from(amount)
            .ok()
            .and_then(|amount| budget.voxels.checked_sub(amount))
        {
            Some(updated) => budget.voxels = updated,
            None => return Err(InEvalError::BudgetExceeded),
        }
        cell.set(budget);
        Ok(())
    }

    pub(in crate::block) fn recurse(
        cell: &Cell<Budget>,
    ) -> Result<BudgetRecurseGuard<'_>, InEvalError> {
        let current = cell.get();
        let mut recursed = current;
        match recursed.recursion.checked_sub(1) {
            Some(updated) => recursed.recursion = updated,
            None => return Err(InEvalError::BudgetExceeded),
        }
        cell.set(recursed);
        Ok(BudgetRecurseGuard { cell })
    }

    /// Express a budget as a [`Cost`] value, for public consumption.
    pub(in crate::block) fn to_cost(self) -> Cost {
        Cost {
            components: self.components,
            voxels: self.voxels,
            recursion: self.recursion_used,
        }
    }
}

impl Default for Budget {
    /// Returns the standard budget for starting any evaluation.
    fn default() -> Self {
        Self {
            components: 1000,
            voxels: 64 * 64 * 128,
            recursion: 30,
            recursion_used: 0,
        }
    }
}

#[must_use]
pub(crate) struct BudgetRecurseGuard<'a> {
    cell: &'a Cell<Budget>,
}

impl Drop for BudgetRecurseGuard<'_> {
    fn drop(&mut self) {
        let mut budget = self.cell.get();
        budget.recursion = budget.recursion.checked_add(1).unwrap();
        self.cell.set(budget);
    }
}

/// The cost of evaluating a [`Block`].
///
/// In principle, what we want is a time budget, but in order to offer determinism and
/// comprehensibility, it is instead measured in discrete quantities
/// such as the number of voxels processed.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Cost {
    /// Number of [`Primitive`]s and [`Modifier`]s evaluated.
    pub(crate) components: u32,

    /// Number of individual voxels produced (e.g. by a `Primitive::Recur`]) or altered
    /// (e.g. by a [`Modifier::Composite`]).
    pub(crate) voxels: u32,

    /// Number of recursion levels used by the evaluation.
    ///
    /// Recursion occurs when a primitive or modifier which itself contains  [`Block`] is
    /// evaluated; currently, these are [`Primitive::Text`] and `Modifier::Composite`].
    /// If there are none of those, then this will be zero.
    pub(crate) recursion: u8,
}

impl Cost {
    /// Zero cost.
    pub const ZERO: Self = {
        Self {
            components: 0,
            voxels: 0,
            recursion: 0,
        }
    };

    /// Compute a cost from change in budget.
    pub(crate) fn from_difference(original_budget: Budget, final_budget: Budget) -> Self {
        let Some(new_self) = (|| {
            Some(Self {
                components: original_budget
                    .components
                    .checked_sub(final_budget.components)?,
                voxels: original_budget.voxels.checked_sub(final_budget.voxels)?,
                recursion: original_budget
                    .recursion_used
                    .checked_sub(final_budget.recursion_used)?,
            })
        })() else {
            panic!("overflow computing budget difference: {final_budget:#?} - {original_budget:#?}")
        };
        new_self
    }
}

/// Errors resulting from [`Block::evaluate()`].
#[derive(Clone, Debug, Eq, Hash, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum EvalBlockError {
    /// The evaluation budget was exceeded.
    #[displaydoc("block definition exceeded evaluation budget; used {used:?} so far and only {budget:?} available")]
    BudgetExceeded {
        /// Budget that was available for the evaluation.
        budget: Cost,
        /// Computation steps actually used before failure.
        used: Cost,
    },

    /// The evaluation budget was exceeded, in a previous cached evaluation.
    /// rather than the current one (so the current evaluation's budget
    /// could not have affected the outcome).
    #[displaydoc("cached block definition exceeded evaluation budget; used {used:?} so far and only {budget:?} available")]
    PriorBudgetExceeded {
        /// Budget that was available for the evaluation.
        budget: Cost,
        /// Computation steps actually used before failure.
        used: Cost,
    },

    /// The block definition contained a [`Handle`] which was not currently available to read.
    ///
    /// This may be temporary or permanent; consult the [`HandleError`] to determine that.
    #[displaydoc("block data inaccessible: {0}")]
    Handle(HandleError),
}

/// Intra-evaluation error type; corresponds to [`EvalBlockError`]
/// as `MinEval` corresponds to `EvaluatedBlock`.
#[derive(Debug)]
pub(in crate::block) enum InEvalError {
    BudgetExceeded,
    PriorBudgetExceeded { budget: Cost, used: Cost },
    Handle(HandleError),
}

#[cfg(feature = "std")]
impl std::error::Error for EvalBlockError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            EvalBlockError::BudgetExceeded { .. } => None,
            EvalBlockError::PriorBudgetExceeded { .. } => None,
            EvalBlockError::Handle(e) => Some(e),
        }
    }
}

impl From<HandleError> for InEvalError {
    fn from(value: HandleError) -> Self {
        InEvalError::Handle(value)
    }
}

impl InEvalError {
    pub(in crate::block) fn into_eval_error(self, budget: Cost, used: Cost) -> EvalBlockError {
        match self {
            InEvalError::BudgetExceeded => EvalBlockError::BudgetExceeded { budget, used },
            InEvalError::PriorBudgetExceeded { budget, used } => {
                EvalBlockError::PriorBudgetExceeded { budget, used }
            }
            InEvalError::Handle(e) => EvalBlockError::Handle(e),
        }
    }
}

impl EvalBlockError {
    pub(in crate::block) fn into_internal_error_for_block_def(self) -> InEvalError {
        match self {
            EvalBlockError::PriorBudgetExceeded { budget, used } => {
                InEvalError::PriorBudgetExceeded { budget, used }
            }
            EvalBlockError::BudgetExceeded { .. } => InEvalError::BudgetExceeded,
            EvalBlockError::Handle(e) => InEvalError::Handle(e),
        }
    }

    /// Returns whether this error is presumably transient because of simultaneous mutation
    /// of the underlying data.
    ///
    /// This is a simple match, but we declare it as a method to ensure that any future introduced
    /// variants of [`EvalBlockError`] or [`HandleError`], that are similar but not equal,
    /// don't break the logic depending on this property.
    pub(crate) fn is_in_use(&self) -> bool {
        matches!(self, EvalBlockError::Handle(HandleError::InUse(_)))
    }

    /// Convert this error into an [`EvaluatedBlock`] which represents that an error has
    /// occurred.
    ///
    /// This block is fully opaque and as inert to game mechanics as currently possible.
    // TODO: test this
    pub fn to_placeholder(&self) -> EvaluatedBlock {
        let resolution = Resolution::R8;
        // TODO: indicate type of error or at least have some kind of icon,
        let pattern = [palette::BLOCK_EVAL_ERROR, Rgba::BLACK].map(Evoxel::from_color);

        EvaluatedBlock::from_voxels(
            BlockAttributes {
                display_name: format!("Block error: {self}").into(),
                selectable: false, // TODO: make this selectable but immutable
                ..Default::default()
            },
            Evoxels::Many(
                resolution,
                Vol::from_fn(GridAab::for_block(resolution), |cube| {
                    pattern[((cube.x + cube.y + cube.z).rem_euclid(2)) as usize]
                }),
            ),
            match *self {
                EvalBlockError::BudgetExceeded { used, .. } => used,
                EvalBlockError::PriorBudgetExceeded { .. } => Cost::ZERO,
                EvalBlockError::Handle(_) => Cost::ZERO,
            },
        )
    }
}

/// Convert intermediate evaluation result to final evaluation result,
/// including calculating the evaluation cost.
///
/// Note that the order of operands is such that the original budget may
/// be passed in the form `filter.budget.get()` without a temporary variable.
pub(in crate::block) fn finish_evaluation(
    original_budget: Budget,
    result: Result<block::MinEval, InEvalError>,
    filter: &EvalFilter,
) -> Result<EvaluatedBlock, EvalBlockError> {
    let cost = Cost::from_difference(original_budget, filter.budget.get());
    match result {
        Ok(ev) => Ok(ev.finish(cost)),
        Err(err) => Err(err.into_eval_error(original_budget.to_cost(), cost)),
    }
}
