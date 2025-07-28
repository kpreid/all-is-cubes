//! Details of block evaluation control flow, such as [`EvalFilter`], [`Budget`],
//! and errors.

use core::cell::Cell;
use core::fmt;

use crate::block::{
    self, Block, BlockAttributes, BlockChange, EvaluatedBlock, Evoxel, Evoxels, Resolution,
};
use crate::content::palette;
use crate::listen;
use crate::math::{GridAab, Rgba, Vol};
use crate::universe::{HandleError, ReadTicket};

#[cfg(doc)]
use crate::{block::BlockDef, space::Space, universe::Handle};

/// Parameters to [`Block::evaluate2()`] to choose which information to compute.
#[derive(Clone, Debug)]
pub(crate) struct EvalFilter<'a> {
    /// Read access to the [`BlockDef`]s and [`Space`]s that may be referenced to by the
    /// block.
    pub read_ticket: ReadTicket<'a>,

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

impl<'a> EvalFilter<'a> {
    /// Returns a basic `EvalFilter` which requests a complete result and installs no
    /// listener.
    pub fn new(read_ticket: ReadTicket<'a>) -> Self {
        Self {
            read_ticket,
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
    pub(crate) components: u32,

    /// Number of individual voxels produced (e.g. by a [`Primitive::Recur`]) or altered
    /// (e.g. by a [`Modifier::Composite`]).
    pub(crate) voxels: u32,

    /// Number of levels of evaluation recursion permitted.
    ///
    /// Recursion occurs when a primitive or modifier which itself contains a [`Block`] is
    /// evaluated; currently, these are [`Primitive::Text`] and [`Modifier::Composite`].
    ///
    /// This must be set low enough to avoid Rust stack overflows which cannot be recovered from.
    /// Unlike the other budget parameters, this is not cumulative over the entire evaluation.
    pub(crate) recursion: u8,

    /// Lowest value that `self.recursion` has ever had.
    ///
    /// This is tracked separately so that it can be reported afterward,
    /// whereas the other counters are only ever decremented and so a subtraction suffices.
    pub(crate) min_recursion: u8,
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
        match u32::try_from(amount).ok().and_then(|amount| budget.voxels.checked_sub(amount)) {
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
            Some(updated) => {
                recursed.recursion = updated;
                recursed.min_recursion = recursed.min_recursion.min(updated);
            }
            None => return Err(InEvalError::BudgetExceeded),
        }
        cell.set(recursed);
        Ok(BudgetRecurseGuard { cell })
    }

    /// Express a budget as a [`Cost`] value, for public consumption.
    ///
    /// The [`Budget`] type is not public, so we only use [`Cost`] even when describing budgets.
    ///
    /// This method is only suitable for describing initial unused budgets.
    /// For budgets that have been consumed, use [`Cost::from_difference()`].
    pub(crate) fn to_cost(self) -> Cost {
        assert_eq!(
            self.recursion, self.min_recursion,
            "do not use `to_cost()` on used budgets"
        );
        Cost {
            components: self.components,
            voxels: self.voxels,
            recursion: self.recursion,
        }
    }
}

impl const Default for Budget {
    /// Returns the standard budget for starting any evaluation.
    fn default() -> Self {
        let recursion = 30;
        Self {
            components: 1000,
            voxels: 64 * 64 * 128,
            recursion,
            min_recursion: recursion,
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
        budget.recursion = budget.recursion.strict_add(1);
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
        let Some(new_self) = (try {
            Self {
                components: original_budget.components.checked_sub(final_budget.components)?,
                voxels: original_budget.voxels.checked_sub(final_budget.voxels)?,
                // note use of .min_recursion!
                recursion: original_budget.recursion.checked_sub(final_budget.min_recursion)?,
            }
        }) else {
            panic!("overflow computing budget difference: {final_budget:#?} - {original_budget:#?}")
        };
        new_self
    }
}

/// Errors resulting from [`Block::evaluate()`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct EvalBlockError {
    /// The block whose evaluation failed.
    pub(crate) block: Block,

    /// Computation budget that was available for the evaluation.
    //---
    // Design note: This field is not of type `Budget` because `Budget` is private, and
    // structured to support its use *during* evaluation.
    pub(crate) budget: Cost,

    /// Computation steps actually used before the error was encountered.
    pub(crate) used: Cost,

    /// What specific failure was encountered.
    pub(crate) kind: ErrorKind,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
// TODO: should this be public? It may have been private by accident.
pub(crate) enum ErrorKind {
    /// The evaluation budget was exceeded.
    BudgetExceeded,

    /// The evaluation budget was exceeded, in a previous cached evaluation.
    /// rather than the current one (so the current evaluation's budget
    /// could not have affected the outcome).
    PriorBudgetExceeded {
        /// Budget that was available for the prior evaluation.
        budget: Cost,
        /// Computation steps actually used before failure of the prior evaluation.
        used: Cost,
    },

    /// The block definition contained a [`Handle`] which was not currently available to read.
    ///
    /// This may be temporary or permanent; consult the [`HandleError`] to determine that.
    Handle(HandleError),
}

/// Intra-evaluation error type; corresponds to [`EvalBlockError`]
/// as `MinEval` corresponds to `EvaluatedBlock`.
///
/// TODO: This seems no longer needed since it has ended up identical.
#[derive(Debug)]
pub(in crate::block) enum InEvalError {
    BudgetExceeded,
    PriorBudgetExceeded { budget: Cost, used: Cost },
    Handle(HandleError),
}

impl fmt::Display for EvalBlockError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ErrorKind::BudgetExceeded => {
                let Self { budget, used, .. } = self;
                write!(
                    f,
                    "block definition exceeded evaluation budget; \
                    used {used:?} so far and only {budget:?} available"
                )
            }
            ErrorKind::PriorBudgetExceeded { budget, used } => write!(
                f,
                "cached block definition exceeded evaluation budget; \
                used {used:?} so far and only {budget:?} available"
            ),
            ErrorKind::Handle(_) => write!(f, "block data inaccessible"),
        }
    }
}

impl core::error::Error for EvalBlockError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match &self.kind {
            ErrorKind::BudgetExceeded => None,
            ErrorKind::PriorBudgetExceeded { .. } => None,
            ErrorKind::Handle(e) => Some(e),
        }
    }
}

impl const From<HandleError> for InEvalError {
    fn from(value: HandleError) -> Self {
        InEvalError::Handle(value)
    }
}

impl InEvalError {
    pub(in crate::block) fn into_eval_error(
        self,
        block: Block,
        budget: Cost,
        used: Cost,
    ) -> EvalBlockError {
        EvalBlockError {
            block,
            budget,
            used,
            kind: match self {
                InEvalError::BudgetExceeded => ErrorKind::BudgetExceeded,
                #[expect(clippy::shadow_unrelated)]
                InEvalError::PriorBudgetExceeded { budget, used } => {
                    ErrorKind::PriorBudgetExceeded { budget, used }
                }
                InEvalError::Handle(e) => ErrorKind::Handle(e),
            },
        }
    }
}

impl EvalBlockError {
    /// Returns the block whose evaluation failed.
    pub fn block(&self) -> &Block {
        &self.block
    }

    pub(in crate::block) fn into_internal_error_for_block_def(self) -> InEvalError {
        match self.kind {
            ErrorKind::PriorBudgetExceeded { budget, used } => {
                InEvalError::PriorBudgetExceeded { budget, used }
            }
            ErrorKind::BudgetExceeded => InEvalError::BudgetExceeded,
            ErrorKind::Handle(e) => InEvalError::Handle(e),
        }
    }

    #[doc(hidden)] // TODO(read_ticket): eventually stop needing this
    pub fn is_wrong_universe(&self) -> bool {
        matches!(
            self.kind,
            ErrorKind::Handle(ref e) if e.is_wrong_universe()
        )
    }

    /// Returns whether this error relates to the timing or [`ReadTicket`] used to evaluate the
    /// block, rather than the current state of the block’s definition or the evaluation budget.
    ///
    /// If `true`, then reevaluating later or with a more proper [`ReadTicket`] may succeed.
    pub(crate) fn is_transient(&self) -> bool {
        match self.kind {
            ErrorKind::Handle(ref h) => h.is_transient(),
            ErrorKind::BudgetExceeded => false,
            ErrorKind::PriorBudgetExceeded { .. } => false,
        }
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
            block::AIR, // TODO: wrong value. should get block through self
            BlockAttributes {
                display_name: format!("Block error: {self}").into(),
                selectable: false, // TODO: make this selectable but immutable
                ..Default::default()
            },
            Evoxels::from_many(
                resolution,
                Vol::from_fn(GridAab::for_block(resolution), |cube| {
                    pattern[((cube.x + cube.y + cube.z).rem_euclid(2)) as usize]
                }),
            ),
            self.used,
        )
    }
}

/// Convert intermediate evaluation result to final evaluation result,
/// including calculating the evaluation cost.
///
/// Note that the order of operands is such that the original budget may
/// be passed in the form `filter.budget.get()` without a temporary variable.
pub(in crate::block) fn finish_evaluation(
    block: Block,
    original_budget: Budget,
    result: Result<block::MinEval, InEvalError>,
    filter: &EvalFilter<'_>,
) -> Result<EvaluatedBlock, EvalBlockError> {
    let cost = Cost::from_difference(original_budget, filter.budget.get());
    match result {
        Ok(ev) => Ok(ev.finish(block, cost)),
        Err(err) => Err(err.into_eval_error(block, original_budget.to_cost(), cost)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Most cost-tracking tests are block evaluation tests, but recursion is subtler than
    /// the other cases and had a bug this is a regression test for.
    #[test]
    fn tracking_recursion_cost() {
        let cell = Cell::new(Budget::default());
        assert_eq!((cell.get().recursion, cell.get().min_recursion), (30, 30));

        {
            let _guard1: BudgetRecurseGuard<'_> = Budget::recurse(&cell).unwrap();
            assert_eq!((cell.get().recursion, cell.get().min_recursion), (29, 29));
            {
                let _guard2: BudgetRecurseGuard<'_> = Budget::recurse(&cell).unwrap();
                assert_eq!((cell.get().recursion, cell.get().min_recursion), (28, 28));
            }
        }
        assert_eq!((cell.get().recursion, cell.get().min_recursion), (30, 28));

        let cost = Cost::from_difference(Budget::default(), cell.get());
        assert_eq!(cost.recursion, 2);
    }
}
