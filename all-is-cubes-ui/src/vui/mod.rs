//! Voxel User Interface framework.
//!
//! To make a UI, create a [`WidgetTree`], then [`install_widgets`] into a [`Space`].
//!
//! [`Space`]: all_is_cubes::space::Space

use all_is_cubes::block::EvalBlockError;
use all_is_cubes::{block, universe};

#[doc(hidden)] // public for use by test-renderers only
pub mod blocks;
#[cfg(feature = "session")]
pub(crate) use blocks::UiBlocks;
mod layout;
pub use layout::*;
#[cfg(feature = "session")]
mod page;
#[cfg(feature = "session")]
pub(crate) use page::*;
mod widget_trait;
pub use widget_trait::*;
pub mod widgets;

/// Copy the evaluation of `source` into a new block which has no effects and no [`Handle`]s,
/// and is thus safe to put in a different universe.
///
/// TODO: This needs to be able to hook up a listener to know if and when the block changes,
/// but that is not available yet.
///
/// TODO: This needs a clear module location.
#[cfg_attr(not(feature = "session"), expect(dead_code))]
pub(crate) fn quote_and_snapshot_block(
    read_tickets: [universe::ReadTicket<'_>; 2],
    source: &block::Block,
) -> block::Block {
    let quoted_block = source.clone().with_modifier(block::Quote::default());
    let mut eval_result = quoted_block.evaluate(read_tickets[0]);
    if eval_result
        .as_ref()
        .is_err_and(EvalBlockError::is_invalid_ticket)
    {
        eval_result = quoted_block.evaluate(read_tickets[1]);
    }
    let evaluated = eval_result.unwrap_or_else(|e| e.to_placeholder());
    let snapshotted = block::Block::from(block::Primitive::Raw {
        attributes: evaluated.attributes().clone(),
        voxels: evaluated.voxels().clone(),
    });

    #[cfg(debug_assertions)]
    {
        universe::VisitHandles::visit_handles(
            &snapshotted,
            &mut |handle: &dyn universe::ErasedHandle| {
                panic!("handle {handle:?} not stripped from tool icon {snapshotted:#?}");
            },
        );
    }

    snapshotted
}
