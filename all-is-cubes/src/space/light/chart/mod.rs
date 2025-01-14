//! Types and data pertaining to the pattern of rays that are cast from a block to potential
//! light sources. Used by the algorithms in [`crate::space::light::updater`].

use alloc::vec::Vec;

use manyfmt::Refmt as _;

use crate::time::Instant;

// -------------------------------------------------------------------------------------------------

mod shared;
#[cfg(feature = "rerun")]
pub use shared::*;
#[cfg(not(feature = "rerun"))]
pub(in crate::space) use shared::*;

#[cfg(feature = "rerun")]
#[doc(hidden)] // exposed only for
pub mod generator;
#[cfg(not(feature = "rerun"))]
mod generator;

// -------------------------------------------------------------------------------------------------

/// Returns the light propagation chart, computing it if necessary.
///
/// This is data about how light propagates through the cube grid,
/// used to traverse a `Space` to determine what light falls on a single block.
pub(crate) fn get() -> &'static [FlatNode] {
    // TODO: Make it possible to initialize the chart asynchronously, so that there is never a
    // noticeable hang (just a lack of light updates, which are already throttled by available
    // time).

    cfg_if::cfg_if! {
        if #[cfg(feature = "std")] {
            static FLAT_TREE: std::sync::OnceLock<Vec<FlatNode>> =
                std::sync::OnceLock::new();
            FLAT_TREE.get_or_init(generate_chart_with_logging)
        } else {
            static FLAT_TREE: once_cell::race::OnceBox<Vec<FlatNode>> =
                once_cell::race::OnceBox::new();
            FLAT_TREE.get_or_init(|| alloc::boxed::Box::new(generate_chart_with_logging()))
        }
    }
}

fn generate_chart_with_logging() -> Vec<FlatNode> {
    let t0 = Instant::now();
    let output = generator::generate_flat_tree_chart();
    log::trace!(
        "Lazy initialized light propagation chart ({size_mib} MiB) in {time}",
        time = t0.elapsed().refmt(&crate::util::ConciseDebug),
        size_mib = output.len() * size_of::<FlatNode>() / (1024 * 1024),
    );
    output
}
