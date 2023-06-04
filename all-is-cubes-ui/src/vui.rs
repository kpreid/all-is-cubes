//! Voxel User Interface framework.
//!
//! To make a UI, create a [`WidgetTree`], then [`install_widgets`] into a [`Space`].
//!
//! [`Space`]: all_is_cubes::space::Space

#[doc(hidden)] // public for use by test-renderers only
pub mod blocks;
pub(crate) use blocks::UiBlocks;
mod layout;
pub use layout::*;
mod page;
pub(crate) use page::*;
mod widget_trait;
pub use widget_trait::*;
pub mod widgets;
