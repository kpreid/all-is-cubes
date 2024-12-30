use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec;

use all_is_cubes::block::Block;
use all_is_cubes::euclid::size3;
use all_is_cubes::math::{Face6, FaceMap};

use crate::vui;
use crate::vui::widgets::BoxStyle;

/// Widget that fills its volume with some [`BoxStyle`], and requests at least 1 cube of
/// depth.
///
/// Useful for “dialog box” backgrounds and for identifying the bounds of a layout region.
#[derive(Debug)]
pub struct Frame {
    style: BoxStyle,
}

impl Frame {
    /// Creates a `Frame` with the specified style.
    pub fn new(style: BoxStyle) -> Arc<Self> {
        Arc::new(Self { style })
    }

    /// experimental
    #[doc(hidden)]
    #[allow(
        clippy::needless_pass_by_value,
        reason = "consistency and we're not too worried about a couple extra refcounts"
    )]
    pub fn with_block(block: Block) -> Arc<Self> {
        Self::new(BoxStyle::from_fn(|_| Some(block.clone())))
    }

    /// Put this frame behind the given widget tree.
    ///
    /// TODO: Allow fully enclosing the widgets (this will require new layout capabilities)
    pub fn as_background_of(self: Arc<Self>, tree: vui::WidgetTree) -> vui::WidgetTree {
        // Add margins around all 4 sides.
        let tree = Arc::new(vui::LayoutTree::Margin {
            margin: FaceMap {
                nx: 1,
                ny: 1,
                nz: 0,
                px: 1,
                py: 1,
                pz: 0,
            },
            child: tree,
        });

        Arc::new(vui::LayoutTree::Stack {
            direction: Face6::PZ,
            children: vec![vui::leaf_widget(self), tree],
        })
    }
}

// Frame can be any size with at least 1 depth.
impl vui::Layoutable for Frame {
    fn requirements(&self) -> vui::LayoutRequest {
        // TODO: account for size of the chosen VoxelBrushes (currently not possible to change)
        vui::LayoutRequest {
            minimum: size3(0, 0, 1),
        }
    }
}

impl vui::Widget for Frame {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        super::OneshotController::new(self.style.create_box(position.bounds))
    }
}
