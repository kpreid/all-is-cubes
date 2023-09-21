use alloc::sync::Arc;

use all_is_cubes::block::Block;
use all_is_cubes::math::{Face6, FaceMap, GridVector};

use crate::vui;

// Reexported for use with VUI because it isn't currently publicly exported otherwise.
// TODO: unclear where this type should be canonically exported.
pub use all_is_cubes::content::BoxStyle;

/// Widget that fills its volume with some [`BoxStyle`], and requests at least 1 cube of
/// depth.
///
/// Useful for “dialog box” backgrounds and for identifying the bounds of a layout region.
#[derive(Debug)]
pub struct Frame {
    style: BoxStyle,
}

impl Frame {
    pub fn new(style: BoxStyle) -> Arc<Self> {
        Arc::new(Self { style })
    }

    /// experimental
    #[doc(hidden)]
    pub fn with_block(block: Block) -> Arc<Self> {
        Self::new(BoxStyle::from_geometric_categories(
            Some(block.clone()),
            Some(block.clone()),
            Some(block.clone()),
            Some(block),
        ))
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
            children: vec![vui::LayoutTree::leaf(self as Arc<dyn vui::Widget>), tree],
        })
    }
}

// Frame can be any size with at least 1 depth.
impl vui::Layoutable for Frame {
    fn requirements(&self) -> vui::LayoutRequest {
        // TODO: account for size of the chosen VoxelBrushes (currently not possible to change)
        vui::LayoutRequest {
            minimum: GridVector::new(0, 0, 1),
        }
    }
}

impl vui::Widget for Frame {
    fn controller(self: Arc<Self>, position: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        super::OneshotController::new(self.style.create_box(position.bounds))
    }
}
