use std::sync::Arc;

use all_is_cubes::block::Block;
use all_is_cubes::content::palette;
use all_is_cubes::math::GridVector;

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

    pub fn for_menu() -> Arc<Self> {
        let background = Block::from(palette::MENU_BACK);
        let frame = Block::from(palette::MENU_FRAME);
        Self::new(BoxStyle::from_geometric_categories(
            None,
            Some(background),
            Some(frame.clone()),
            Some(frame),
        ))
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
