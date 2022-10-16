use std::sync::Arc;

use embedded_graphics::prelude::Point;
use embedded_graphics::primitives::{Primitive as _, PrimitiveStyleBuilder, Rectangle};
use embedded_graphics::Drawable as _;

use crate::block::Block;
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::math::{GridMatrix, GridVector};
use crate::space::SpaceTransaction;
use crate::vui;

/// Widget that fills a flat XY surface with a "dialog box" sort of background.
/// Also useful for identifying the bounds of a layout region.
///
/// TODO: Define what it does in 3D.
#[derive(Debug)]
pub struct Frame {
    pub(crate) background: VoxelBrush<'static>,
    pub(crate) frame: VoxelBrush<'static>,
}

impl Frame {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            background: VoxelBrush::single(Block::from(palette::MENU_BACK)),
            frame: VoxelBrush::single(Block::from(palette::MENU_FRAME)),
        })
    }

    /// experimental
    #[doc(hidden)]
    pub fn with_block(block: Block) -> Arc<Self> {
        Arc::new(Self {
            background: VoxelBrush::single(block.clone()),
            frame: VoxelBrush::single(block),
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
        let bounds = position.bounds;
        let mut txn = SpaceTransaction::default();

        if !bounds.is_empty() {
            let background_rect = Rectangle::with_corners(
                Point::new(bounds.lower_bounds().x, bounds.lower_bounds().y),
                Point::new(bounds.upper_bounds().x - 1, bounds.upper_bounds().y - 1),
            );

            let dt = &mut txn.draw_target(GridMatrix::from_translation([
                0,
                0,
                bounds.lower_bounds().z,
            ]));

            background_rect
                .into_styled(
                    PrimitiveStyleBuilder::new()
                        .stroke_width(1)
                        .stroke_color(&self.frame)
                        .fill_color(&self.background)
                        .build(),
                )
                .draw(dt)
                .unwrap();
        }

        super::OneshotController::new(txn)
    }
}
