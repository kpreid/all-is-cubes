use alloc::boxed::Box;
use alloc::sync::Arc;

use all_is_cubes::block::{self, Block, Resolution::R64, text};
use all_is_cubes::space::{CubeTransaction, SpaceTransaction};
use all_is_cubes::util::{ConciseDebug, Refmt};

use crate::vui::widgets::WidgetTheme;
use crate::vui::{self, Layoutable as _};

/// Widget that uses another widget's [`Layoutable`] parameters, but instead of displaying that
/// widget, displays a bounding box of the area it's granted, and data as text.
#[derive(Debug)]
#[doc(hidden)] // experimental
pub struct LayoutDebugFrame {
    theme: WidgetTheme,
    widget: Arc<dyn vui::Widget>,
}

impl LayoutDebugFrame {
    pub fn new(theme: WidgetTheme, widget: Arc<dyn vui::Widget>) -> Arc<Self> {
        Arc::new(Self { theme, widget })
    }
}

impl vui::Layoutable for LayoutDebugFrame {
    fn requirements(&self) -> vui::LayoutRequest {
        self.widget.requirements()
    }
}

impl vui::Widget for LayoutDebugFrame {
    fn controller(self: Arc<Self>, grant: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        let box_style = &self.theme.layout_debug_box_style;
        let bounds = grant.bounds;

        let info_text = text::Text::builder()
            .string(all_is_cubes::arcstr::format!(
                "Req {:?}\nGrant {:?}\nGrav {:?}",
                self.requirements().refmt(&ConciseDebug),
                grant.bounds,
                grant.gravity.to_array(),
            ))
            .font(text::Font::System16)
            .resolution(R64)
            .positioning(text::Positioning {
                x: text::PositioningX::Left,
                line_y: text::PositioningY::BodyMiddle,
                z: text::PositioningZ::Front,
            })
            .build();

        super::OneshotController::new(SpaceTransaction::filling(bounds, |cube| {
            // not bothering to skip outside text bounds.
            let text_block = Block::from_primitive(block::Primitive::Text {
                text: info_text.clone(),
                offset: cube.lower_bounds() - bounds.lower_bounds(),
            });
            let block: Block = if let Some(box_block) = box_style.cube_at(bounds, cube).cloned() {
                block::Composite::new(text_block, block::CompositeOperator::Over)
                    .compose_or_replace(box_block)
            } else {
                text_block
            };
            CubeTransaction::replacing(None, Some(block))
        }))
    }
}
