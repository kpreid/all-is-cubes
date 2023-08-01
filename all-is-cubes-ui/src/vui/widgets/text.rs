use std::borrow::Cow;
use std::sync::Arc;

use all_is_cubes::drawing::embedded_graphics::{
    mono_font::{MonoFont, MonoTextStyle},
    prelude::{Dimensions, Point},
    text::{Text, TextStyle},
    Drawable,
};
use all_is_cubes::drawing::{rectangle_to_aab, VoxelBrush};
use all_is_cubes::math::{GridAab, Gridgid};
use all_is_cubes::space::SpaceTransaction;

use crate::vui::{widgets, LayoutGrant, LayoutRequest, Layoutable, Widget, WidgetController};

/// Widget which draws text using a block per font pixel.
///
/// It is “large” in that it is not building blocks that fit entire characters.
///
/// TODO: Give this a more precise name, and a nice constructor...
#[derive(Clone, Debug)]
#[allow(clippy::exhaustive_structs)] // TODO: find a better strategy
pub struct LargeText {
    pub text: Cow<'static, str>,
    /// Needs to be a function to be Send+Sync
    pub font: fn() -> &'static MonoFont<'static>,
    pub brush: VoxelBrush<'static>,
    pub text_style: TextStyle,
}

impl LargeText {
    fn drawable(&self) -> Text<'_, MonoTextStyle<'_, &VoxelBrush<'_>>> {
        Text::with_text_style(
            &self.text,
            Point::new(0, 0),
            MonoTextStyle::new((self.font)(), &self.brush),
            self.text_style,
        )
    }

    fn bounds(&self) -> GridAab {
        // TODO: this conversion should be less fiddly
        rectangle_to_aab(
            self.drawable().bounding_box(),
            Gridgid::FLIP_Y,
            self.brush.bounds().unwrap_or(GridAab::ORIGIN_CUBE),
        )
    }
}

impl Layoutable for LargeText {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: self.bounds().size(),
        }
    }
}

impl Widget for LargeText {
    fn controller(self: Arc<Self>, position: &LayoutGrant) -> Box<dyn WidgetController> {
        let mut txn = SpaceTransaction::default();
        let drawable = self.drawable();
        let draw_bounds = self.bounds();
        drawable
            .draw(
                &mut txn.draw_target(
                    Gridgid::from_translation(
                        position
                            .shrink_to(draw_bounds.size(), false)
                            .bounds
                            .lower_bounds()
                            - draw_bounds.lower_bounds(),
                    ) * Gridgid::FLIP_Y,
                ),
            )
            .unwrap();

        widgets::OneshotController::new(txn)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::Block;
    use all_is_cubes::drawing::embedded_graphics::mono_font::iso_8859_1::FONT_9X15_BOLD;
    use all_is_cubes::math::{GridVector, Rgba};

    #[test]
    fn text_size() {
        let text = "abc";
        let widget = LargeText {
            text: text.into(),
            font: || &FONT_9X15_BOLD,
            brush: VoxelBrush::single(Block::from(Rgba::WHITE)),
            text_style: TextStyle::default(),
        };
        assert_eq!(
            widget.requirements(),
            LayoutRequest {
                minimum: GridVector::new(9 * text.len() as i32, 15, 1)
            }
        );
    }
}
