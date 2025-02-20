//! Drawing the All is Cubes logo.

use alloc::sync::Arc;

use all_is_cubes::{
    arcstr::literal,
    block::{Block, text::Font},
    content::palette,
    drawing::{
        VoxelBrush,
        embedded_graphics::text::{Alignment, Baseline, TextStyleBuilder},
    },
};

use crate::vui;

/// All is Cubes logo text as a widget, at "1:1" scale (1 block per font pixel).
#[expect(clippy::module_name_repetitions)]
pub fn logo_text() -> Arc<dyn vui::Widget> {
    let foreground_text_block: Block = palette::LOGO_FILL.into();
    let background_text_block: Block = palette::LOGO_STROKE.into();

    Arc::new(vui::widgets::LargeText {
        text: literal!("All is Cubes"),
        font: Font::Logo,
        brush: {
            VoxelBrush::new([
                ([0, 0, 1], foreground_text_block),
                ([1, 0, 0], background_text_block.clone()),
                ([-1, 0, 0], background_text_block.clone()),
                ([0, 1, 0], background_text_block.clone()),
                ([0, -1, 0], background_text_block),
            ])
        },
        text_style: TextStyleBuilder::new()
            .alignment(Alignment::Center)
            .baseline(Baseline::Middle)
            .build(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::euclid::size3;

    #[test]
    fn logo_extent_as_expected() {
        assert_eq!(logo_text().requirements().minimum, size3(110, 17, 2));
    }
}
