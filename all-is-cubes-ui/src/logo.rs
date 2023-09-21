use alloc::{borrow::Cow, sync::Arc};

use all_is_cubes::{
    block::Block,
    content::palette,
    drawing::{
        embedded_graphics::{
            mono_font::iso_8859_1::FONT_9X15_BOLD,
            text::{Alignment, Baseline, TextStyleBuilder},
        },
        VoxelBrush,
    },
};

use crate::vui;

/// All is Cubes logo text as a widget, at "1:1" scale (1 block per font pixel).
pub fn logo_text() -> Arc<dyn vui::Widget> {
    let foreground_text_block: Block = palette::LOGO_FILL.into();
    let background_text_block: Block = palette::LOGO_STROKE.into();

    Arc::new(vui::widgets::LargeText {
        text: Cow::Borrowed("All is Cubes"),
        font: || &FONT_9X15_BOLD,
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
    use all_is_cubes::math::GridVector;

    #[test]
    fn logo_extent_as_expected() {
        assert_eq!(
            logo_text().requirements().minimum,
            GridVector::new(110, 17, 2)
        );
    }
}
