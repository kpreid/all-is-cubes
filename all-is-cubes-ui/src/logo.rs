//! Drawing the All is Cubes logo.

use alloc::sync::Arc;

use all_is_cubes::{
    arcstr::literal,
    block::{Block, text},
    content::palette,
};

use crate::vui;

/// All is Cubes logo text as a widget, at "1:1" scale (1 block per font pixel).
#[expect(clippy::module_name_repetitions)]
pub fn logo_text() -> Arc<dyn vui::Widget> {
    let foreground_text_block: Block = palette::LOGO_FILL.into();
    let background_text_block: Block = palette::LOGO_STROKE.into();

    Arc::new(vui::widgets::LargeText {
        text: text::Text::builder()
            .string(literal!("All is Cubes"))
            .font(text::Font::Logo)
            .foreground(foreground_text_block)
            .outline(Some(background_text_block))
            .positioning(text::Positioning {
                x: text::PositioningX::Center,
                line_y: text::PositioningY::BodyMiddle,
                z: text::PositioningZ::Back,
            })
            .build(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::euclid::size3;

    #[test]
    fn logo_extent_as_expected() {
        assert_eq!(logo_text().requirements().minimum, size3(86, 18, 2));
    }
}
