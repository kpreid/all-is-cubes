//! Drawing the All is Cubes logo.

use alloc::sync::Arc;

use all_is_cubes::{
    arcstr::literal,
    block::{self, Block},
    content::palette,
    text, universe,
};

use crate::vui;

/// Returns the logo font.
/// Someday we may have a unique font.
fn logo_font() -> text::Font {
    universe::Builtin::font_system16().clone()
}

/// All is Cubes logo text as a widget, at "1:1" scale (1 block per font pixel).
#[expect(clippy::module_name_repetitions)]
#[expect(clippy::missing_panics_doc, reason = "infallible")]
pub fn logo_text() -> Arc<dyn vui::Widget> {
    let foreground_text_block: Block = palette::LOGO_FILL.into();
    let background_text_block: Block = palette::LOGO_STROKE.into();

    Arc::new(
        vui::widgets::LargeText::new(
            universe::ReadTicket::stub(), // only using a builtin font
            block::Text::builder()
                .string(literal!("All is Cubes"))
                .font(logo_font())
                .foreground(foreground_text_block)
                .outline(Some(background_text_block))
                .positioning(text::Positioning {
                    x: text::PositioningX::Center,
                    line_y: text::PositioningY::BodyMiddle,
                    z: text::PositioningZ::Back,
                })
                .build(),
        )
        .unwrap(),
    )
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
