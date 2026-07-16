//! Fonts and text layout.
//!
//! Usually used together with
//! [`block::Text`][crate::block::Text] and
//! [`block::Primitive::Text`][crate::block::Primitive::Text].

mod font;
pub(in crate::text) use font::GlyphIndex;
pub(crate) use font::{FONT_BODY_TEXT, FONT_SYSTEM_16};
pub use font::{Font, FontDef, InGlyph, Metrics, Value};

mod layout;
pub use layout::Measurement;
pub(crate) use layout::{Layout, LayoutHeader, compute_layout, glyph_bounding_box_to_3d};

mod positioning;
pub use positioning::{Positioning, PositioningX, PositioningY, PositioningZ};
