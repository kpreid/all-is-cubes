//! Fonts and text layout.
//!
//! Usually used together with
//! [`block::Text`][crate::block::Text] and
//! [`block::Primitive::Text`][crate::block::Primitive::Text].

mod font;
pub use font::{Font, FontDef, Metrics, Value};

mod layout;
pub use layout::Measurement;
pub(crate) use layout::{InGlyph, Layout, LayoutHeader, compute_layout, glyph_bounding_box_to_3d};

mod positioning;
pub use positioning::{Positioning, PositioningX, PositioningY, PositioningZ};
