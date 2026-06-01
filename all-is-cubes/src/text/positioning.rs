#[cfg(doc)]
use crate::block::Text;

/// How a [`Text`] is to be positioned within a block.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[expect(
    clippy::exhaustive_structs,
    reason = "TODO: probably want to do something else"
)]
pub struct Positioning {
    /// How to place the text horizontally relative to the layout bounds.
    pub x: PositioningX,

    // TODO: implement this
    // /// How to place the text's first or last line relative to the layout bounds.
    // pub total_y: (),
    /// How to place the characters of the first line relative to the layout bounds.
    pub line_y: PositioningY,

    /// How to place the text depthwise relative to the layout bounds.
    pub z: PositioningZ,
}

/// How a [`Text`] is to be positioned within the layout bounds, along the X axis (horizontally).
///
/// A component of [`Positioning`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum PositioningX {
    // TODO: Distinguish 'end of graphic' (last bit of ink) from 'nominal character spacing'?
    /// Left (most negative X) end of the line of text is positioned at the left edge of the
    /// layout bounds.
    ///
    /// In the event that RTL text support is added, this is not necessarily the start of the text.
    Left,

    /// Center the text within the layout bounds.
    Center,

    /// Right (most positive X) end of the line of text is positioned at the right edge of the
    /// layout bounds.
    ///
    /// In the event that RTL text support is added, this is not necessarily the end of the text.
    Right,
}

/// How a [`Text`] is to be positioned within the layout bounds, along the Y axis (vertically).
///
/// A component of [`Positioning`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum PositioningY {
    /// The top of a line of text (past which no voxels extend) is aligned with the top edge
    /// of the layout bounds.
    BodyTop,

    /// The text is positioned halfway between `BodyTop` and `BodyBottom`, centered within the
    /// layout bounds.
    /// This may not necessarily visually center the font, but it will leave the most actually
    /// blank margin.
    BodyMiddle,

    /// The bottom edge (of most characters, excluding descenders and accents) is positioned
    /// at the bottom edge of the layout bounds.
    Baseline,

    /// The bottom of a line of text (past which no voxels extend) is aligned with the bottom edge
    /// of the layout bounds.
    BodyBottom,
}

/// How a [`Text`] is to be positioned within the layout bounds, along the Z axis (depth).
///
/// A component of [`Positioning`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum PositioningZ {
    /// Against the back (negative Z) face of the layout bounds.
    Back,

    /// Against the front (positive Z) face of the layout bounds.
    Front,
}

impl Positioning {
    #[doc(hidden)] // not sure if good idea
    pub const LOW: Self = Positioning {
        x: PositioningX::Left,
        line_y: PositioningY::BodyBottom,
        z: PositioningZ::Back,
    };
}
