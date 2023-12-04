use alloc::sync::Arc;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::block::text::{self, Text as BlockText};
use all_is_cubes::block::{self, Resolution::*};
use all_is_cubes::drawing::embedded_graphics::{
    mono_font::{MonoFont, MonoTextStyle},
    prelude::{Dimensions, Point},
    text::{Text as EgText, TextStyle},
    Drawable,
};
use all_is_cubes::drawing::{rectangle_to_aab, VoxelBrush};
use all_is_cubes::math::{GridAab, Gridgid};
use all_is_cubes::space::{CubeTransaction, SpaceTransaction};

use crate::vui::{self, widgets, LayoutGrant, LayoutRequest, Layoutable, Widget, WidgetController};

/// Widget which draws text using a block per font pixel.
///
/// It is “large” in that it is not building blocks that fit entire characters.
///
/// TODO: Give this a more precise name, and a nice constructor...
#[derive(Clone, Debug)]
#[allow(clippy::exhaustive_structs)] // TODO: find a better strategy
pub struct LargeText {
    /// Text to be displayed.
    pub text: ArcStr,
    /// Font with which to draw the text.
    /// Needs to be a function to be Send+Sync.
    pub font: fn() -> &'static MonoFont<'static>,
    /// Brush with which to draw the text.
    pub brush: VoxelBrush<'static>,
    /// Text positioning within the bounds of the widget.
    pub text_style: TextStyle,
}

impl LargeText {
    fn drawable(&self) -> EgText<'_, MonoTextStyle<'_, &VoxelBrush<'_>>> {
        EgText::with_text_style(
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

/// Widget which draws a static [`Text`](BlockText) for use as a text label in UI.
///
/// This cannot be used for dynamic text.
#[derive(Clone, Debug)]
pub struct Label {
    text: ArcStr,
    font: text::Font,
    positioning: Option<text::Positioning>,
}

impl Label {
    /// Constructs a [`Label`] that draws the given text, with the standard UI label font.
    pub fn new(string: ArcStr) -> Self {
        Self {
            text: string,
            font: text::Font::System16,
            positioning: None,
        }
    }

    /// Constructs a [`Label`] that draws the given text, with a specified font.
    //---
    // TODO: undecided what's a good API
    pub(crate) fn with_font(
        string: ArcStr,
        font: text::Font,
        positioning: text::Positioning,
    ) -> Self {
        Self {
            text: string,
            font,
            positioning: Some(positioning),
        }
    }

    fn text(&self, gravity: vui::Gravity) -> text::Text {
        text_for_widget(
            self.text.clone(),
            self.font.clone(),
            self.positioning
                .unwrap_or_else(|| gravity_to_positioning(gravity, true)),
        )
    }
}

impl Layoutable for Label {
    fn requirements(&self) -> LayoutRequest {
        // TODO: memoize

        // Note that we use `Align::Low` (or we could equivalently use `Align::High`).
        // If we were to use `Center`, then we might create bounds 1 block wider than is actually
        // needed because the underlying text rendering is (by default) centering within a cube,
        // so for example a 1.5-cube-long text would occupy 3 cubes by sticking out 0.25 on each
        // end, when it would actually fit in 2.
        //
        // When we actually go to render text, we'll use the actual gravity and bounds, so this
        // alignment choice won't matter.
        LayoutRequest {
            minimum: self
                .text(vui::Gravity::splat(vui::Align::Low))
                .bounding_blocks()
                .size(),
        }
    }
}

impl Widget for Label {
    fn controller(self: Arc<Self>, grant: &LayoutGrant) -> Box<dyn WidgetController> {
        // TODO: memoize `Text` construction for slightly more efficient reuse of widget
        // (this will only matter once `Text` memoizes glyph layout)

        widgets::OneshotController::new(draw_text_txn(&self.text(grant.gravity), grant))
    }
}

fn text_for_widget(text: ArcStr, font: text::Font, positioning: text::Positioning) -> text::Text {
    text::Text::builder()
        .resolution(R32)
        .string(text)
        .font(font)
        .positioning(positioning)
        .build()
}

fn gravity_to_positioning(gravity: vui::Gravity, ignore_y: bool) -> text::Positioning {
    text::Positioning {
        x: match gravity.x {
            vui::Align::Low => text::PositioningX::Left,
            vui::Align::Center => text::PositioningX::Center,
            vui::Align::High => text::PositioningX::Right,
        },
        line_y: if ignore_y {
            text::PositioningY::BodyMiddle
        } else {
            match gravity.y {
                vui::Align::Low => text::PositioningY::BodyBottom,
                vui::Align::Center => text::PositioningY::BodyMiddle,
                vui::Align::High => text::PositioningY::BodyTop,
            }
        },
        z: match gravity.z {
            vui::Align::Low | vui::Align::Center => text::PositioningZ::Back,
            vui::Align::High => text::PositioningZ::Front,
        },
    }
}

fn draw_text_txn(text: &BlockText, grant: &LayoutGrant) -> SpaceTransaction {
    let text_aabb = text.bounding_blocks();
    let grant = grant.shrink_to(text_aabb.size(), true);
    let translation = grant.bounds.lower_bounds() - text_aabb.lower_bounds();

    // This is like `BlockText::installation()` but if the text ends up too big it is truncated.
    // TODO: But it shouldn't necessarily be truncated to the lower-left corner which is what
    // our choice of translation calculation is doing
    SpaceTransaction::filling(
        grant.bounds, // doesn't over-fill because it's been shrunk
        |cube| {
            CubeTransaction::replacing(
                None,
                Some(block::Block::from_primitive(block::Primitive::Text {
                    text: text.clone(),
                    offset: cube.lower_bounds().to_vector() - translation,
                })),
            )
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vui;
    use all_is_cubes::arcstr::literal;
    use all_is_cubes::block::Block;
    use all_is_cubes::drawing::embedded_graphics::mono_font::iso_8859_1::FONT_9X15_BOLD;
    use all_is_cubes::euclid::vec3;
    use all_is_cubes::math::Rgba;
    use all_is_cubes::space::{SpaceBuilder, SpacePhysics};

    #[test]
    fn large_text_size() {
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
                minimum: vec3(9 * text.len() as i32, 15, 1)
            }
        );
    }

    #[test]
    fn label_layout() {
        let tree: vui::WidgetTree = vui::LayoutTree::leaf(Arc::new(Label::new(literal!("hi"))));

        // to_space() serves as a widget building sanity check. TODO: make a proper widget tester
        tree.to_space(
            SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
            vui::Gravity::new(vui::Align::Center, vui::Align::Center, vui::Align::Low),
        )
        .unwrap();
    }

    #[test]
    fn label_requirements() {
        // In the current system font and scale, this is exactly 1.5 blocks wide.
        let string = literal!("abcdef");

        let tree: vui::WidgetTree = vui::LayoutTree::leaf(Arc::new(Label::new(string)));

        // A previous bug would cause this to be 3 wide.
        assert_eq!(
            tree.requirements(),
            LayoutRequest {
                minimum: vec3(2, 1, 1)
            }
        );
    }
}
