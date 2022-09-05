use std::sync::Arc;

use all_is_cubes::{
    block::Block,
    drawing::{
        embedded_graphics::{
            mono_font::{iso_8859_1::FONT_9X15_BOLD, MonoTextStyle},
            prelude::{Dimensions as _, Drawable, Point},
            text::{Alignment, Baseline, Text, TextStyleBuilder},
        },
        rectangle_to_aab, VoxelBrush,
    },
    math::{FaceMap, GridAab, GridMatrix},
    space::SpaceTransaction,
    vui::{
        widgets::OneshotController, LayoutGrant, LayoutRequest, Layoutable, Widget,
        WidgetController,
    },
};

use crate::palette;

/// All is Cubes logo text as a widget, at "1:1" scale.
#[derive(Clone, Debug)]
pub(crate) struct LogoTextLarge;

impl Layoutable for LogoTextLarge {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: logo_text_extent().size(),
        }
    }
}
impl Widget for LogoTextLarge {
    fn controller(self: Arc<Self>, position: &LayoutGrant) -> Box<dyn WidgetController> {
        let logo_extent = logo_text_extent();

        let txn = logo_text_drawable(|d| {
            let mut txn = SpaceTransaction::default();
            d.draw(&mut txn.draw_target(
                GridMatrix::from_translation(
                    position.shrink_to(logo_extent.size()).bounds.lower_bounds()
                        - logo_extent.lower_bounds(),
                ) * GridMatrix::FLIP_Y,
            ))
            .unwrap();
            txn
        });

        Box::new(OneshotController(Some(txn)))
    }
}

fn logo_text_extent() -> GridAab {
    logo_text_drawable(|d| {
        rectangle_to_aab(
            d.bounding_box(),
            GridMatrix::FLIP_Y,
            d.character_style.text_color.unwrap().bounds().unwrap(),
        )
        .expand(FaceMap {
            px: -1, // font has blank column right edge
            ny: -2, // unused rows given the ascii-only characters
            ..Default::default()
        })
    })
}

/// Calls the given function with `Drawable` logo text.
/// Unfortunately there is no way to return an owned Drawable.
fn logo_text_drawable<F, R>(f: F) -> R
where
    F: for<'a> FnOnce(Text<'static, MonoTextStyle<'a, &VoxelBrush<'a>>>) -> R,
{
    let foreground_text_block: Block = palette::LOGO_FILL.into();
    let background_text_block: Block = palette::LOGO_STROKE.into();
    let brush = VoxelBrush::new([
        ((0, 0, 1), &foreground_text_block),
        ((1, 0, 0), &background_text_block),
        ((-1, 0, 0), &background_text_block),
        ((0, 1, 0), &background_text_block),
        ((0, -1, 0), &background_text_block),
    ]);

    let text = Text::with_text_style(
        "All is Cubes",
        Point::new(0, 0),
        MonoTextStyle::new(&FONT_9X15_BOLD, &brush),
        TextStyleBuilder::new()
            .alignment(Alignment::Center)
            .baseline(Baseline::Middle)
            .build(),
    );
    f(text)
}

#[test]
fn logo_extent_as_expected() {
    assert_eq!(
        logo_text_extent(),
        GridAab::from_lower_upper([-54, -6, 0], [55, 9, 2])
    );
}
