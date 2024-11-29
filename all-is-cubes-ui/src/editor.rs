//! VUI components related to allowing the user to inspect universe contents.

use alloc::sync::Arc;
use alloc::vec;
use alloc::vec::Vec;

use all_is_cubes::arcstr::{self, literal, ArcStr};
use all_is_cubes::block::{self, Block};
use all_is_cubes::character::Cursor;
use all_is_cubes::euclid::size3;
use all_is_cubes::math::Face6;
use all_is_cubes::universe::Handle;

use crate::ui_content::hud::HudInputs;
use crate::ui_content::pages::back_button;
use crate::vui::parts::paragraph;
use crate::vui::{self, widgets};

pub fn inspect_block_at_cursor(
    inputs: &HudInputs, // TODO: need this for the back button only ... feels wrong
    cursor: &Cursor,
) -> vui::Page {
    let hit = cursor.hit();

    let contents = Arc::new(vui::LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            vui::leaf_widget(hit.block.clone().with_modifier(block::Quote::new())),
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::PX,
                children: vec![
                    Arc::new(vui::LayoutTree::Stack {
                        direction: Face6::NY,
                        children: vec![
                            vui::leaf_widget(widgets::Label::new(literal!("Definition"))),
                            inspect_block_definition(&hit.block),
                        ],
                    }),
                    inspect_evaluated(&hit.evaluated),
                ],
            }),
        ],
    });

    vui::Page::new_modal_dialog(
        &inputs.hud_blocks.widget_theme,
        literal!("Inspect Block"),
        Some(back_button(inputs)),
        contents,
    )
}

fn inspect_block_definition(block: &Block) -> vui::WidgetTree {
    let mut stack = Vec::new();
    stack.push(inspect_primitive(block.primitive()));
    for i in 0..block.modifiers().len() {
        stack.push(inspect_modifier(block, i));
    }
    Arc::new(vui::LayoutTree::Stack {
        direction: Face6::NY,
        children: stack,
    })
}

fn inspect_primitive(primitive: &block::Primitive) -> vui::WidgetTree {
    let (name, details): (ArcStr, vui::WidgetTree) = match primitive {
        block::Primitive::Indirect(block_def) => (literal!("Indirect"), inspect_handle(block_def)),
        block::Primitive::Atom(block::Atom {
            color,
            emission,
            collision,
        }) => (
            literal!("Atom"),
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::NY,
                children: vec![paragraph(arcstr::format!(
                    "\
                        Color: {color:?}\n\
                        Emission: {emission:?}\n\
                        Collision: {collision:?}\
                        "
                ))],
            }),
        ),
        block::Primitive::Recur {
            space,
            offset,
            resolution,
        } => (
            literal!("Recur"),
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::NY,
                children: vec![
                    inspect_handle(space),
                    paragraph(arcstr::format!(
                        "\
                        Resolution: {resolution}\n\
                        Offset: {offset:?}\
                        "
                    )),
                ],
            }),
        ),
        block::Primitive::Air => (literal!("Air"), vui::LayoutTree::empty()),
        block::Primitive::Text { text, offset } => (
            literal!("Text"),
            // TODO: truncate text
            paragraph(arcstr::format!(
                "\
                Text: {text:#?}\n\
                Offset: {offset:?}\
                "
            )),
        ),
        // TODO: do a debug print instead of nothing
        _ => (literal!("<unknown>"), vui::LayoutTree::empty()),
    };
    Arc::new(vui::LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::PX,
                children: vec![
                    vui::leaf_widget(
                        Block::from_primitive(primitive.clone()).with_modifier(block::Quote::new()),
                    ),
                    vui::leaf_widget(widgets::Label::new(name)),
                ],
            }),
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::PX,
                children: vec![indent(), details],
            }),
        ],
    })
}

fn inspect_modifier(block: &Block, modifier_index: usize) -> vui::WidgetTree {
    let modifier = &block.modifiers()[modifier_index];

    let mut block_up_to_this = block.clone();
    block_up_to_this
        .modifiers_mut()
        .truncate(modifier_index + 1);

    let (name, details) = match modifier {
        block::Modifier::Attributes(a) => {
            (literal!("Attributes"), paragraph(arcstr::format!("{a:?}")))
        }
        block::Modifier::Quote(q) => (literal!("Quote"), paragraph(arcstr::format!("{q:?}"))),
        block::Modifier::Rotate(rotation) => (
            literal!("Rotate"),
            paragraph(arcstr::format!("{rotation:?}")),
        ),
        block::Modifier::Composite(block::Composite {
            source,
            operator,
            reverse,
            disassemblable,
            ..
        }) => (
            literal!("Composite"),
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::NY,
                children: vec![
                    inspect_block_definition(source),
                    paragraph(arcstr::format!(
                        "\
                        Operator: {operator:?}\n\
                        Reverse: {reverse:?}\n\
                        Disassemblable: {disassemblable:?}\
                        "
                    )),
                ],
            }),
        ),
        block::Modifier::Zoom(zoom) => {
            let scale = zoom.scale();
            let offset = zoom.offset();
            (
                literal!("Zoom"),
                paragraph(arcstr::format!(
                    "\
                Scale: {scale:?}\n\
                Offset: {offset:?}\
                "
                )),
            )
        }
        block::Modifier::Move(block::Move {
            direction,
            distance,
            velocity,
            ..
        }) => (
            literal!("Move"),
            paragraph(arcstr::format!(
                "{direction:?} distance {distance:?} velocity {velocity:?}"
            )),
        ),
        _ => (literal!("<unknown>"), vui::LayoutTree::empty()),
    };

    Arc::new(vui::LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::PX,
                children: vec![
                    vui::leaf_widget(block_up_to_this.with_modifier(block::Quote::new())),
                    vui::leaf_widget(widgets::Label::new(name)),
                ],
            }),
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::PX,
                children: vec![indent(), details],
            }),
        ],
    })
}

fn inspect_evaluated(ev: &block::EvaluatedBlock) -> vui::WidgetTree {
    Arc::new(vui::LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            vui::leaf_widget(widgets::Label::new(literal!("Evaluation"))),
            paragraph(arcstr::format!("{ev:#?}")),
        ],
    })
}

fn inspect_handle<T: 'static>(handle: &Handle<T>) -> vui::WidgetTree {
    let name = handle.name();
    // TODO: have an icon and a custom background style
    paragraph(arcstr::format!("Handle:\n{name}"))
}

fn indent() -> vui::WidgetTree {
    Arc::new(vui::LayoutTree::Spacer(vui::LayoutRequest {
        // Y/Z size is zero so that it can collapse if it has no content
        minimum: size3(1, 0, 0),
    }))
}
