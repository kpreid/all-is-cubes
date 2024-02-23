//! VUI components related to allowing the user to inspect universe contents.

use alloc::sync::Arc;

use all_is_cubes::arcstr::{self, literal, ArcStr};
use all_is_cubes::block::{self, Block};
use all_is_cubes::character::Cursor;
use all_is_cubes::euclid::size3;
use all_is_cubes::math::Face6;

use crate::ui_content::hud::HudInputs;
use crate::ui_content::pages::back_button;
use crate::vui::parts::paragraph;
use crate::vui::{self, widgets};

pub fn inspect_block_at_cursor(
    inputs: &HudInputs, // TODO: need this for the back button only ... feels wrong
    cursor: &Cursor,
) -> vui::WidgetTree {
    let hit = cursor.hit();

    let mut def_stack = vec![vui::leaf_widget(widgets::Label::new(literal!(
        "Definition"
    )))];

    def_stack.push(inspect_primitive(hit.block.primitive()));
    for i in 0..hit.block.modifiers().len() {
        def_stack.push(inspect_modifier(&hit.block, i));
    }

    let contents = Arc::new(vui::LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            back_button(inputs),
            vui::leaf_widget(hit.block.clone().with_modifier(block::Quote::new())),
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::PX,
                children: vec![
                    Arc::new(vui::LayoutTree::Stack {
                        direction: Face6::NY,
                        children: def_stack,
                    }),
                    inspect_evaluated(&hit.evaluated),
                ],
            }),
        ],
    });

    vui::page_modal_backdrop(Arc::new(vui::LayoutTree::Shrink(
        inputs
            .hud_blocks
            .widget_theme
            .dialog_background()
            .as_background_of(contents),
    )))
}

fn inspect_primitive(primitive: &block::Primitive) -> vui::WidgetTree {
    let (name, details): (ArcStr, vui::WidgetTree) = match primitive {
        block::Primitive::Indirect(_) => (literal!("Indirect"), vui::LayoutTree::empty()),
        block::Primitive::Atom(_) => (literal!("Atom"), vui::LayoutTree::empty()),
        block::Primitive::Recur { .. } => (literal!("Recur"), vui::LayoutTree::empty()),
        block::Primitive::Air => (literal!("Air"), vui::LayoutTree::empty()),
        block::Primitive::Text { .. } => (literal!("Text"), vui::LayoutTree::empty()),
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

    let name = match modifier {
        block::Modifier::Quote(_) => literal!("Quote"),
        block::Modifier::Rotate(_) => literal!("Rotate"),
        block::Modifier::Composite(_) => literal!("Composite"),
        block::Modifier::Zoom(_) => literal!("Zoom"),
        block::Modifier::Move(_) => literal!("Move"),
        _ => literal!("<unknown>"),
    };
    let details = vui::LayoutTree::empty();

    Arc::new(vui::LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            Arc::new(vui::LayoutTree::Stack {
                direction: Face6::PX,
                children: vec![
                    vui::leaf_widget(block_up_to_this),
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

fn indent() -> vui::WidgetTree {
    Arc::new(vui::LayoutTree::Spacer(vui::LayoutRequest {
        minimum: size3(1, 1, 1),
    }))
}
