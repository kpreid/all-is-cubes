use std::sync::Arc;

use crate::apps::ControlMessage;
use crate::camera::GraphicsOptions;
use crate::vui::hud::HudInputs;
use crate::vui::widgets::{ToggleButtonVisualState, ToggleButtonWidget};
use crate::vui::{LayoutTree, UiBlocks, Widget, WidgetTree};

#[allow(clippy::redundant_clone)]
pub(crate) fn graphics_options_widgets(hud_inputs: &HudInputs) -> Vec<WidgetTree> {
    vec![
        LayoutTree::leaf(graphics_toggle_button(
            hud_inputs,
            UiBlocks::DebugInfoTextButton,
            |g| g.debug_info_text,
            |g, v| g.debug_info_text = v,
        )),
        LayoutTree::leaf(graphics_toggle_button(
            hud_inputs,
            UiBlocks::DebugChunkBoxesButton,
            |g| g.debug_chunk_boxes,
            |g, v| g.debug_chunk_boxes = v,
        )),
        LayoutTree::leaf(graphics_toggle_button(
            hud_inputs,
            UiBlocks::DebugCollisionBoxesButton,
            |g| g.debug_collision_boxes,
            |g, v| g.debug_collision_boxes = v,
        )),
        LayoutTree::leaf(graphics_toggle_button(
            hud_inputs,
            UiBlocks::DebugLightRaysButton,
            |g| g.debug_light_rays_at_cursor,
            |g, v| g.debug_light_rays_at_cursor = v,
        )),
    ]
}

/// Generate a button that toggles a boolean graphics option.
fn graphics_toggle_button(
    hud_inputs: &HudInputs,
    icon_ctor: fn(ToggleButtonVisualState) -> UiBlocks,
    getter: fn(&GraphicsOptions) -> bool,
    setter: fn(&mut GraphicsOptions, bool),
) -> Arc<dyn Widget> {
    ToggleButtonWidget::new(
        hud_inputs.graphics_options.clone(),
        getter,
        |state| hud_inputs.hud_blocks.blocks[icon_ctor(state)].clone(),
        {
            let cc = hud_inputs.app_control_channel.clone();
            move || {
                let _ignore_errors = cc.send(ControlMessage::ModifyGraphicsOptions(Box::new(
                    move |mut g| {
                        let mg = Arc::make_mut(&mut g);
                        setter(mg, !getter(mg));
                        g
                    },
                )));
            }
        },
    )
}

pub(crate) fn pause_toggle_button(hud_inputs: &HudInputs) -> Arc<dyn Widget> {
    ToggleButtonWidget::new(
        hud_inputs.paused.clone(),
        |&value| value,
        |state| hud_inputs.hud_blocks.blocks[UiBlocks::PauseButton(state)].clone(),
        {
            let cc = hud_inputs.app_control_channel.clone();
            move || {
                let _ignore_errors = cc.send(ControlMessage::TogglePause);
            }
        },
    )
}
