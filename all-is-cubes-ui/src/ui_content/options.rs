use all_is_cubes::arcstr::literal;
use alloc::sync::Arc;

use all_is_cubes::camera::{AntialiasingOption, GraphicsOptions};

use crate::apps::ControlMessage;
use crate::ui_content::hud::HudInputs;
use crate::vui::{widgets, LayoutTree, UiBlocks, Widget, WidgetTree};

#[derive(Clone, Copy, Debug)]
pub(crate) enum OptionsStyle {
    CompactRow,
    LabeledColumn,
}

pub(crate) fn graphics_options_widgets(
    hud_inputs: &HudInputs,
    style: OptionsStyle,
) -> Vec<WidgetTree> {
    let mut w: Vec<WidgetTree> = Vec::with_capacity(5);
    if let Some(setter) = hud_inputs.set_fullscreen.clone() {
        w.push(LayoutTree::leaf(widgets::ToggleButton::new(
            hud_inputs.fullscreen_mode.clone(),
            |opt_value| opt_value.unwrap_or(false),
            hud_inputs.hud_blocks.ui_blocks[UiBlocks::FullscreenButtonLabel].clone(),
            &hud_inputs.hud_blocks.widget_theme,
            {
                let cell = hud_inputs.fullscreen_mode.clone();
                move || {
                    setter(!cell.get().unwrap_or(false));
                }
            },
        )));
    }
    w.extend([
        // TODO: this needs to be a different kind of button for the multiple states. But
        // for now, while we have only small interactive controls and the IfCheap option
        // is just conditional on the renderer type, there's no reason to select IfCheap.
        graphics_toggle_button(
            hud_inputs,
            style,
            UiBlocks::AntialiasButtonLabel,
            |g| g.antialiasing != AntialiasingOption::None,
            |g, _v| {
                g.antialiasing = match g.antialiasing {
                    AntialiasingOption::None => AntialiasingOption::Always,
                    AntialiasingOption::IfCheap => AntialiasingOption::None,
                    AntialiasingOption::Always => AntialiasingOption::None,
                    _ => AntialiasingOption::None,
                }
            },
        ),
        graphics_toggle_button(
            hud_inputs,
            style,
            UiBlocks::DebugInfoTextButtonLabel,
            |g| g.debug_info_text,
            |g, v| g.debug_info_text = v,
        ),
        graphics_toggle_button(
            hud_inputs,
            style,
            UiBlocks::DebugBehaviorsButtonLabel,
            |g| g.debug_behaviors,
            |g, v| g.debug_behaviors = v,
        ),
        graphics_toggle_button(
            hud_inputs,
            style,
            UiBlocks::DebugChunkBoxesButtonLabel,
            |g| g.debug_chunk_boxes,
            |g, v| g.debug_chunk_boxes = v,
        ),
        graphics_toggle_button(
            hud_inputs,
            style,
            UiBlocks::DebugCollisionBoxesButtonLabel,
            |g| g.debug_collision_boxes,
            |g, v| g.debug_collision_boxes = v,
        ),
        graphics_toggle_button(
            hud_inputs,
            style,
            UiBlocks::DebugLightRaysButtonLabel,
            |g| g.debug_light_rays_at_cursor,
            |g, v| g.debug_light_rays_at_cursor = v,
        ),
    ]);
    w
}

/// Generate a button that toggles a boolean graphics option.
fn graphics_toggle_button(
    hud_inputs: &HudInputs,
    style: OptionsStyle,
    icon_key: UiBlocks,
    getter: fn(&GraphicsOptions) -> bool,
    setter: fn(&mut GraphicsOptions, bool),
) -> WidgetTree {
    let icon = hud_inputs.hud_blocks.ui_blocks[icon_key].clone();
    let text: Option<widgets::Label> = match style {
        OptionsStyle::CompactRow => None,
        OptionsStyle::LabeledColumn => Some(
            icon.evaluate()
                .unwrap()
                .attributes
                .display_name
                .clone()
                .into(),
        ),
    };
    let label = widgets::ButtonLabel {
        icon: Some(icon),
        text,
    };
    let button: Arc<dyn Widget> = widgets::ToggleButton::new(
        hud_inputs.graphics_options.clone(),
        getter,
        label,
        &hud_inputs.hud_blocks.widget_theme,
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
    );
    LayoutTree::leaf(button)
}

pub(crate) fn pause_toggle_button(hud_inputs: &HudInputs, style: OptionsStyle) -> Arc<dyn Widget> {
    widgets::ToggleButton::new(
        hud_inputs.paused.clone(),
        |&value| value,
        widgets::ButtonLabel {
            icon: Some(hud_inputs.hud_blocks.ui_blocks[UiBlocks::PauseButtonLabel].clone()),
            text: match style {
                OptionsStyle::CompactRow => None,
                OptionsStyle::LabeledColumn => Some(literal!("Pause").into()),
            },
        },
        &hud_inputs.hud_blocks.widget_theme,
        {
            let cc = hud_inputs.app_control_channel.clone();
            move || {
                let _ignore_errors = cc.send(ControlMessage::TogglePause);
            }
        },
    )
}
