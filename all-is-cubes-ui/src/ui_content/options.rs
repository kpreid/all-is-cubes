use alloc::sync::Arc;
use core::fmt;

use all_is_cubes::arcstr::{self, literal};
use all_is_cubes::camera::{self, AntialiasingOption, GraphicsOptions};
use all_is_cubes::math::Face6;
use all_is_cubes::notnan;

use crate::apps::ControlMessage;
use crate::ui_content::hud::HudInputs;
use crate::vui::{self, widgets, LayoutTree, UiBlocks, Widget, WidgetTree};

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
    // TODO: Improve labeled button layout and enable these
    if false {
        w.extend([
            graphics_enum_button(
                hud_inputs,
                style,
                |g| &g.fog,
                |g, v| g.fog = v,
                // TODO: put these lists somewhere that is not individual UI code
                [
                    camera::FogOption::None,
                    camera::FogOption::Abrupt,
                    camera::FogOption::Compromise,
                    camera::FogOption::Physical,
                ],
            ),
            graphics_enum_button(
                hud_inputs,
                style,
                |g| &g.lighting_display,
                |g, v| g.lighting_display = v,
                [
                    camera::LightingOption::None,
                    camera::LightingOption::Flat,
                    camera::LightingOption::Smooth,
                ],
            ),
            graphics_enum_button(
                hud_inputs,
                style,
                |g| &g.transparency,
                |g, v| g.transparency = v,
                [
                    camera::TransparencyOption::Surface,
                    camera::TransparencyOption::Volumetric,
                    camera::TransparencyOption::Threshold(notnan!(0.5)),
                ],
            ),
        ]);
    }
    w.extend([
        // TODO: this needs to be an enum button set for the multiple states, in principle. But
        // for now, while we aren't actually saving the options, there's no reason to select
        // IfCheap.
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
    getter: impl Fn(&GraphicsOptions) -> bool + Clone + Send + Sync + 'static,
    setter: impl Fn(&mut GraphicsOptions, bool) + Clone + Send + Sync + 'static,
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
        getter.clone(),
        label,
        &hud_inputs.hud_blocks.widget_theme,
        {
            let cc = hud_inputs.app_control_channel.clone();
            move || {
                let getter = getter.clone();
                let setter = setter.clone();
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

/// Generate a group of buttons that selects one of an enum of options.
///
/// These buttons do not show up in the compact style.
fn graphics_enum_button<T: Clone + fmt::Debug + PartialEq + Send + Sync + 'static>(
    hud_inputs: &HudInputs,
    style: OptionsStyle,
    getter: fn(&GraphicsOptions) -> &T,
    setter: fn(&mut GraphicsOptions, T),
    list: impl IntoIterator<Item = T>,
) -> WidgetTree {
    match style {
        OptionsStyle::CompactRow => LayoutTree::spacer(vui::LayoutRequest::EMPTY),
        OptionsStyle::LabeledColumn => Arc::new(LayoutTree::Stack {
            direction: Face6::PX,
            children: list
                .into_iter()
                .map(|value| {
                    let value2 = value.clone();
                    let button: Arc<dyn Widget> = widgets::ToggleButton::new(
                        hud_inputs.graphics_options.clone(),
                        move |options| *getter(options) == value,
                        arcstr::format!("{:?}", value2), // TODO: quick kludge; need real labels
                        &hud_inputs.hud_blocks.widget_theme,
                        {
                            let cc = hud_inputs.app_control_channel.clone();
                            move || {
                                let value = value2.clone();
                                let _ignore_errors =
                                    cc.send(ControlMessage::ModifyGraphicsOptions(Box::new(
                                        move |mut g| {
                                            let mg = Arc::make_mut(&mut g);
                                            setter(mg, value);
                                            g
                                        },
                                    )));
                            }
                        },
                    );
                    LayoutTree::leaf(button)
                })
                .collect(),
        }),
    }
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
