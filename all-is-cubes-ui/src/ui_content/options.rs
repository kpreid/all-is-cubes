use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;

use all_is_cubes::arcstr::{self, literal};
use all_is_cubes::block::text;
use all_is_cubes::math::{Face6, zo32};
use all_is_cubes::universe::ReadTicket;
use all_is_cubes_render::camera::{self, AntialiasingOption, GraphicsOptions};

use crate::apps::ControlMessage;
use crate::ui_content::hud::HudInputs;
use crate::vui::{self, LayoutTree, UiBlocks, Widget, WidgetTree, widgets};

#[derive(Clone, Copy, Debug)]
pub(crate) enum OptionsStyle {
    CompactRow,
    LabeledColumn,
}

pub(crate) fn graphics_options_widgets(
    read_ticket: ReadTicket<'_>,
    hud_inputs: &HudInputs,
    style: OptionsStyle,
) -> Vec<WidgetTree> {
    let mut w: Vec<WidgetTree> = Vec::with_capacity(5);
    if let Some(setter) = hud_inputs.set_fullscreen.clone() {
        w.push(vui::leaf_widget(widgets::ToggleButton::new(
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
    // TODO: Find a way to offer the lists of enum values that is closer to the definitions
    // of the enums, to reduce the chances of unintentional non-exhaustiveness.
    // TODO: allow setting fov_y, tone_mapping, exposure, view_distance
    w.extend([
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("Render Method"),
            |g| &g.render_method,
            |g, v| g.render_method = v,
            [camera::RenderMethod::Mesh, camera::RenderMethod::Reference],
        ),
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("Fog"),
            |g| &g.fog,
            |g, v| g.fog = v,
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
            literal!("Bloom"),
            |g| &g.bloom_intensity,
            |g, v| g.bloom_intensity = v,
            [zo32(0.0), zo32(0.03125), zo32(0.125)],
        ),
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("Light"),
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
            literal!("Transparency"),
            |g| &g.transparency,
            |g, v| g.transparency = v,
            [
                camera::TransparencyOption::Surface,
                camera::TransparencyOption::Volumetric,
                camera::TransparencyOption::Threshold(zo32(0.5)),
            ],
        ),
        // TODO: this properly should be a graphics_enum_button with 3 states,
        // but it is currently convenient to have toggleability for testing.
        // Find a way to support this better.
        graphics_toggle_button(
            read_ticket,
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
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugInfoTextButtonLabel,
            |g| g.debug_info_text,
            |g, v| g.debug_info_text = v,
        ),
        graphics_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugPixelPerformanceButtonLabel,
            |g| g.debug_pixel_cost,
            |g, v| g.debug_pixel_cost = v,
        ),
        graphics_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugBehaviorsButtonLabel,
            |g| g.debug_behaviors,
            |g, v| g.debug_behaviors = v,
        ),
        graphics_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugChunkBoxesButtonLabel,
            |g| g.debug_chunk_boxes,
            |g, v| g.debug_chunk_boxes = v,
        ),
        graphics_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugCollisionBoxesButtonLabel,
            |g| g.debug_collision_boxes,
            |g, v| g.debug_collision_boxes = v,
        ),
        graphics_toggle_button(
            read_ticket,
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
    read_ticket: ReadTicket<'_>,
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
            icon.evaluate(read_ticket) // TODO(read_ticket): we should probably get the label text elsewhere and remove this need for a read ticket
                .unwrap()
                .attributes()
                .display_name
                .clone()
                .into(),
        ),
    };
    let label = widgets::ButtonLabel {
        icon: Some(icon),
        text,
    };
    let button = widgets::ToggleButton::new(
        hud_inputs.graphics_options.clone(),
        {
            let getter = getter.clone();
            move |arc_opt| getter(arc_opt)
        },
        label,
        &hud_inputs.hud_blocks.widget_theme,
        {
            let cc = hud_inputs.app_control_channel.clone();
            move || {
                let getter = getter.clone();
                let setter = setter.clone();
                let _ignore_errors =
                    cc.send(ControlMessage::ModifySettings(Box::new(move |settings| {
                        settings.mutate_graphics_options(|go| setter(go, !getter(go)))
                    })));
            }
        },
    );
    vui::leaf_widget(button)
}

/// Generate a group of buttons that selects one of an enum of options.
///
/// These buttons do not show up in the compact style.
fn graphics_enum_button<T: Clone + fmt::Debug + PartialEq + Send + Sync + 'static>(
    hud_inputs: &HudInputs,
    style: OptionsStyle,
    label: arcstr::ArcStr,
    getter: fn(&GraphicsOptions) -> &T,
    setter: fn(&mut GraphicsOptions, T),
    list: impl IntoIterator<Item = T>,
) -> WidgetTree {
    let label = vui::leaf_widget(widgets::Label::with_font(
        label,
        text::Font::System16,
        text::Positioning {
            x: text::PositioningX::Right,
            line_y: text::PositioningY::BodyMiddle,
            z: text::PositioningZ::Back,
        },
    ));
    match style {
        OptionsStyle::CompactRow => LayoutTree::spacer(vui::LayoutRequest::EMPTY),
        OptionsStyle::LabeledColumn => Arc::new(LayoutTree::Stack {
            direction: Face6::PX,
            children: [label]
                .into_iter()
                .chain(list.into_iter().map(|value| {
                    let button = widgets::ToggleButton::new(
                        hud_inputs.graphics_options.clone(),
                        {
                            let value_to_compare = value.clone();
                            move |options| *getter(options) == value_to_compare
                        },
                        arcstr::format!("{:?}", value), // TODO: quick kludge; need real labels
                        &hud_inputs.hud_blocks.widget_theme,
                        {
                            let cc = hud_inputs.app_control_channel.clone();
                            move || {
                                let value_to_send = value.clone();
                                let _ignore_errors = cc.send(ControlMessage::ModifySettings(
                                    Box::new(move |settings| {
                                        settings
                                            .mutate_graphics_options(|go| setter(go, value_to_send))
                                    }),
                                ));
                            }
                        },
                    );
                    vui::leaf_widget(button)
                }))
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
