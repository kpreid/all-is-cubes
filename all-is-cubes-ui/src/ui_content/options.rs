use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;use all_is_cubes::arcstr::{self, literal};
use all_is_cubes::block::text;
use all_is_cubes::math::{Face6, zo32};
use all_is_cubes::universe::ReadTicket;
use all_is_cubes_render::camera::{self, AntialiasingOption, GraphicsOptions};use crate::apps::ControlMessage;
use crate::ui_content::hud::HudInputs;
use crate::vui::{self, LayoutTree, UiBlocks, Widget, WidgetTree, widgets};#[derive(Clone, Copy, Debug)]
pub(crate) enum OptionsStyle {
    CompactRow,
    LabeledColumn,
}pub(crate) fn graphics_options_widgets(
    read_ticket: ReadTicket<'>,
    hud_inputs: &HudInputs,
    style: OptionsStyle,
) -> Vec<WidgetTree> {
    let mut w: Vec<WidgetTree> = Vec::with_capacity(10); // Increased capacity for added options
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
    // Added the missing options as per TODO
    // Assumed types and values; adjust as necessary based on actual definitions
    w.extend([
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("Render Method"),
            |g| &g.render_method,
            |g, v| g.render_method = v,
            [camera::RenderMethod::Mesh, camera::RenderMethod::Reference],
            |rm| arcstr::literal!(match *rm {
                camera::RenderMethod::Mesh => "Mesh",
                camera::RenderMethod::Reference => "Reference",
            }),
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
            |fo| arcstr::literal!(match *fo {
                camera::FogOption::None => "None",
                camera::FogOption::Abrupt => "Abrupt",
                camera::FogOption::Compromise => "Compromise",
                camera::FogOption::Physical => "Physical",
            }),
        ),
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("Bloom"),
            |g| &g.bloom_intensity,
            |g, v| g.bloom_intensity = v,
            [zo32(0.0), zo32(0.03125), zo32(0.125)],
            |bi| arcstr::literal!(if *bi == zo32(0.0) {
                "Off"
            } else if *bi == zo32(0.03125) {
                "Weak"
            } else {
                "Strong"
            }),
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
                camera::LightingOption::Bounce,
            ],
            |lo| arcstr::literal!(match *lo {
                camera::LightingOption::None => "None",
                camera::LightingOption::Flat => "Flat",
                camera::LightingOption::Smooth => "Smooth",
                camera::LightingOption::Bounce => "Bounce",
            }),
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
            |to| arcstr::literal!(match *to {
                camera::TransparencyOption::Surface => "Surface",
                camera::TransparencyOption::Volumetric => "Volumetric",
                camera::TransparencyOption::Threshold() => "Threshold",
            }),
        ),
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("Antialiasing"),
            |g| &g.antialiasing,
            |g, v| g.antialiasing = v,
            [
                AntialiasingOption::None,
                AntialiasingOption::IfCheap,
                AntialiasingOption::Always,
            ],
            |aa| arcstr::literal!(match *aa {
                AntialiasingOption::None => "None",
                AntialiasingOption::IfCheap => "If Cheap",
                AntialiasingOption::Always => "Always",
            }),
        ),
        // Added missing options with assumed discrete values
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("Field of View"),
            |g| &g.fov_y,
            |g, v| g.fov_y = v,
            [zo32(30.0), zo32(60.0), zo32(90.0), zo32(120.0)],
            |fov| arcstr::format!("{}°", fov),
        ),
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("Tone Mapping"),
            |g| &g.tone_mapping,
            |g, v| g.tone_mapping = v,
            // Assumed enum variants
            [camera::ToneMappingOption::Identity, camera::ToneMappingOption::Reinhard],
            |tm| arcstr::literal!(match *tm {
                camera::ToneMappingOption::Identity => "Identity",
                camera::ToneMappingOption::Reinhard => "Reinhard",
            }),
        ),
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("Exposure"),
            |g| &g.exposure,
            |g, v| g.exposure = v,
            [zo32(0.5), zo32(1.0), zo32(2.0)],
            |exp| arcstr::format!("{:.1}x", exp),
        ),
        graphics_enum_button(
            hud_inputs,
            style,
            literal!("View Distance"),
            |g| &g.view_distance,
            |g, v| g.view_distance = v,
            [zo32(8.0), zo32(16.0), zo32(32.0), zo32(64.0), zo32(128.0)],
            |vd| arcstr::format!("{} chunks", vd),
        ),
    ]);// Grouped debug toggles
let debug_toggles = [
    (
        UiBlocks::DebugInfoTextButtonLabel,
        |g: &GraphicsOptions| g.debug_info_text,
        |g: &mut GraphicsOptions, v: bool| g.debug_info_text = v,
    ),
    (
        UiBlocks::DebugPixelPerformanceButtonLabel,
        |g| g.debug_pixel_cost,
        |g, v| g.debug_pixel_cost = v,
    ),
    (
        UiBlocks::DebugBehaviorsButtonLabel,
        |g| g.debug_behaviors,
        |g, v| g.debug_behaviors = v,
    ),
    (
        UiBlocks::DebugChunkBoxesButtonLabel,
        |g| g.debug_chunk_boxes,
        |g, v| g.debug_chunk_boxes = v,
    ),
    (
        UiBlocks::DebugCollisionBoxesButtonLabel,
        |g| g.debug_collision_boxes,
        |g, v| g.debug_collision_boxes = v,
    ),
    (
        UiBlocks::DebugLightRaysButtonLabel,
        |g| g.debug_light_rays_at_cursor,
        |g, v| g.debug_light_rays_at_cursor = v,
    ),
];
w.extend(debug_toggles.iter().map(|&(icon_key, getter, setter)| {
    graphics_toggle_button(
        read_ticket,
        hud_inputs,
        style,
        icon_key,
        getter,
        setter,
    )
}));
w}/// Generate a button that toggles a boolean graphics option.
fn graphics_toggle_button(
    read_ticket: ReadTicket<'_>,
    hud_inputs: &HudInputs,
    style: OptionsStyle,
    icon_key: UiBlocks,
    getter: impl Fn(&GraphicsOptions) -> bool + Clone + Send + Sync + 'static,
    setter: impl Fn(&mut GraphicsOptions, bool) + Clone + Send + Sync + 'static,
) -> WidgetTree {
    let icon = hud_inputs.hud_blocks.ui_blocks[icon_key].clone();
    let text: Optionwidgets::Label = match style {
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
}/// Generate a group of buttons that selects one of an enum of options.
///
/// These buttons do not show up in the compact style.
fn graphics_enum_button<T: Clone + fmt::Debug + PartialEq + Send + Sync + 'static>(
    hud_inputs: &HudInputs,
    style: OptionsStyle,
    label: arcstr::ArcStr,
    getter: fn(&GraphicsOptions) -> &T,
    setter: fn(&mut GraphicsOptions, T),
    list: impl IntoIterator<Item = T> + Clone, // Added Clone if needed for multiple uses, but since once, ok
    label_fn: impl Fn(&T) -> arcstr::ArcStr + Clone + Send + Sync + 'static,
) -> WidgetTree {
    let label_widget = vui::leaf_widget(widgets::Label::with_font(
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
            children: [label_widget]
                .into_iter()
                .chain(list.into_iter().map(|value| {
                    let button_label = label_fn(&value);
                    let button = widgets::ToggleButton::new(
                        hud_inputs.graphics_options.clone(),
                        {
                            let value_to_compare = value.clone();
                            move |options| *getter(options) == value_to_compare
                        },
                        button_label,
                        &hud_inputs.hud_blocks.widget_theme,
                        {
                            let cc = hud_inputs.app_control_channel.clone();
                            let value_to_send = value.clone();
                            move || {
                                let _ignore_errors = cc.send(ControlMessage::ModifySettings(
                                    Box::new(move |settings| {
                                        settings
                                            .mutate_graphics_options(|go| setter(go, value_to_send.clone()))
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
}pub(crate) fn pause_toggle_button(hud_inputs: &HudInputs, style: OptionsStyle) -> Arc<dyn Widget> {
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

