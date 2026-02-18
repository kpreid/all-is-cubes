use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;
use exhaust::Exhaust;

use all_is_cubes::arcstr::{self, literal};
use all_is_cubes::block::text;
use all_is_cubes::math::{Face6, zo32};
use all_is_cubes::universe::ReadTicket;
use all_is_cubes::util::ShowStatus;
use all_is_cubes_render::camera::{self, AntialiasingOption};

use crate::apps::ControlMessage;
use crate::settings::{self, Key};
use crate::ui_content::hud::HudInputs;
use crate::vui::{self, LayoutTree, UiBlocks, Widget, WidgetTree, widgets};

// -------------------------------------------------------------------------------------------------

/// Label used in the user interface for editing [`settings::Settings`].
///
/// TODO: Kept as “Options” rather than “Settings” for historical reasons.
/// Decide whether to change it.
pub(crate) const SETTINGS_LABEL: arcstr::ArcStr = literal!("Options");

/// Specify what shape of widgets [`settings_widgets()`] should produce.
#[derive(Clone, Copy, Debug)]
pub(crate) enum SettingsStyle {
    CompactRow,
    LabeledColumn,
}

/// Produce a row or column of widgets to change [`crate::settings`].
pub(crate) fn settings_widgets(
    read_ticket: ReadTicket<'_>,
    hud_inputs: &HudInputs,
    style: SettingsStyle,
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

    // TODO: add a way of sorting settings for display that isn’t just “the order they are defined”
    for key in Key::exhaust() {
        w.extend(setting_widget(read_ticket, hud_inputs, style, key));
    }

    w
}

/// Given a setting [`Key`], produce the widget for editing that setting,
/// or [`None`] if the setting should not be shown in this style.
///
/// * `read_ticket` must be a read ticket for the provided `HudBlocks`’s universe.
pub(crate) fn setting_widget(
    read_ticket: ReadTicket<'_>,
    // TODO: accept narrower needed pieces instead of HudInputs
    hud_inputs: &HudInputs,
    style: SettingsStyle,
    key: Key,
) -> Option<WidgetTree> {
    // TODO: Make this computed from the settings’ metadata instead of hardcoded.

    match key {
        Key::RenderMethod => Some(setting_enum_button(
            hud_inputs,
            style,
            literal!("Render Method"),
            settings::RENDER_METHOD,
            [camera::RenderMethod::Mesh, camera::RenderMethod::Reference],
        )),
        Key::Fog => Some(setting_enum_button(
            hud_inputs,
            style,
            literal!("Fog"),
            settings::FOG,
            [
                camera::FogOption::None,
                camera::FogOption::Abrupt,
                camera::FogOption::Compromise,
                camera::FogOption::Physical,
            ],
        )),
        // TODO: allow setting fov_y, tone_mapping, exposure, view_distance
        Key::FovY => None,
        Key::ToneMapping => None,
        Key::MaximumIntensity => None,
        Key::ExposureMode => None,
        Key::Exposure => None,
        Key::BloomIntensity => Some(setting_enum_button(
            hud_inputs,
            style,
            literal!("Bloom"),
            settings::BLOOM_INTENSITY,
            [zo32(0.0), zo32(0.03125), zo32(0.125)],
        )),
        Key::ViewDistance => None, // TODO
        Key::LightingDisplay => Some(setting_enum_button(
            hud_inputs,
            style,
            literal!("Light"),
            settings::LIGHTING_DISPLAY,
            [
                camera::LightingOption::None,
                camera::LightingOption::Flat,
                camera::LightingOption::Smooth,
                camera::LightingOption::Bounce,
            ],
        )),
        Key::Transparency => Some(setting_enum_button(
            hud_inputs,
            style,
            literal!("Transparency"),
            settings::TRANSPARENCY,
            [
                settings::TransparencyMode::Surface,
                settings::TransparencyMode::Volumetric,
                settings::TransparencyMode::Threshold,
            ],
        )),
        Key::TransparencyThreshold => None, // TODO
        Key::ShowUi => None, // not a user-facing setting until we add something like a “screenshot mode”
        Key::Antialiasing => Some(arb_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::AntialiasButtonLabel,
            |s| *s.get(settings::ANTIALIASING) != AntialiasingOption::None,
            |s, _v| {
                s.set(
                    settings::ANTIALIASING,
                    match s.get().get(settings::ANTIALIASING) {
                        AntialiasingOption::None => AntialiasingOption::Always,
                        AntialiasingOption::IfCheap => AntialiasingOption::None,
                        AntialiasingOption::Always => AntialiasingOption::None,
                        _ => AntialiasingOption::None,
                    },
                );
            },
        )),
        Key::DebugInfoText => Some(setting_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugInfoTextButtonLabel,
            settings::DEBUG_INFO_TEXT,
        )),
        Key::DebugInfoTextContents => Some(info_text_contents_flags_button(
            read_ticket,
            hud_inputs,
            style,
        )),
        Key::DebugBehaviors => Some(setting_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugBehaviorsButtonLabel,
            settings::DEBUG_BEHAVIORS,
        )),
        Key::DebugChunkBoxes => Some(setting_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugChunkBoxesButtonLabel,
            settings::DEBUG_CHUNK_BOXES,
        )),
        Key::DebugCollisionBoxes => Some(setting_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugCollisionBoxesButtonLabel,
            settings::DEBUG_COLLISION_BOXES,
        )),
        Key::DebugLightRaysAtCursor => Some(setting_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugLightRaysButtonLabel,
            settings::DEBUG_LIGHT_RAYS_AT_CURSOR,
        )),
        Key::DebugPixelCost => Some(setting_toggle_button(
            read_ticket,
            hud_inputs,
            style,
            UiBlocks::DebugPixelPerformanceButtonLabel,
            settings::DEBUG_PIXEL_COST,
        )),
        // Not a user-facing setting but really a debug tool.
        // TODO: it should be visible in a “really advanced settings” page
        Key::DebugReduceViewFrustum => None,
    }
}

fn info_text_contents_flags_button(
    _read_ticket: ReadTicket<'_>,
    hud_inputs: &HudInputs,
    style: SettingsStyle,
) -> Arc<LayoutTree<Arc<dyn Widget>>> {
    let key = settings::DEBUG_INFO_TEXT_CONTENTS;
    match style {
        // hidden entirely
        SettingsStyle::CompactRow => LayoutTree::spacer(vui::LayoutRequest::EMPTY),
        // TODO this should be wrapped and also have a label. maybe even its own page
        SettingsStyle::LabeledColumn => Arc::new(LayoutTree::Stack {
            direction: Face6::PX,
            children: ShowStatus::all()
                .iter_names()
                .map(|(bit_name, bit_mask)| {
                    let button = widgets::ToggleButton::new(
                        hud_inputs.settings.as_source(),
                        move |options| options.get(key).contains(bit_mask),
                        arcstr::ArcStr::from(bit_name),
                        &hud_inputs.hud_blocks.widget_theme,
                        {
                            let settings = hud_inputs.settings.clone();
                            move || {
                                settings.update(key, |current, _| current ^ bit_mask);
                            }
                        },
                    );
                    vui::leaf_widget(button)
                })
                .collect(),
        }),
    }
}

/// Generate a button that toggles a boolean that is read and written from the settings
/// using arbitrary functions.
fn arb_toggle_button(
    read_ticket: ReadTicket<'_>,
    hud_inputs: &HudInputs,
    style: SettingsStyle,
    icon_key: UiBlocks,
    getter: impl Fn(&settings::Data) -> bool + Clone + Send + Sync + 'static,
    setter: impl Fn(&settings::Settings, bool) + Clone + Send + Sync + 'static,
) -> WidgetTree {
    let icon = hud_inputs.hud_blocks.ui_blocks[icon_key].clone();
    let text: Option<widgets::Label> = match style {
        SettingsStyle::CompactRow => None,
        SettingsStyle::LabeledColumn => Some(
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
        hud_inputs.settings.as_source(),
        {
            let getter = getter.clone();
            move |data| getter(data)
        },
        label,
        &hud_inputs.hud_blocks.widget_theme,
        {
            let settings = hud_inputs.settings.clone();
            move || {
                setter(&settings, getter(&settings.get()));
            }
        },
    );
    vui::leaf_widget(button)
}

/// Generate a button that toggles a boolean setting.
fn setting_toggle_button(
    read_ticket: ReadTicket<'_>,
    hud_inputs: &HudInputs,
    style: SettingsStyle,
    icon_key: UiBlocks,
    key: &'static settings::TypedKey<bool>,
) -> WidgetTree {
    let icon = hud_inputs.hud_blocks.ui_blocks[icon_key].clone();
    let text: Option<widgets::Label> = match style {
        SettingsStyle::CompactRow => None,
        SettingsStyle::LabeledColumn => Some(
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
        hud_inputs.settings.as_source(),
        move |data| *data.get(key),
        label,
        &hud_inputs.hud_blocks.widget_theme,
        {
            let settings = hud_inputs.settings.clone();
            move || {
                key.write(&settings, !settings.get().get(key));
            }
        },
    );
    vui::leaf_widget(button)
}

/// Generate a group of buttons that set a setting to one of a set of values of an enum.
///
/// These buttons do not show up in [`SettingsStyle::CompactRow`].
fn setting_enum_button<T: Clone + fmt::Debug + PartialEq + Send + Sync + 'static>(
    hud_inputs: &HudInputs,
    style: SettingsStyle,
    label: arcstr::ArcStr,
    key: &'static settings::TypedKey<T>,
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
        SettingsStyle::CompactRow => LayoutTree::spacer(vui::LayoutRequest::EMPTY),
        SettingsStyle::LabeledColumn => Arc::new(LayoutTree::Stack {
            direction: Face6::PX,
            children: [label]
                .into_iter()
                .chain(list.into_iter().map(|value| {
                    let button = widgets::ToggleButton::new(
                        hud_inputs.settings.as_source(),
                        {
                            let value_to_compare = value.clone();
                            move |options| *options.get(key) == value_to_compare
                        },
                        arcstr::format!("{:?}", value), // TODO: quick kludge; need real labels
                        &hud_inputs.hud_blocks.widget_theme,
                        {
                            let settings = hud_inputs.settings.clone();
                            move || {
                                settings.set(key, value.clone());
                            }
                        },
                    );
                    vui::leaf_widget(button)
                }))
                .collect(),
        }),
    }
}

pub(crate) fn pause_toggle_button(hud_inputs: &HudInputs, style: SettingsStyle) -> Arc<dyn Widget> {
    widgets::ToggleButton::new(
        hud_inputs.paused.clone(),
        |&value| value,
        widgets::ButtonLabel {
            icon: Some(hud_inputs.hud_blocks.ui_blocks[UiBlocks::PauseButtonLabel].clone()),
            text: match style {
                SettingsStyle::CompactRow => None,
                SettingsStyle::LabeledColumn => Some(literal!("Pause").into()),
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
