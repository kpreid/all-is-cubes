use alloc::sync::Arc;

use all_is_cubes::block;
use all_is_cubes::camera::{AntialiasingOption, GraphicsOptions};
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::{mono_font::iso_8859_1 as font, text::TextStyle};
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::math::Face6;
use all_is_cubes::space::{SpaceBuilder, SpacePhysics};
use all_is_cubes::universe::{Name, URef};

use crate::apps::ControlMessage;
use crate::ui_content::hud::HudInputs;
use crate::vui::{self, widgets};
use crate::vui::{LayoutTree, UiBlocks, Widget, WidgetTree};

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
            hud_inputs.hud_blocks.blocks[UiBlocks::FullscreenButtonLabel].clone(),
            &hud_inputs.hud_blocks.blocks,
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
    let icon = hud_inputs.hud_blocks.blocks[icon_key].clone();
    let text_label = String::from(icon.evaluate().unwrap().attributes.display_name);
    let button: Arc<dyn Widget> = widgets::ToggleButton::new(
        hud_inputs.graphics_options.clone(),
        getter,
        icon,
        &hud_inputs.hud_blocks.blocks,
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

    match style {
        OptionsStyle::CompactRow => LayoutTree::leaf(button),
        OptionsStyle::LabeledColumn => Arc::new(LayoutTree::Stack {
            direction: Face6::PX,
            children: vec![LayoutTree::leaf(button), {
                // TODO: extract this for general use and reconcile with pages::parts::shrink()
                let text: WidgetTree = LayoutTree::leaf(Arc::new(widgets::LargeText {
                    text: text_label.into(),
                    font: || &font::FONT_6X10,
                    brush: VoxelBrush::single(block::Block::from(palette::ALMOST_BLACK)),
                    text_style: TextStyle::default(),
                }));
                let space = text
                    .to_space(
                        SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
                        vui::Gravity::new(vui::Align::Low, vui::Align::Center, vui::Align::Low),
                    )
                    .unwrap();
                LayoutTree::leaf(Arc::new(widgets::Voxels::new(
                    space.bounds(),
                    // TODO: Using a pending ref is working only by accident here.
                    // The space is never actually inserted, so `Anonym(0)` is never
                    // rejected as it should be, and only this widget itself is keeping
                    // the space alive -- but we don't yet have the ability to ask for
                    // an anonymous pending ref and actually insert it.
                    URef::new_pending(Name::Anonym(0), space),
                    block::Resolution::R32,
                    block::BlockAttributes::default(),
                )))
            }],
        }),
    }
}

pub(crate) fn pause_toggle_button(hud_inputs: &HudInputs) -> Arc<dyn Widget> {
    widgets::ToggleButton::new(
        hud_inputs.paused.clone(),
        |&value| value,
        hud_inputs.hud_blocks.blocks[UiBlocks::PauseButtonLabel].clone(),
        &hud_inputs.hud_blocks.blocks,
        {
            let cc = hud_inputs.app_control_channel.clone();
            move || {
                let _ignore_errors = cc.send(ControlMessage::TogglePause);
            }
        },
    )
}
