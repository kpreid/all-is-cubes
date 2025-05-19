use alloc::sync::Arc;
use core::fmt;
use std::sync::Mutex;

use all_is_cubes::inv::Icons;
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::listen;
use all_is_cubes::math::Face6;
use all_is_cubes::universe::{ReadTicket, UniverseTransaction};
use all_is_cubes::util::YieldProgress;

use crate::apps::ControlMessage;
use crate::ui_content::options::{OptionsStyle, graphics_options_widgets, pause_toggle_button};
use crate::ui_content::pages::open_page_button;
use crate::ui_content::{CueNotifier, UiTargets, VuiMessage, VuiPageState};
use crate::vui::widgets::{self, TooltipState, WidgetBlocks};
use crate::vui::{self, LayoutTree, UiBlocks, Widget, WidgetTree};

pub(crate) const TOOLBAR_POSITIONS: u16 = 10;

/// Ad-hoc bundle of elements needed to construct HUD UI widgets.
///
/// TODO: Disentangle general UI from the concept of "HUD" — this is used for lots of things
/// that aren't HUD
pub(crate) struct HudInputs {
    // All the parts that are not the UI itself or created by it.
    pub(crate) base: UiTargets,

    pub hud_blocks: Arc<HudBlocks>,
    pub cue_channel: CueNotifier,
    pub vui_control_channel: flume::Sender<VuiMessage>,
    pub page_state: listen::DynSource<Arc<VuiPageState>>,
}

impl fmt::Debug for HudInputs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HudInputs").finish_non_exhaustive()
    }
}

// TODO: This is a convenience while we are still developing the UI.
// Ideally the final form would not include this `Deref` and would organize things differently.
impl core::ops::Deref for HudInputs {
    type Target = UiTargets;
    fn deref(&self) -> &Self::Target {
        &self.base
    }
}

pub(super) fn new_hud_page(
    read_ticket: ReadTicket<'_>,
    hud_inputs: &HudInputs,
    tooltip_state: Arc<Mutex<TooltipState>>,
) -> vui::Page {
    let character_source = hud_inputs.character_source.clone();
    let toolbar = widgets::Toolbar::new(
        character_source,
        Arc::clone(&hud_inputs.hud_blocks),
        0..TOOLBAR_POSITIONS,
        hud_inputs.cue_channel.clone(),
    );
    let tooltip = widgets::Tooltip::new(tooltip_state, hud_inputs.hud_blocks.clone());
    let hud_widget_tree: WidgetTree = Arc::new(LayoutTree::Hud {
        crosshair: vui::leaf_widget(widgets::Crosshair::new(
            hud_inputs.hud_blocks.widget_theme.widget_blocks[WidgetBlocks::Crosshair].clone(),
            hud_inputs.mouselook_mode.clone(),
        )),
        toolbar: Arc::new(LayoutTree::Stack {
            direction: Face6::PY,
            children: vec![vui::leaf_widget(toolbar), vui::leaf_widget(tooltip)],
        }),
        control_bar: control_bar(read_ticket, hud_inputs),
    });
    vui::Page {
        tree: hud_widget_tree,
        layout: vui::PageLayout::Hud,
        focus_on_ui: false,
    }
}

/// Miscellaneous controls (pause, debug, etc., not gameplay controls) intended to be
/// positioned in the top right corner.
pub(crate) fn control_bar(read_ticket: ReadTicket<'_>, hud_inputs: &HudInputs) -> WidgetTree {
    let control_bar_widgets: WidgetTree = Arc::new(LayoutTree::Stack {
        direction: Face6::NX,
        children: vec![
            Arc::new(LayoutTree::Stack {
                direction: Face6::NX,
                children: graphics_options_widgets(
                    read_ticket,
                    hud_inputs,
                    OptionsStyle::CompactRow,
                ),
            }),
            vui::leaf_widget(open_page_button(
                hud_inputs,
                VuiPageState::AboutText,
                hud_inputs.hud_blocks.ui_blocks[UiBlocks::AboutButtonLabel].clone(),
            )),
            vui::leaf_widget(pause_toggle_button(hud_inputs, OptionsStyle::CompactRow)),
            vui::leaf_widget(save_button(hud_inputs)),
            vui::leaf_widget(widgets::ToggleButton::new(
                hud_inputs.mouselook_mode.clone(),
                |&value| value,
                hud_inputs.hud_blocks.ui_blocks[UiBlocks::MouselookButtonLabel].clone(),
                &hud_inputs.hud_blocks.widget_theme,
                {
                    let cc = hud_inputs.app_control_channel.clone();
                    move || {
                        let _ignore_errors = cc.send(ControlMessage::ToggleMouselook);
                    }
                },
            )),
        ],
    });
    if false {
        // reveal the bounds by adding a widgets::Frame
        Arc::new(LayoutTree::Stack {
            direction: Face6::PZ,
            children: vec![
                vui::leaf_widget(hud_inputs.hud_blocks.widget_theme.dialog_background()),
                control_bar_widgets,
            ],
        })
    } else {
        control_bar_widgets
    }
}

fn save_button(hud_inputs: &HudInputs) -> Arc<dyn Widget> {
    let cc = hud_inputs.app_control_channel.clone();
    widgets::ActionButton::new(
        hud_inputs.hud_blocks.ui_blocks[UiBlocks::SaveButtonLabel].clone(),
        &hud_inputs.hud_blocks.widget_theme,
        move || {
            let _ignore_errors = cc.send(ControlMessage::Save);
        },
    )
}

// TODO: Unclear if HudBlocks should exist; maybe it should be reworked into a BlockProvider for widget graphics instead.
#[derive(Debug, Clone)]
pub(crate) struct HudBlocks {
    pub(crate) widget_theme: widgets::WidgetTheme,
    pub(crate) ui_blocks: BlockProvider<UiBlocks>,
    pub(crate) icons: BlockProvider<Icons>,
}

impl HudBlocks {
    pub(crate) async fn new(
        read_ticket: ReadTicket<'_>,
        txn: &mut UniverseTransaction,
        p: YieldProgress,
    ) -> Self {
        let [p12, p3] = p.split(0.667);
        let [p1, p2] = p12.split(0.5);
        let widget_theme = widgets::WidgetTheme::new(read_ticket, txn, p1)
            .await
            .unwrap();
        let ui_blocks = UiBlocks::new(txn, p2)
            .await
            .install(read_ticket, txn)
            .unwrap();
        let icons = Icons::new(txn, p3).await.install(read_ticket, txn).unwrap();
        Self {
            widget_theme,
            ui_blocks,
            icons,
        }
    }
}
