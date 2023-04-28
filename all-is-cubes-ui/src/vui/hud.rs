use std::fmt;
use std::sync::{mpsc, Arc, Mutex};

use crate::apps::{ControlMessage, FullscreenSetter, FullscreenState};
use crate::vui::options::{graphics_options_widgets, pause_toggle_button};
use crate::vui::widgets::{self, Crosshair, TooltipState};
use crate::vui::{CueNotifier, LayoutTree, UiBlocks, VuiMessage, VuiPageState, Widget, WidgetTree};
use all_is_cubes::block::Block;
use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::character::Character;
use all_is_cubes::content::palette;
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::inv::Icons;
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::math::Face6;
use all_is_cubes::universe::{URef, Universe};
use all_is_cubes::util::YieldProgress;

pub(crate) use all_is_cubes::drawing::embedded_graphics::mono_font::iso_8859_1::FONT_8X13_BOLD as HudFont;

pub(crate) const TOOLBAR_POSITIONS: usize = 10;

/// Ad-hoc bundle of elements needed to construct HUD UI widgets.
///
/// TODO: Still looking for the right general abstraction here...
pub(crate) struct HudInputs {
    pub hud_blocks: Arc<HudBlocks>,
    pub cue_channel: CueNotifier,
    pub vui_control_channel: mpsc::SyncSender<VuiMessage>,
    pub app_control_channel: mpsc::SyncSender<ControlMessage>,
    pub graphics_options: ListenableSource<GraphicsOptions>,
    pub paused: ListenableSource<bool>,
    pub page_state: ListenableSource<VuiPageState>,
    pub mouselook_mode: ListenableSource<bool>,
    pub fullscreen_mode: ListenableSource<FullscreenState>,
    pub set_fullscreen: FullscreenSetter,
}

impl fmt::Debug for HudInputs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HudInputs").finish_non_exhaustive()
    }
}

#[allow(clippy::redundant_clone)]
pub(super) fn new_hud_widget_tree(
    // TODO: mess of tightly coupled parameters
    character_source: ListenableSource<Option<URef<Character>>>,
    hud_inputs: &HudInputs,
    // TODO: stop mutating the universe in widget construction
    universe: &mut Universe,
    tooltip_state: Arc<Mutex<TooltipState>>,
) -> WidgetTree {
    let toolbar: Arc<dyn Widget> = widgets::Toolbar::new(
        character_source,
        Arc::clone(&hud_inputs.hud_blocks),
        TOOLBAR_POSITIONS,
        universe,
        hud_inputs.cue_channel.clone(),
    );
    let tooltip: Arc<dyn Widget> = widgets::Tooltip::new(
        Arc::clone(&tooltip_state),
        hud_inputs.hud_blocks.clone(),
        universe,
    );
    let hud_widget_tree: WidgetTree = Arc::new(LayoutTree::Hud {
        crosshair: LayoutTree::leaf(Crosshair::new(
            hud_inputs.hud_blocks.blocks[UiBlocks::Crosshair].clone(),
            hud_inputs.mouselook_mode.clone(),
        )),
        toolbar: Arc::new(LayoutTree::Stack {
            direction: Face6::PY,
            children: vec![LayoutTree::leaf(toolbar), LayoutTree::leaf(tooltip)],
        }),
        control_bar: control_bar(hud_inputs),
    });
    hud_widget_tree
}

/// Miscellaneous controls (pause, debug, etc., not gameplay controls) intended to be
/// positioned in the top right corner.
pub(crate) fn control_bar(hud_inputs: &HudInputs) -> WidgetTree {
    let control_bar_widgets: WidgetTree = Arc::new(LayoutTree::Stack {
        direction: Face6::NX,
        children: vec![
            Arc::new(LayoutTree::Stack {
                direction: Face6::NX,
                children: graphics_options_widgets(hud_inputs),
            }),
            LayoutTree::leaf(about_button(hud_inputs)),
            LayoutTree::leaf(pause_toggle_button(hud_inputs)),
            LayoutTree::leaf(widgets::ToggleButton::new(
                hud_inputs.mouselook_mode.clone(),
                |&value| value,
                hud_inputs.hud_blocks.blocks[UiBlocks::MouselookButtonLabel].clone(),
                &hud_inputs.hud_blocks.blocks,
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
                LayoutTree::leaf(widgets::Frame::for_menu()),
                control_bar_widgets,
            ],
        })
    } else {
        control_bar_widgets
    }
}

pub(crate) fn about_button(hud_inputs: &HudInputs) -> Arc<dyn Widget> {
    widgets::ToggleButton::new(
        hud_inputs.page_state.clone(),
        |page_state| matches!(page_state, VuiPageState::AboutText),
        hud_inputs.hud_blocks.blocks[UiBlocks::AboutButtonLabel].clone(),
        &hud_inputs.hud_blocks.blocks,
        {
            let cc = hud_inputs.vui_control_channel.clone();
            move || {
                let _ignore_errors = cc.send(VuiMessage::About);
            }
        },
    )
}

// TODO: Unclear if HudBlocks should exist; maybe it should be reworked into a BlockProvider for widget graphics instead.
#[derive(Debug, Clone)]
pub(crate) struct HudBlocks {
    pub(crate) blocks: BlockProvider<UiBlocks>,
    pub(crate) icons: BlockProvider<Icons>,
    pub(crate) text: VoxelBrush<'static>,
}

impl HudBlocks {
    pub(crate) async fn new(universe: &mut Universe, p: YieldProgress) -> Self {
        let [p1, p2] = p.split(0.5);
        let ui_blocks = UiBlocks::new(universe, p1).await.install(universe).unwrap();
        let icons = Icons::new(universe, p2).await.install(universe).unwrap();

        let text_brush = VoxelBrush::new::<_, Block>([
            ([0, 0, 1], palette::HUD_TEXT_FILL.into()),
            ([1, 0, 0], palette::HUD_TEXT_STROKE.into()),
            ([-1, 0, 0], palette::HUD_TEXT_STROKE.into()),
            ([0, 1, 0], palette::HUD_TEXT_STROKE.into()),
            ([0, -1, 0], palette::HUD_TEXT_STROKE.into()),
        ]);

        Self {
            blocks: ui_blocks,
            icons,
            text: text_brush,
        }
    }
}
