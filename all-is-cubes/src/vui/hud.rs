use std::fmt;
use std::sync::{mpsc, Arc, Mutex};

use cgmath::Vector2;

use crate::apps::{ControlMessage, FullscreenSetter, FullscreenState};
use crate::block::{Block, AIR};
use crate::camera::{GraphicsOptions, Viewport};
use crate::character::Character;
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::inv::Icons;
use crate::linking::BlockProvider;
use crate::listen::ListenableSource;
use crate::math::{Face6, FreeCoordinate, GridAab, GridCoordinate, Rgba};
use crate::space::{Space, SpacePhysics};
use crate::universe::{URef, Universe};
use crate::util::YieldProgress;
use crate::vui::options::{graphics_options_widgets, pause_toggle_button};
use crate::vui::widgets::{self, Crosshair, TooltipState};
use crate::vui::{CueNotifier, LayoutTree, UiBlocks, VuiMessage, VuiPageState, Widget, WidgetTree};

pub(crate) use embedded_graphics::mono_font::iso_8859_1::FONT_8X13_BOLD as HudFont;

/// Knows where and how to place graphics within the HUD space, but does not store
/// the space or any related state itself; depends only on the screen size and other
/// parameters not primarily dependent on user interaction. This split is intended to
/// simplify the problem of adapting to size changes.
///
/// TODO: Since introducing widgets, `HudLayout` does much less work. Think about whether it should exist.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct HudLayout {
    size: Vector2<GridCoordinate>,
    pub(crate) toolbar_positions: usize,
}

impl HudLayout {
    pub(crate) const DEPTH_BEHIND_VIEW_PLANE: GridCoordinate = 5;

    /// Construct `HudLayout` with a size that suits the given viewport
    /// (based on pixel resolution and aspect ratio)
    pub fn new(viewport: Viewport) -> Self {
        // Note: Dimensions are enforced to be odd so that the crosshair can work.
        // The toolbar is also designed to be odd width when it has an even number of positions.
        let width = 25;
        // we want to ceil() the height because the camera setup makes the height match
        // the viewport and ignores width, so we want to prefer too-narrow over too-wide
        let height = ((FreeCoordinate::from(width) / viewport.nominal_aspect_ratio()).ceil()
            as GridCoordinate)
            .max(8);
        let height = height / 2 * 2 + 1; // ensure odd
        Self {
            size: Vector2::new(width, height),
            toolbar_positions: 10,
        }
    }

    pub(crate) fn bounds(&self) -> GridAab {
        GridAab::from_lower_upper(
            (0, 0, -Self::DEPTH_BEHIND_VIEW_PLANE),
            (self.size.x, self.size.y, 5),
        )
    }

    /// Create a new space with the size controlled by this layout,
    /// and a standard lighting condition.
    // TODO: validate this doesn't crash on wonky sizes.
    pub(crate) fn new_space(&self) -> Space {
        let Vector2 { x: w, y: h } = self.size;
        let bounds = self.bounds();
        let mut space = Space::builder(bounds)
            .physics(SpacePhysics {
                sky_color: palette::HUD_SKY,
                ..SpacePhysics::default()
            })
            .build();

        if false {
            // Visualization of the bounds of the space we're drawing.
            let mut add_frame = |z, color| {
                let frame_block = Block::from(color);
                space
                    .fill_uniform(GridAab::from_lower_size([0, 0, z], [w, h, 1]), frame_block)
                    .unwrap();
                space
                    .fill_uniform(GridAab::from_lower_size([1, 1, z], [w - 2, h - 2, 1]), &AIR)
                    .unwrap();
            };
            add_frame(bounds.lower_bounds().z, Rgba::new(0.5, 0., 0., 1.));
            add_frame(-1, Rgba::new(0.5, 0.5, 0.5, 1.));
            add_frame(bounds.upper_bounds().z - 1, Rgba::new(0., 1., 1., 1.));
        }

        space
    }
}

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

#[allow(clippy::too_many_arguments, clippy::redundant_clone)]
pub(super) fn new_hud_widget_tree(
    // TODO: terrible mess of tightly coupled parameters
    character_source: ListenableSource<Option<URef<Character>>>,
    hud_inputs: &HudInputs,
    hud_layout: &HudLayout,
    // TODO: stop mutating the universe in widget construction
    universe: &mut Universe,
    tooltip_state: Arc<Mutex<TooltipState>>,
) -> WidgetTree {
    let toolbar: Arc<dyn Widget> = widgets::Toolbar::new(
        character_source,
        Arc::clone(&hud_inputs.hud_blocks),
        hud_layout.toolbar_positions,
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
            LayoutTree::leaf(widgets::ToggleButton::new(
                hud_inputs.page_state.clone(),
                |page_state| matches!(page_state, VuiPageState::AboutText),
                |state| hud_inputs.hud_blocks.blocks[UiBlocks::AboutButton(state)].clone(),
                {
                    let cc = hud_inputs.vui_control_channel.clone();
                    move || {
                        let _ignore_errors = cc.send(VuiMessage::About);
                    }
                },
            )),
            LayoutTree::leaf(pause_toggle_button(hud_inputs)),
            LayoutTree::leaf(widgets::ToggleButton::new(
                hud_inputs.mouselook_mode.clone(),
                |&value| value,
                |state| hud_inputs.hud_blocks.blocks[UiBlocks::MouselookButton(state)].clone(),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hud_layout_sizes() {
        let cases: Vec<([u32; 2], [i32; 2])> =
            vec![([800, 600], [25, 19]), ([1000, 600], [25, 15])];
        let mut failed = 0;
        for (nominal_viewport, expected_size) in cases {
            let actual_size =
                HudLayout::new(Viewport::with_scale(1.0, nominal_viewport.into())).size;
            let actual_size: [i32; 2] = actual_size.into();
            if actual_size != expected_size {
                println!("{nominal_viewport:?} expected to produce {expected_size:?}; got {actual_size:?}");
                failed += 1;
            }
        }
        if failed > 0 {
            panic!("{failed} cases failed");
        }
    }
}
