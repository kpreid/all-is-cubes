use std::sync::Arc;

use crate::block::Block;
use crate::math::{Face6, GridVector, Rgba};
use crate::space::Space;
use crate::universe::{URef, Universe};
use crate::vui::hud::{HudInputs, HudLayout};
use crate::vui::options::pause_toggle_button;
use crate::vui::widgets::FrameWidget;
use crate::vui::{install_widgets, LayoutGrant, LayoutRequest, LayoutTree, Widget, WidgetTree};

/// Pair of a widget tree and a cached space it is instantiated in with a particular size,
/// which can be recreated with a different size as needed.
///
/// TODO: Give this a better name.
#[derive(Clone, Debug)]
pub(crate) struct PageInst {
    tree: WidgetTree,
    space: Option<URef<Space>>,
}

impl PageInst {
    pub fn new(tree: WidgetTree) -> Self {
        Self { tree, space: None }
    }

    // TODO: Disentangle this from the concept of "HUD" — i.e. the input should be
    // not a `HudLayout` or `HudLayout` should become less specific.
    pub fn get_or_create_space(
        &mut self,
        layout: &HudLayout,
        universe: &mut Universe,
    ) -> URef<Space> {
        if let Some(space) = self.space.as_ref() {
            if space.borrow().bounds() == layout.bounds() {
                return space.clone();
            }
        }

        // Size didn't match, so recreate the space.
        // TODO: Resize in-place instead.
        let space = universe.insert_anonymous(layout.new_space());
        // TODO: error handling for layout
        space
            .execute(
                &install_widgets(LayoutGrant::new(layout.bounds()), &self.tree)
                    .expect("layout/widget error"),
            )
            .expect("transaction error");

        // Initialize lighting
        space
            .try_modify(|space| {
                space.fast_evaluate_light();
                space.evaluate_light(10, |_| {});
            })
            .unwrap();

        self.space = Some(space.clone());
        space
    }
}

// TODO: Disentangle general UI from the concept of "HUD" — i.e. the input accepted should be
// not a `HudInputs` should become less specific, since this isn't actually part of the HUD.
pub(super) fn new_paused_widget_tree(hud_inputs: &HudInputs) -> WidgetTree {
    Arc::new(LayoutTree::Stack {
        direction: Face6::PZ,
        children: vec![
            // TODO: have a better way to communicate our choice of "baseline" alignment
            Arc::new(LayoutTree::Spacer(LayoutRequest {
                // magic number 2 allows us to fill the edges of the viewport, ish
                // TODO: HudLayout should give us the option of "overscan"
                minimum: GridVector::new(0, 0, HudLayout::DEPTH_BEHIND_VIEW_PLANE + 2),
            })),
            LayoutTree::leaf(
                FrameWidget::with_block(Block::from(Rgba::new(0., 0., 0., 0.7))) as Arc<dyn Widget>,
            ),
            LayoutTree::leaf(pause_toggle_button(hud_inputs)),
        ],
    })
}
