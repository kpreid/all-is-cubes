use std::borrow::Cow;
use std::sync::Arc;

use embedded_graphics::mono_font::iso_8859_1 as font;
use embedded_graphics::text::TextStyle;

use crate::block::{
    Block, BlockAttributes,
    Resolution::{self, *},
};
use crate::content::logo::logo_text;
use crate::drawing::VoxelBrush;
use crate::math::{Face6, GridVector, Rgba};
use crate::space::{Space, SpaceBuilder, SpacePhysics};
use crate::universe::{URef, Universe};
use crate::vui::hud::{HudInputs, HudLayout};
use crate::vui::options::pause_toggle_button;
use crate::vui::widgets::{self, FrameWidget};
use crate::vui::{
    install_widgets, Align, Gravity, InstallVuiError, LayoutGrant, LayoutRequest, LayoutTree,
    Widget, WidgetTree,
};

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

fn page_modal_backdrop(foreground: WidgetTree) -> WidgetTree {
    Arc::new(LayoutTree::Stack {
        direction: Face6::PZ,
        children: vec![
            // TODO: have a better way to communicate our choice of "baseline" alignment
            Arc::new(LayoutTree::Spacer(LayoutRequest {
                // magic number 2 allows us to fill the edges of the viewport, ish
                // TODO: VUI camera positioning should give us the option of "overscan",
                // where all edges of the space spill off the window.
                minimum: GridVector::new(0, 0, HudLayout::DEPTH_BEHIND_VIEW_PLANE + 2),
            })),
            LayoutTree::leaf(
                FrameWidget::with_block(Block::from(Rgba::new(0., 0., 0., 0.7))) as Arc<dyn Widget>,
            ),
            foreground,
        ],
    })
}

// TODO: Disentangle general UI from the concept of "HUD" — i.e. the input accepted should be
// not a `HudInputs` should become less specific, since this isn't actually part of the HUD.
pub(super) fn new_paused_widget_tree(hud_inputs: &HudInputs) -> WidgetTree {
    page_modal_backdrop(LayoutTree::leaf(pause_toggle_button(hud_inputs)))
}

/// TODO: The content of the about page should be customizable in the final build or
/// by configuration of the [`Session`].
pub(super) fn new_about_widget_tree(
    universe: &mut Universe,
) -> Result<WidgetTree, InstallVuiError> {
    // TODO: refactor this into something reusable since it will be a key element of any
    // UI with labeled things.
    let mut shrink =
        |resolution: Resolution, large: WidgetTree| -> Result<Arc<dyn Widget>, InstallVuiError> {
            let space = large.to_space(
                SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
                Gravity::new(Align::Center, Align::Center, Align::Low),
            )?;
            Ok(Arc::new(widgets::Voxels::new(
                space.bounds(),
                universe.insert_anonymous(space),
                resolution,
                BlockAttributes::default(),
            )))
        };

    fn heading(text: impl Into<Cow<'static, str>>) -> WidgetTree {
        LayoutTree::leaf(Arc::new(widgets::LargeText {
            text: text.into(),
            font: || &font::FONT_9X15_BOLD,
            brush: VoxelBrush::single(Block::from(Rgba::WHITE)),
            text_style: TextStyle::default(),
        }))
    }

    fn paragraph(text: impl Into<Cow<'static, str>>) -> WidgetTree {
        LayoutTree::leaf(Arc::new(widgets::LargeText {
            text: text.into(),
            font: || &font::FONT_6X10,
            brush: VoxelBrush::single(Block::from(Rgba::WHITE)),
            text_style: TextStyle::default(),
        }))
    }

    let controls_text = indoc::indoc! {"
        W A S D    movement
          E C      fly up/down (requires jetpack item)
        Arrows     turn
           L       toggle mouselook
          0-9      select items on toolbar
      Left mouse   use first toolbar item
      Right mouse  use selected toolbar item
           P       toggle pause
        Escape     toggle pause; exit menu
    "};

    let about_text = String::from(indoc::indoc! {r#"
                    https://github.com/kpreid/all-is-cubes/
        All is Cubes is a game-or-engine about building things out of voxels,
        which I've been working on as a hobby since 2020. It's intended to be
        a flexible and "self-hosting" system where everything can be edited
        interactively (but it's not there yet, because I'm still building the
        user interface architecture).

    "#}) + env!("CARGO_PKG_VERSION");

    Ok(page_modal_backdrop(Arc::new(LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            LayoutTree::leaf(shrink(R8, LayoutTree::leaf(logo_text()))?),
            LayoutTree::leaf(shrink(R32, heading("Controls"))?),
            LayoutTree::leaf(shrink(R32, paragraph(controls_text))?),
            LayoutTree::leaf(shrink(R32, heading("About"))?),
            LayoutTree::leaf(shrink(R32, paragraph(about_text))?),
            // LayoutTree::leaf(shrink(R32, heading("License"))?),
            // LayoutTree::leaf(shrink(R32, paragraph("TODO"))?),
        ],
    })))
}
