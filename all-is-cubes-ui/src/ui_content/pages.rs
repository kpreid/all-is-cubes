//! Contents of various UI pages.

use std::sync::Arc;

use all_is_cubes::block::{Block, Resolution::*};
use all_is_cubes::math::Face6;
use all_is_cubes::universe::Universe;

use crate::logo::logo_text;
use crate::ui_content::hud::HudInputs;
use crate::ui_content::options::{graphics_options_widgets, pause_toggle_button, OptionsStyle};
use crate::ui_content::{VuiMessage, VuiPageState};
use crate::vui::{
    page_modal_backdrop, parts, widgets, InstallVuiError, LayoutTree, UiBlocks, Widget, WidgetTree,
};

// TODO: Disentangle general UI from the concept of "HUD" — i.e. the input accepted should be
// not a `HudInputs` should become less specific, since this isn't actually part of the HUD.
pub(super) fn new_paused_widget_tree(
    u: &mut Universe,
    hud_inputs: &HudInputs,
) -> Result<WidgetTree, InstallVuiError> {
    use parts::{heading, shrink};

    let contents = Arc::new(LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            // TODO: establish standard resolutions for logo etc
            LayoutTree::leaf(shrink(u, R16, LayoutTree::leaf(logo_text()))?),
            LayoutTree::leaf(shrink(u, R32, heading("Paused"))?),
            LayoutTree::leaf(open_page_button(
                hud_inputs,
                VuiPageState::AboutText,
                hud_inputs.hud_blocks.blocks[UiBlocks::AboutButtonLabel].clone(),
            )),
            LayoutTree::leaf(open_page_button(
                hud_inputs,
                VuiPageState::Options,
                hud_inputs.hud_blocks.blocks[UiBlocks::OptionsButtonLabel].clone(),
            )),
            LayoutTree::leaf(pause_toggle_button(hud_inputs)),
        ],
    });
    Ok(page_modal_backdrop(Arc::new(LayoutTree::Shrink(
        hud_inputs
            .hud_blocks
            .dialog_background()
            .as_background_of(contents),
    ))))
}

pub(super) fn new_options_widget_tree(
    u: &mut Universe,
    hud_inputs: &HudInputs,
) -> Result<WidgetTree, InstallVuiError> {
    use parts::{heading, shrink};

    let contents = Arc::new(LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            LayoutTree::leaf(shrink(u, R32, LayoutTree::leaf(logo_text()))?),
            LayoutTree::leaf(shrink(u, R32, heading("Options"))?),
            back_button(hud_inputs),
            Arc::new(LayoutTree::Stack {
                direction: Face6::NY,
                children: graphics_options_widgets(hud_inputs, OptionsStyle::LabeledColumn),
            }),
        ],
    });
    Ok(page_modal_backdrop(Arc::new(LayoutTree::Shrink(
        hud_inputs
            .hud_blocks
            .dialog_background()
            .as_background_of(contents),
    ))))
}

/// TODO: The content of the about page should be customizable in the final build or
/// by configuration of the [`Session`].
pub(super) fn new_about_widget_tree(
    u: &mut Universe,
    hud_inputs: &HudInputs,
) -> Result<WidgetTree, InstallVuiError> {
    use parts::{heading, paragraph, shrink};

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

    let contents = Arc::new(LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            LayoutTree::leaf(shrink(u, R8, LayoutTree::leaf(logo_text()))?),
            back_button(hud_inputs),
            LayoutTree::leaf(shrink(u, R32, heading("Controls"))?),
            LayoutTree::leaf(shrink(u, R32, paragraph(controls_text))?),
            LayoutTree::leaf(shrink(u, R32, heading("About"))?),
            LayoutTree::leaf(shrink(u, R32, paragraph(about_text))?),
            // LayoutTree::leaf(shrink(u, R32, heading("License"))?),
            // LayoutTree::leaf(shrink(u, R32, paragraph("TODO"))?),
        ],
    });
    Ok(page_modal_backdrop(Arc::new(LayoutTree::Shrink(
        hud_inputs
            .hud_blocks
            .dialog_background()
            .as_background_of(contents),
    ))))
}

/// Make a button that sends [`VuiMessage::Open`].
pub(crate) fn open_page_button(
    hud_inputs: &HudInputs,
    page: VuiPageState,
    label: Block,
) -> Arc<dyn Widget> {
    // TODO: For some purposes this should not be a toggle button, and for some it should,
    // depending on whether the outcome is still having such a button. Even then, it should
    // be some button that communicates “pressing again will not turn this off”.
    widgets::ToggleButton::new(
        hud_inputs.page_state.clone(),
        {
            let page = page.clone();
            move |page_state| *page_state == page
        },
        label,
        &hud_inputs.hud_blocks.blocks,
        {
            let cc = hud_inputs.vui_control_channel.clone();
            move || {
                let _ignore_errors = cc.send(VuiMessage::Open(page.clone()));
            }
        },
    )
}

// TODO: find a better place for this to live -- it's page-related but not a page.
/// A one-cube action button which performs the VUI 'back' action.
pub(crate) fn back_button(hud_inputs: &HudInputs) -> WidgetTree {
    // TODO: define a narrower set of inputs than HudInputs
    // TODO: this function should maybe live in a 'UI mid-level components' module?
    LayoutTree::leaf(widgets::ActionButton::new(
        hud_inputs.hud_blocks.blocks[UiBlocks::BackButtonLabel].clone(),
        &hud_inputs.hud_blocks.blocks,
        {
            let cc = hud_inputs.vui_control_channel.clone();
            move || {
                let _ignore_errors = cc.send(VuiMessage::Back);
            }
        },
    ))
}
