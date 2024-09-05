//! Contents of various UI pages.

use alloc::sync::Arc;

use all_is_cubes::arcstr::{literal, ArcStr};
use all_is_cubes::block::Resolution::*;
use all_is_cubes::euclid::size3;
use all_is_cubes::math::Face6;
use all_is_cubes::universe::Universe;

use crate::logo::logo_text;
use crate::ui_content::hud::HudInputs;
use crate::ui_content::options::{graphics_options_widgets, pause_toggle_button, OptionsStyle};
use crate::ui_content::{notification, VuiMessage, VuiPageState};
use crate::vui::widgets::ButtonLabel;
use crate::vui::{self, parts, widgets, InstallVuiError, LayoutTree, UiBlocks, Widget, WidgetTree};

// TODO: Disentangle general UI from the concept of "HUD" — i.e. the input accepted should be
// not a `HudInputs` should become less specific, since this isn't actually part of the HUD.
pub(super) fn new_paused_page(
    u: &mut Universe,
    hud_inputs: &HudInputs,
) -> Result<vui::Page, InstallVuiError> {
    use parts::{heading, shrink};

    let mut children = vec![
        // TODO: establish standard resolutions for logo etc
        vui::leaf_widget(shrink(u, R16, &vui::leaf_widget(logo_text()))?),
        heading("Paused"),
        vui::leaf_widget(open_page_button(
            hud_inputs,
            VuiPageState::AboutText,
            ButtonLabel {
                icon: Some(hud_inputs.hud_blocks.ui_blocks[UiBlocks::AboutButtonLabel].clone()),
                text: Some(literal!("About").into()),
            },
        )),
        vui::leaf_widget(open_page_button(
            hud_inputs,
            VuiPageState::Options,
            ButtonLabel {
                icon: Some(hud_inputs.hud_blocks.ui_blocks[UiBlocks::OptionsButtonLabel].clone()),
                text: Some(literal!("Options").into()),
            },
        )),
        vui::leaf_widget(pause_toggle_button(hud_inputs, OptionsStyle::LabeledColumn)),
    ];
    if let Some(quit_fn) = hud_inputs.quit.clone() {
        children.push(vui::leaf_widget(widgets::ActionButton::new(
            literal!("Quit"),
            &hud_inputs.hud_blocks.widget_theme,
            // TODO: quit_fn should be an async function, but we don't have a way to
            // kick off a “Quitting...” task yet.
            move || match quit_fn() {
                #[allow(unreachable_patterns, reason = "TODO: Remove this arm after Rust 1.82")]
                Ok(s) => match s {},
                Err(_cancelled) => {

                    // TODO: display message indicating failure
                }
            },
        )))
    }
    let contents = Arc::new(LayoutTree::Stack {
        direction: Face6::NY,
        children,
    });
    Ok(vui::Page::new_modal_dialog(
        &hud_inputs.hud_blocks.widget_theme,
        contents,
    ))
}

pub(super) fn new_progress_page(
    theme: &widgets::WidgetTheme,
    hub: &notification::Hub,
) -> vui::Page {
    let children = vec![
        // impose desired width. TODO: better way to add a constraint
        LayoutTree::spacer(vui::LayoutRequest {
            minimum: size3(10, 0, 0),
        }),
        vui::leaf_widget(widgets::ProgressBar::new(
            theme,
            Face6::PX,
            hub.primary_progress(),
        )),
    ];

    // TODO: should have at least a title giving context
    let contents = Arc::new(LayoutTree::Stack {
        direction: Face6::NY,
        children,
    });
    vui::Page::new_modal_dialog(theme, contents)
}

pub(super) fn new_options_widget_tree(
    u: &mut Universe,
    hud_inputs: &HudInputs,
) -> Result<vui::Page, InstallVuiError> {
    use parts::{heading, shrink};

    let contents = Arc::new(LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            vui::leaf_widget(shrink(u, R32, &vui::leaf_widget(logo_text()))?),
            heading("Options"),
            back_button(hud_inputs),
            Arc::new(LayoutTree::Stack {
                direction: Face6::NY,
                children: graphics_options_widgets(hud_inputs, OptionsStyle::LabeledColumn),
            }),
        ],
    });
    Ok(vui::Page::new_modal_dialog(
        &hud_inputs.hud_blocks.widget_theme,
        contents,
    ))
}

/// TODO: The content of the about page should be customizable in the final build or
/// by configuration of the [`Session`].
pub(super) fn new_about_page(
    u: &mut Universe,
    hud_inputs: &HudInputs,
) -> Result<vui::Page, InstallVuiError> {
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
            vui::leaf_widget(shrink(u, R8, &vui::leaf_widget(logo_text()))?),
            back_button(hud_inputs),
            heading("Controls"),
            paragraph(controls_text),
            heading("About"),
            paragraph(about_text),
            // heading("License"),
            // paragraph("TODO"),
        ],
    });

    Ok(vui::Page::new_modal_dialog(
        &hud_inputs.hud_blocks.widget_theme,
        contents,
    ))
}

/// A message in a "modal dialog box".
pub(super) fn new_message_page(message: ArcStr, hud_inputs: &HudInputs) -> vui::Page {
    use parts::paragraph;

    let contents = Arc::new(LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![paragraph(message), back_button(hud_inputs)],
    });

    vui::Page::new_modal_dialog(&hud_inputs.hud_blocks.widget_theme, contents)
}

/// Make a button that sends [`VuiMessage::Open`].
pub(crate) fn open_page_button(
    hud_inputs: &HudInputs,
    page: VuiPageState,
    label: impl Into<ButtonLabel>,
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
        label.into(),
        &hud_inputs.hud_blocks.widget_theme,
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
    vui::leaf_widget(widgets::ActionButton::new(
        hud_inputs.hud_blocks.ui_blocks[UiBlocks::BackButtonLabel].clone(),
        &hud_inputs.hud_blocks.widget_theme,
        {
            let cc = hud_inputs.vui_control_channel.clone();
            move || {
                let _ignore_errors = cc.send(VuiMessage::Back);
            }
        },
    ))
}
