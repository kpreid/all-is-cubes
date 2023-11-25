use core::fmt;

use exhaust::Exhaust;

use all_is_cubes::drawing::embedded_graphics::mono_font::iso_8859_1 as font;
use all_is_cubes::include_image;
use all_is_cubes::linking::{BlockModule, BlockProvider};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::vui::widgets::{make_button_label_block, ButtonIcon};

/// Blocks that are used as part of the UI content.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[doc(hidden)] // public for testing only
#[non_exhaustive]
pub enum UiBlocks {
    /// Label of the action button for navigating “back” in the user interface (closing
    /// dialogs, etc).
    BackButtonLabel,

    AboutButtonLabel,
    PauseButtonLabel,
    SaveButtonLabel,
    QuitButtonLabel,
    OptionsButtonLabel,
    MouselookButtonLabel,
    FullscreenButtonLabel,
    AntialiasButtonLabel,
    DebugInfoTextButtonLabel,
    DebugBehaviorsButtonLabel,
    DebugChunkBoxesButtonLabel,
    DebugCollisionBoxesButtonLabel,
    DebugLightRaysButtonLabel,
}

impl BlockModule for UiBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/vui/content-blocks"
    }
}

// TODO: convert this impl back to `strum` or something
impl fmt::Display for UiBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UiBlocks::BackButtonLabel => write!(f, "back-button"),
            UiBlocks::AboutButtonLabel => write!(f, "about-button"),
            UiBlocks::PauseButtonLabel => write!(f, "pause-button"),
            UiBlocks::SaveButtonLabel => write!(f, "save-button"),
            UiBlocks::QuitButtonLabel => write!(f, "quit-button"),
            UiBlocks::OptionsButtonLabel => write!(f, "options-button"),
            UiBlocks::MouselookButtonLabel => write!(f, "mouselook-button"),
            UiBlocks::FullscreenButtonLabel => write!(f, "fullscreen-button"),
            UiBlocks::AntialiasButtonLabel => write!(f, "antialias-button"),
            UiBlocks::DebugInfoTextButtonLabel => write!(f, "debug-info-text-button"),
            UiBlocks::DebugBehaviorsButtonLabel => write!(f, "debug-behaviors-button"),
            UiBlocks::DebugChunkBoxesButtonLabel => {
                write!(f, "debug-chunk-boxes-button")
            }
            UiBlocks::DebugCollisionBoxesButtonLabel => {
                write!(f, "debug-collision-boxes-button")
            }
            UiBlocks::DebugLightRaysButtonLabel => write!(f, "debug-light-rays-button"),
        }
    }
}

impl UiBlocks {
    pub async fn new(universe: &mut Universe, p: YieldProgress) -> BlockProvider<UiBlocks> {
        BlockProvider::new(p, |key| {
            Ok(match key {
                UiBlocks::BackButtonLabel => make_button_label_block(
                    universe,
                    "Back",
                    ButtonIcon::Icon(include_image!("icons/button-back.png")),
                )?
                .build(),

                UiBlocks::AboutButtonLabel => make_button_label_block(
                    universe,
                    "About",
                    ButtonIcon::Text(&font::FONT_10X20, "?"),
                )?
                .build(),

                UiBlocks::PauseButtonLabel => make_button_label_block(
                    universe,
                    "Pause",
                    ButtonIcon::Icon(include_image!("icons/button-pause.png")),
                )?
                .build(),

                UiBlocks::SaveButtonLabel => make_button_label_block(
                    universe,
                    "Save",
                    ButtonIcon::Icon(include_image!("icons/button-save.png")),
                )?
                .build(),

                UiBlocks::QuitButtonLabel => make_button_label_block(
                    universe,
                    "Quit",
                    ButtonIcon::Text(&font::FONT_7X13, "Quit"),
                )?
                .build(),

                UiBlocks::OptionsButtonLabel => make_button_label_block(
                    universe,
                    "Options",
                    ButtonIcon::Icon(include_image!("icons/button-options.png")),
                )?
                .build(),

                UiBlocks::MouselookButtonLabel => make_button_label_block(
                    universe,
                    "Mouselook",
                    ButtonIcon::Icon(include_image!("icons/button-mouselook.png")),
                )?
                .build(),

                UiBlocks::FullscreenButtonLabel => make_button_label_block(
                    universe,
                    "Fullscreen",
                    ButtonIcon::Icon(include_image!("icons/button-fullscreen.png")),
                )?
                .build(),

                UiBlocks::AntialiasButtonLabel => make_button_label_block(
                    universe,
                    "Antialiasing",
                    ButtonIcon::Icon(include_image!("icons/button-antialias.png")),
                )?
                .build(),

                UiBlocks::DebugInfoTextButtonLabel => make_button_label_block(
                    universe,
                    "Debug: Info Text",
                    ButtonIcon::Icon(include_image!("icons/button-debug-info-text.png")),
                )?
                .build(),

                UiBlocks::DebugChunkBoxesButtonLabel => make_button_label_block(
                    universe,
                    "Debug: Chunk Boxes",
                    ButtonIcon::Icon(include_image!("icons/button-debug-chunk-boxes.png")),
                )?
                .build(),

                UiBlocks::DebugBehaviorsButtonLabel => make_button_label_block(
                    universe,
                    "Debug: Behaviors",
                    ButtonIcon::Icon(include_image!("icons/button-debug-behaviors.png")),
                )?
                .build(),

                UiBlocks::DebugCollisionBoxesButtonLabel => make_button_label_block(
                    universe,
                    "Debug: Collision Boxes",
                    ButtonIcon::Icon(include_image!("icons/button-debug-collision-boxes.png")),
                )?
                .build(),

                UiBlocks::DebugLightRaysButtonLabel => make_button_label_block(
                    universe,
                    "Debug: Light Rays at Cursor",
                    ButtonIcon::Icon(include_image!("icons/button-debug-light-rays.png")),
                )?
                .build(),
            })
        })
        .await
        .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::util::yield_progress_for_testing;

    #[tokio::test]
    async fn blocks_smoke_test() {
        UiBlocks::new(&mut Universe::new(), yield_progress_for_testing()).await;
    }
}
