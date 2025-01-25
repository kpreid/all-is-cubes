#![expect(
    clippy::module_name_repetitions,
    reason = "module is private; https://github.com/rust-lang/rust-clippy/issues/8524"
)]

use core::fmt;

use exhaust::Exhaust;

use all_is_cubes::include_image;
use all_is_cubes::linking::{BlockModule, BlockProvider};
use all_is_cubes::universe::UniverseTransaction;
use all_is_cubes::util::YieldProgress;

use crate::vui::widgets::{make_button_label_block, ButtonIcon};

/// Blocks that are used as part of the UI content.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[doc(hidden)] // public for testing only
#[allow(clippy::enum_variant_names)]
#[non_exhaustive]
pub enum UiBlocks {
    /// Label of the action button for navigating “back” in the user interface (closing
    /// dialogs, etc).
    BackButtonLabel,

    AboutButtonLabel,
    PauseButtonLabel,
    SaveButtonLabel,
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
    pub async fn new(txn: &mut UniverseTransaction, p: YieldProgress) -> BlockProvider<UiBlocks> {
        BlockProvider::new(p, |key| {
            Ok(match key {
                UiBlocks::BackButtonLabel => make_button_label_block(
                    txn,
                    "Back",
                    ButtonIcon::Icon(include_image!("icons/button-back.png")),
                )?,

                UiBlocks::AboutButtonLabel => make_button_label_block(
                    txn,
                    "About",
                    ButtonIcon::Icon(include_image!("icons/button-help.png")),
                )?,

                UiBlocks::PauseButtonLabel => make_button_label_block(
                    txn,
                    "Pause",
                    ButtonIcon::Icon(include_image!("icons/button-pause.png")),
                )?,

                UiBlocks::SaveButtonLabel => make_button_label_block(
                    txn,
                    "Save",
                    ButtonIcon::Icon(include_image!("icons/button-save.png")),
                )?,

                UiBlocks::OptionsButtonLabel => make_button_label_block(
                    txn,
                    "Options",
                    ButtonIcon::Icon(include_image!("icons/button-options.png")),
                )?,

                UiBlocks::MouselookButtonLabel => make_button_label_block(
                    txn,
                    "Mouselook",
                    ButtonIcon::Icon(include_image!("icons/button-mouselook.png")),
                )?,

                UiBlocks::FullscreenButtonLabel => make_button_label_block(
                    txn,
                    "Fullscreen",
                    ButtonIcon::Icon(include_image!("icons/button-fullscreen.png")),
                )?,

                UiBlocks::AntialiasButtonLabel => make_button_label_block(
                    txn,
                    "Antialiasing",
                    ButtonIcon::Icon(include_image!("icons/button-antialias.png")),
                )?,

                UiBlocks::DebugInfoTextButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Info Text",
                    ButtonIcon::Icon(include_image!("icons/button-debug-info-text.png")),
                )?,

                UiBlocks::DebugChunkBoxesButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Chunk Boxes",
                    ButtonIcon::Icon(include_image!("icons/button-debug-chunk-boxes.png")),
                )?,

                UiBlocks::DebugBehaviorsButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Behaviors",
                    ButtonIcon::Icon(include_image!("icons/button-debug-behaviors.png")),
                )?,

                UiBlocks::DebugCollisionBoxesButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Collision Boxes",
                    ButtonIcon::Icon(include_image!("icons/button-debug-collision-boxes.png")),
                )?,

                UiBlocks::DebugLightRaysButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Light Rays at Cursor",
                    ButtonIcon::Icon(include_image!("icons/button-debug-light-rays.png")),
                )?,
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
        UiBlocks::new(
            &mut UniverseTransaction::default(),
            yield_progress_for_testing(),
        )
        .await;
    }
}
