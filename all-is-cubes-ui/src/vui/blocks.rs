#![expect(
    clippy::module_name_repetitions,
    reason = "module is private; https://github.com/rust-lang/rust-clippy/issues/8524"
)]

use core::fmt;

use exhaust::Exhaust;

use all_is_cubes::content::load_image::include_image;
use all_is_cubes::linking::{BlockModule, BlockProvider};
use all_is_cubes::universe::UniverseTransaction;
use all_is_cubes::util::YieldProgress;

#[cfg(feature = "session")]
use crate::settings;
use crate::vui::widgets::{ButtonIcon, make_button_label_block};

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
    SettingsButtonLabel,
    MouselookButtonLabel,
    FullscreenButtonLabel,
    AntialiasButtonLabel,
    DebugInfoTextButtonLabel,
    DebugBehaviorsButtonLabel,
    DebugChunkBoxesButtonLabel,
    DebugCollisionBoxesButtonLabel,
    DebugLightRaysButtonLabel,
    DebugPixelPerformanceButtonLabel,
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
            UiBlocks::SettingsButtonLabel => write!(f, "options-button"),
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

            UiBlocks::DebugPixelPerformanceButtonLabel => {
                write!(f, "debug-pixel-performance-button")
            }
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

                UiBlocks::SettingsButtonLabel => make_button_label_block(
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
                    "Debug: Show info text",
                    ButtonIcon::Icon(include_image!("icons/button-debug-info-text.png")),
                )?,

                UiBlocks::DebugChunkBoxesButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Show chunk boxes",
                    ButtonIcon::Icon(include_image!("icons/button-debug-chunk-boxes.png")),
                )?,

                UiBlocks::DebugBehaviorsButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Show behaviors",
                    ButtonIcon::Icon(include_image!("icons/button-debug-behaviors.png")),
                )?,

                UiBlocks::DebugCollisionBoxesButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Show collision boxes",
                    ButtonIcon::Icon(include_image!("icons/button-debug-collision-boxes.png")),
                )?,

                UiBlocks::DebugLightRaysButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Show light rays at cursor",
                    ButtonIcon::Icon(include_image!("icons/button-debug-light-rays.png")),
                )?,

                UiBlocks::DebugPixelPerformanceButtonLabel => make_button_label_block(
                    txn,
                    "Debug: Show rendering cost",
                    ButtonIcon::Icon(include_image!("icons/button-debug-pixel-performance.png")),
                )?,
            })
        })
        .await
        .unwrap()
    }

    #[cfg(feature = "session")]
    /// Returns the icon representing the given setting, if we have one.
    pub fn icon_for_setting(key: settings::Key) -> Option<Self> {
        match key {
            settings::Key::RenderMethod => None,
            settings::Key::Fog => None,
            settings::Key::FovY => None,
            settings::Key::ToneMapping => None,
            settings::Key::MaximumIntensity => None,
            settings::Key::ExposureMode => None,
            settings::Key::Exposure => None,
            settings::Key::BloomIntensity => None,
            settings::Key::ViewDistance => None,
            settings::Key::LightingDisplay => None,
            settings::Key::Transparency => None,
            settings::Key::TransparencyThreshold => None,
            settings::Key::ShowUi => None,
            settings::Key::Antialiasing => Some(Self::AntialiasButtonLabel),
            settings::Key::DebugInfoText => Some(Self::DebugInfoTextButtonLabel),
            settings::Key::DebugInfoTextContents => None,
            settings::Key::DebugPixelCost => Some(Self::DebugPixelPerformanceButtonLabel),
            settings::Key::DebugBehaviors => Some(Self::DebugBehaviorsButtonLabel),
            settings::Key::DebugChunkBoxes => Some(Self::DebugChunkBoxesButtonLabel),
            settings::Key::DebugCollisionBoxes => Some(Self::DebugCollisionBoxesButtonLabel),
            settings::Key::DebugLightRaysAtCursor => Some(Self::DebugLightRaysButtonLabel),
            settings::Key::DebugReduceViewFrustum => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::util::{async_test, yield_progress_for_testing};

    #[async_test]
    async fn blocks_smoke_test_and_icon_test() {
        // Prove that the construction doesn’t panic
        let mut txn = UniverseTransaction::default();
        #[cfg_attr(not(feature = "session"), expect(unused_variables))]
        let blocks = UiBlocks::new(&mut txn, yield_progress_for_testing()).await;

        #[cfg(feature = "session")]
        for setting_key in settings::Key::exhaust() {
            if let Some(block_key) = UiBlocks::icon_for_setting(setting_key) {
                assert_eq!(
                    setting_key.display_name(),
                    blocks[block_key]
                        .evaluate(txn.read_ticket())
                        .unwrap()
                        .attributes()
                        .display_name,
                    "name of {setting_key:?} does not match {block_key:?}"
                );
            }
        }
    }
}
