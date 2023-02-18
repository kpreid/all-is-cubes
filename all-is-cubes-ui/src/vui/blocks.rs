use std::fmt;

use exhaust::Exhaust;

use all_is_cubes::block::{Block, Resolution::*, AIR};
use all_is_cubes::content::load_image::{default_srgb, space_from_image};
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::mono_font::iso_8859_1 as font;
use all_is_cubes::drawing::VoxelBrush;
#[cfg(doc)]
use all_is_cubes::inv::Tool;
use all_is_cubes::inv::TOOL_SELECTIONS;
use all_is_cubes::linking::{BlockModule, BlockProvider};
use all_is_cubes::math::GridRotation;
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;
use all_is_cubes::{include_image, rgba_const};

#[cfg(doc)]
use crate::vui::widgets;
use crate::vui::widgets::{
    make_button_label_block, ButtonBase, ButtonIcon, ButtonVisualState, ToggleButtonVisualState,
};

/// Blocks that are used within the VUI, only.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[doc(hidden)] // public for testing only
#[non_exhaustive]
pub enum UiBlocks {
    /// HUD crosshair indicating cursor position.
    Crosshair,

    /// 3×1×3 multiblock which is drawn on the XZ plane just underneath a toolbar slot.
    ToolbarSlotFrame,

    /// Marker indicating that a toolbar item is bound to a mouse button.
    ///
    /// Each array element is the relationship of this toolbar item to that button index.
    ToolbarPointer([ToolbarButtonState; TOOL_SELECTIONS]),

    // TODO: consider moving these to a separate "WidgetTheme" enum to shift the complexity
    /// Appearance of a [`widgets::ActionButton`] without label.
    ActionButton(ButtonVisualState),
    /// Appearance of a [`widgets::ToggleButton`] without label.
    ToggleButton(ToggleButtonVisualState),

    /// Label of the action button for navigating “back” in the user interface (closing
    /// dialogs, etc).
    BackButtonLabel,

    AboutButtonLabel,
    PauseButtonLabel,
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
        "all-is-cubes/vui/blocks"
    }
}

impl fmt::Display for UiBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UiBlocks::Crosshair => write!(f, "crosshair"),
            UiBlocks::ToolbarSlotFrame => write!(f, "toolbar-slot-frame"),
            UiBlocks::ToolbarPointer([b0, b1, b2]) => {
                write!(f, "toolbar-pointer/{b0}-{b1}-{b2}")
            }
            UiBlocks::ActionButton(state) => write!(f, "action-button/{state}"),
            UiBlocks::ToggleButton(state) => write!(f, "toggle-button/{state}"),
            UiBlocks::BackButtonLabel => write!(f, "back-button"),
            UiBlocks::AboutButtonLabel => write!(f, "about-button"),
            UiBlocks::PauseButtonLabel => write!(f, "pause-button"),
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
                UiBlocks::Crosshair => Block::builder()
                    .display_name("Crosshair")
                    .voxels_ref(
                        R64, // TODO: get resolution from image file
                        universe.insert_anonymous(space_from_image(
                            include_image!("icons/crosshair.png"),
                            GridRotation::RXyZ,
                            default_srgb,
                        )?),
                    )
                    .build(),

                UiBlocks::ToolbarSlotFrame => {
                    Block::builder()
                        .display_name("Toolbar Slot Frame")
                        .voxels_ref(
                            R64,
                            universe.insert_anonymous(space_from_image(
                                include_image!("icons/toolbar-slot.png"),
                                GridRotation::RXZY,
                                // TODO: better way to do translations
                                |pixel| default_srgb(pixel).translate([0, 16 - 1, 0]),
                            )?),
                        )
                        .build()
                }

                #[rustfmt::skip] // otherwise it breaks
                UiBlocks::ToolbarPointer([
                    ToolbarButtonState::Unmapped,
                    ToolbarButtonState::Unmapped,
                    ToolbarButtonState::Unmapped,
                ]) => AIR,
                UiBlocks::ToolbarPointer(buttons) => Block::builder()
                    .display_name("Selected")
                    .voxels_ref(
                        R32, // TODO: get resolution from image file
                        universe.insert_anonymous(space_from_image(
                            include_image!("icons/toolbar-sel-cursor.png"),
                            GridRotation::RXyZ,
                            |color| match color {
                                // Map placeholder colors to the color for each button's state.
                                image::Rgba([255, 0, 0, 255]) => buttons[0].brush(),
                                image::Rgba([0, 255, 0, 255]) => buttons[1].brush(),
                                image::Rgba([0, 0, 255, 255]) => buttons[2].brush(),
                                _ => default_srgb(color),
                            },
                        )?),
                    )
                    .build(),

                UiBlocks::ActionButton(state) => state.button_block(universe)?,
                UiBlocks::ToggleButton(state) => state.button_block(universe)?,

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
                    "Back",
                    ButtonIcon::Icon(include_image!("icons/button-pause.png")),
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

/// The state of a mouse button's relationship to a toolbar slot.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[doc(hidden)] // public only because UiBlocks is
#[allow(clippy::exhaustive_enums)]
pub enum ToolbarButtonState {
    /// This button is not mapped to this toolbar slot.
    Unmapped,
    /// This button is mapped to this toolbar slot.
    Mapped,
    /// This button is mapped to this toolbar slot and being pressed.
    Pressed,
}

impl ToolbarButtonState {
    fn brush(self) -> VoxelBrush<'static> {
        match self {
            // same color as the icon image has for a background
            // (but TODO: this choice of constant doesn't make sense)
            Self::Unmapped => VoxelBrush::with_thickness(palette::HUD_TOOLBAR_BACK, 0..1),
            Self::Mapped => VoxelBrush::with_thickness(palette::BUTTON_BACK, 0..3),
            // TODO: figure out a palette color for this
            Self::Pressed => VoxelBrush::with_thickness(rgba_const!(0.1, 0.1, 0.1, 1.0), 0..2),
        }
    }
}

impl fmt::Display for ToolbarButtonState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unmapped => "u",
            Self::Mapped => "m",
            Self::Pressed => "p",
        }
        .fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn blocks_smoke_test() {
        UiBlocks::new(&mut Universe::new(), YieldProgress::noop()).await;
    }
}
