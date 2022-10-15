use std::fmt;

use embedded_graphics::mono_font::iso_8859_1 as font;
use exhaust::Exhaust;

use crate::block::{Block, Resolution::*, AIR};
use crate::content::load_image::{default_srgb, include_image, space_from_image};
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::inv::TOOL_SELECTIONS;
use crate::linking::{BlockModule, BlockProvider};
use crate::math::GridRotation;
use crate::universe::Universe;

#[cfg(doc)]
use crate::inv::Tool;
use crate::util::YieldProgress;
use crate::vui::widgets::{ActionButtonVisualState, ButtonBase, ToggleButtonVisualState};

/// Blocks that are used within the VUI, only.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[doc(hidden)] // public for testing only
#[non_exhaustive]
pub enum UiBlocks {
    /// HUD crosshair indicating cursor position.
    Crosshair,

    /// Marker indicating that a toolbar item is bound to a mouse button.
    ///
    /// Each array element is the relationship of this toolbar item to that button index.
    ToolbarPointer([ToolbarButtonState; TOOL_SELECTIONS]),

    /// Action button for navigating “back” in the user interface (closing dialogs, etc).
    BackButton(ActionButtonVisualState),

    // TODO: Should we do a `Button(ButtonLabel, ToggleButtonVisualState)` variant instead?
    AboutButton(ToggleButtonVisualState),
    PauseButton(ToggleButtonVisualState),
    MouselookButton(ToggleButtonVisualState),
    DebugInfoTextButton(ToggleButtonVisualState),
    DebugChunkBoxesButton(ToggleButtonVisualState),
    DebugCollisionBoxesButton(ToggleButtonVisualState),
    DebugLightRaysButton(ToggleButtonVisualState),
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
            UiBlocks::ToolbarPointer([b0, b1, b2]) => {
                write!(f, "toolbar-pointer/{b0}-{b1}-{b2}")
            }
            UiBlocks::BackButton(state) => write!(f, "back-button/{}", state),
            UiBlocks::AboutButton(state) => write!(f, "about-button/{}", state),
            UiBlocks::PauseButton(state) => write!(f, "pause-button/{}", state),
            UiBlocks::MouselookButton(state) => write!(f, "mouselook-button/{}", state),
            UiBlocks::DebugInfoTextButton(state) => write!(f, "debug-info-text-button/{}", state),
            UiBlocks::DebugChunkBoxesButton(state) => {
                write!(f, "debug-chunk-boxes-button/{}", state)
            }
            UiBlocks::DebugCollisionBoxesButton(state) => {
                write!(f, "debug-collision-boxes-button/{}", state)
            }
            UiBlocks::DebugLightRaysButton(state) => write!(f, "debug-light-rays-button/{}", state),
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

                UiBlocks::BackButton(state) => {
                    let mut button_builder = state.button_builder()?;
                    button_builder.draw_icon(include_image!("icons/button-back.png"))?;
                    button_builder.build(universe, "Back")
                }

                UiBlocks::AboutButton(state) => {
                    let mut button_builder = state.button_builder()?;
                    button_builder.draw_text(&font::FONT_10X20, "?")?;
                    button_builder.build(universe, "About")
                }

                UiBlocks::PauseButton(state) => {
                    let mut button_builder = state.button_builder()?;
                    button_builder.draw_icon(include_image!("icons/button-pause.png"))?;
                    button_builder.build(universe, "Pause")
                }

                UiBlocks::MouselookButton(state) => {
                    let mut button_builder = state.button_builder()?;
                    button_builder.draw_icon(include_image!("icons/button-mouselook.png"))?;
                    button_builder.build(universe, "Mouselook")
                }

                UiBlocks::DebugInfoTextButton(state) => {
                    let mut button_builder = state.button_builder()?;
                    button_builder.draw_icon(include_image!("icons/button-debug-info-text.png"))?;
                    button_builder.build(universe, "Debug: Info Text")
                }

                UiBlocks::DebugChunkBoxesButton(state) => {
                    let mut button_builder = state.button_builder()?;
                    button_builder
                        .draw_icon(include_image!("icons/button-debug-chunk-boxes.png"))?;
                    button_builder.build(universe, "Debug: Chunk Boxes")
                }

                UiBlocks::DebugCollisionBoxesButton(state) => {
                    let mut button_builder = state.button_builder()?;
                    button_builder
                        .draw_icon(include_image!("icons/button-debug-collision-boxes.png"))?;
                    button_builder.build(universe, "Debug: Collision Boxes")
                }

                UiBlocks::DebugLightRaysButton(state) => {
                    let mut button_builder = state.button_builder()?;
                    button_builder
                        .draw_icon(include_image!("icons/button-debug-light-rays.png"))?;
                    button_builder.build(universe, "Debug: Light Rays at Cursor")
                }
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
    use futures_executor::block_on;
    #[test]
    fn blocks_smoke_test() {
        block_on(UiBlocks::new(&mut Universe::new(), YieldProgress::noop()));
    }
}
