use alloc::format;
use alloc::sync::Arc;
use core::fmt;

use exhaust::Exhaust;

use all_is_cubes::block::{self, AIR, Block, Resolution::*};
use all_is_cubes::content::load_image::{default_srgb, space_from_image};
use all_is_cubes::content::palette;
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::include_image;
use all_is_cubes::inv::TOOL_SELECTIONS;
use all_is_cubes::linking::{BlockModule, BlockProvider, GenError};
use all_is_cubes::math::GridRotation;
use all_is_cubes::universe::UniverseTransaction;
use all_is_cubes::util::YieldProgress;

use crate::vui::widgets;
use crate::vui::widgets::{BoxStyle, ButtonBase as _, ButtonVisualState, ToggleButtonVisualState};

/// Blocks and other data for [`widgets`] to construct their appearance,
/// such as button shapes.
#[derive(Debug, Clone)]
pub struct WidgetTheme {
    pub(crate) widget_blocks: BlockProvider<WidgetBlocks>,
    pub(crate) dialog_box_style: BoxStyle,
    pub(crate) layout_debug_box_style: BoxStyle,
    pub(crate) progress_bar_empty: BoxStyle,
    pub(crate) progress_bar_full: BoxStyle,
}

impl WidgetTheme {
    /// Generate the default theme and install its components in `txn`, which should be a
    /// transaction that will be committed to the same universe as the widgets using this theme
    /// are to be installed in.
    ///
    /// Returns an error if the universe already contains the items that were to be installed.
    pub async fn new(
        txn: &mut UniverseTransaction,
        progress: YieldProgress,
    ) -> Result<Self, GenError> {
        let widget_blocks = WidgetBlocks::new(txn, progress).await.install(txn)?;

        Ok(Self::from_provider(widget_blocks))
    }

    // TODO: make this public and documented but figure out what our general "linking and loading" story is
    #[doc(hidden)]
    pub fn from_provider(widget_blocks: BlockProvider<WidgetBlocks>) -> Self {
        Self {
            dialog_box_style: BoxStyle::from_nine_and_thin(
                &widget_blocks[WidgetBlocks::DialogBackground],
            ),
            layout_debug_box_style: BoxStyle::from_composited_corner_and_edge(
                widget_blocks[WidgetBlocks::LayoutDebugBoxCorner].clone(),
                widget_blocks[WidgetBlocks::LayoutDebugBoxEdge].clone(),
            ),
            progress_bar_empty: BoxStyle::from_nine_and_thin(
                &widget_blocks[WidgetBlocks::ProgressBar { full: false }],
            ),
            progress_bar_full: BoxStyle::from_nine_and_thin(
                &widget_blocks[WidgetBlocks::ProgressBar { full: true }],
            ),

            widget_blocks,
        }
    }

    /// Returns a [`widgets::Frame`] to be placed behind some other widgets as a dialog box.
    pub fn dialog_background(&self) -> Arc<widgets::Frame> {
        widgets::Frame::new(self.dialog_box_style.clone())
    }
}

/// Blocks that are used by [`widgets`] to make up the shapes of the
/// generic UI elements themselves, not anything that gives them meaning.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[doc(hidden)] // public for testing only — TODO: should be public for real?
#[non_exhaustive]
pub enum WidgetBlocks {
    /// HUD crosshair indicating cursor position.
    Crosshair,

    /// 3×1×3 multiblock which is drawn on the XZ plane just underneath a toolbar slot.
    ToolbarSlotFrame,

    /// Marker indicating that a toolbar item is bound to a mouse button.
    ///
    /// Each array element is the relationship of this toolbar item to that button index.
    ToolbarPointer([ToolbarButtonState; TOOL_SELECTIONS]),

    /// 4x4x1 multiblock defining a `BoxStyle` for [`widgets::Frame`] dialog box backgrounds.
    DialogBackground,

    /// 4x4x1 multiblock defining a pair of `BoxStyle`s for [`widgets::ProgressBar`].
    ProgressBar {
        full: bool,
    },

    // TODO: consider moving these to a separate "WidgetTheme" enum to shift the complexity
    /// Appearance of a [`widgets::ActionButton`] without label.
    ActionButton(ButtonVisualState),
    /// Appearance of a [`widgets::ToggleButton`] without label.
    ToggleButton(ToggleButtonVisualState),

    LayoutDebugBoxCorner,
    LayoutDebugBoxEdge,
}

impl BlockModule for WidgetBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/vui/widget-blocks"
    }
}

impl fmt::Display for WidgetBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WidgetBlocks::Crosshair => write!(f, "crosshair"),
            WidgetBlocks::ToolbarSlotFrame => write!(f, "toolbar-slot-frame"),
            WidgetBlocks::ToolbarPointer([b0, b1, b2]) => {
                write!(f, "toolbar-pointer/{b0}-{b1}-{b2}")
            }
            WidgetBlocks::DialogBackground => write!(f, "dialog-background"),
            WidgetBlocks::ProgressBar { full } => write!(f, "progress-bar/{full}"),
            WidgetBlocks::ActionButton(state) => write!(f, "action-button/{state}"),
            WidgetBlocks::ToggleButton(state) => write!(f, "toggle-button/{state}"),
            WidgetBlocks::LayoutDebugBoxCorner => write!(f, "layout-debug-box-corner"),
            WidgetBlocks::LayoutDebugBoxEdge => write!(f, "layout-debug-box-edge"),
        }
    }
}

impl WidgetBlocks {
    pub async fn new(txn: &mut UniverseTransaction, p: YieldProgress) -> BlockProvider<Self> {
        BlockProvider::new(p, |key| {
            Ok(match key {
                WidgetBlocks::Crosshair => Block::builder()
                    .display_name("Crosshair")
                    .voxels_handle(
                        R64, // TODO: get resolution from image file
                        txn.insert_anonymous(space_from_image(
                            include_image!("theme/crosshair.png"),
                            GridRotation::RXyZ,
                            &default_srgb,
                        )?),
                    )
                    .build(),

                WidgetBlocks::ToolbarSlotFrame => {
                    Block::builder()
                        .display_name("Toolbar Slot Frame")
                        .voxels_handle(
                            R64,
                            txn.insert_anonymous(space_from_image(
                                include_image!("theme/toolbar-slot.png"),
                                GridRotation::RXZY,
                                // TODO: better way to do translations
                                &|pixel| default_srgb(pixel).translate([0, 16 - 1, 0]),
                            )?),
                        )
                        .build()
                }

                #[rustfmt::skip] // otherwise it breaks
                WidgetBlocks::ToolbarPointer([
                    ToolbarButtonState::Unmapped,
                    ToolbarButtonState::Unmapped,
                    ToolbarButtonState::Unmapped,
                ]) => AIR,
                WidgetBlocks::ToolbarPointer(buttons) => Block::builder()
                    .display_name("Selected")
                    .voxels_handle(
                        R32, // TODO: get resolution from image file
                        txn.insert_anonymous(space_from_image(
                            include_image!("theme/toolbar-sel-cursor.png"),
                            GridRotation::RXyZ,
                            &|color| match color {
                                // Map placeholder colors to the color for each button's state.
                                [255, 0, 0, 255] => buttons[0].brush(),
                                [0, 255, 0, 255] => buttons[1].brush(),
                                [0, 0, 255, 255] => buttons[2].brush(),
                                _ => default_srgb(color),
                            },
                        )?),
                    )
                    .build(),

                WidgetBlocks::DialogBackground => {
                    Block::builder()
                        .display_name("Dialog Background")
                        .voxels_handle(
                            R64, // 16 res × 4 tiles
                            txn.insert_anonymous(space_from_image(
                                include_image!("theme/dialog-background.png"),
                                GridRotation::RXYZ,
                                // place image on the front face (of the R16 individual blocks!)
                                // so it meets the back of the widgets in the dialog.
                                &|color| default_srgb(color).translate([0, 0, 15]),
                            )?),
                        )
                        .build()
                }

                WidgetBlocks::ProgressBar { full } => {
                    let image = if full {
                        include_image!("theme/progress-bar-full.png")
                    } else {
                        include_image!("theme/progress-bar-empty.png")
                    };

                    Block::builder()
                        .display_name(format! {"Progress Bar {}", if full {"Full"} else {"Empty"}})
                        .voxels_handle(
                            R64, // 16 res × 4 tiles
                            txn.insert_anonymous(space_from_image(
                                image,
                                GridRotation::IDENTITY,
                                &default_srgb,
                            )?),
                        )
                        .build()
                }

                WidgetBlocks::ActionButton(state) => state.button_block(txn)?,
                WidgetBlocks::ToggleButton(state) => state.button_block(txn)?,

                WidgetBlocks::LayoutDebugBoxCorner => Block::builder()
                    .display_name("LayoutDebugBoxCorner")
                    .voxels_handle(
                        R32,
                        txn.insert_anonymous(space_from_image(
                            include_image!("theme/layout-debug-box-corner.png"),
                            GridRotation::RXyZ,
                            &default_srgb,
                        )?),
                    )
                    .build(),
                WidgetBlocks::LayoutDebugBoxEdge => Block::builder()
                    .display_name("LayoutDebugBoxEdge")
                    .voxels_handle(
                        R32,
                        txn.insert_anonymous(space_from_image(
                            include_image!("theme/layout-debug-box-edge.png"),
                            GridRotation::RZYX,
                            &default_srgb,
                        )?),
                    )
                    .build(),
            })
        })
        .await
        .unwrap()
    }
}

/// The state of a mouse button's relationship to a toolbar slot.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[doc(hidden)] // public only because WidgetBlocks is
#[expect(clippy::exhaustive_enums)]
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
            Self::Unmapped => {
                VoxelBrush::with_thickness(block::from_color!(palette::HUD_TOOLBAR_BACK), 0..1)
            }
            Self::Mapped => {
                VoxelBrush::with_thickness(block::from_color!(palette::BUTTON_BACK), 0..3)
            }
            // TODO: figure out a palette color for this
            Self::Pressed => {
                VoxelBrush::with_thickness(block::from_color!(0.1, 0.1, 0.1, 1.0), 0..2)
            }
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
    use all_is_cubes::util::yield_progress_for_testing;

    #[tokio::test]
    async fn blocks_smoke_test() {
        WidgetBlocks::new(
            &mut UniverseTransaction::default(),
            yield_progress_for_testing(),
        )
        .await;
    }
}
