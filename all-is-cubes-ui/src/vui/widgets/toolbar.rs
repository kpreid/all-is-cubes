use alloc::sync::{Arc, Weak};
use std::error::Error;
use std::fmt;
use std::sync::Mutex;

use all_is_cubes::arcstr;
use all_is_cubes::block::{self, text, Block, Resolution};
use all_is_cubes::character::Character;
use all_is_cubes::inv::{Slot, TOOL_SELECTIONS};
use all_is_cubes::listen::{DirtyFlag, Gate, Listen as _, ListenableSource, Listener};
use all_is_cubes::math::{Cube, FaceMap, GridAab, GridCoordinate, GridPoint, GridSize, GridVector};
use all_is_cubes::space::{CubeTransaction, SpaceTransaction};
use all_is_cubes::time::Duration;
use all_is_cubes::transaction::Merge as _;
use all_is_cubes::universe::{URef, Universe};

use crate::ui_content::{hud::HudBlocks, CueMessage, CueNotifier};
use crate::vui::widgets::{ToolbarButtonState, WidgetBlocks};
use crate::vui::{
    self, InstallVuiError, LayoutRequest, Layoutable, Widget, WidgetController, WidgetTransaction,
};

/// Widget that displays inventory contents in toolbar format.
///
/// TODO: We may or may not want to expand this to handle general inventory viewing
#[derive(Debug)]
pub(crate) struct Toolbar {
    hud_blocks: Arc<HudBlocks>,
    /// Which character we display the inventory of
    character_source: ListenableSource<Option<URef<Character>>>,
    cue_channel: CueNotifier,

    slot_count: usize,

    slot_info_text_template: text::TextBuilder,
}

impl Toolbar {
    // Stride between individual tool icon positions.
    const TOOLBAR_STEP: GridCoordinate = 2;

    pub fn new(
        character_source: ListenableSource<Option<URef<Character>>>,
        // TODO: Take WidgetTheme instead of HudBlocks, or move this widget out of the widgets module.
        hud_blocks: Arc<HudBlocks>,
        slot_count: usize,
        _universe: &mut Universe,
        cue_channel: CueNotifier,
    ) -> Arc<Self> {
        Arc::new(Self {
            hud_blocks,
            character_source,
            cue_channel,
            slot_count,
            slot_info_text_template: text::Text::builder()
                .resolution(Resolution::R32)
                .font(text::Font::SmallerBodyText)
                .positioning(text::Positioning {
                    x: text::PositioningX::Right,
                    line_y: text::PositioningY::BodyBottom,
                    z: text::PositioningZ::Front,
                })
                .layout_bounds(
                    Resolution::R32,
                    GridAab::from_lower_upper([0, 4, 0], [30, 32, 32]),
                ),
        })
    }
}

impl Layoutable for Toolbar {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: GridSize::new(
                self.slot_count as GridCoordinate * Self::TOOLBAR_STEP + 1,
                3,
                3,
            ),
        }
    }
}

impl Widget for Toolbar {
    fn controller(self: Arc<Self>, grant: &crate::vui::LayoutGrant) -> Box<dyn WidgetController> {
        let bounds = grant.bounds;

        let todo_change_character = DirtyFlag::listening(false, &self.character_source);
        let todo_inventory = DirtyFlag::new(true);
        let todo_more = Arc::new(Mutex::new(ToolbarTodo {
            button_pressed_decay: [Duration::ZERO; TOOL_SELECTIONS],
        }));
        self.cue_channel
            .listen(CueListener(Arc::downgrade(&todo_more)));

        let character = self.character_source.snapshot();

        let (character_listener_gate, character_listener) =
            Listener::<()>::gate(todo_inventory.listener());
        if let Some(character) = &character {
            character.read().unwrap().listen(character_listener);
        }

        Box::new(ToolbarController {
            todo_change_character,
            todo_inventory,
            todo_more,
            character,
            character_listener_gate,
            // TODO: obey gravity when positioning within the grant
            first_slot_position: Cube::new(
                (bounds.lower_bounds().x + bounds.upper_bounds().x) / 2
                    - (self.slot_count as GridCoordinate) * Toolbar::TOOLBAR_STEP / 2
                    + 1,
                bounds.lower_bounds().y + 1,
                bounds.lower_bounds().z + 1,
            ),
            definition: self,
        })
    }
}

#[derive(Debug)]
struct ToolbarController {
    definition: Arc<Toolbar>,
    todo_change_character: DirtyFlag,
    todo_inventory: DirtyFlag,
    todo_more: Arc<Mutex<ToolbarTodo>>,
    /// Latest character we've fetched from character_source,
    /// and the character whose inventory changes todo_inventory is tracking
    /// TODO: Generalize to noncharacters
    character: Option<URef<Character>>,
    character_listener_gate: Gate,
    first_slot_position: Cube,
}

impl ToolbarController {
    fn slot_position(&self, slot_index: usize) -> Cube {
        self.first_slot_position + GridVector::new(2, 0, 0) * slot_index as GridCoordinate
    }

    /// Returns a transaction to draw items and their stack counts, without using self.character
    /// but only the given inputs.
    fn write_items(
        &self,
        slots: &[Slot],
    ) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        let mut txn = SpaceTransaction::default();
        for (index, stack) in slots.iter().enumerate() {
            if index >= self.definition.slot_count {
                // TODO: must clear nonexistent positions, eventually
                break;
            }

            let icon_cube = self.slot_position(index);
            let info_cube = icon_cube + GridVector::new(-1, 0, 0);

            // Draw icon
            txn.merge_from(
                CubeTransaction::replacing(
                    None,
                    Some(stack.icon(&self.definition.hud_blocks.icons).into_owned()),
                )
                .at(icon_cube),
            )?;

            // Draw stack count text
            txn.merge_from(
                CubeTransaction::replacing(
                    None,
                    Some(
                        self.definition
                            .slot_info_text_template
                            .clone()
                            .string(match slots.get(index).unwrap_or(&Slot::Empty).count() {
                                0 | 1 => arcstr::ArcStr::default(),
                                count => arcstr::format!("{count}"),
                            })
                            .build()
                            .single_block(),
                    ),
                )
                .at(info_cube),
            )?;
        }

        Ok(txn)
    }

    /// Returns a transaction to draw the selected-slot pointers.
    fn write_pointers(
        &self,
        selected_slots: &[usize],
        pressed: [bool; TOOL_SELECTIONS],
    ) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        let mut txn = SpaceTransaction::default();
        for index in 0..self.definition.slot_count {
            let position = self.slot_position(index);
            let this_slot_selected_mask = core::array::from_fn(|sel| {
                if selected_slots
                    .get(sel)
                    .map(|&i| i == index)
                    .unwrap_or(false)
                {
                    if pressed[sel] {
                        ToolbarButtonState::Pressed
                    } else {
                        ToolbarButtonState::Mapped
                    }
                } else {
                    ToolbarButtonState::Unmapped
                }
            });
            txn.at(position + GridVector::new(0, 1, 0)).overwrite(
                self.definition.hud_blocks.widget_theme.widget_blocks
                    [WidgetBlocks::ToolbarPointer(this_slot_selected_mask)]
                .clone(),
            );
        }

        Ok(txn)
    }
}

impl WidgetController for ToolbarController {
    fn initialize(
        &mut self,
        _: &vui::WidgetContext<'_>,
    ) -> Result<WidgetTransaction, InstallVuiError> {
        let slot_count = self.definition.slot_count;

        let mut txn = SpaceTransaction::default();

        // Compute the volume in which the slot frame graphics land.
        let frame_region = GridAab::single_cube(self.first_slot_position).expand(FaceMap {
            nx: 1,
            ny: 1,
            nz: 1,
            px: (slot_count as i32) * 2 - 1, // TODO: magic number for spacing
            py: -1,
            pz: 1,
            ..Default::default()
        });

        /// TODO: figure out how to express this as a general helper
        fn zoom(block: &Block, pos: GridPoint) -> Block {
            block
                .clone()
                .with_modifier(block::Zoom::new(block::Resolution::R4, pos))
        }
        let frame_multiblock =
            &self.definition.hud_blocks.widget_theme.widget_blocks[WidgetBlocks::ToolbarSlotFrame];
        for cube in frame_region.interior_iter() {
            let relative = cube - self.first_slot_position;
            let x = relative.x;

            let left_or_middle = zoom(
                frame_multiblock,
                GridPoint::new(
                    (relative.x + 1).rem_euclid(2),
                    relative.y + 1,
                    relative.z + 1,
                ),
            );
            let right = zoom(
                frame_multiblock,
                GridPoint::new(2, relative.y + 1, relative.z + 1),
            );

            let frame_part = if x == (slot_count as i32) * 2 - 1 {
                // Right end cap of row.
                right
            } else if x.rem_euclid(2) == 1 && x > 0 {
                // Overlap between adjacent slots' frames: composite left and right.
                block::Composite::new(left_or_middle, block::CompositeOperator::Over)
                    .compose_or_replace(right)
            } else {
                left_or_middle
            };
            txn.at(cube).overwrite(frame_part);
        }

        Ok(txn)
    }

    fn step(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<(WidgetTransaction, vui::Then), Box<dyn Error + Send + Sync>> {
        if self.todo_change_character.get_and_clear() {
            self.character = self.definition.character_source.snapshot();

            let (gate, listener) = Listener::<()>::gate(self.todo_inventory.listener());
            if let Some(character) = &self.character {
                character.read().unwrap().listen(listener);
            }
            self.character_listener_gate = gate;
            self.todo_inventory.set();
        }

        // Extract button pressed state from todo (don't hold the lock more than necessary)
        let mut pressed_buttons: [bool; TOOL_SELECTIONS] = [false; TOOL_SELECTIONS];
        let mut should_update_pointers = false;
        {
            let mut todo = self.todo_more.lock().unwrap();
            for (i, t) in todo.button_pressed_decay.iter_mut().enumerate() {
                if *t != Duration::ZERO {
                    // include a final goes-to-zero update
                    should_update_pointers = true;
                }
                *t = t.saturating_sub(context.tick().delta_t());
                pressed_buttons[i] = *t != Duration::ZERO;
            }
        }

        // TODO: consolidate the two character borrows

        let should_update_inventory = self.todo_inventory.get_and_clear();
        let slots_txn = if should_update_inventory {
            if let Some(inventory_source) = &self.character {
                let character = inventory_source.read().unwrap();
                self.write_items(&character.inventory().slots)?
            } else {
                // TODO: clear toolbar ... once self.inventory_source can transition from Some to None at all
                vui::WidgetTransaction::default()
            }
        } else {
            vui::WidgetTransaction::default()
        };

        // should_update_inventory is currently true when the selected_slots value changes.
        // TODO: it should be separate by listening more precisely to the CharacterChange
        let pointers_txn = if should_update_inventory || should_update_pointers {
            if let Some(inventory_source) = &self.character {
                let character = inventory_source.read().unwrap();
                self.write_pointers(&character.selected_slots(), pressed_buttons)?
            } else {
                self.write_pointers(&[], pressed_buttons)?
            }
        } else {
            vui::WidgetTransaction::default()
        };

        Ok((slots_txn.merge(pointers_txn).unwrap(), vui::Then::Step))
    }
}

#[derive(Debug)]
struct ToolbarTodo {
    button_pressed_decay: [Duration; TOOL_SELECTIONS],
}

struct CueListener(Weak<Mutex<ToolbarTodo>>);

impl Listener<CueMessage> for CueListener {
    fn receive(&self, message: CueMessage) {
        match message {
            CueMessage::Clicked(button) => {
                let Some(cell) = self.0.upgrade() else { return }; // noop if dead listener
                let Ok(mut todo) = cell.lock() else { return }; // noop if poisoned
                if let Some(t) = todo.button_pressed_decay.get_mut(button) {
                    *t = Duration::from_millis(200);
                }
            }
        }
    }

    fn alive(&self) -> bool {
        self.0.strong_count() > 0
    }
}

impl fmt::Debug for CueListener {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CueListener").finish_non_exhaustive()
    }
}
