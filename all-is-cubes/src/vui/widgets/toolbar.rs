use std::error::Error;
use std::sync::{Arc, Mutex, Weak};

use cgmath::EuclideanSpace;
use embedded_graphics::mono_font::{iso_8859_1, MonoTextStyle};
use embedded_graphics::prelude::Point;
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use embedded_graphics::Drawable;
use instant::Duration;

use crate::block::{self, Block, BlockAttributes, Primitive, Resolution, AIR};
use crate::character::Character;
use crate::content::palette;
use crate::inv::{Slot, TOOL_SELECTIONS};
use crate::listen::{DirtyFlag, Gate, ListenableSource, Listener};
use crate::math::{FaceMap, GridAab, GridCoordinate, GridMatrix, GridPoint, GridVector};
use crate::space::{Space, SpacePhysics, SpaceTransaction};
use crate::time::Tick;
use crate::transaction::Merge as _;
use crate::universe::{URef, Universe};
use crate::vui::blocks::ToolbarButtonState;
use crate::vui::hud::HudBlocks;
use crate::vui::{
    CueMessage, CueNotifier, InstallVuiError, LayoutRequest, Layoutable, UiBlocks, Widget,
    WidgetController, WidgetTransaction,
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
    /// Space for drawing per-slot text labels
    slot_text_space: URef<Space>,
    slot_text_resolution: Resolution,
}

impl Toolbar {
    // Stride between individual tool icon positions.
    const TOOLBAR_STEP: GridCoordinate = 2;

    pub fn new(
        character_source: ListenableSource<Option<URef<Character>>>,
        hud_blocks: Arc<HudBlocks>,
        slot_count: usize,
        universe: &mut Universe,
        cue_channel: CueNotifier,
    ) -> Arc<Self> {
        let slot_text_resolution = Resolution::R32;
        let slot_text_space = universe.insert_anonymous(
            Space::builder(GridAab::from_lower_size(
                GridPoint::origin(),
                // TODO: shrink vertical axis to fit text, once we've debugged it
                GridVector::new(
                    GridCoordinate::from(slot_text_resolution) * slot_count as GridCoordinate,
                    GridCoordinate::from(slot_text_resolution),
                    1,
                ),
            ))
            .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
            .build(),
        );
        Arc::new(Self {
            hud_blocks,
            character_source,
            cue_channel,
            slot_text_resolution,
            slot_text_space,
            slot_count,
        })
    }
}

impl Layoutable for Toolbar {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: GridVector::new(
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

        let todo_change_character =
            DirtyFlag::listening(false, |l| self.character_source.listen(l));
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
            first_slot_position: GridPoint::new(
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
    first_slot_position: GridPoint,
}

impl ToolbarController {
    fn slot_position(&self, slot_index: usize) -> GridPoint {
        self.first_slot_position + GridVector::unit_x() * 2 * slot_index as GridCoordinate
    }

    /// Returns a transaction to draw items and their stack counts, without using self.character
    /// but only the given inputs.
    fn write_items(
        &self,
        slots: &[Slot],
    ) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        // Update stack count text.
        // TODO: This needs to stop being direct modification, eventually, at least if
        // we want to have parallel updates.
        // Also, trouble with multiple controllers sharing the same space...
        self.definition.slot_text_space.try_modify(|text_space| {
            // Erase old text.
            // TODO: Do this incrementally and only-if-different.
            // Maybe we should have a text-updating abstraction for this *and* the tooltip?
            text_space.fill_uniform(text_space.bounds(), &AIR).unwrap();

            let plane = &mut text_space.draw_target(GridMatrix::FLIP_Y);
            for index in 0..self.definition.slot_count {
                Text::with_text_style(
                    &match slots.get(index).unwrap_or(&Slot::Empty).count() {
                        0 | 1 => String::default(),
                        count => format!("{count}"),
                    },
                    Point::new(
                        // index + 1 locates the right edge of the space for index
                        (index as i32 + 1) * i32::from(self.definition.slot_text_resolution),
                        // baseline tweak to taste
                        -4,
                    ),
                    // TODO: review font choices
                    MonoTextStyle::new(&iso_8859_1::FONT_6X10, palette::ALMOST_BLACK),
                    TextStyleBuilder::new()
                        .baseline(Baseline::Bottom)
                        .alignment(Alignment::Right)
                        .build(),
                )
                .draw(plane)
                .unwrap();
            }
        })?;

        let mut txn = SpaceTransaction::default();
        for (index, stack) in slots.iter().enumerate() {
            if index >= self.definition.slot_count {
                // TODO: must clear nonexistent positions, eventually
                break;
            }

            let position = self.slot_position(index);
            // Draw icon
            txn.set(
                position,
                None,
                Some(stack.icon(&self.definition.hud_blocks.icons).into_owned()),
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
            let this_slot_selected_mask = std::array::from_fn(|sel| {
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
            txn.set_overwrite(
                position + GridVector::unit_y(),
                self.definition.hud_blocks.blocks
                    [UiBlocks::ToolbarPointer(this_slot_selected_mask)]
                .clone(),
            );
        }

        Ok(txn)
    }
}

impl WidgetController for ToolbarController {
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        let slot_count = self.definition.slot_count;
        let slot_text_resolution = self.definition.slot_text_resolution;

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
            let mut block = block.clone();
            block
                .modifiers_mut()
                .push(block::Zoom::new(block::Resolution::R4, pos).into());
            block
        }
        let frame_multiblock = &self.definition.hud_blocks.blocks[UiBlocks::ToolbarSlotFrame];
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
            txn.set_overwrite(cube, frame_part);
        }

        // Place stack-count text blocks.
        for index in 0..slot_count {
            txn.set_overwrite(
                self.slot_position(index) + GridVector::new(-1, 0, 0),
                Block::from_primitive(Primitive::Recur {
                    attributes: BlockAttributes::default(),
                    offset: GridPoint::new(
                        index as GridCoordinate * GridCoordinate::from(slot_text_resolution),
                        0,
                        1 - GridCoordinate::from(slot_text_resolution), // align to front face
                    ),
                    resolution: slot_text_resolution,
                    space: self.definition.slot_text_space.clone(),
                }),
            );
        }
        Ok(txn)
    }

    fn step(&mut self, tick: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
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
                *t = t.saturating_sub(tick.delta_t);
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
                WidgetTransaction::default()
            }
        } else {
            WidgetTransaction::default()
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
            WidgetTransaction::default()
        };

        Ok(slots_txn.merge(pointers_txn).unwrap())
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
                if let Some(cell) = self.0.upgrade() {
                    if let Ok(mut todo) = cell.lock() {
                        if let Some(t) = todo.button_pressed_decay.get_mut(button) {
                            *t = Duration::from_millis(200);
                        }
                    }
                }
            }
        }
    }

    fn alive(&self) -> bool {
        self.0.strong_count() > 0
    }
}
