// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::error::Error;
use std::sync::Arc;

use cgmath::EuclideanSpace;
use embedded_graphics::mono_font::{iso_8859_1, MonoTextStyle};
use embedded_graphics::prelude::Point;
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use embedded_graphics::Drawable;

use crate::block::{Block, BlockAttributes, Primitive, Resolution, AIR};
use crate::character::Character;
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::inv::Slot;
use crate::listen::{DirtyFlag, Gate, ListenableSource, Listener};
use crate::math::{GridCoordinate, GridMatrix, GridPoint, GridVector};
use crate::space::{Grid, Space, SpacePhysics, SpaceTransaction};
use crate::time::Tick;
use crate::transaction::Merge as _;
use crate::universe::{URef, Universe};
use crate::vui::{
    hud::{HudBlocks, HudLayout},
    InstallVuiError, WidgetController, WidgetTransaction,
};

/// Displays inventory contents in toolbar format.
///
/// TODO: We may or may not want to expand this to handle general inventory viewing
#[derive(Debug)]
pub(crate) struct ToolbarController {
    hud_blocks: Arc<HudBlocks>,
    todo_change_character: DirtyFlag,
    todo_inventory: DirtyFlag,
    /// Which character self.character should be
    character_source: ListenableSource<Option<URef<Character>>>,
    /// TODO: Generalize to noncharacters
    character: Option<URef<Character>>,
    character_listener_gate: Gate,
    first_slot_position: GridPoint,
    slot_count: usize,
    /// Space for drawing per-slot text labels
    slot_text_space: URef<Space>,
    slot_text_resolution: Resolution,
}

impl ToolbarController {
    pub(crate) const TOOLBAR_STEP: GridCoordinate = 2;

    pub fn new(
        character_source: ListenableSource<Option<URef<Character>>>,
        hud_blocks: Arc<HudBlocks>,
        layout: &HudLayout,
        universe: &mut Universe,
    ) -> Self {
        let slot_count = layout.toolbar_positions;

        let todo_change_character = DirtyFlag::listening(false, |l| character_source.listen(l));
        let todo_inventory = DirtyFlag::new(true);

        let character = character_source.snapshot();

        let (character_listener_gate, character_listener) =
            Listener::<()>::gate(todo_inventory.listener());
        if let Some(character) = &character {
            character.borrow().listen(character_listener);
        }

        let slot_text_resolution: Resolution = 32;
        let slot_text_space = universe.insert_anonymous(
            Space::builder(Grid::new(
                GridPoint::origin(),
                // TODO: shrink vertical axis to fit text, once we've debugged it
                GridVector::new(
                    GridCoordinate::from(slot_text_resolution) * slot_count as GridCoordinate,
                    GridCoordinate::from(slot_text_resolution),
                    1,
                ),
            ))
            .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
            .build_empty(),
        );

        Self {
            hud_blocks,
            todo_change_character,
            todo_inventory,
            character_source,
            character,
            character_listener_gate,
            first_slot_position: layout.first_tool_icon_position(),
            slot_count,
            slot_text_space,
            slot_text_resolution,
        }
    }

    fn slot_position(&self, slot_index: usize) -> GridPoint {
        self.first_slot_position + GridVector::unit_x() * 2 * slot_index as GridCoordinate
    }

    /// Helper for WidgetController impl; generates a transaction without using self.character
    fn write_items(
        &self,
        slots: &[Slot],
        selected_slots: &[usize],
    ) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        // Update stack count text.
        // TODO: This needs to stop being direct modification, eventually, at least if
        // we want to have parallel updates.
        self.slot_text_space.try_modify(|text_space| {
            // Erase old text.
            // TODO: Do this incrementally and only-if-different.
            // Maybe we should have a text-updating abstraction for this *and* the tooltip?
            text_space.fill_uniform(text_space.grid(), &AIR).unwrap();

            let plane = &mut text_space.draw_target(GridMatrix::FLIP_Y);
            for index in 0..self.slot_count {
                Text::with_text_style(
                    &match slots.get(index).unwrap_or(&Slot::Empty).count() {
                        0 | 1 => String::default(),
                        count => format!("{}", count),
                    },
                    Point::new(
                        // index + 1 locates the right edge of the space for index
                        (index as i32 + 1) * i32::from(self.slot_text_resolution),
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
            if index >= self.slot_count {
                // TODO: must clear nonexistent positions, eventually
                break;
            }

            let position = self.slot_position(index);
            // Draw icon
            txn.set(
                position,
                None,
                Some(stack.icon(&self.hud_blocks.icons).to_owned().into_owned()),
            )?;
            // Draw pointers.
            // TODO: magic number in how many selections we display
            let this_slot_selected_mask: usize = (0..2_usize)
                .map(|sel| {
                    usize::from(
                        selected_slots
                            .get(sel)
                            .map(|&i| i == index)
                            .unwrap_or(false),
                    ) << sel
                })
                .sum();
            let brush: &VoxelBrush<'_> = &self.hud_blocks.toolbar_pointer[this_slot_selected_mask];
            txn = txn.merge(brush.paint_transaction(position)).unwrap();
        }

        Ok(txn)
    }
}

impl WidgetController for ToolbarController {
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        let hud_blocks = &self.hud_blocks;
        let mut txn = SpaceTransaction::default();

        txn = txn
            .merge(
                hud_blocks
                    .toolbar_left_cap
                    .paint_transaction(self.slot_position(0) + GridVector::new(-1, 0, 0)),
            )
            .unwrap();
        txn = txn
            .merge(hud_blocks.toolbar_right_cap.paint_transaction(
                self.slot_position(self.slot_count - 1) + GridVector::new(1, 0, 0),
            ))
            .unwrap();
        for index in 0..self.slot_count {
            txn = txn
                .merge(
                    hud_blocks
                        .toolbar_middle
                        .paint_transaction(self.slot_position(index)),
                )
                .unwrap();
            if index > 0 {
                txn = txn
                    .merge(
                        hud_blocks.toolbar_divider.paint_transaction(
                            self.slot_position(index) + GridVector::new(-1, 0, 0),
                        ),
                    )
                    .unwrap();
            }
        }

        // Place stack-count text blocks. This is done separately because it's easier
        // without getting `draw_target` involved.
        for index in 0..self.slot_count {
            txn.set_overwrite(
                self.slot_position(index) + GridVector::new(-1, 0, 0),
                Block::from_primitive(Primitive::Recur {
                    attributes: BlockAttributes::default(),
                    offset: GridPoint::new(
                        index as GridCoordinate * GridCoordinate::from(self.slot_text_resolution),
                        0,
                        1 - GridCoordinate::from(self.slot_text_resolution), // align to front face
                    ),
                    resolution: self.slot_text_resolution,
                    space: self.slot_text_space.clone(),
                }),
            );
        }
        Ok(txn)
    }

    fn step(&mut self, _: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        if self.todo_change_character.get_and_clear() {
            self.character = self.character_source.snapshot();

            let (gate, listener) = Listener::<()>::gate(self.todo_inventory.listener());
            if let Some(character) = &self.character {
                character.borrow().listen(listener);
            }
            self.character_listener_gate = gate;
            self.todo_inventory.set();
        }

        Ok(if self.todo_inventory.get_and_clear() {
            if let Some(inventory_source) = &self.character {
                let character = inventory_source.borrow();
                let slots: &[Slot] = &character.inventory().slots;
                self.write_items(slots, &character.selected_slots())?
            } else {
                // TODO: clear toolbar ... once self.inventory_source can transition from Some to None at all
                WidgetTransaction::default()
            }
        } else {
            WidgetTransaction::default()
        })
    }
}
