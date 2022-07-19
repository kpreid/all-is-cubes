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
use crate::math::{GridAab, GridCoordinate, GridMatrix, GridPoint, GridVector};
use crate::space::{Space, SpacePhysics, SpaceTransaction};
use crate::time::Tick;
use crate::transaction::Merge as _;
use crate::universe::{URef, Universe};
use crate::vui::hud::HudBlocks;
use crate::vui::{
    InstallVuiError, LayoutRequest, Layoutable, Widget, WidgetController, WidgetTransaction,
};

/// Widget that displays inventory contents in toolbar format.
///
/// TODO: We may or may not want to expand this to handle general inventory viewing
#[derive(Debug)]
pub(crate) struct Toolbar {
    hud_blocks: Arc<HudBlocks>,
    /// Which character we display the inventory of
    character_source: ListenableSource<Option<URef<Character>>>,
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
    ) -> Arc<Self> {
        let slot_text_resolution: Resolution = 32;
        let slot_text_space = universe.insert_anonymous(
            Space::builder(GridAab::new(
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
        Arc::new(Self {
            hud_blocks,
            character_source,
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

        let character = self.character_source.snapshot();

        let (character_listener_gate, character_listener) =
            Listener::<()>::gate(todo_inventory.listener());
        if let Some(character) = &character {
            character.borrow().listen(character_listener);
        }

        Box::new(ToolbarController {
            todo_change_character,
            todo_inventory,
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

    /// Helper for WidgetController impl; generates a transaction without using self.character
    fn write_items(
        &self,
        slots: &[Slot],
        selected_slots: &[usize],
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
                        count => format!("{}", count),
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
                Some(
                    stack
                        .icon(&self.definition.hud_blocks.icons)
                        .to_owned()
                        .into_owned(),
                ),
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
            let brush: &VoxelBrush<'_> =
                &self.definition.hud_blocks.toolbar_pointer[this_slot_selected_mask];
            txn = txn.merge(brush.paint_transaction(position)).unwrap();
        }

        Ok(txn)
    }
}

impl WidgetController for ToolbarController {
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        let hud_blocks = &self.definition.hud_blocks;
        let slot_count = self.definition.slot_count;
        let slot_text_resolution = self.definition.slot_text_resolution;

        let mut txn = SpaceTransaction::default();

        txn = txn
            .merge(
                hud_blocks
                    .toolbar_left_cap
                    .paint_transaction(self.slot_position(0) + GridVector::new(-1, 0, 0)),
            )
            .unwrap();
        txn = txn
            .merge(
                hud_blocks.toolbar_right_cap.paint_transaction(
                    self.slot_position(slot_count - 1) + GridVector::new(1, 0, 0),
                ),
            )
            .unwrap();
        for index in 0..slot_count {
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

    fn step(&mut self, _: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        if self.todo_change_character.get_and_clear() {
            self.character = self.definition.character_source.snapshot();

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
