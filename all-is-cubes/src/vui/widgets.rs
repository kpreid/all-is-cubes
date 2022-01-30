// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Specific UI widgets.

use std::error::Error;
use std::fmt::Debug;
use std::sync::{Arc, Mutex};

use cgmath::EuclideanSpace as _;
use embedded_graphics::mono_font::iso_8859_1;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::prelude::Point;
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use embedded_graphics::Drawable as _;
use instant::Duration;
use once_cell::sync::Lazy;

use crate::behavior::BehaviorSetTransaction;
use crate::block::{space_to_blocks, AnimationHint, Block, BlockAttributes, Resolution, AIR};
use crate::character::{Character, CharacterChange};
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::inv::{EphemeralOpaque, Slot};
use crate::listen::{DirtyFlag, FnListener, Gate, ListenableSource, Listener as _};
use crate::math::{GridCoordinate, GridMatrix, GridPoint, GridVector};
use crate::space::{Grid, Space, SpacePhysics, SpaceTransaction};
use crate::time::Tick;
use crate::transaction::Merge as _;
use crate::universe::{URef, Universe};
use crate::vui::hud::{HudBlocks, HudFont, HudLayout};
use crate::vui::{
    ActivatableRegion, InstallVuiError, LayoutGrant, LayoutRequest, Layoutable, Widget,
    WidgetController, WidgetTransaction,
};

/// Generic widget controller that only does something initialize.
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct OneshotController(pub Option<WidgetTransaction>);

impl WidgetController for OneshotController {
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        Ok(self.0.take().unwrap_or_default())
    }

    // TODO: Arrange somehow for this controller to be deleted since it doesn't need to be step()ped
}

#[derive(Clone, Debug)]
pub(crate) struct ToggleButtonWidget {
    states: [Block; 2],
    data_source: ListenableSource<bool>,
    action: EphemeralOpaque<dyn Fn() + Send + Sync>,
}

impl ToggleButtonWidget {
    pub(crate) fn new(
        data_source: ListenableSource<bool>,
        off: Block,
        on: Block,
        action: impl Fn() + Send + Sync + 'static,
    ) -> Arc<Self> {
        Arc::new(Self {
            data_source,
            states: [off, on],
            action: EphemeralOpaque::from(Arc::new(action) as Arc<dyn Fn() + Send + Sync>),
        })
    }
}

impl Layoutable for ToggleButtonWidget {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: GridVector::new(1, 1, 1),
        }
    }
}

impl Widget for ToggleButtonWidget {
    fn controller(self: Arc<Self>, position: &LayoutGrant) -> Box<dyn WidgetController> {
        Box::new(ToggleButtonController::new(
            position.bounds.lower_bounds(),
            self,
        ))
    }
}

/// Manages a single-block toggle button.
#[derive(Debug)]
pub(crate) struct ToggleButtonController {
    definition: Arc<ToggleButtonWidget>,
    position: GridPoint,
    todo: DirtyFlag,
}

impl ToggleButtonController {
    pub(crate) fn new(position: GridPoint, definition: Arc<ToggleButtonWidget>) -> Self {
        let todo = DirtyFlag::new(true);
        definition.data_source.listen(todo.listener());
        Self {
            position,
            definition,
            todo,
        }
    }
}

impl WidgetController for ToggleButtonController {
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        Ok(SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            Arc::new(ActivatableRegion {
                region: Grid::single_cube(self.position),
                effect: self.definition.action.clone(),
            }),
        )))
    }

    fn step(&mut self, _: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        Ok(if self.todo.get_and_clear() {
            SpaceTransaction::set_cube(
                self.position,
                None,
                Some(
                    self.definition.states[self.definition.data_source.snapshot() as usize].clone(),
                ),
            )
        } else {
            SpaceTransaction::default()
        })
    }
}

/// Shows/hides the crosshair depending on mouselook mode.
#[derive(Debug)]
pub(crate) struct CrosshairController {
    icon: Block,
    todo: DirtyFlag,
    position: GridPoint,
    mouselook_mode: ListenableSource<bool>,
}

impl CrosshairController {
    pub fn new(position: GridPoint, icon: Block, mouselook_mode: ListenableSource<bool>) -> Self {
        let todo = DirtyFlag::new(false);
        mouselook_mode.listen(todo.listener());
        Self {
            icon,
            todo,
            position,
            mouselook_mode,
        }
    }
}

impl WidgetController for CrosshairController {
    fn step(&mut self, _tick: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        Ok(if self.todo.get_and_clear() {
            SpaceTransaction::set_cube(
                self.position,
                None,
                Some(if *self.mouselook_mode.get() {
                    self.icon.clone()
                } else {
                    AIR
                }),
            )
        } else {
            SpaceTransaction::default()
        })
    }
}

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

        let todo_change_character = DirtyFlag::new(false);
        let todo_inventory = DirtyFlag::new(true);

        character_source.listen(todo_change_character.listener());
        let character = character_source.snapshot();

        let (character_listener_gate, character_listener) = todo_inventory.listener().gate();
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
                    (selected_slots
                        .get(sel)
                        .map(|&i| i == index)
                        .unwrap_or(false) as usize)
                        << sel
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
                Block::Recur {
                    attributes: BlockAttributes::default(),
                    offset: GridPoint::new(
                        index as GridCoordinate * GridCoordinate::from(self.slot_text_resolution),
                        0,
                        1 - GridCoordinate::from(self.slot_text_resolution), // align to front face
                    ),
                    resolution: self.slot_text_resolution,
                    space: self.slot_text_space.clone(),
                },
            );
        }
        Ok(txn)
    }

    fn step(&mut self, _: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        if self.todo_change_character.get_and_clear() {
            self.character = self.character_source.snapshot();

            let (gate, listener) = self.todo_inventory.listener().gate();
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

static EMPTY_ARC_STR: Lazy<Arc<str>> = Lazy::new(|| "".into());

#[derive(Debug)]
pub(crate) struct TooltipState {
    /// Character we're reading inventory state from
    character: Option<URef<Character>>,
    /// Listener gate to stop the listener if we change characters
    character_gate: Gate,

    /// Whether the tool we should be displaying might have changed.
    dirty_inventory: bool,
    /// Whether the `current_contents` has changed and should be drawn.
    dirty_text: bool,
    /// Text to actually show on screen.
    current_contents: TooltipContents,
    /// Last value of `current_contents` that was an inventory item.
    last_inventory_message: TooltipContents,
    /// How long ago the `current_contents` were shown. None if `Blanked`.
    age: Option<Duration>,
}

impl TooltipState {
    pub(crate) fn bind_to_character(this_ref: &Arc<Mutex<Self>>, character: URef<Character>) {
        let (gate, listener) =
            FnListener::new(this_ref, move |this: &Mutex<Self>, change| match change {
                // TODO: Don't dirty if an unrelated inventory slot changed
                CharacterChange::Inventory(_) | CharacterChange::Selections => {
                    if let Ok(mut this) = this.lock() {
                        this.dirty_inventory = true;
                    }
                }
            })
            .gate();

        // TODO: Think about what state results if either of the locks/borrows fails
        character.borrow().listen(listener);
        {
            let mut this = this_ref.lock().unwrap();
            this.character = Some(character);
            this.character_gate = gate;
            this.dirty_inventory = true;
        }
    }

    pub fn set_message(&mut self, text: Arc<str>) {
        self.dirty_inventory = false;
        self.set_contents(TooltipContents::Message(text))
    }

    fn set_contents(&mut self, contents: TooltipContents) {
        self.dirty_text = true;
        self.current_contents = contents;
        self.age = Some(Duration::ZERO);
    }

    /// Advances time and returns the string that should be newly written to the screen, if different than the previous call.
    fn step(&mut self, hud_blocks: &HudBlocks, tick: Tick) -> Option<Arc<str>> {
        if let Some(ref mut age) = self.age {
            *age += tick.delta_t;
            if *age > Duration::from_secs(1) {
                self.set_contents(TooltipContents::Blanked);
                self.age = None;
            }
        }

        if self.dirty_inventory {
            self.dirty_inventory = false;

            if let Some(character_ref) = &self.character {
                let character = character_ref.borrow();
                let selected_slot = character
                    .selected_slots()
                    .get(1)
                    .copied()
                    .unwrap_or(usize::MAX);
                if let Some(tool) = character.inventory().slots.get(selected_slot).cloned() {
                    let new_text = tool
                        .icon(&hud_blocks.icons)
                        .evaluate()
                        .ok()
                        .map(|ev_block| ev_block.attributes.display_name.to_owned().into())
                        .unwrap_or_else(|| EMPTY_ARC_STR.clone());
                    let new_contents = TooltipContents::InventoryItem {
                        source_slot: selected_slot,
                        text: new_text,
                    };

                    // Comparison ensures that inventory changes that don't change the
                    // displayed text are ignored, even if the text has timed out, unless
                    // the change is to a different slot with the *same name*.
                    if new_contents != self.last_inventory_message {
                        // log::info!(
                        //     "changing from {:?} to {:?}",
                        //     self.last_inventory_message,
                        //     new_contents
                        // );
                        if self.last_inventory_message != TooltipContents::JustStartedExisting {
                            self.set_contents(new_contents.clone());
                        }
                        self.last_inventory_message = new_contents;
                    }
                }
            }
        }

        if self.dirty_text {
            self.dirty_text = false;
            Some(self.current_contents.text().clone())
        } else {
            None
        }
    }
}

impl Default for TooltipState {
    fn default() -> Self {
        Self {
            character: None,
            character_gate: Gate::default(),
            dirty_inventory: false,
            dirty_text: false,
            current_contents: TooltipContents::JustStartedExisting,
            last_inventory_message: TooltipContents::JustStartedExisting,
            age: None,
        }
    }
}

/// Describes some content the tooltip might be showing.
///
/// Right now, this data structure aids distinguishing between cases where text should be
/// shown even if it is nominally equal (e.g. two tools with the same name) but in the
/// future it might also provide styling information.
#[derive(Debug, Clone, PartialEq, Eq)]
enum TooltipContents {
    /// Special value for when the UI is initialized, to avoid popping up a tooltip
    /// right away.
    JustStartedExisting,
    Blanked,
    Message(Arc<str>),
    InventoryItem {
        source_slot: usize,
        text: Arc<str>,
    },
}

impl TooltipContents {
    fn text(&self) -> &Arc<str> {
        match self {
            TooltipContents::JustStartedExisting | TooltipContents::Blanked => &*EMPTY_ARC_STR,
            TooltipContents::Message(m) => m,
            TooltipContents::InventoryItem { text, .. } => text,
        }
    }
}

#[derive(Debug)]
pub(crate) struct TooltipController {
    hud_blocks: Arc<HudBlocks>,
    /// Tracks what we should be displaying and serves as dirty flag.
    state: Arc<Mutex<TooltipState>>,
    text_space: URef<Space>,
}

impl TooltipController {
    const RESOLUTION: Resolution = 16;

    pub(crate) fn new(
        state: Arc<Mutex<TooltipState>>,
        space: &mut Space,
        layout: &HudLayout,
        hud_blocks: Arc<HudBlocks>,
        universe: &mut Universe,
    ) -> Self {
        let frame = layout.toolbar_text_frame();
        let text_space = Space::builder(Grid::new(
            GridPoint::origin(),
            GridVector::new(
                frame.size().x * GridCoordinate::from(Self::RESOLUTION),
                frame.size().y * GridCoordinate::from(Self::RESOLUTION),
                2,
            ),
        ))
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build_empty();
        let text_space_ref = universe.insert_anonymous(text_space);
        let toolbar_text_blocks = space_to_blocks(
            Self::RESOLUTION,
            BlockAttributes {
                // TODO: We need an animation_hint that describes the thing that the text does:
                // toggling visible/invisible and not wanting to get lighting artifacts that might
                // result from that. (Though I have a notion to add fade-out, which wants CONTINUOUS
                // anyway.)
                //
                // ...wait, maybe tooltip vanishing should be based on removing the blocks entirely,
                // instead of _just_ changing the text space. That would cooperate with light
                // more straightforwardly.
                animation_hint: AnimationHint::CONTINUOUS,
                ..BlockAttributes::default()
            },
            text_space_ref.clone(),
        )
        .unwrap();
        debug_assert_eq!(toolbar_text_blocks.grid().size(), frame.size());
        space
            .fill(frame, |p| {
                Some(&toolbar_text_blocks[p - frame.lower_bounds().to_vec()])
            })
            .unwrap();

        Self {
            hud_blocks,
            state,
            text_space: text_space_ref,
        }
    }
}

impl WidgetController for TooltipController {
    fn step(&mut self, tick: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        // None if no update is needed
        let text_update: Option<Arc<str>> = self
            .state
            .try_lock()
            .ok()
            .and_then(|mut state| state.step(&self.hud_blocks, tick));

        if let Some(text) = text_update {
            self.text_space.try_modify(|text_space| {
                let grid = text_space.grid();
                text_space.fill_uniform(grid, &AIR).unwrap();

                // Note on dimensions: HudFont is currently 13 pixels tall, and we're using
                // the standard 16-voxel space resolution, and hud_blocks.text has a 1-pixel border,
                // so we have 16 - (13 + 2) = 1 voxel of free alignment, which I've chosen to put on
                // the top edge.
                let text_obj = Text::with_text_style(
                    &text,
                    Point::new(grid.size().x / 2, -1),
                    MonoTextStyle::new(&HudFont, &self.hud_blocks.text),
                    TextStyleBuilder::new()
                        .baseline(Baseline::Bottom)
                        .alignment(Alignment::Center)
                        .build(),
                );
                text_obj.draw(&mut text_space.draw_target(GridMatrix::FLIP_Y))?;
                Ok::<(), Box<dyn Error + Send + Sync>>(())
            })??;
        }
        Ok(WidgetTransaction::default())
    }
}

#[cfg(test)]
mod tests {
    use crate::util::YieldProgress;

    use super::*;
    use futures_executor::block_on;

    #[test]
    fn tooltip_timeout_and_dirty_text() {
        // TODO: reduce boilerplate
        let mut universe = Universe::new();
        let hud_blocks = &block_on(HudBlocks::new(&mut universe, YieldProgress::noop(), 16));

        // Initial state: no update.
        let mut t = TooltipState::default();
        assert_eq!(t.step(hud_blocks, Tick::from_seconds(0.5)), None);
        assert_eq!(t.age, None);

        // Add a message.
        t.set_message("Hello world".into());
        assert_eq!(t.age, Some(Duration::ZERO));
        assert_eq!(
            t.step(hud_blocks, Tick::from_seconds(0.25)),
            Some("Hello world".into())
        );
        // Message is only emitted from step() once.
        assert_eq!(t.step(hud_blocks, Tick::from_seconds(0.25)), None);
        assert_eq!(t.age, Some(Duration::from_millis(500)));

        // Advance time until it should time out.
        assert_eq!(
            t.step(hud_blocks, Tick::from_seconds(0.501)),
            Some("".into())
        );
        assert_eq!(t.age, None);
        // Empty string is only emitted from step() once.
        assert_eq!(t.step(hud_blocks, Tick::from_seconds(2.00)), None);
    }
}
