// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! TODO: Explain this file properly once it is stabler. Right now it is just a piece of refactoring the VUI code towards modularity.

use std::error::Error;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::sync::{Arc, Mutex};

use cgmath::EuclideanSpace as _;
use embedded_graphics::mono_font::iso_8859_1;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::prelude::Point;
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use embedded_graphics::Drawable as _;
use instant::Duration;
use once_cell::sync::Lazy;

use crate::apps::Tick;
use crate::block::{space_to_blocks, AnimationHint, Block, BlockAttributes, Resolution, AIR};
use crate::character::{Character, CharacterChange};
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::inv::Slot;
use crate::listen::{DirtyFlag, FnListener, Gate, ListenableSource, Listener as _};
use crate::math::{GridCoordinate, GridMatrix, GridPoint, GridVector};
use crate::space::{Grid, Space, SpacePhysics, SpaceTransaction};
use crate::transaction::Merge;
use crate::universe::{URef, Universe};
use crate::vui::hud::{HudBlocks, HudFont, HudLayout};

// Placeholder for likely wanting to change this later
pub(super) type WidgetTransaction = SpaceTransaction;

pub(crate) struct WidgetSpaceView<'a> {
    pub(super) _phantom: PhantomData<&'a ()>,
    pub(super) space: URef<Space>,
    // TODO: Define a coordinate transform rather than each widget knowing its location
    // view_to_space_transform: GridMatrix,
}

/// A form of using a region of a [`Space`] as a UI widget.
///
/// TODO: Merge this into the Behavior trait
pub(crate) trait WidgetController: Debug {
    /// Write the initial state of the widget to the space.
    ///
    /// TODO: Be more specific than Box<dyn Error> -- perhaps InGenError
    /// TODO: Stop using &mut self in favor of a transaction, like Behavior
    fn initialize(
        &mut self,
        _sv: &WidgetSpaceView<'_>,
    ) -> Result<WidgetTransaction, Box<dyn Error>> {
        Ok(WidgetTransaction::default())
    }

    /// TODO: Be more specific than Box<dyn Error> -- perhaps InGenError
    /// TODO: Stop using &mut self in favor of a transaction, like Behavior
    fn step(
        &mut self,
        sv: &WidgetSpaceView<'_>,
        tick: Tick,
    ) -> Result<WidgetTransaction, Box<dyn Error>>;

    /// Update which character this widget displays the state of.
    ///
    /// TODO: Replace this with maybe a ListenableSource<URef<Character>> or some other data binding
    /// strategy that doesn't hardwire the Character type in quite this way
    /// TODO: Stop using &mut self in favor of a transaction, like Behavior
    fn set_character(&mut self, _character: Option<&URef<Character>>) {}
}

/// Manages a single-block toggle button.
#[derive(Debug)]
pub(crate) struct ToggleButtonController {
    position: GridPoint,
    states: [Block; 2],
    data_source: ListenableSource<bool>,
    todo: DirtyFlag,
    //action: Box<dyn Fn() + Send + Sync>,
}

impl ToggleButtonController {
    pub(crate) fn new(
        position: GridPoint,
        data_source: ListenableSource<bool>,
        off: Block,
        on: Block,
    ) -> Self {
        let todo = DirtyFlag::new(true);
        data_source.listen(todo.listener());
        Self {
            position,
            states: [off, on],
            todo,
            data_source,
        }
    }
}

impl WidgetController for ToggleButtonController {
    fn step(
        &mut self,
        _: &WidgetSpaceView<'_>,
        _: Tick,
    ) -> Result<WidgetTransaction, Box<dyn Error>> {
        Ok(if self.todo.get_and_clear() {
            SpaceTransaction::set_cube(
                self.position,
                None,
                Some(self.states[self.data_source.snapshot() as usize].clone()),
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
    fn step(
        &mut self,
        _sv: &WidgetSpaceView<'_>,
        _tick: Tick,
    ) -> Result<WidgetTransaction, Box<dyn Error>> {
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
    todo: DirtyFlag,
    /// TODO: Generalize to noncharacters
    inventory_source: Option<URef<Character>>,
    inventory_gate: Gate,
    first_slot_position: GridPoint,
    slot_count: usize,
    /// Space for drawing per-slot text labels
    slot_text_space: URef<Space>,
    slot_text_resolution: Resolution,
}

impl ToolbarController {
    pub(crate) const TOOLBAR_STEP: GridCoordinate = 2;

    pub fn new(
        inventory_source: Option<URef<Character>>,
        hud_blocks: Arc<HudBlocks>,
        layout: &HudLayout,
        universe: &mut Universe,
    ) -> Self {
        let slot_count = layout.toolbar_positions;

        let todo = DirtyFlag::new(true);

        let (gate, listener) = todo.listener().gate();
        if let Some(character) = &inventory_source {
            character.borrow().listen(listener);
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
            todo,
            inventory_source,
            inventory_gate: gate,
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
        _sv: &WidgetSpaceView<'_>,
        slots: &[Slot],
        selected_slots: &[usize],
    ) -> Result<WidgetTransaction, Box<dyn Error>> {
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
    fn initialize(&mut self, _: &WidgetSpaceView<'_>) -> Result<WidgetTransaction, Box<dyn Error>> {
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

    fn step(
        &mut self,
        sv: &WidgetSpaceView<'_>,
        _: Tick,
    ) -> Result<WidgetTransaction, Box<dyn Error>> {
        Ok(if self.todo.get_and_clear() {
            if let Some(inventory_source) = &self.inventory_source {
                let character = inventory_source.borrow();
                let slots: &[Slot] = &character.inventory().slots;
                self.write_items(sv, slots, &character.selected_slots())?
            } else {
                // TODO: clear toolbar ... once self.inventory_source can transition from Some to None at all
                WidgetTransaction::default()
            }
        } else {
            WidgetTransaction::default()
        })
    }

    fn set_character(&mut self, character: Option<&URef<Character>>) {
        let (gate, listener) = self.todo.listener().gate();
        if let Some(character) = character {
            character.borrow().listen(listener);
        }
        self.inventory_source = character.cloned();
        self.inventory_gate = gate;
        self.todo.set();
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
    fn step(
        &mut self,
        hud_blocks: &HudBlocks,
        _sv: &WidgetSpaceView<'_>,
        tick: Tick,
    ) -> Option<Arc<str>> {
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
    fn step(
        &mut self,
        sv: &WidgetSpaceView<'_>,
        tick: Tick,
    ) -> Result<WidgetTransaction, Box<dyn Error>> {
        // None if no update is needed
        let text_update: Option<Arc<str>> = self
            .state
            .try_lock()
            .ok()
            .and_then(|mut state| state.step(&self.hud_blocks, sv, tick));

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
                Ok::<(), Box<dyn Error>>(())
            })??;
        }
        Ok(WidgetTransaction::default())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tooltip_timeout_and_dirty_text() {
        // TODO: reduce boilerplate
        let mut universe = Universe::new();
        let hud_blocks = &HudBlocks::new(&mut universe, 16);
        let sv = WidgetSpaceView {
            _phantom: PhantomData,
            space: universe.insert_anonymous(Space::empty_positive(1, 1, 1)),
        };

        // Initial state: no update.
        let mut t = TooltipState::default();
        assert_eq!(t.step(hud_blocks, &sv, Tick::from_seconds(0.5)), None);
        assert_eq!(t.age, None);

        // Add a message.
        t.set_message("Hello world".into());
        assert_eq!(t.age, Some(Duration::ZERO));
        assert_eq!(
            t.step(hud_blocks, &sv, Tick::from_seconds(0.25)),
            Some("Hello world".into())
        );
        // Message is only emitted from step() once.
        assert_eq!(t.step(hud_blocks, &sv, Tick::from_seconds(0.25)), None);
        assert_eq!(t.age, Some(Duration::from_millis(500)));

        // Advance time until it should time out.
        assert_eq!(
            t.step(hud_blocks, &sv, Tick::from_seconds(0.501)),
            Some("".into())
        );
        assert_eq!(t.age, None);
        // Empty string is only emitted from step() once.
        assert_eq!(t.step(hud_blocks, &sv, Tick::from_seconds(2.00)), None);
    }
}
