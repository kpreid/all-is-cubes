// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! TODO: Explain this file properly once it is stabler. Right now it is just a piece of refactoring the VUI code towards modularity.

use std::error::Error;
use std::fmt::Debug;
use std::sync::{Arc, Mutex};

use cgmath::EuclideanSpace as _;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::prelude::Point;
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use embedded_graphics::{Drawable as _, Pixel};
use instant::Duration;
use once_cell::sync::Lazy;

use crate::apps::Tick;
use crate::block::{space_to_blocks, AnimationHint, BlockAttributes, Resolution, AIR};
use crate::character::{Character, CharacterChange};
use crate::drawing::VoxelBrush;
use crate::inv::Slot;
use crate::listen::{DirtyFlag, FnListener, ListenableSource};
use crate::math::{GridCoordinate, GridMatrix, GridPoint, GridVector};
use crate::raycast::Face;
use crate::space::{Grid, Space, SpacePhysics};
use crate::universe::{URef, Universe};
use crate::vui::hud::{HudBlocks, HudFont, HudLayout};
use crate::vui::Icons;

pub(crate) struct WidgetSpaceView<'a> {
    // TODO: Eliminate HudLayout's functions in favor of widgets having been configured with their location
    pub(super) hud_blocks: &'a HudBlocks,
    pub(super) space: URef<Space>,
    // TODO: Define a coordinate transform rather than each widget knowing its location
    // view_to_space_transform: GridMatrix,
}

impl WidgetSpaceView<'_> {}

/// A form of using a region of a [`Space`] as a UI widget.
///
/// TODO: Merge this into the Behavior trait
pub(crate) trait WidgetController: Debug {
    /// Write the initial state of the widget to the space.
    ///
    /// TODO: Replace direct mutations with returning a UniverseTransaction
    /// TODO: Be more specific than Box<dyn Error> -- perhaps InGenError
    fn initialize(&mut self, _sv: &WidgetSpaceView<'_>) -> Result<(), Box<dyn Error>> {
        Ok(())
    }

    /// TODO: Replace direct mutations with returning a UniverseTransaction
    /// TODO: Be more specific than Box<dyn Error> -- perhaps InGenError
    fn step(&mut self, sv: &WidgetSpaceView<'_>, tick: Tick) -> Result<(), Box<dyn Error>>;
}

/// Shows/hides the crosshair depending on mouselook mode.
#[derive(Debug)]
pub(crate) struct CrosshairController {
    todo: DirtyFlag,
    position: GridPoint,
    mouselook_mode: ListenableSource<bool>,
}

impl CrosshairController {
    pub fn new(position: GridPoint, mouselook_mode: ListenableSource<bool>) -> Self {
        let todo = DirtyFlag::new(false);
        mouselook_mode.listen(todo.listener());
        Self {
            todo,
            position,
            mouselook_mode,
        }
    }
}

impl WidgetController for CrosshairController {
    fn step(&mut self, sv: &WidgetSpaceView<'_>, _tick: Tick) -> Result<(), Box<dyn Error>> {
        if self.todo.get_and_clear() {
            sv.space.try_modify(|space| {
                space.set(
                    self.position,
                    if *self.mouselook_mode.get() {
                        &sv.hud_blocks.icons[Icons::Crosshair]
                    } else {
                        &AIR
                    },
                )
            })??;
        }
        Ok(())
    }
}

/// Displays inventory contents in toolbar format.
///
/// TODO: We may or may not want to expand this to handle general inventory viewing
#[derive(Debug)]
pub(crate) struct ToolbarController {
    todo: DirtyFlag,
    /// TODO: Generalize to noncharacters
    inventory_source: Option<URef<Character>>,
    first_slot_position: GridPoint,
    /// TODO: replace with a range so we can have multiple things
    slot_count: usize,
}

impl ToolbarController {
    pub(crate) const TOOLBAR_STEP: GridCoordinate = 2;

    pub fn new(inventory_source: Option<URef<Character>>, layout: &HudLayout) -> Self {
        let todo = DirtyFlag::new(true);

        if let Some(character) = &inventory_source {
            character.borrow().listen(todo.listener());
        }

        Self {
            todo,
            inventory_source,
            first_slot_position: layout.first_tool_icon_position(),
            slot_count: layout.toolbar_positions,
        }
    }

    fn slot_position(&self, slot_index: usize) -> GridPoint {
        self.first_slot_position + GridVector::unit_x() * 2 * slot_index as GridCoordinate
    }

    /// Helper for WidgetController impl; writes to the space without using self.character
    fn write_items(
        &self,
        sv: &WidgetSpaceView<'_>,
        slots: &[Slot],
        selected_slots: &[usize],
    ) -> Result<(), Box<dyn Error>> {
        sv.space.try_modify(|space| {
            for (index, stack) in slots.iter().enumerate() {
                if index >= self.slot_count {
                    // TODO: must clear nonexistent positions, eventually
                    break;
                }

                let position = self.slot_position(index);
                // Draw icon
                space.set(position, &*stack.icon(&sv.hud_blocks.icons))?;
                // Draw pointers.
                // TODO: refactor to not use FLIP_Y now that it isn't a hardcoded feature
                // TODO: draw_target isn't especially helpful here
                let toolbar_disp = &mut space.draw_target(
                    GridMatrix::from_translation(position.to_vec()) * GridMatrix::FLIP_Y,
                );
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
                let brush: &VoxelBrush<'_> =
                    &sv.hud_blocks.toolbar_pointer[this_slot_selected_mask];
                Pixel(Point::new(0, 0), brush).draw(toolbar_disp)?;
            }
            Ok(())
        })?
    }
}

impl WidgetController for ToolbarController {
    fn initialize(&mut self, sv: &WidgetSpaceView<'_>) -> Result<(), Box<dyn Error>> {
        let hud_blocks = sv.hud_blocks;
        sv.space.try_modify(|space| {
            let toolbar_disp = &mut space.draw_target(GridMatrix::from_origin(
                self.slot_position(0),
                Face::PX,
                Face::NY,
                Face::PZ,
            ));
            Pixel(Point::new(-1, 0), &hud_blocks.toolbar_left_cap)
                .draw(toolbar_disp)
                .unwrap();
            Pixel(
                Point::new((self.slot_count as i32 - 1) * Self::TOOLBAR_STEP + 1, 0),
                &hud_blocks.toolbar_right_cap,
            )
            .draw(toolbar_disp)
            .unwrap();
            for index in 0..self.slot_count {
                let x = index as i32 * Self::TOOLBAR_STEP;
                Pixel(Point::new(x, 0), &hud_blocks.toolbar_middle)
                    .draw(toolbar_disp)
                    .unwrap();
                if index > 0 {
                    Pixel(Point::new(x - 1, 0), &hud_blocks.toolbar_divider)
                        .draw(toolbar_disp)
                        .unwrap();
                }
            }
        })?;
        Ok(())
    }

    fn step(&mut self, sv: &WidgetSpaceView<'_>, _: Tick) -> Result<(), Box<dyn Error>> {
        if self.todo.get_and_clear() {
            if let Some(inventory_source) = &self.inventory_source {
                let character = inventory_source.borrow();
                let slots: &[Slot] = &character.inventory().slots;
                self.write_items(sv, slots, &character.selected_slots())?;
            } else {
                // TODO: clear toolbar ... once self.inventory_source can transition from Some to None at all
            }
        }
        Ok(())
    }
}

static EMPTY_ARC_STR: Lazy<Arc<str>> = Lazy::new(|| "".into());

#[derive(Debug)]
pub(crate) struct TooltipState {
    /// Character we're reading inventory state from
    character: Option<URef<Character>>,
    /// Whether the tool we should be displaying might have changed.
    dirty_inventory: bool,
    /// Whether the text has changed, possibly from a non-tool source.
    dirty_text: bool,
    /// Which inventory slot we last displayed.
    source_slot: usize,
    /// Text to display. Updated from character inventory when `dirty_inventory` is true.
    text: Arc<str>,
    /// None if the tooltip is blanked
    age: Option<Duration>,
}

impl TooltipState {
    pub(crate) fn bind_to_character(this: &Arc<Mutex<Self>>, character: URef<Character>) {
        character
            .borrow()
            .listen(FnListener::new(
                this,
                move |this: &Mutex<Self>, change| match change {
                    // TODO: Don't dirty if an unrelated inventory slot changed
                    CharacterChange::Inventory(_) | CharacterChange::Selections => {
                        if let Ok(mut this) = this.lock() {
                            this.dirty_inventory = true;
                        }
                    }
                },
            ));
        this.lock().unwrap().character = Some(character);
    }

    pub fn set_text(&mut self, text: Arc<str>) {
        self.dirty_text = true;
        self.dirty_inventory = false;
        self.source_slot = usize::MAX;
        self.text = text;
        self.age = Some(Duration::ZERO);
    }

    /// Advances time and returns the string that should be newly written to the screen, if different than the previous call.
    fn step(&mut self, sv: &WidgetSpaceView<'_>, tick: Tick) -> Option<Arc<str>> {
        if let Some(ref mut age) = self.age {
            *age += tick.delta_t;
            if *age > Duration::from_secs(1) {
                self.set_text(EMPTY_ARC_STR.clone());
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
                        .icon(&sv.hud_blocks.icons)
                        .evaluate()
                        .ok()
                        .map(|ev_block| ev_block.attributes.display_name.to_owned().into())
                        .unwrap_or_else(|| EMPTY_ARC_STR.clone());

                    // Comparing source_slot ensures that if the user toggles between two
                    // inventory slots that have the same name, this is not ignored.
                    if new_text != self.text || selected_slot != self.source_slot {
                        self.text = new_text;
                        self.source_slot = selected_slot;
                        self.age = Some(Duration::ZERO);
                        self.dirty_text = true;
                    }
                }
            }
        }

        if self.dirty_text {
            Some(self.text.clone())
        } else {
            None
        }
    }
}

impl Default for TooltipState {
    fn default() -> Self {
        Self {
            character: None,
            dirty_inventory: false,
            dirty_text: false,
            source_slot: usize::MAX,
            text: EMPTY_ARC_STR.clone(),
            age: None,
        }
    }
}

#[derive(Debug)]
pub(crate) struct TooltipController {
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
            state,
            text_space: text_space_ref,
        }
    }
}

impl WidgetController for TooltipController {
    fn step(&mut self, sv: &WidgetSpaceView<'_>, tick: Tick) -> Result<(), Box<dyn Error>> {
        // None if no update is needed
        let text_update: Option<Arc<str>> = self
            .state
            .try_lock()
            .ok()
            .and_then(|mut state| state.step(sv, tick));

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
                    MonoTextStyle::new(&HudFont, &sv.hud_blocks.text),
                    TextStyleBuilder::new()
                        .baseline(Baseline::Bottom)
                        .alignment(Alignment::Center)
                        .build(),
                );
                text_obj.draw(&mut text_space.draw_target(GridMatrix::FLIP_Y))?;
                Ok::<(), Box<dyn Error>>(())
            })??;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tooltip_timeout() {
        let mut universe = Universe::new();
        let sv = WidgetSpaceView {
            hud_blocks: &HudBlocks::new(&mut universe, 16),
            space: universe.insert_anonymous(Space::empty_positive(1, 1, 1)),
        };

        let mut t = TooltipState::default();
        assert_eq!(t.age, None);
        t.set_text("Hello world".into());
        assert_eq!(t.age, Some(Duration::ZERO));
        t.step(&sv, Tick::from_seconds(0.5));
        assert_eq!(t.age, Some(Duration::from_millis(500)));
        t.step(&sv, Tick::from_seconds(0.501));
        assert_eq!(t.age, None);
        // TODO: assert results of step()
    }
}
