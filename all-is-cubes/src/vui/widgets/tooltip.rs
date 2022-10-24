use std::error::Error;
use std::sync::{Arc, Mutex};

use cgmath::EuclideanSpace as _;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::prelude::Point;
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use embedded_graphics::Drawable as _;
use instant::Duration;
use once_cell::sync::Lazy;

use crate::block::{space_to_blocks, AnimationHint, BlockAttributes, Resolution, AIR};
use crate::character::{Character, CharacterChange};
use crate::listen::{FnListener, Gate, Listener};
use crate::math::{GridAab, GridCoordinate, GridMatrix, GridPoint, GridVector};
use crate::space::{Space, SpacePhysics, SpaceTransaction};
use crate::time::Tick;
use crate::universe::{URef, Universe};
use crate::vui::hud::{HudBlocks, HudFont};
use crate::vui::{LayoutRequest, Layoutable, Widget, WidgetController, WidgetTransaction};

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
                        .map(|ev_block| ev_block.attributes.display_name.into_owned().into())
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
            TooltipContents::JustStartedExisting | TooltipContents::Blanked => &EMPTY_ARC_STR,
            TooltipContents::Message(m) => m,
            TooltipContents::InventoryItem { text, .. } => text,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Tooltip {
    width_in_hud: GridCoordinate,
    hud_blocks: Arc<HudBlocks>,
    /// Tracks what we should be displaying and serves as dirty flag.
    state: Arc<Mutex<TooltipState>>,
    /// Space we write the text into.
    text_space: URef<Space>,
}

impl Tooltip {
    const RESOLUTION: Resolution = Resolution::R16;

    pub(crate) fn new(
        state: Arc<Mutex<TooltipState>>,
        hud_blocks: Arc<HudBlocks>,
        universe: &mut Universe,
    ) -> Arc<Self> {
        let width_in_hud = 25; // TODO: magic number
        let text_space = Space::builder(GridAab::from_lower_size(
            GridPoint::origin(),
            GridVector::new(
                width_in_hud * GridCoordinate::from(Self::RESOLUTION),
                GridCoordinate::from(Self::RESOLUTION),
                2,
            ),
        ))
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build();
        Arc::new(Self {
            width_in_hud,
            hud_blocks,
            state,
            text_space: universe.insert_anonymous(text_space),
        })
    }
}

impl Layoutable for Tooltip {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: GridVector::new(self.width_in_hud, 1, 1),
        }
    }
}

impl Widget for Tooltip {
    fn controller(self: Arc<Self>, grant: &crate::vui::LayoutGrant) -> Box<dyn WidgetController> {
        Box::new(TooltipController {
            definition: self,
            position: grant.bounds,
        })
    }
}

#[derive(Debug)]
struct TooltipController {
    definition: Arc<Tooltip>,
    position: GridAab,
}

impl WidgetController for TooltipController {
    fn initialize(&mut self) -> Result<WidgetTransaction, crate::vui::InstallVuiError> {
        let toolbar_text_blocks = space_to_blocks(
            Tooltip::RESOLUTION,
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
            self.definition.text_space.clone(),
        )
        .unwrap(); // TODO: should be InstallVuiError but we don't have a good way of constructing it

        let mut txn = SpaceTransaction::default();
        let origin: GridPoint = self.position.lower_bounds();

        // TODO: there should be a space_to_transaction_copy function or something
        // to implement this systematically
        for i in 0..self.position.size().x {
            txn.set_overwrite(
                origin + i * GridVector::unit_x(),
                toolbar_text_blocks[GridPoint::from_vec(i * GridVector::unit_x())].clone(),
            );
        }
        Ok(txn)
    }

    fn step(&mut self, tick: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        // None if no update is needed
        let text_update: Option<Arc<str>> = self
            .definition
            .state
            .try_lock()
            .ok()
            .and_then(|mut state| state.step(&self.definition.hud_blocks, tick));

        if let Some(text) = text_update {
            self.definition.text_space.try_modify(|text_space| {
                let bounds = text_space.bounds();
                text_space.fill_uniform(bounds, &AIR).unwrap();

                // Note on dimensions: HudFont is currently 13 pixels tall, and we're using
                // the standard 16-voxel space resolution, and hud_blocks.text has a 1-pixel border,
                // so we have 16 - (13 + 2) = 1 voxel of free alignment, which I've chosen to put on
                // the top edge.
                let text_obj = Text::with_text_style(
                    &text,
                    Point::new(bounds.size().x / 2, -1),
                    MonoTextStyle::new(&HudFont, &self.definition.hud_blocks.text),
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
        let hud_blocks = &block_on(HudBlocks::new(
            &mut universe,
            YieldProgress::noop(),
            Resolution::R16,
        ));

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
