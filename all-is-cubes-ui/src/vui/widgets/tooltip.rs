use alloc::boxed::Box;
use alloc::sync::Arc;
use std::sync::Mutex;

use all_is_cubes::arcstr::{ArcStr, literal};
use all_is_cubes::block::{self, Block, text};
use all_is_cubes::character::{Character, CharacterChange};
use all_is_cubes::content::palette;
use all_is_cubes::euclid::size3;
use all_is_cubes::inv;
use all_is_cubes::listen::{FnListener, Gate, Listen, Listener};
use all_is_cubes::time::{Duration, Tick};
use all_is_cubes::universe::{Handle, ReadTicket};

use crate::ui_content::hud::HudBlocks;
use crate::vui::{self, LayoutRequest, Layoutable, Widget, WidgetController, widgets};

#[derive(Debug)]
pub(crate) struct TooltipState {
    /// Character we're reading inventory state from
    character: Option<Handle<Character>>,
    /// Listener gate to stop the listener if we change characters
    character_gate: Gate,

    /// Whether the tool we should be displaying might have changed.
    dirty_inventory: bool,
    /// Text to actually show on screen.
    current_contents: TooltipContents,
    /// Last value of `current_contents` that was an inventory item.
    last_inventory_message: TooltipContents,
    /// How long ago the `current_contents` were shown. None if `Blanked`.
    age: Option<Duration>,
}

impl TooltipState {
    pub(crate) fn bind_to_character(
        world_read_ticket: ReadTicket<'_>,
        this_ref: &Arc<Mutex<Self>>,
        character: Handle<Character>,
    ) {
        let (gate, listener) = FnListener::new(
            this_ref,
            move |this: &Mutex<Self>, change: &CharacterChange| match change {
                // TODO: Don't dirty if an unrelated inventory slot changed
                CharacterChange::Inventory(_) | CharacterChange::Selections => {
                    if let Ok(mut this) = this.lock() {
                        this.dirty_inventory = true;
                    }
                }
            },
        )
        .gate();

        // TODO: Think about what state results if either of the locks/borrows fails
        character.read(world_read_ticket).unwrap().listen(listener);
        {
            let mut this = this_ref.lock().unwrap();
            this.character = Some(character);
            this.character_gate = gate;
            this.dirty_inventory = true;
        }
    }

    pub fn set_message(&mut self, text: ArcStr) {
        self.dirty_inventory = false;
        self.set_contents(TooltipContents::Message(text))
    }

    fn set_contents(&mut self, contents: TooltipContents) {
        self.current_contents = contents;
        self.age = Some(Duration::ZERO);
    }

    fn synchronize(
        &mut self,
        world_read_ticket: ReadTicket<'_>,
        ui_read_ticket: ReadTicket<'_>,
        hud_blocks: &HudBlocks,
    ) {
        if self.dirty_inventory {
            self.dirty_inventory = false;

            if let Some(character_handle) = &self.character {
                let character = character_handle.read(world_read_ticket).unwrap();
                let selected_slot = character
                    .selected_slots()
                    .get(1)
                    .copied()
                    .unwrap_or(inv::Ix::MAX);
                if let Some(tool) = character.inventory().get(selected_slot).cloned() {
                    // TODO: This logic is redundant with what `InventoryWatcher` does and should be replaced with it.
                    let icon = tool.icon(&hud_blocks.icons);
                    let new_text = match icon
                        .evaluate(ui_read_ticket.expect_may_fail())
                        .or_else(|_| icon.evaluate(world_read_ticket.expect_may_fail()))
                        .ok()
                    {
                        Some(ev_block) => ev_block.attributes().display_name.clone(),
                        None => literal!(""),
                    };
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
    }

    fn step(&mut self, tick: Tick) {
        if let Some(ref mut age) = self.age {
            *age += tick.delta_t();
            if *age > Duration::from_secs(1) {
                self.set_contents(TooltipContents::Blanked);
                self.age = None;
            }
        }
    }
}

impl Default for TooltipState {
    fn default() -> Self {
        Self {
            character: None,
            character_gate: Gate::default(),
            dirty_inventory: false,
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
    Message(ArcStr),
    InventoryItem {
        source_slot: inv::Ix,
        text: ArcStr,
    },
}

impl TooltipContents {
    fn string(&self) -> &ArcStr {
        static EMPTY: ArcStr = literal!("");

        match self {
            TooltipContents::JustStartedExisting | TooltipContents::Blanked => &EMPTY,
            TooltipContents::Message(m) => m,
            TooltipContents::InventoryItem { text, .. } => text,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Tooltip {
    width_in_hud: u16,
    hud_blocks: Arc<HudBlocks>,

    text_builder: text::TextBuilder,
    /// Tracks what we should be displaying and serves as dirty flag.
    state: Arc<Mutex<TooltipState>>,
}

impl Tooltip {
    pub(crate) fn new(
        state: Arc<Mutex<TooltipState>>,
        // TODO: Take WidgetTheme instead of HudBlocks, or move this widget out of the widgets module.
        hud_blocks: Arc<HudBlocks>,
    ) -> Arc<Self> {
        Arc::new(Self {
            width_in_hud: 25, // TODO: magic number
            hud_blocks,
            text_builder: text::Text::builder()
                .foreground(Block::from(palette::HUD_TEXT_FILL))
                .outline(Some(Block::from(palette::HUD_TEXT_STROKE)))
                .font(text::Font::System16)
                .positioning(text::Positioning {
                    x: text::PositioningX::Center,
                    line_y: text::PositioningY::BodyMiddle,
                    z: text::PositioningZ::Back,
                }),
            state,
        })
    }
}

impl Layoutable for Tooltip {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: size3(self.width_in_hud.into(), 1, 1),
        }
    }
}

impl Widget for Tooltip {
    fn controller(self: Arc<Self>, _: &vui::LayoutGrant) -> Box<dyn WidgetController> {
        Box::new(TooltipController {
            definition: self,
            currently_displayed: TooltipContents::JustStartedExisting,
        })
    }
}

#[derive(Debug)]
struct TooltipController {
    definition: Arc<Tooltip>,
    currently_displayed: TooltipContents,
}

impl WidgetController for TooltipController {
    fn synchronize(&mut self, world_read_ticket: ReadTicket<'_>, ui_read_ticket: ReadTicket<'_>) {
        match self.definition.state.try_lock().ok() {
            Some(mut state) => state.synchronize(
                world_read_ticket,
                ui_read_ticket,
                &self.definition.hud_blocks,
            ),
            None => {}
        }
    }

    fn step(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<vui::StepSuccess, vui::StepError> {
        // None if no update is needed
        let new_contents: Option<TooltipContents> =
            self.definition.state.try_lock().ok().and_then(|mut state| {
                let contents = &state.current_contents;
                let contents = if *contents != self.currently_displayed {
                    Some(contents.clone())
                } else {
                    None
                };

                state.step(context.tick());

                contents
            });

        let txn = if let Some(new_contents) = new_contents {
            let grant = context.grant();
            let text = self
                .definition
                .text_builder
                .clone()
                .layout_bounds(
                    block::Resolution::R16,
                    grant
                        .bounds
                        .translate(-grant.bounds.lower_bounds().to_vector())
                        .multiply(16),
                )
                .string(new_contents.string().clone())
                .build();
            let txn = widgets::text::draw_text_txn(&text, grant, false);

            // Remember what we are about to draw so we know we don't need to redraw it.
            // TODO: We should do this only if the transaction doesn't fail, but there's no way to express that yet.
            self.currently_displayed = new_contents;

            txn
        } else {
            vui::WidgetTransaction::default()
        };
        Ok((txn, vui::Then::Step))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::transaction::{self, Transaction as _};
    use all_is_cubes::universe::{Universe, UniverseTransaction};
    use all_is_cubes::util::yield_progress_for_testing;

    #[tokio::test]
    async fn tooltip_timeout() {
        // TODO: reduce boilerplate

        let mut universe = Universe::new();
        let mut install_txn = UniverseTransaction::default();
        let hud_blocks = &HudBlocks::new(
            universe.read_ticket(),
            &mut install_txn,
            yield_progress_for_testing(),
        )
        .await;
        install_txn
            .execute(&mut universe, (), &mut transaction::no_outputs)
            .unwrap();

        // Initial state: no update.
        let mut t = TooltipState::default();
        t.step(Tick::from_seconds(0.5));
        t.synchronize(universe.read_ticket(), universe.read_ticket(), hud_blocks);
        assert_eq!(t.current_contents, TooltipContents::JustStartedExisting);
        assert_eq!(t.age, None);

        // Add a message.
        t.set_message("Hello world".into());
        assert_eq!(t.age, Some(Duration::ZERO));
        t.step(Tick::from_seconds(0.5));
        t.synchronize(universe.read_ticket(), universe.read_ticket(), hud_blocks);
        assert_eq!(
            t.current_contents,
            TooltipContents::Message(literal!("Hello world"))
        );

        // Advance time until it should time out.
        t.step(Tick::from_seconds(0.501));
        assert_eq!(t.current_contents, TooltipContents::Blanked);
        assert_eq!(t.age, None);
    }
}
