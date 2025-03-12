use alloc::boxed::Box;
use alloc::sync::Arc;
use core::error::Error;
use core::iter;
use core::ops::Range;
use std::sync::Mutex;

use all_is_cubes::block::{self, Block, Resolution, text};
use all_is_cubes::character::Character;
use all_is_cubes::content::palette;
use all_is_cubes::inv;
use all_is_cubes::listen::{self, Listen as _};
use all_is_cubes::math::{
    Cube, GridAab, GridCoordinate, GridPoint, GridSize, GridSizeCoord, GridVector,
};
use all_is_cubes::space::{CubeTransaction, SpaceTransaction};
use all_is_cubes::time::Duration;
use all_is_cubes::transaction::Merge as _;
use all_is_cubes::universe::{ReadTicket, StrongHandle};
use all_is_cubes::{arcstr, universe};

use crate::inv_watch::InventoryWatcher;
use crate::ui_content::{CueMessage, CueNotifier, hud::HudBlocks};
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
    watcher: Arc<Mutex<InventoryWatcher>>,
    cue_channel: CueNotifier,

    /// Number of slots this toolbar draws.
    slot_range: Range<u16>,

    slot_info_text_template: text::TextBuilder,
}

impl Toolbar {
    // Stride between individual tool icon positions.
    const TOOLBAR_STEP: GridCoordinate = 2;

    pub fn new(
        character_source: listen::DynSource<Option<StrongHandle<Character>>>,
        // TODO: Take WidgetTheme instead of HudBlocks, or move this widget out of the widgets module.
        hud_blocks: Arc<HudBlocks>,
        slot_range: Range<u16>,
        cue_channel: CueNotifier,
    ) -> Arc<Self> {
        Arc::new(Self {
            watcher: Arc::new(Mutex::new(InventoryWatcher::new(
                character_source,
                hud_blocks.icons.clone(),
            ))),
            hud_blocks,
            cue_channel,
            slot_range,
            slot_info_text_template: text::Text::builder()
                .foreground(block::from_color!(palette::HUD_TEXT_FILL))
                .outline(Some(block::from_color!(palette::HUD_TEXT_STROKE)))
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

    fn slot_count(&self) -> u16 {
        self.slot_range.end.saturating_sub(self.slot_range.start)
    }
}

impl Layoutable for Toolbar {
    fn requirements(&self) -> LayoutRequest {
        LayoutRequest {
            minimum: GridSize::new(
                GridSizeCoord::from(self.slot_count()) * Self::TOOLBAR_STEP.cast_unsigned() + 1,
                3,
                3,
            ),
        }
    }
}

impl Widget for Toolbar {
    fn controller(self: Arc<Self>, grant: &vui::LayoutGrant) -> Box<dyn WidgetController> {
        let bounds = grant.bounds;

        let todo_more = listen::StoreLock::new(ToolbarTodo {
            button_pressed_decay: [Duration::ZERO; inv::TOOL_SELECTIONS],
        });
        self.cue_channel.listen(todo_more.listener());

        Box::new(ToolbarController {
            todo_more,
            // TODO: obey gravity when positioning within the grant
            first_slot_position: Cube::new(
                bounds.lower_bounds().x.midpoint(bounds.upper_bounds().x)
                    - GridCoordinate::from(self.slot_count()) * Toolbar::TOOLBAR_STEP / 2
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
    todo_more: listen::StoreLock<ToolbarTodo>,
    first_slot_position: Cube,
}

impl ToolbarController {
    /// Given an inventory index, returns the place it should be drawn.
    fn slot_position(&self, slot_index: u16) -> Cube {
        assert!(self.definition.slot_range.contains(&slot_index));

        self.first_slot_position
            + GridVector::new(2, 0, 0)
                * GridCoordinate::from(slot_index - self.definition.slot_range.start)
    }

    /// Returns a transaction to draw items and their stack counts, without using self.character
    /// but only the given inputs.
    fn write_items(
        &self,
        watcher: &InventoryWatcher,
    ) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        let mut txn = SpaceTransaction::default();

        // Note that `contents_iter` is infinite; when zipped with `slot_range` it is then
        // guaranteed to be the right length for our number of slots.
        let slots = watcher.inventory().slots();
        let slot_range = self.definition.slot_range.clone();
        let contents_iter = slots[usize::from(slot_range.start).min(slots.len())..]
            .iter()
            .chain(iter::repeat(&inv::Slot::Empty));
        let icons_iter = watcher.snapshotted_icons()
            [usize::from(slot_range.start).min(slots.len())..]
            .iter()
            .chain(iter::repeat(&block::AIR));

        for ((slot_index, stack), icon_block) in slot_range.zip(contents_iter).zip(icons_iter) {
            let icon_cube = self.slot_position(slot_index);
            let info_cube = icon_cube + GridVector::new(-1, 0, 0);

            // Draw icon
            txn.merge_from(
                CubeTransaction::replacing(None, Some(icon_block.clone())).at(icon_cube),
            )?;

            // Draw stack count text
            txn.merge_from(
                CubeTransaction::replacing(
                    None,
                    Some(
                        self.definition
                            .slot_info_text_template
                            .clone()
                            .string(match stack.count() {
                                0 | 1 => arcstr::ArcStr::default(),
                                count => arcstr::format!("{count}×"),
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
        selected_slots: &[inv::Ix],
        pressed: [bool; inv::TOOL_SELECTIONS],
    ) -> WidgetTransaction {
        let mut txn = SpaceTransaction::default();
        for index in self.definition.slot_range.clone() {
            let position = self.slot_position(index);
            let this_slot_selected_mask = core::array::from_fn(|sel| {
                if selected_slots.get(sel).is_some_and(|&i| i == index) {
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
        txn
    }
}

impl WidgetController for ToolbarController {
    fn initialize(
        &mut self,
        _: &vui::WidgetContext<'_, '_>,
    ) -> Result<WidgetTransaction, InstallVuiError> {
        let slot_count = self.definition.slot_count();

        let mut txn = SpaceTransaction::default();

        // Compute the volume in which the slot frame graphics land.
        let frame_region = GridAab::from_lower_upper(
            [
                self.first_slot_position.x - 1,
                self.first_slot_position.y - 1,
                self.first_slot_position.z - 1,
            ],
            [
                self.first_slot_position.x + GridCoordinate::from(slot_count) * 2,
                self.first_slot_position.y,
                self.first_slot_position.z + 2,
            ],
        );

        /// TODO: figure out how to express this as a general helper
        fn zoom(block: &Block, pos: GridPoint) -> Block {
            block.clone().with_modifier(block::Zoom::new(Resolution::R4, pos))
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

            let frame_part = if x == i32::from(slot_count) * 2 - 1 {
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

    fn synchronize(&mut self, world_read_ticket: ReadTicket<'_>, ui_read_ticket: ReadTicket<'_>) {
        let watcher = &mut *self.definition.watcher.lock().unwrap();
        watcher.update(world_read_ticket, ui_read_ticket);
    }

    fn step(
        &mut self,
        context: &vui::WidgetContext<'_, '_>,
    ) -> Result<vui::StepSuccess, vui::StepError> {
        // Extract button pressed state from todo (don't hold the lock more than necessary)
        let mut pressed_buttons: [bool; inv::TOOL_SELECTIONS] = [false; inv::TOOL_SELECTIONS];
        let mut should_update_pointers = false;
        {
            let todo = &mut self.todo_more.lock();
            for (i, t) in todo.button_pressed_decay.iter_mut().enumerate() {
                if *t != Duration::ZERO {
                    // include a final goes-to-zero update
                    should_update_pointers = true;
                }
                *t = t.saturating_sub(context.tick().delta_t_duration());
                pressed_buttons[i] = *t != Duration::ZERO;
            }
        }

        let watcher = &mut *self.definition.watcher.lock().unwrap();

        // TODO: check watcher's notifs on whether we should update or not
        let should_update_inventory = true;

        let slots_txn = if should_update_inventory {
            self.write_items(watcher)?
        } else {
            WidgetTransaction::default()
        };

        // should_update_inventory is currently true when the selected_slots value changes.
        // TODO: InventoryWatcher should provide us this
        let pointers_txn = if should_update_inventory || should_update_pointers {
            self.write_pointers(&watcher.selected_slots(), pressed_buttons)
        } else {
            WidgetTransaction::default()
        };

        // TODO: Use Then::Sleep and a waker
        Ok((slots_txn.merge(pointers_txn).unwrap(), vui::Then::Step))
    }
}

impl universe::VisitHandles for ToolbarController {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        let Self {
            definition: _,
            todo_more: _,
            first_slot_position: _,
        } = self;
        // arguably we should visit the definition but traversing HudBlocks is almost certainly redundant
    }
}

#[derive(Debug)]
struct ToolbarTodo {
    button_pressed_decay: [Duration; inv::TOOL_SELECTIONS],
}

impl listen::Store<CueMessage> for ToolbarTodo {
    fn receive(&mut self, messages: &[CueMessage]) {
        for message in messages {
            match *message {
                CueMessage::Clicked(button) => {
                    if let Some(t) = self.button_pressed_decay.get_mut(button) {
                        *t = Duration::from_millis(200);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::euclid::Vector3D;
    use all_is_cubes::math::Face6;
    use all_is_cubes::space::Space;
    use all_is_cubes::universe::{Universe, UniverseTransaction};
    use all_is_cubes::util::yield_progress_for_testing;
    use all_is_cubes::{inv, space, time};

    /// Test that [`Toolbar`] will not panic if given inventory slot ranges exceeding the size of
    /// the inventory.
    #[macro_rules_attribute::apply(smol_macros::test)]
    async fn inventory_too_short() {
        // Create test setup with a character with inventory with only 1 slot.
        let mut universe = Universe::new();
        let space_for_character = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
        let mut spawn = space_for_character.read(universe.read_ticket()).unwrap().spawn().clone();
        spawn.set_inventory(vec![inv::Slot::Empty]);
        let character = StrongHandle::new(
            universe.insert_anonymous(Character::spawn(&spawn, space_for_character)),
        );
        let character_source = listen::constant(Some(character));
        let hud_blocks = Arc::new(
            HudBlocks::new(
                universe.read_ticket(),
                &mut UniverseTransaction::default(),
                yield_progress_for_testing(),
            )
            .await,
        );
        let cue_channel = Arc::new(listen::Notifier::new());
        let gravity = Vector3D::splat(vui::layout::Align::Low);

        let widgets = Arc::new(vui::LayoutTree::Stack {
            direction: Face6::PX,
            children: vec![
                // This one is longer than the inventory
                vui::leaf_widget(Toolbar::new(
                    character_source.clone(),
                    hud_blocks.clone(),
                    0..2,
                    cue_channel.clone(),
                )),
                // This one is completely out of range
                vui::leaf_widget(Toolbar::new(
                    character_source,
                    hud_blocks,
                    10..12,
                    cue_channel,
                )),
            ],
        });

        let space = widgets
            .to_space(universe.read_ticket(), space::Builder::default(), gravity)
            .unwrap();
        let space_handle = StrongHandle::new(universe.insert_anonymous(space));

        // Step and observe that no panic ocurred
        vui::synchronize_widgets(
            universe.read_ticket(),
            universe.read_ticket(),
            &space_handle.read(universe.read_ticket()).unwrap(),
        );
        universe.step(false, time::Deadline::Whenever);
    }
}
