//! Support for widgets that display inventory contents.
//!
//! TODO: This is a pattern that, if it works out, probably generalizes to many other
//! "derived information from a `listen::DynSource` that requires computation" and should become
//! general code that handles the re-listening problem.

use alloc::sync::Arc;
use alloc::vec::Vec;

use all_is_cubes::block::Block;
use all_is_cubes::character::{Character, CharacterChange};
use all_is_cubes::inv;
use all_is_cubes::linking::BlockProvider;
use all_is_cubes::listen::{self, Listen as _, Listener as _};
use all_is_cubes::universe::{ReadTicket, StrongHandle};

use crate::vui;

// -------------------------------------------------------------------------------------------------

/// Some game entity that has an inventory we are watching, or none (treated as empty inventory).
///
/// Currently, no other game object has an inventory, but eventually this will have
/// to become an enum of possibilities.
type Owner = Option<StrongHandle<Character>>;

/// Track the contents of an [`Inventory`] stored elsewhere, and make UI elements for it.
#[derive(Debug)]
pub(crate) struct InventoryWatcher {
    icon_provider: BlockProvider<inv::Icons>,

    /// Source of what inventory we should be looking at.
    inventory_source: listen::DynSource<Owner>,

    /// Last value gotten from `inventory_source`.
    inventory_owner: Owner,

    /// Last inventory gotten from `inventory_owner`
    /// as of the last call to `update()`.
    inventory: inv::Inventory,

    /// Snapshotted (safe to use in UI) icons for each slot in `inventory`.
    snapshotted_icons: Vec<Block>,

    /// Last selected slots gotten from `inventory_owner`
    /// as of the last call to `update()`.
    selected_slots: [inv::Ix; inv::TOOL_SELECTIONS],

    /// Listener gate to cancel the listening when we change [`Owner`]s.
    owner_gate: listen::Gate,

    /// Notifies of changes in the watched contents.
    notifier: Arc<listen::Notifier<WatcherChange>>,

    /// Flag is set whenever either `inventory_source` or `inventory_owner` send a change
    /// notification, so we know to resynchronize.
    dirty: listen::Flag,
}

impl InventoryWatcher {
    /// Create an [`InventoryWatcher`] which will track the contents of the inventory
    /// belonging to the [`Character`] in the given `inventory_source`.
    ///
    /// `ui_universe` will be used to create anonymous resources used to depict the inventory.
    ///
    /// The presented inventory will be empty until the first [`update()`][Self::update].
    pub fn new(
        inventory_source: listen::DynSource<Owner>,
        icon_provider: BlockProvider<inv::Icons>,
    ) -> Self {
        let dirty = listen::Flag::new(true);

        inventory_source.listen(dirty.listener());

        Self {
            icon_provider,
            inventory_source,
            inventory_owner: None, // will be replaced
            inventory: inv::Inventory::new(0),
            snapshotted_icons: Vec::new(),
            selected_slots: [inv::Ix::MAX; inv::TOOL_SELECTIONS],
            owner_gate: listen::Gate::default(),
            notifier: Arc::new(listen::Notifier::new()),
            dirty,
        }
    }

    /// Update this watcher's state from the inventory source, if any change has occurred.
    /// This should be called before making use of updated inventory information.
    pub fn update(
        &mut self,
        inventory_read_ticket: ReadTicket<'_>,
        icons_read_ticket: ReadTicket<'_>,
    ) {
        if !self.dirty.get_and_clear() {
            return;
        }

        // Check if we have a new inventory owner which we need to install a listener on.
        let new_owner = self.inventory_source.get();
        let listener_to_install = if new_owner != self.inventory_owner {
            // Listen directly (so we have a dirty flag to consult) and indirectly to wake our
            // client to have them update us.
            let (new_gate, new_listener) = (
                self.dirty.listener(),
                listen::Notifier::forwarder(Arc::downgrade(&self.notifier)),
            )
                .gate();

            self.inventory_owner = new_owner;
            self.owner_gate = new_gate;
            Some(new_listener)
        } else {
            None
        };

        // Consult the inventory owner, and install a listener if needed.
        let empty_inventory = (
            &inv::Inventory::new(0),
            [inv::Ix::MAX; inv::TOOL_SELECTIONS],
        );
        let character_guard;
        let (new_inventory, new_selections) = match &self.inventory_owner {
            Some(character_handle) => {
                match character_handle.read(inventory_read_ticket) {
                    Ok(cg) => {
                        character_guard = cg;
                        if let Some(l) = listener_to_install {
                            character_guard.listen(
                                l.filter(|cc| match cc {
                                    // This match is useless now, but in the future there will probably
                                    // be CharacterChange messages we want to ignore.
                                    CharacterChange::Inventory(_) | CharacterChange::Selections => {
                                        Some(WatcherChange::NeedsUpdate)
                                    }
                                })
                                .with_stack_buffer::<100>(),
                            );
                        }
                        (
                            character_guard.inventory(),
                            character_guard.selected_slots(),
                        )
                    }
                    Err(handle_error) => {
                        if handle_error.is_transient() && listener_to_install.is_some() {
                            // spin until we can successfully write the listener
                            // TODO: send some kind of deduplicated warning on this case...
                            // or even better, give Handles a way to notify when they become ready
                            // to read.
                            self.dirty.set();
                            self.notifier.notify(&WatcherChange::NeedsUpdate);
                        }
                        empty_inventory
                    }
                }
            }
            _ => {
                // no inventory currently being watched
                empty_inventory
            }
        };

        if *new_inventory != self.inventory {
            self.inventory = new_inventory.clone();
            self.notifier.notify(&WatcherChange::Inventory);

            self.snapshotted_icons.clear();
            self.snapshotted_icons.extend(self.inventory.slots().iter().map(|stack| {
                vui::quote_and_snapshot_block(
                    [icons_read_ticket, inventory_read_ticket],
                    &stack.icon(&self.icon_provider),
                )
            }));
        }
        if new_selections != self.selected_slots {
            self.selected_slots = new_selections;
            self.notifier.notify(&WatcherChange::Selections);
        }
    }

    /// Returns the current [`Character`] whose inventory is being tracked, as of the last
    /// [`Self::update()`].
    #[cfg(test)] // TODO: only used in tests at the moment
    fn character(&self) -> Option<&all_is_cubes::universe::Handle<Character>> {
        use all_is_cubes::universe::Handle;

        self.inventory_owner
            .as_ref()
            .map(<StrongHandle<Character> as AsRef<Handle<Character>>>::as_ref)
    }

    /// Returns the current contents of the watched inventory, as of the last [`Self::update()`].
    pub fn inventory(&self) -> &inv::Inventory {
        &self.inventory
    }

    /// Returns icons for the current contents of the watched inventory, as of the last
    /// [`Self::update()`].
    pub fn snapshotted_icons(&self) -> &[Block] {
        &self.snapshotted_icons
    }

    /// Returns the current selected slots, as of the last [`Self::update()`].
    ///
    /// If the inventory is not a type that has selections, returns `[usize::MAX; TOOL_SELECTIONS]`.
    /// TODO: Better API
    pub fn selected_slots(&self) -> [inv::Ix; inv::TOOL_SELECTIONS] {
        self.selected_slots
    }
}

impl listen::Listen for InventoryWatcher {
    type Msg = WatcherChange;
    type Listener = <listen::Notifier<Self::Msg> as listen::Listen>::Listener;

    fn listen_raw(&self, listener: Self::Listener) {
        self.notifier.listen_raw(listener)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum WatcherChange {
    /// Please call [`InventoryWatcher::update()`] to check for changes.
    NeedsUpdate,

    /// The inventory has changed.
    ///
    /// This message only happens when [`InventoryWatcher::update()`] is called.
    Inventory,

    /// The selected slots have changed.
    ///
    /// This message only happens when [`InventoryWatcher::update()`] is called.
    Selections,
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block;
    use all_is_cubes::character::CharacterTransaction;
    use all_is_cubes::inv;
    use all_is_cubes::space::Space;
    use all_is_cubes::universe::{Handle, Universe};
    use alloc::boxed::Box;

    struct Tester {
        universe: Box<Universe>,
        space: Handle<Space>,
        character: StrongHandle<Character>,
        character_cell: listen::Cell<Option<StrongHandle<Character>>>,
        watcher: InventoryWatcher,
        log: listen::Log<WatcherChange>,
    }
    impl Tester {
        pub fn new() -> Self {
            let mut universe = Universe::new();
            let space = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
            let character = StrongHandle::new(universe.insert_anonymous(Character::spawn_default(
                universe.read_ticket(),
                space.clone(),
            )));
            let character_cell = listen::Cell::new(Some(character.clone()));
            let mut watcher = InventoryWatcher::new(
                character_cell.as_source(),
                BlockProvider::new_sync(|_| block::AIR),
            );
            watcher.update(universe.read_ticket(), ReadTicket::stub());

            // Install listener
            let log: listen::Log<WatcherChange> = listen::Log::new();
            watcher.listen(log.listener());

            Self {
                universe,
                space,
                character,
                character_cell,
                watcher,
                log,
            }
        }
        pub fn update(&mut self) {
            self.watcher.update(self.universe.read_ticket(), ReadTicket::stub());
        }
    }

    #[test]
    fn basic_and_changed_slot_in_character() {
        let mut t = Tester::new();
        // Run redundant update -- should see no effect
        assert_eq!(t.log.drain(), vec![]);
        t.update();
        assert_eq!(t.log.drain(), vec![]);

        assert_eq!(t.watcher.character().unwrap(), &t.character);

        // Make a change to the character and observe update
        t.universe
            .execute_1(
                &t.character,
                CharacterTransaction::inventory(inv::InventoryTransaction::insert([
                    inv::Tool::Activate,
                ])),
            )
            .unwrap();
        assert_eq!(t.log.drain(), vec![WatcherChange::NeedsUpdate]);
        t.update();
        assert_eq!(t.log.drain(), vec![WatcherChange::Inventory]);
    }

    #[test]
    fn follow_changed_character() {
        let mut t = Tester::new();

        // Construct new character with different inventory.
        let new_character = StrongHandle::new(t.universe.insert_anonymous(
            Character::spawn_default(t.universe.read_ticket(), t.space.clone()),
        ));
        t.universe
            .execute_1(
                &new_character,
                CharacterTransaction::inventory(inv::InventoryTransaction::insert([
                    inv::Tool::Activate,
                ])),
            )
            .unwrap();

        t.character_cell.set(Some(new_character.clone()));
        assert_eq!(t.watcher.character().unwrap(), &t.character);
        t.update();
        assert_eq!(t.watcher.character().unwrap(), &new_character);
        assert_eq!(t.log.drain(), vec![WatcherChange::Inventory]);
    }
}
