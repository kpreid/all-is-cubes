//! Support for widgets that display inventory contents.
//!
//! TODO: This is a pattern that, if it works out, probably generalizes to many other
//! "derived information from a `listen::DynSource` that requires computation" and should become
//! general code that handles the re-listening problem.

use alloc::sync::Arc;

use all_is_cubes::character::{Character, CharacterChange};
use all_is_cubes::inv;
use all_is_cubes::listen::{self, Listen as _, Listener as _};
use all_is_cubes::universe::{Handle, HandleError, ReadTicket};

/// Some game entity that has an inventory we are watching, or none (treated as empty inventory).
///
/// Currently, no other game object has an inventory, but eventually this will have
/// to become an enum of possibilities.
type Owner = Option<Handle<Character>>;

/// Track the contents of an [`Inventory`] stored elsewhere, and make UI elements for it.
#[derive(Debug)]
pub(crate) struct InventoryWatcher {
    /// Source of what inventory we should be looking at.
    inventory_source: listen::DynSource<Owner>,

    /// Last value gotten from `inventory_source`.
    inventory_owner: Owner,

    /// Last inventory gotten from `inventory_owner`
    /// as of the last call to `update()`.
    inventory: inv::Inventory,

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
    pub fn new(inventory_source: listen::DynSource<Option<Handle<Character>>>) -> Self {
        let dirty = listen::Flag::new(true);

        inventory_source.listen(dirty.listener());

        Self {
            inventory_source,
            inventory_owner: None, // will be replaced
            inventory: inv::Inventory::new(0),
            selected_slots: [inv::Ix::MAX; inv::TOOL_SELECTIONS],
            owner_gate: listen::Gate::default(),
            notifier: Arc::new(listen::Notifier::new()),
            dirty,
        }
    }

    /// Update this watcher's state from the inventory source, if any change has occurred.
    /// This should be called before making use of updated inventory information.
    pub fn update(&mut self, read_ticket: ReadTicket<'_>) {
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
                match character_handle.read(read_ticket) {
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
                    Err(HandleError::InUse(_) | HandleError::NotReady(_)) => {
                        if listener_to_install.is_some() {
                            // spin until we can successfully write the listener
                            // TODO: send some kind of deduplicated warning on this case...
                            // or even better, give Handles a way to notify when they become ready
                            // to read.
                            self.dirty.set();
                            self.notifier.notify(&WatcherChange::NeedsUpdate);
                        }
                        empty_inventory
                    }
                    Err(HandleError::Gone(_)) => {
                        // No inventory exists any more, so nothing to do.
                        empty_inventory
                    }
                    Err(e) => {
                        // TODO: … perhaps the enum should be exhaustive.
                        unreachable!("unknown HandleError: {e:?}");
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
        }
        if new_selections != self.selected_slots {
            self.selected_slots = new_selections;
            self.notifier.notify(&WatcherChange::Selections);
        }
    }

    /// Returns the current [`Character`] whose inventory is being tracked, as of the last
    /// [`Self::update()`].
    #[cfg(test)] // TODO: only used in tests at the moment
    fn character(&self) -> Option<&Handle<Character>> {
        self.inventory_owner.as_ref()
    }

    /// Returns the current contents of the watched inventory, as of the last [`Self::update()`].
    pub fn inventory(&self) -> &inv::Inventory {
        &self.inventory
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
    use all_is_cubes::character::CharacterTransaction;
    use all_is_cubes::inv;
    use all_is_cubes::space::Space;
    use all_is_cubes::universe::Universe;

    struct Tester {
        universe: Universe,
        space: Handle<Space>,
        character: Handle<Character>,
        character_cell: listen::Cell<Option<Handle<Character>>>,
        watcher: InventoryWatcher,
        sink: listen::Sink<WatcherChange>,
    }
    impl Tester {
        pub fn new() -> Self {
            let mut universe = Universe::new();
            let space = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
            let character = universe.insert_anonymous(Character::spawn_default(
                universe.read_ticket(),
                space.clone(),
            ));
            let character_cell = listen::Cell::new(Some(character.clone()));
            let mut watcher = InventoryWatcher::new(character_cell.as_source());
            watcher.update(universe.read_ticket());

            // Install listener
            let sink: listen::Sink<WatcherChange> = listen::Sink::new();
            watcher.listen(sink.listener());

            Self {
                universe,
                space,
                character,
                character_cell,
                watcher,
                sink,
            }
        }
        pub fn update(&mut self) {
            self.watcher.update(self.universe.read_ticket());
        }
    }

    #[test]
    fn basic_and_changed_slot_in_character() {
        let mut t = Tester::new();
        // Run redundant update -- should see no effect
        assert_eq!(t.sink.drain(), vec![]);
        t.update();
        assert_eq!(t.sink.drain(), vec![]);

        assert_eq!(t.watcher.character(), Some(&t.character));

        // Make a change to the character and observe update
        t.character
            .execute(
                t.universe.read_ticket(),
                &CharacterTransaction::inventory(inv::InventoryTransaction::insert([
                    inv::Tool::Activate,
                ])),
            )
            .unwrap();
        assert_eq!(t.sink.drain(), vec![WatcherChange::NeedsUpdate]);
        t.update();
        assert_eq!(t.sink.drain(), vec![WatcherChange::Inventory]);
    }

    #[test]
    fn follow_changed_character() {
        let mut t = Tester::new();

        // Construct new character with different inventory.
        let new_character = t.universe.insert_anonymous(Character::spawn_default(
            t.universe.read_ticket(),
            t.space.clone(),
        ));
        new_character
            .execute(
                t.universe.read_ticket(),
                &CharacterTransaction::inventory(inv::InventoryTransaction::insert([
                    inv::Tool::Activate,
                ])),
            )
            .unwrap();

        t.character_cell.set(Some(new_character.clone()));
        assert_eq!(t.watcher.character(), Some(&t.character));
        t.update();
        assert_eq!(t.watcher.character(), Some(&new_character));
        assert_eq!(t.sink.drain(), vec![WatcherChange::Inventory]);
    }
}
