use bevy_ecs::prelude as ecs;

use crate::inv;
use crate::listen;
use crate::transaction::{self, Transaction};
use crate::universe;

// -------------------------------------------------------------------------------------------------

/// Component for inventories which are attached to an entity and are responsible for producing
/// change notifications.
///
/// The differences between this and [`inv::Inventory`] are:
///
/// * This has a [`listen::Notifier`].
/// * This implements [`ecs::Component`].
#[derive(Debug, ecs::Component)]
pub(crate) struct InventoryComponent {
    inv: inv::Inventory,
    notifier: listen::Notifier<inv::InventoryChange>,
}
impl InventoryComponent {
    pub fn new(inv: inv::Inventory) -> Self {
        Self {
            inv,
            notifier: listen::Notifier::new(),
        }
    }

    pub fn inventory(&self) -> &inv::Inventory {
        &self.inv
    }

    /// Execute a transaction on the contained inventory, and deliver change notifications
    /// resulting from the transaction.
    pub fn execute(
        &mut self,
        txn: inv::InventoryTransaction,
    ) -> Result<(), transaction::ExecuteError<inv::InventoryTransaction>> {
        txn.execute(&mut self.inv, (), &mut notify_adapter(&self.notifier))
    }

    pub(crate) fn commit_inventory_transaction(
        &mut self,
        txn: inv::InventoryTransaction,
        check: <inv::InventoryTransaction as Transaction>::CommitCheck,
    ) -> Result<(), transaction::CommitError> {
        txn.commit(&mut self.inv, check, &mut notify_adapter(&self.notifier))
    }
}

impl listen::Listen for InventoryComponent {
    type Msg = inv::InventoryChange;
    type Listener = listen::DynListener<inv::InventoryChange>;

    fn listen_raw(&self, listener: Self::Listener) {
        self.notifier.listen_raw(listener);
    }

    fn listen<L>(&self, listener: L)
    where
        L: nosy::IntoListener<Self::Listener, Self::Msg>,
        Self: Sized,
    {
        self.notifier.listen(listener)
    }
}

impl universe::VisitHandles for InventoryComponent {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self { inv, notifier: _ } = self;
        inv.visit_handles(visitor);
    }
}

fn notify_adapter(
    notifier: &listen::Notifier<inv::InventoryChange>,
) -> impl Fn(inv::InventoryChange) {
    |change| notifier.notify(&change)
}
