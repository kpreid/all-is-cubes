//! [`Inventory`] for storing items.

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::{vec, vec::Vec};

use crate::inv::{Inventory, InventoryChange, Ix, Slot};
use crate::transaction::{CommitError, Merge, Transaction};
use crate::universe::{HandleVisitor, VisitHandles};

// -------------------------------------------------------------------------------------------------

/// Transaction type for [`Inventory`].
///
/// The output type is the change notification which should be passed on after commit,
/// if any change is made.
#[derive(Clone, Debug, Default, PartialEq)]
#[expect(clippy::derive_partial_eq_without_eq)]
#[must_use]
pub struct InventoryTransaction {
    replace: BTreeMap<Ix, (Slot, Slot)>,
    insert: Vec<Slot>,
}

impl InventoryTransaction {
    /// Transaction to insert items/stacks into an inventory, which will fail if there is
    /// not sufficient space.
    pub fn insert<S: Into<Slot>, I: IntoIterator<Item = S>>(stacks: I) -> Self {
        // TODO: Should we coalesce identical insertions? Or leave that for when the
        // transaction is executed?
        Self {
            replace: BTreeMap::default(),
            insert: stacks
                .into_iter()
                .map(|s| -> Slot { s.into() })
                .filter(|s| s.count() > 0)
                .collect(),
        }
    }

    /// Transaction to replace the contents of an existing slot in an inventory, which
    /// will fail if the existing slot is not as expected.
    ///
    /// TODO: Right now, this requires an exact match. In the future, we should be able
    /// to compose multiple modifications like "add 1 item to stack" ×2 into "add 2 items".
    pub fn replace(slot: Ix, old: Slot, new: Slot) -> Self {
        let mut replace = BTreeMap::new();
        replace.insert(slot, (old, new));
        InventoryTransaction {
            replace,
            insert: vec![],
        }
    }

    /// Returns whether this transaction does nothing and checks nothing.
    ///
    /// # Example
    ///
    /// ```
    /// use all_is_cubes::inv;
    ///
    /// assert!(inv::InventoryTransaction::default().is_empty());
    ///
    /// assert!(!inv::InventoryTransaction::insert([inv::Tool::Activate]).is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        let Self { replace, insert } = self;
        replace.is_empty() && insert.is_empty()
    }
}

impl Transaction for InventoryTransaction {
    type Target = Inventory;
    type Context<'a> = ();
    type CommitCheck = Option<InventoryCheck>;
    type Output = InventoryChange;
    type Mismatch = InventoryMismatch;

    fn check(
        &self,
        inventory: &Inventory,
        (): Self::Context<'_>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        // Don't do the expensive copy if we have one already
        if self.replace.is_empty() && self.insert.is_empty() {
            return Ok(None);
        }

        // The simplest bulletproof algorithm to ensure we're stacking everything right
        // and not overflowing is to build the entire new inventory. The disadvantage of
        // this strategy is, of course, that we're cloning the entire inventory. If that
        // proves to be a performance problem, we can improve things by adding per-slot
        // copy-on-write or something like that.

        let mut slots = inventory.slots.clone();
        let mut changed = Vec::new();

        // Check and apply .replace, explicit slot replacements
        for (&index, (old, new)) in self.replace.iter() {
            match slots.get_mut(usize::from(index)) {
                None => {
                    return Err(InventoryMismatch::OutOfBounds);
                }
                Some(actual_old) if actual_old != old => {
                    return Err(InventoryMismatch::UnexpectedSlot(index));
                }
                Some(slot) => {
                    *slot = new.clone();
                    changed.push(index);
                }
            }
        }

        // Find locations for .insert items
        for new_stack in self.insert.iter() {
            let mut new_stack = new_stack.clone();
            for (slot, index) in slots.iter_mut().zip(0..) {
                if new_stack == Slot::Empty {
                    break;
                }
                if new_stack.unload_to(slot) {
                    changed.push(index);
                }
            }
            if new_stack != Slot::Empty {
                return Err(InventoryMismatch::Full);
            }
        }

        Ok(Some(InventoryCheck {
            new: slots,
            change: InventoryChange {
                slots: changed.into(),
            },
        }))
    }

    fn commit(
        self,
        inventory: &mut Inventory,
        check: Self::CommitCheck,
        outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), CommitError> {
        if let Some(InventoryCheck { new, change }) = check {
            assert_eq!(new.len(), inventory.slots.len());
            inventory.slots = new;
            outputs(change);
        }
        Ok(())
    }
}

impl Merge for InventoryTransaction {
    type MergeCheck = ();
    type Conflict = InventoryConflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        if let Some(&slot) = self.replace.keys().find(|slot| other.replace.contains_key(slot)) {
            return Err(InventoryConflict::ReplaceSameSlot { slot });
        }
        Ok(())
    }

    fn commit_merge(&mut self, other: Self, (): Self::MergeCheck) {
        self.replace.extend(other.replace);
        self.insert.extend(other.insert);
    }
}

impl VisitHandles for InventoryTransaction {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self { replace, insert } = self;
        replace.visit_handles(visitor);
        insert.visit_handles(visitor);
    }
}

/// Implementation type for [`InventoryTransaction::CommitCheck`].
#[derive(Debug)]
pub struct InventoryCheck {
    new: Box<[Slot]>,
    change: InventoryChange,
}

/// Transaction precondition error type for an [`InventoryTransaction`].
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum InventoryMismatch {
    /// insufficient empty slots
    Full,

    /// slot out of bounds
    OutOfBounds,

    /// contents of slot {0} not as expected
    UnexpectedSlot(Ix),
}

/// Transaction conflict error type for an [`InventoryTransaction`].
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum InventoryConflict {
    /// Tried to replace the same inventory slot.
    #[displaydoc("tried to replace the same inventory slot, {slot}, twice")]
    #[non_exhaustive]
    #[allow(missing_docs)]
    ReplaceSameSlot { slot: Ix },
}

impl core::error::Error for InventoryMismatch {}
impl core::error::Error for InventoryConflict {}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block;
    use crate::content::make_some_blocks;
    use crate::inv::{InventoryTransaction, Tool};
    use crate::math::Rgba;
    use alloc::sync::Arc;
    use pretty_assertions::assert_eq;

    #[test]
    fn identity_no_notification() {
        InventoryTransaction::default()
            .execute(&mut Inventory::from_slots([Slot::Empty]), (), &mut |_| {
                unreachable!("shouldn't notify")
            })
            .unwrap()
    }

    #[test]
    fn insert_empty_list() {
        let list: [Slot; 0] = [];
        assert_eq!(
            InventoryTransaction::insert(list),
            InventoryTransaction::default()
        );
    }

    #[test]
    fn insert_filtered_empty() {
        assert_eq!(
            InventoryTransaction::insert([Slot::Empty, Slot::Empty]),
            InventoryTransaction::default()
        );
    }

    #[test]
    fn insert_success() {
        let occupied_slot: Slot = Tool::CopyFromSpace.into();
        let mut inventory = Inventory::from_slots([
            occupied_slot.clone(),
            occupied_slot.clone(),
            Slot::Empty,
            occupied_slot,
            Slot::Empty,
        ]);
        let new_item = Tool::InfiniteBlocks(Rgba::WHITE.into());
        assert_eq!(inventory.slots[2], Slot::Empty);

        let mut outputs = Vec::new();
        InventoryTransaction::insert([new_item.clone()])
            .execute(&mut inventory, (), &mut |x| outputs.push(x))
            .unwrap();

        assert_eq!(
            outputs,
            [InventoryChange {
                slots: Arc::new([2])
            }]
        );
        assert_eq!(inventory.slots[2], new_item.into());
    }

    #[test]
    fn insert_no_space() {
        let contents = vec![
            Slot::from(Tool::CopyFromSpace),
            Slot::from(Tool::CopyFromSpace),
        ]
        .into_boxed_slice();
        let inventory = Inventory::from_slots(contents.clone());
        let new_item = Tool::InfiniteBlocks(Rgba::WHITE.into());

        assert_eq!(inventory.slots, contents);
        InventoryTransaction::insert([new_item.clone()])
            .check(&inventory, ())
            .expect_err("should have failed");
        assert_eq!(inventory.slots, contents);
    }

    #[test]
    fn insert_into_existing_stack() {
        // TODO: make_some_tools to simplify this?
        let [this, other] = make_some_blocks();
        let this = Tool::Block(this);
        let other = Tool::Block(other);
        let mut inventory = Inventory::from_slots([
            Slot::stack(10, other.clone()),
            Slot::stack(10, this.clone()),
            Slot::stack(10, other.clone()),
            Slot::stack(10, this.clone()),
            Slot::Empty,
        ]);
        InventoryTransaction::insert([this.clone()])
            .execute(&mut inventory, (), &mut drop)
            .unwrap();
        assert_eq!(
            inventory.slots,
            vec![
                Slot::stack(10, other.clone()),
                Slot::stack(11, this.clone()),
                Slot::stack(10, other.clone()),
                Slot::stack(10, this.clone()),
                Slot::Empty,
            ]
            .into_boxed_slice()
        );
    }

    #[test]
    fn systematic() {
        use crate::transaction::{TransactionTester, assert};

        let old_item = Tool::InfiniteBlocks(block::from_color!(1.0, 0.0, 0.0));
        let new_item_1 = Tool::InfiniteBlocks(block::from_color!(0.0, 1.0, 0.0));
        let new_item_2 = Tool::InfiniteBlocks(block::from_color!(0.0, 0.0, 1.0));

        // TODO: Add tests of stack modification, emptying, merging

        TransactionTester::new()
            .transaction(
                InventoryTransaction::insert([new_item_1.clone()]),
                |before, after| {
                    assert(
                        after.count_of(&new_item_1) > before.count_of(&new_item_1),
                        "missing added new_item_1",
                    )
                },
            )
            .transaction(
                InventoryTransaction::replace(
                    0,
                    old_item.clone().into(),
                    new_item_1.clone().into(),
                ),
                |_, after| {
                    assert(
                        after.slots[0].count_of(&old_item) == 0,
                        "did not replace old_item",
                    )?;
                    assert(
                        after.slots[0].count_of(&new_item_1) > 0,
                        "did not insert new_item_1",
                    )?;
                    Ok(())
                },
            )
            .transaction(
                // This one conflicts with the above one
                InventoryTransaction::replace(
                    0,
                    old_item.clone().into(),
                    new_item_2.clone().into(),
                ),
                |_, after| {
                    assert(
                        after.slots[0].count_of(&old_item) == 0,
                        "did not replace old_item",
                    )?;
                    assert(
                        after.slots[0].count_of(&new_item_2) > 0,
                        "did not insert new_item_2",
                    )?;
                    Ok(())
                },
            )
            .target(|| Inventory::from_slots([]))
            .target(|| Inventory::from_slots([Slot::Empty]))
            .target(|| Inventory::from_slots([const { Slot::Empty }; 10]))
            .target(|| Inventory::from_slots([Slot::from(old_item.clone()), Slot::Empty]))
            .test(());
    }
}
