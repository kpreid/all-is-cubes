//! [`Inventory`] for storing items.

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::fmt;
use core::num::NonZeroU16;

use crate::block::Block;
use crate::character::{Character, CharacterTransaction, Cursor};
use crate::inv::{Icons, Ix, Tool, ToolError, ToolInput};
use crate::linking::BlockProvider;
use crate::transaction::{CommitError, Merge, Transaction};
use crate::universe::{Handle, HandleVisitor, ReadTicket, UniverseTransaction, VisitHandles};

/// A collection of [`Tool`]s (items).
///
/// Note that unlike many other game objects in `all_is_cubes`, an `Inventory` does not
/// deliver change notifications. Instead, this is the responsibility of the `Inventory`'s
/// owner; its operations produce [`InventoryChange`]s (sometimes indirectly via
/// [`InventoryTransaction`]'s output) which the owner is responsible for forwarding
/// appropriately. This design choice allows an [`Inventory`] to be placed inside
/// other objects directly rather than via [`Handle`].
///
#[doc = include_str!("../save/serde-warning.md")]
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub struct Inventory {
    // Design note: This is a boxed slice to keep the size of the type small,
    // and because inventories have sizes determined by game mechanics separately from their
    // contents, so they do not need to resize to accomodate their contents expanding.
    pub(crate) slots: Box<[Slot]>,
}

impl Inventory {
    /// Construct an [`Inventory`] with the specified number of slots.
    ///
    /// Ordinary user actions cannot change the number of slots.
    pub fn new(size: Ix) -> Self {
        Inventory {
            slots: vec![Slot::Empty; size.into()].into_boxed_slice(),
        }
    }

    /// Construct an [`Inventory`] with the given slots.
    ///
    /// Ordinary user actions cannot change the number of slots.
    #[track_caller]
    pub fn from_slots(items: impl Into<Box<[Slot]>>) -> Self {
        let slots = items.into();
        Ix::try_from(slots.len())
            .expect("input has more slots than an inventory is allowed to have");
        Inventory { slots }
    }

    /// Returns whether all slots in this inventory are empty.
    pub fn is_empty(&self) -> bool {
        self.slots.iter().all(|slot| matches!(slot, Slot::Empty))
    }

    /// Returns a view of all slots in this inventory.
    pub fn slots(&self) -> &[Slot] {
        &self.slots
    }

    /// Returns the number of slots in the inventory.
    ///
    /// This is equal in value to `self.slots().len()`.
    pub fn size(&self) -> Ix {
        // cannot overflow because we checked on construction
        self.slots.len() as Ix
    }

    /// Returns the contents of the slot in this inventory with the given index.
    ///
    /// Returns `None` if, and only if, `slot_index >= self.size()`.
    pub fn get(&self, slot_index: Ix) -> Option<&Slot> {
        self.slots.get(usize::from(slot_index))
    }

    /// Use a tool stored in this inventory.
    ///
    /// Returns a [`inv::InventoryTransaction`] for the change to the inventory, if any,
    /// and a [`UniverseTransaction`] for the outside effects of the tool.
    /// The caller should commit both or neither.
    pub(crate) fn use_tool_it(
        &self,
        read_ticket: ReadTicket<'_>,
        cursor: Option<&Cursor>,
        character: Option<Handle<Character>>,
        slot_index: Ix,
    ) -> Result<(InventoryTransaction, UniverseTransaction), ToolError> {
        let original_slot = self.get(slot_index);
        match original_slot {
            None | Some(Slot::Empty) => Err(ToolError::NoTool),
            Some(Slot::Stack(count, original_tool)) => {
                let input = ToolInput {
                    read_ticket,
                    cursor: cursor.cloned(),
                    character,
                };
                let (new_tool, effect_transaction) = original_tool.clone().use_tool(&input)?;

                // TODO: This is way too long. Inventory-stacking logic should be in InventoryTransaction, probably?
                let tool_transaction = match (count, new_tool) {
                    (_, None) => {
                        // Tool deletes itself.
                        Some(InventoryTransaction::replace(
                            slot_index,
                            original_slot.unwrap().clone(),
                            Slot::stack(count.get() - 1, original_tool.clone()),
                        ))
                    }
                    (_, Some(new_tool)) if new_tool == *original_tool => {
                        // Tool is unaffected.
                        None
                    }
                    (&Slot::COUNT_ONE, Some(new_tool)) => {
                        // Tool modifies itself and is not stacked.
                        Some(InventoryTransaction::replace(
                            slot_index,
                            original_slot.unwrap().clone(),
                            new_tool.into(),
                        ))
                    }
                    (count_greater_than_one, Some(new_tool)) => {
                        // Tool modifies itself and is in a stack, so we have to unstack the new tool.
                        // TODO: In some cases it might make sense to put the stack aside and keep the modified tool.
                        Some(
                            InventoryTransaction::replace(
                                slot_index,
                                original_slot.unwrap().clone(),
                                Slot::stack(
                                    count_greater_than_one.get() - 1,
                                    original_tool.clone(),
                                ),
                            )
                            .merge(InventoryTransaction::insert([new_tool]))
                            .unwrap(),
                        )
                    }
                };

                Ok((tool_transaction.unwrap_or_default(), effect_transaction))
            }
        }
    }

    /// Use a tool stored in this inventory.
    ///
    /// `character` must be the character containing the inventory. TODO: Bad API
    pub fn use_tool(
        &self,
        read_ticket: ReadTicket<'_>,
        cursor: Option<&Cursor>,
        character: Handle<Character>,
        slot_index: Ix,
    ) -> Result<UniverseTransaction, ToolError> {
        let (tool_transaction, effect_transaction) =
            self.use_tool_it(read_ticket, cursor, Some(character.clone()), slot_index)?;

        if tool_transaction.is_empty() {
            Ok(effect_transaction)
        } else {
            effect_transaction
                .merge(CharacterTransaction::inventory(tool_transaction).bind(character))
                .map_err(|_| ToolError::Internal("failed to merge tool self-update".into()))
        }
    }

    /// Returns the total count of the given item in this inventory.
    ///
    /// Note on numeric range: this can overflow if the inventory has over 65537 slots.
    /// Let's not do that.
    ///
    /// TODO: Added for tests; is this generally useful?
    #[cfg(test)]
    pub(crate) fn count_of(&self, item: &Tool) -> u32 {
        self.slots.iter().map(|slot| u32::from(slot.count_of(item))).sum::<u32>()
    }
}

impl VisitHandles for Inventory {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self { slots } = self;
        slots.visit_handles(visitor);
    }
}

/// The direct child of [`Inventory`]; a container for any number of identical [`Tool`]s.
#[derive(Clone, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub enum Slot {
    /// Slot contains nothing.
    Empty,
    /// Slot contains one or more of the given [`Tool`].
    Stack(NonZeroU16, Tool),
}

impl Slot {
    const COUNT_ONE: NonZeroU16 = NonZeroU16::MIN;

    /// Construct a [`Slot`] containing `count` copies of `tool`.
    ///
    /// If `count` is zero, the `tool` will be ignored.
    pub fn stack(count: u16, tool: Tool) -> Self {
        match NonZeroU16::new(count) {
            Some(count) => Self::Stack(count, tool),
            None => Self::Empty,
        }
    }

    /// Temporary const version of `<Slot as From<Tool>>::from`.
    #[doc(hidden)]
    pub const fn one(tool: Tool) -> Self {
        Self::Stack(Self::COUNT_ONE, tool)
    }

    /// Returns the icon to use for this tool in the user interface.
    ///
    /// Note that this is _not_ the same as the block that a [`Tool::Block`] places.
    pub fn icon<'a>(&'a self, predefined: &'a BlockProvider<Icons>) -> Cow<'a, Block> {
        match self {
            Slot::Empty => Cow::Borrowed(&predefined[Icons::EmptySlot]),
            Slot::Stack(_, tool) => tool.icon(predefined),
        }
    }

    /// Kludge restricted version of `icon()` to get inventory-in-a-block rendering working at all.
    /// TODO(inventory): <https://github.com/kpreid/all-is-cubes/issues/480>
    pub(crate) fn icon_only_if_intrinsic(&self) -> Option<&Block> {
        match self {
            Slot::Empty => None,
            Slot::Stack(_, tool) => tool.icon_only_if_intrinsic(),
        }
    }

    /// Returns the count of items in this slot.
    pub fn count(&self) -> u16 {
        match self {
            Slot::Empty => 0,
            Slot::Stack(count, _) => count.get(),
        }
    }

    /// If the given tool is in this slot, return the count thereof.
    ///
    /// TODO: Added for tests; is this generally useful?
    #[cfg(test)]
    pub(crate) fn count_of(&self, item: &Tool) -> u16 {
        match self {
            Slot::Stack(count, slot_item) if slot_item == item => count.get(),
            Slot::Stack(_, _) => 0,
            Slot::Empty => 0,
        }
    }

    /// Moves as many items as possible from `self` to `destination` while obeying item
    /// stacking rules.
    ///
    /// Does nothing if `self` and `destination` contain different items.
    ///
    /// Returns whether anything was moved.
    fn unload_to(&mut self, destination: &mut Self) -> bool {
        // First, handle the simple cases, or decide how many to move.
        // This has to be multiple passes to satisfy the borrow checker.
        let count_to_move = match (&mut *self, &mut *destination) {
            (Slot::Empty, _) => {
                // Source is empty; nothing to do.
                return false;
            }
            (source @ Slot::Stack(_, _), destination @ Slot::Empty) => {
                // Destination is empty (and source isn't); just swap.
                core::mem::swap(source, destination);
                return true;
            }
            (Slot::Stack(s_count, source_item), Slot::Stack(d_count, destination_item)) => {
                if source_item == destination_item {
                    // Stacks of identical items; figure out how much to move.
                    let max_stack = destination_item.stack_limit().get();
                    let count_to_move = s_count.get().min(max_stack.saturating_sub(d_count.get()));
                    if count_to_move == 0 {
                        return false;
                    } else if count_to_move < s_count.get() {
                        // The source stack is not completely transferred; update counts.
                        *s_count = NonZeroU16::new(s_count.get() - count_to_move).unwrap();
                        *d_count = NonZeroU16::new(d_count.get() + count_to_move).unwrap();
                        return true;
                    } else {
                        // The source stack is completely transferred; exit this match so that we
                        // can reassign *self.
                        count_to_move
                    }
                } else {
                    // Stacks of different items.
                    return false;
                }
            }
        };
        debug_assert_eq!(count_to_move, self.count());
        if let Slot::Stack(d_count, _) = destination {
            *self = Slot::Empty;
            *d_count = NonZeroU16::new(d_count.get() + count_to_move).unwrap();
        } else {
            unreachable!();
        }
        true
    }
}

impl fmt::Debug for Slot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "Empty"),
            Self::Stack(count, tool) => {
                write!(f, "{count} × ")?;
                tool.fmt(f) // pass through formatter options
            }
        }
    }
}

impl From<Tool> for Slot {
    fn from(tool: Tool) -> Self {
        Self::Stack(Self::COUNT_ONE, tool)
    }
}

impl From<Option<Tool>> for Slot {
    fn from(tool: Option<Tool>) -> Self {
        match tool {
            Some(tool) => Self::Stack(Self::COUNT_ONE, tool),
            None => Self::Empty,
        }
    }
}

impl VisitHandles for Slot {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        match self {
            Slot::Empty => {}
            Slot::Stack(_count, tool) => tool.visit_handles(visitor),
        }
    }
}

/// Specifies a limit on the number of a particular item that should be combined in a
/// single [`Slot`].
///
/// Each value of this enum is currently equivalent to a particular number, but (TODO:)
/// in the future, it may be possible for inventories or universes to specify a normal
/// stack size and specific deviations from it.
#[non_exhaustive]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub(crate) enum StackLimit {
    One,
    Standard,
}

impl StackLimit {
    /// TODO: This is not public because we don't know what environment parameters it
    /// should need yet.
    pub(crate) fn get(self) -> u16 {
        match self {
            StackLimit::One => 1,
            // TODO: This should be a per-universe (at least) configuration.
            StackLimit::Standard => 100,
        }
    }
}

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

/// Description of a change to an [`Inventory`] for use in listeners.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct InventoryChange {
    /// Which slots of the inventory have been changed.
    pub slots: Arc<[Ix]>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block;
    use crate::content::make_some_blocks;
    use crate::math::Rgba;
    use crate::transaction::TransactionTester;
    use itertools::Itertools;
    use pretty_assertions::assert_eq;

    // TODO: test for Inventory::use_tool

    #[test]
    fn txn_identity_no_notification() {
        InventoryTransaction::default()
            .execute(
                &mut Inventory::from_slots(vec![Slot::Empty]),
                (),
                &mut |_| unreachable!("shouldn't notify"),
            )
            .unwrap()
    }

    #[test]
    fn txn_insert_empty_list() {
        let list: [Slot; 0] = [];
        assert_eq!(
            InventoryTransaction::insert(list),
            InventoryTransaction::default()
        );
    }

    #[test]
    fn txn_insert_filtered_empty() {
        assert_eq!(
            InventoryTransaction::insert([Slot::Empty, Slot::Empty]),
            InventoryTransaction::default()
        );
    }

    #[test]
    fn txn_insert_success() {
        let occupied_slot: Slot = Tool::CopyFromSpace.into();
        let mut inventory = Inventory::from_slots(vec![
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
            vec![InventoryChange {
                slots: Arc::new([2])
            }]
        );
        assert_eq!(inventory.slots[2], new_item.into());
    }

    #[test]
    fn txn_insert_no_space() {
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
    fn txn_insert_into_existing_stack() {
        // TODO: make_some_tools to simplify this?
        let [this, other] = make_some_blocks();
        let this = Tool::Block(this);
        let other = Tool::Block(other);
        let mut inventory = Inventory::from_slots(vec![
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
    fn txn_systematic() {
        let old_item = Tool::InfiniteBlocks(block::from_color!(1.0, 0.0, 0.0));
        let new_item_1 = Tool::InfiniteBlocks(block::from_color!(0.0, 1.0, 0.0));
        let new_item_2 = Tool::InfiniteBlocks(block::from_color!(0.0, 0.0, 1.0));

        // TODO: Add tests of stack modification, emptying, merging

        TransactionTester::new()
            .transaction(
                InventoryTransaction::insert([new_item_1.clone()]),
                |before, after| {
                    if after.count_of(&new_item_1) <= before.count_of(&new_item_1) {
                        return Err("missing added new_item_1".into());
                    }
                    Ok(())
                },
            )
            .transaction(
                InventoryTransaction::replace(
                    0,
                    old_item.clone().into(),
                    new_item_1.clone().into(),
                ),
                |_, after| {
                    if after.slots[0].count_of(&old_item) != 0 {
                        return Err("did not replace old_item".into());
                    }
                    if after.slots[0].count_of(&new_item_1) == 0 {
                        return Err("did not insert new_item_1".into());
                    }
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
                    if after.slots[0].count_of(&old_item) != 0 {
                        return Err("did not replace old_item".into());
                    }
                    if after.slots[0].count_of(&new_item_2) == 0 {
                        return Err("did not insert new_item_2".into());
                    }
                    Ok(())
                },
            )
            .target(|| Inventory::from_slots(vec![]))
            .target(|| Inventory::from_slots(vec![Slot::Empty]))
            .target(|| Inventory::from_slots(vec![Slot::Empty; 10]))
            .target(|| Inventory::from_slots(vec![Slot::from(old_item.clone()), Slot::Empty]))
            .test(());
    }

    #[test]
    fn slot_unload_systematic() {
        let [block1, block2] = make_some_blocks();
        let tools = [
            Tool::Block(block1),
            Tool::Block(block2),
            Tool::Activate, // not stackable
        ];
        const MAX: u16 = u16::MAX;
        let gen_slots = move || {
            [
                0,
                1,
                2,
                3,
                10,
                MAX / 2,
                MAX / 2 + 1,
                MAX - 10,
                MAX - 2,
                MAX - 1,
                MAX,
            ]
            .into_iter()
            .cartesian_product(tools.clone())
            .map(|(count, item)| Slot::stack(count, item))
        };
        for slot1_in in gen_slots() {
            for slot2_in in gen_slots() {
                let different = matches!((&slot1_in, &slot2_in), (Slot::Stack(_, i1), Slot::Stack(_, i2)) if i1 != i2);

                let mut slot1_out = slot1_in.clone();
                let mut slot2_out = slot2_in.clone();
                slot1_out.unload_to(&mut slot2_out);

                assert_eq!(
                    u64::from(slot1_in.count()) + u64::from(slot2_in.count()),
                    u64::from(slot1_out.count()) + u64::from(slot2_out.count()),
                    "not conservative"
                );
                if different {
                    assert_eq!(
                        (&slot1_in, &slot2_in),
                        (&slot1_out, &slot2_out),
                        "combined different items"
                    );
                }
            }
        }
    }
}
