//! [`Inventory`] for storing items.

use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec;
use core::fmt;
use core::num::NonZeroU16;

use descriptive_unwrap::ResultExt as _;

use crate::block::Block;
use crate::character::{Character, CharacterTransaction, Cursor};
use crate::inv::{Icons, InventoryTransaction, Ix, Tool, ToolError, ToolInput};
use crate::linking::BlockProvider;
use crate::transaction::{Merge, Transaction};
use crate::universe::{Handle, HandleVisitor, ReadTicket, UniverseTransaction, VisitHandles};

// -------------------------------------------------------------------------------------------------

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
    /// Ordinary player actions cannot change the number of slots, only change what is in them.
    ///
    /// # Panics
    ///
    /// Panics if `items` has more than [`Ix::MAX`] elements.
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
            Some(original_slot @ Slot::Stack(count, original_tool)) => {
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
                            original_slot.clone(),
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
                            original_slot.clone(),
                            new_tool.into(),
                        ))
                    }
                    (count_greater_than_one, Some(new_tool)) => {
                        // Tool modifies itself and is in a stack, so we have to unstack the new tool.
                        // TODO: In some cases it might make sense to put the stack aside and keep the modified tool.
                        Some(
                            InventoryTransaction::replace(
                                slot_index,
                                original_slot.clone(),
                                Slot::stack(
                                    count_greater_than_one.get() - 1,
                                    original_tool.clone(),
                                ),
                            )
                            .merge(InventoryTransaction::insert([new_tool]))
                            .err_is_unreachable(),
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
    ///
    /// # Errors
    ///
    /// Returns an error if:
    ///
    /// * `read_ticket` is not sufficient to read.
    /// * The tool is not usable.
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

    /// Returns the icon to use for this tool in the user interface, or [`None`]
    /// if the slot is empty.
    ///
    /// Note that this is _not_ the same as the block that a [`Tool::Block`] places.
    pub fn icon<'a>(&'a self, predefined: &'a BlockProvider<Icons>) -> Option<Cow<'a, Block>> {
        match self {
            Slot::Empty => None,
            Slot::Stack(_, tool) => Some(tool.icon(predefined)),
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
    pub(crate) fn unload_to(&mut self, destination: &mut Self) -> bool {
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
                        *s_count = strict_sub_nz(*s_count, count_to_move);
                        *d_count = strict_add_nz(*d_count, count_to_move);
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
            *d_count = strict_add_nz(*d_count, count_to_move);
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

const impl From<Tool> for Slot {
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

/// Description of a change to an [`Inventory`] for use in listeners.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct InventoryChange {
    /// Which slots of the inventory have been changed.
    pub slots: Arc<[Ix]>,
}

// -------------------------------------------------------------------------------------------------

/// Adds to a [`NonZeroU16`] and panics on overflow.
///
/// This is largely equivalent to [`NonZeroU16::checked_add()`] but has a sibling for subtraction.
#[track_caller]
fn strict_add_nz(left: NonZeroU16, right: u16) -> NonZeroU16 {
    left.checked_add(right).unwrap_or_else(strict_panic)
}

/// Subtracts from a [`NonZeroU16`] and panics on overflow or reaching 0.
#[track_caller]
fn strict_sub_nz(left: NonZeroU16, right: u16) -> NonZeroU16 {
    // Unfortunately, there is no `NonZeroU16::checked_sub()`.
    NonZeroU16::new(left.get().checked_sub(right).unwrap_or_else(strict_panic))
        .unwrap_or_else(strict_panic)
}

#[track_caller]
fn strict_panic<T>() -> T {
    panic!("can’t happen: stack count arithmetic overflow")
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::content::make_some_blocks;
    use itertools::Itertools;
    use pretty_assertions::assert_eq;

    // TODO: test for Inventory::use_tool

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
