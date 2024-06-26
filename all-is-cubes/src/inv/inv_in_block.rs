//! Configuration of inventories owned by blocks ([`Modifier::Inventory`]).

use alloc::vec::Vec;

use crate::block::Resolution;
use crate::math::{GridCoordinate, GridPoint, GridVector};

#[cfg(doc)]
use crate::block::Modifier;

/// Defines how a [`Modifier::Inventory`] should be configured and displayed within a block.
///
/// TODO: Needs a better name.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InvInBlock {
    /// Number of slots the inventory should have.
    pub(crate) size: usize, // TODO: use platform-independent max size

    /// Scale factor by which to scale down the inventory icon blocks,
    /// relative to the bounds of the block in which they are being displayed.
    pub(crate) icon_scale: Resolution,

    /// Maximum resolution of inventory icons, and resolution in which the `icon_rows`
    /// position coordinatess are expressed.
    ///
    /// [`Modifier::Inventory`] is guaranteed not to increase the block resolution
    /// beyond this resolution.
    pub(crate) icon_resolution: Resolution,

    pub(crate) icon_rows: Vec<IconRow>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IconRow {
    pub(crate) first_slot: usize,
    pub(crate) count: usize,
    pub(crate) origin: GridPoint,
    pub(crate) stride: GridVector,
}

impl InvInBlock {
    /// Returns which inventory slots should be rendered as icons, and the lower corners
    /// of the icons.
    pub(crate) fn icon_positions(&self) -> impl Iterator<Item = (usize, GridPoint)> + '_ {
        self.icon_rows.iter().flat_map(|row| {
            (0..row.count).map_while(move |sub_index| {
                let slot_index = row.first_slot.checked_add(sub_index)?;
                Some((
                    slot_index,
                    // TODO: this should be checked arithmetic
                    row.origin + row.stride * GridCoordinate::try_from(sub_index).ok()?,
                ))
            })
        })
    }
}

impl Default for InvInBlock {
    fn default() -> Self {
        // TODO: placeholder; the real default should be empty (zero slots, invisible).
        Self {
            size: 1,
            icon_scale: Resolution::R4,
            icon_resolution: Resolution::R16,
            icon_rows: vec![
                IconRow {
                    first_slot: 0,
                    count: 3,
                    origin: GridPoint::new(1, 1, 1),
                    stride: GridVector::new(5, 0, 0),
                },
                IconRow {
                    first_slot: 3,
                    count: 3,
                    origin: GridPoint::new(1, 1, 6),
                    stride: GridVector::new(5, 0, 0),
                },
                IconRow {
                    first_slot: 6,
                    count: 3,
                    origin: GridPoint::new(1, 1, 11),
                    stride: GridVector::new(5, 0, 0),
                },
            ],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use euclid::point3;
    use pretty_assertions::assert_eq;

    // TODO: this test should be revised to create an `InvInBlock` for testing instead of the default.
    #[test]
    fn default_icon_positions() {
        let iib = InvInBlock::default();
        assert_eq!(
            iib.icon_positions().take(10).collect::<Vec<_>>(),
            vec![
                (0, point3(1, 1, 1)),
                (1, point3(6, 1, 1)),
                (2, point3(11, 1, 1)),
                (3, point3(1, 1, 6)),
                (4, point3(6, 1, 6)),
                (5, point3(11, 1, 6)),
                (6, point3(1, 1, 11)),
                (7, point3(6, 1, 11)),
                (8, point3(11, 1, 11)),
            ]
        );
    }
}
