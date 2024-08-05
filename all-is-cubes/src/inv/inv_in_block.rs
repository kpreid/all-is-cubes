//! Configuration of inventories owned by blocks ([`Modifier::Inventory`]).

use alloc::vec::Vec;
use euclid::Point3D;

use crate::block::Resolution;
use crate::math::{GridCoordinate, GridPoint, GridRotation, GridVector, Gridgid};

#[cfg(doc)]
use crate::block::Modifier;

/// Defines how a [`Modifier::Inventory`] should be configured and displayed within a block.
//---
// TODO(inventory): better name?
// TODO(inventory): needs accessors or public fields
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
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

    // TODO: following the rule for all `BlockAttributes` being cheap to clone,
    // this should be `Arc<[IconRow]>`, but that's mildly inconvenient for `Arbitrary`, so
    // I'm not bothering for this first iteration.
    // (But maybe we should be handling that at a higher level of the structure.)
    pub(crate) icon_rows: Vec<IconRow>,
}

/// Positioning of a displayed row of inventory icons; part of [`InvInBlock`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct IconRow {
    pub(crate) first_slot: usize,
    pub(crate) count: usize,
    pub(crate) origin: GridPoint,
    pub(crate) stride: GridVector,
}

impl InvInBlock {
    /// Value appropriate for “normal” blocks which should not carry inventories.
    pub const EMPTY: Self = Self {
        size: 0,
        icon_scale: Resolution::R1,      // arbitrary
        icon_resolution: Resolution::R1, // arbitrary
        icon_rows: Vec::new(),
    };

    // TODO(inventory): this substitutes for the lack of ability to actually construct one publicly
    #[doc(hidden)]
    pub fn new_placeholder() -> Self {
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

    /// Returns which inventory slots should be rendered as icons, and the lower corners
    /// of the icons.
    ///
    /// `inventory_size` should be the size of the inventory to be rendered, which will be used
    /// to filter out nonexistent slots and limit the amount of computation performed to match
    /// the inventory.
    pub(crate) fn icon_positions(
        &self,
        inventory_size: usize,
    ) -> impl Iterator<Item = (usize, GridPoint)> + '_ {
        self.icon_rows.iter().flat_map(move |row| {
            (0..row.count).map_while(move |sub_index| {
                let slot_index = row.first_slot.checked_add(sub_index)?;
                if slot_index >= inventory_size {
                    return None;
                }
                let index_coord = GridCoordinate::try_from(sub_index).ok()?;
                let position: GridPoint = transpose_point_option(
                    row.origin
                        .to_vector()
                        .zip(row.stride, |origin_c, stride_c| {
                            origin_c.checked_add(stride_c.checked_mul(index_coord)?)
                        })
                        .to_point(),
                )?;
                Some((slot_index, position))
            })
        })
    }

    pub(crate) fn rotationally_symmetric(&self) -> bool {
        // If it doesn't display any icons, then it's symmetric.
        self.icon_rows.is_empty()
    }

    pub(crate) fn rotate(self, rotation: GridRotation) -> Self {
        let Self {
            size,
            icon_scale,
            icon_resolution,
            icon_rows,
        } = self;
        let transform = rotation.to_positive_octant_transform(icon_resolution.into());
        Self {
            size,
            icon_scale,
            icon_resolution,
            icon_rows: icon_rows
                .into_iter()
                .map(|row| row.rotate(transform))
                .collect(),
        }
    }

    /// Combine the two inputs to form one which has the size and display of both.
    pub(crate) fn concatenate(self, other: InvInBlock) -> InvInBlock {
        if self.size == 0 {
            other
        } else {
            Self {
                size: self.size.saturating_add(other.size),
                // TODO(inventory): scale and resolution need adaptation
                icon_scale: self.icon_scale,
                icon_resolution: self.icon_resolution,
                icon_rows: self
                    .icon_rows
                    .into_iter()
                    .chain(other.icon_rows.into_iter().filter_map(|mut row| {
                        row.first_slot = row.first_slot.checked_add(self.size)?;
                        Some(row)
                    }))
                    .collect(),
            }
        }
    }
}

fn transpose_point_option<T, U>(v: Point3D<Option<T>, U>) -> Option<Point3D<T, U>> {
    Some(Point3D::new(v.x?, v.y?, v.z?))
}

impl Default for InvInBlock {
    /// Returns [`InvInBlock::EMPTY`].
    fn default() -> Self {
        Self::EMPTY
    }
}

impl crate::universe::VisitHandles for InvInBlock {
    fn visit_handles(&self, _: &mut dyn crate::universe::HandleVisitor) {
        let Self {
            size: _,
            icon_scale: _,
            icon_resolution: _,
            icon_rows: _,
        } = self;
    }
}

impl IconRow {
    fn rotate(self, transform: Gridgid) -> Self {
        // TODO: The icons themselves (not their positions) need to be rotated,
        // but this is not supported yet.
        Self {
            first_slot: self.first_slot,
            count: self.count,
            origin: transform.transform_point(self.origin), // TODO: does not account for size of icon
            stride: transform.rotation.transform_vector(self.stride),
        }
    }
}

#[cfg(feature = "arbitrary")]
impl<'a> arbitrary::Arbitrary<'a> for IconRow {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        // TODO: there are lots of extremely useless out-of-bounds values here;
        // bias to useful ones?
        Ok(Self {
            first_slot: u.arbitrary()?,
            count: u.arbitrary()?,
            origin: <[i32; 3]>::arbitrary(u)?.into(),
            stride: <[i32; 3]>::arbitrary(u)?.into(),
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        use arbitrary::{size_hint::and_all, Arbitrary};
        and_all(&[
            <usize as Arbitrary>::size_hint(depth),
            <usize as Arbitrary>::size_hint(depth),
            <[GridCoordinate; 3] as Arbitrary>::size_hint(depth),
            <[GridCoordinate; 3] as Arbitrary>::size_hint(depth),
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use euclid::point3;
    use pretty_assertions::assert_eq;

    #[test]
    fn icon_positions_output() {
        // TODO: this test should be revised to create a specific `InvInBlock` value for testing
        let iib = InvInBlock::new_placeholder();
        assert_eq!(
            iib.icon_positions(999).take(100).collect::<Vec<_>>(),
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

    #[test]
    fn icon_positions_are_truncated_to_inventory_size() {
        let iib = InvInBlock {
            size: 1,
            icon_scale: Resolution::R4,
            icon_resolution: Resolution::R16,
            icon_rows: vec![
                IconRow {
                    first_slot: 0,
                    count: 100,
                    origin: GridPoint::new(0, 0, 0),
                    stride: GridVector::new(5, 0, 0),
                },
                IconRow {
                    first_slot: 1,
                    count: 100,
                    origin: GridPoint::new(0, 0, 5),
                    stride: GridVector::new(5, 0, 0),
                },
            ],
        };
        assert_eq!(
            iib.icon_positions(3).take(100).collect::<Vec<_>>(),
            vec![
                (0, point3(0, 0, 0)),
                (1, point3(5, 0, 0)),
                (2, point3(10, 0, 0)),
                (1, point3(0, 0, 5)),
                (2, point3(5, 0, 5)),
            ]
        );
    }
}
