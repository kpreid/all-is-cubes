//! Configuration of inventories owned by blocks ([`Modifier::Inventory`]).

use core::fmt;

use alloc::vec::Vec;

use euclid::Point3D;
use manyfmt::Refmt;

use crate::block::Resolution;
use crate::inv::Ix;
use crate::math::{GridCoordinate, GridPoint, GridRotation, GridVector, Gridgid};

#[cfg(doc)]
use crate::block::{self, Block, Modifier};
use crate::util::ConciseDebug;

/// Defines how a [`Modifier::Inventory`] should be configured and displayed within a [`Block`].
///
/// Attach this to a block using [`block::Builder::inventory_config()`].
//---
// TODO(inventory): better name?
// TODO(inventory): needs accessors or public fields
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct InvInBlock {
    /// Number of slots the inventory should have.
    pub(crate) size: Ix,

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
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct IconRow {
    // visible for serialization -- TODO: improve on that
    pub(crate) first_slot: Ix,
    pub(crate) count: Ix,
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

    /// Constructs a [`InvInBlock`].
    ///
    /// * `inventory_size` is the number of slots the inventory should have.
    /// * `icon_scale` is the scale factor by which to scale down the inventory icon blocks,
    ///   relative to the bounds of the block in which they are being displayed.
    /// * `icon_resolution` is the maximum resolution of inventory icons, and resolution in which
    ///   the `icon_rows`’ position coordinatess are expressed.
    ///   [`Modifier::Inventory`] is guaranteed not to increase the block resolution
    ///   beyond this resolution.
    /// * `icon_rows` specifies where in the block the inventory icons should be displayed.
    ///   It may be empty in order to keep the inventory invisible.
    pub fn new(
        inventory_size: Ix,
        icon_scale: Resolution,
        icon_resolution: Resolution,
        icon_rows: impl IntoIterator<Item = IconRow>,
    ) -> Self {
        Self {
            size: inventory_size,
            icon_scale,
            icon_resolution,
            icon_rows: icon_rows.into_iter().collect(),
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
        inventory_size: Ix,
    ) -> impl Iterator<Item = (Ix, GridPoint)> + '_ {
        self.icon_rows.iter().flat_map(move |row| {
            (0..row.count).map_while(move |sub_index| {
                let slot_index = row.first_slot.checked_add(sub_index)?;
                if slot_index >= inventory_size {
                    return None;
                }
                let index_coord = GridCoordinate::from(sub_index);
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
        let icon_size =
            GridCoordinate::from((icon_resolution / icon_scale).unwrap_or(Resolution::R1));
        Self {
            size,
            icon_scale,
            icon_resolution,
            icon_rows: icon_rows
                .into_iter()
                .filter_map(|row| row.rotate(transform, icon_size))
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
    /// Constructs an `IconRow`.
    ///
    /// * `slot_range` is the the portion of the inventory that is displayed.
    /// * `origin` is the lower corner of the location within the voxels where the first slot
    ///   in `slot_range` is to be displayed.
    /// * `stride` is the translation between adjacent slots.
    ///
    /// # Panics
    ///
    /// Panics if `slot_range` has `end` less than `start`.
    #[track_caller]
    pub fn new(slot_range: core::ops::Range<Ix>, origin: GridPoint, stride: GridVector) -> Self {
        Self {
            first_slot: slot_range.start,
            count: slot_range
                .end
                .checked_sub(slot_range.start)
                .expect("slot_range must not be reversed"),
            origin,
            stride,
        }
    }

    /// Rotate this row.
    /// Returns [`None`] if rotating it would cause numeric overflow.
    fn rotate(self, transform: Gridgid, icon_size: GridCoordinate) -> Option<Self> {
        // TODO: The icons themselves (not only their positions) need to be rotated,
        // but this is not supported yet.
        Some(Self {
            first_slot: self.first_slot,
            count: self.count,

            // Taking the minimum of opposing corners accounts for which direction the
            // block extends.
            origin: transform.checked_transform_point(self.origin)?.min(
                transform.checked_transform_point(checked_add_point_vector(
                    self.origin,
                    GridVector::splat(icon_size),
                )?)?,
            ),
            stride: transform.rotation.checked_transform_vector(self.stride)?,
        })
    }
}

impl fmt::Debug for IconRow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let &Self {
            first_slot,
            count,
            origin,
            stride,
        } = self;
        write!(
            f,
            "IconRow({slot_range:?} @ {origin} + {stride})",
            slot_range = (first_slot..(first_slot + count)),
            origin = origin.refmt(&ConciseDebug),
            stride = stride.refmt(&ConciseDebug)
        )
    }
}

// Manual implementation of `Arbitrary` because, currently, if we don't then the
// size hint will be missing, because the `euclid` vector types don't give one.
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
        use arbitrary::{Arbitrary, size_hint::and_all};
        and_all(&[
            <usize as Arbitrary>::size_hint(depth),
            <usize as Arbitrary>::size_hint(depth),
            <[GridCoordinate; 3] as Arbitrary>::size_hint(depth),
            <[GridCoordinate; 3] as Arbitrary>::size_hint(depth),
        ])
    }
}

fn checked_add_point_vector(p: GridPoint, v: GridVector) -> Option<GridPoint> {
    Some(GridPoint::new(
        p.x.checked_add(v.x)?,
        p.y.checked_add(v.y)?,
        p.z.checked_add(v.z)?,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use euclid::{point3, vec3};
    use pretty_assertions::assert_eq;

    #[test]
    fn inv_in_block_debug() {
        let iib = InvInBlock::new(
            9,
            Resolution::R4,
            Resolution::R16,
            vec![
                IconRow::new(0..3, point3(1, 1, 1), vec3(5, 0, 0)),
                IconRow::new(3..6, point3(1, 1, 6), vec3(5, 0, 0)),
                IconRow::new(6..9, point3(1, 1, 11), vec3(5, 0, 0)),
            ],
        );

        assert_eq!(
            format!("{iib:#?}"),
            indoc::indoc! {
                "
                InvInBlock {
                    size: 9,
                    icon_scale: 4,
                    icon_resolution: 16,
                    icon_rows: [
                        IconRow(0..3 @ (+1, +1, +1) + (+5, +0, +0)),
                        IconRow(3..6 @ (+1, +1, +6) + (+5, +0, +0)),
                        IconRow(6..9 @ (+1, +1, +11) + (+5, +0, +0)),
                    ],
                }"
            },
        );
    }

    #[test]
    fn icon_positions_output() {
        let iib = InvInBlock::new(
            9,
            Resolution::R4,
            Resolution::R16,
            vec![
                IconRow::new(0..3, point3(1, 1, 1), vec3(5, 0, 0)),
                IconRow::new(3..6, point3(1, 1, 6), vec3(5, 0, 0)),
                IconRow::new(6..9, point3(1, 1, 11), vec3(5, 0, 0)),
            ],
        );
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

    #[test]
    fn rotated_positions() {
        assert_eq!(
            IconRow {
                first_slot: 0,
                count: 10,
                origin: GridPoint::new(2, 2, 0),
                stride: GridVector::new(0, 0, 4),
            }
            .rotate(GridRotation::Rxyz.to_positive_octant_transform(8), 4),
            Some(IconRow {
                first_slot: 0,
                count: 10,
                origin: GridPoint::new(2, 2, 4),
                stride: GridVector::new(0, 0, -4),
            }),
        );
    }

    #[test]
    fn rotate_row_overflow() {
        // For coverage, two cases:
        // 1. case without icon size involved
        assert_eq!(
            IconRow {
                first_slot: 0,
                count: 10,
                origin: GridPoint::new(1397969747, -2147483648, 255827),
                stride: GridVector::new(134767872, 2820644, 7285711),
            }
            .rotate(Gridgid::from_rotation_about_origin(GridRotation::Rxyz), 1),
            None,
        );
        // 1. case where icon size causes the overflow
        assert_eq!(
            IconRow {
                first_slot: 0,
                count: 10,
                origin: GridPoint::new(1397969747, i32::MAX - 1, 255827),
                stride: GridVector::new(134767872, 2820644, 7285711),
            }
            .rotate(Gridgid::from_rotation_about_origin(GridRotation::Rxyz), 4),
            None,
        );
    }
}
