//! Configuration of inventories owned by blocks ([`Modifier::Inventory`]).

use alloc::sync::Arc;
use core::fmt;
use core::iter;

use euclid::Point3D;
use manyfmt::Refmt;

use crate::block::{Modifier, Resolution};
use crate::inv::{Inventory, Ix};
use crate::math::{
    GridAab, GridCoordinate, GridPoint, GridRotation, GridSize, GridVector, Gridgid,
};
use crate::util::ConciseDebug;

#[cfg(doc)]
use crate::block::{self, Block};

// -------------------------------------------------------------------------------------------------

impl From<Inventory> for Modifier {
    fn from(value: Inventory) -> Self {
        Modifier::Inventory(value)
    }
}

// -------------------------------------------------------------------------------------------------

/// Defines how a [`Modifier::Inventory`] should be configured and displayed within a [`Block`].
///
/// Attach this to a block using [`block::Builder::inventory_config()`].
//---
// TODO(inventory): better name?
// TODO(inventory): needs accessors or public fields
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct InvInBlock {
    /// Number of slots the inventory should have.
    inventory_size: Ix,

    /// Scale factor by which to scale down the inventory icon blocks,
    /// relative to the bounds of the block in which they are being displayed.
    icon_scale: Resolution,

    /// Maximum resolution of inventory icons, and resolution in which the `icon_rows`
    /// position coordinates are expressed.
    ///
    /// [`Modifier::Inventory`] is guaranteed not to increase the block resolution
    /// beyond this resolution.
    render_resolution: Resolution,

    /// Specifies where in the block the inventory icons should be displayed.
    ///
    /// The [`Option`] is solely to avoid allocations for empty lists.
    icon_rows: Option<Arc<[IconRow]>>,
}

/// Positioning of a displayed row of inventory icons; part of [`InvInBlock`].
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct IconRow {
    first_slot: Ix,
    count: Ix,
    origin: GridPoint,
    stride: GridVector,
}

// -------------------------------------------------------------------------------------------------

impl InvInBlock {
    /// Value appropriate for “normal” blocks which should not carry inventories.
    pub const EMPTY: Self = Self {
        inventory_size: 0,
        icon_scale: Resolution::R1,        // arbitrary
        render_resolution: Resolution::R1, // arbitrary
        icon_rows: None,
    };

    /// Constructs a [`InvInBlock`].
    ///
    /// * `inventory_size` is the number of slots the inventory should have.
    /// * `icon_scale` is the scale factor by which to scale down the inventory icon blocks,
    ///   relative to the bounds of the block in which they are being displayed.
    ///   It must be no greater than `render_resolution`.
    /// * `render_resolution` is the maximum resolution of inventory icons, and resolution in which
    ///   the `icon_rows`’ position coordinates are expressed.
    ///   [`Modifier::Inventory`] is guaranteed not to increase the block resolution
    ///   beyond this resolution.
    /// * `icon_rows` specifies where in the block the inventory icons should be displayed.
    ///   It may be empty in order to keep the inventory invisible.
    ///
    /// # Panics
    ///
    /// Panics if `icon_scale` is greater than `render_resolution`.
    #[track_caller]
    pub fn new(
        inventory_size: Ix,
        icon_scale: Resolution,
        render_resolution: Resolution,
        icon_rows: impl IntoIterator<Item = IconRow>,
    ) -> Self {
        assert!(icon_scale <= render_resolution);
        Self {
            inventory_size,
            icon_scale,
            render_resolution,
            icon_rows: {
                let collected: Arc<[IconRow]> = icon_rows.into_iter().collect();
                if collected.is_empty() {
                    None
                } else {
                    Some(collected)
                }
            },
        }
    }

    /// Returns the number of slots which inventories created based on this configuration will have.
    pub fn inventory_size(&self) -> Ix {
        self.inventory_size
    }

    /// Returns the scale factor by which to scale down the inventory icon blocks,
    /// relative to the bounds of the block in which they are being displayed.
    ///
    /// This is always less than or equal to [`render_resolution`][Self::render_resolution].
    #[inline]
    pub fn icon_scale(&self) -> Resolution {
        self.icon_scale
    }

    /// Returns the maximum resolution of inventory icons in the block,
    /// which is also the resolution in which the `icon_rows`’ position coordinates are expressed.
    ///
    /// This is always greater than or equal to [`icon_scale`][Self::icon_scale].
    #[inline]
    pub fn render_resolution(&self) -> Resolution {
        self.render_resolution
    }

    /// Returns the size of a single rendered icon, in the voxel coordinate system with resolution
    /// [`render_resolution`][Self::render_resolution].
    ///
    /// This is equal to `self.render_resolution() / self.icon_scale()`.
    #[inline]
    pub fn icon_size_in_resolution(&self) -> Resolution {
        #[cold]
        #[inline(never)]
        fn panic_invalid_icon_size(this: &InvInBlock) -> ! {
            unreachable!("InvInBlock icon_scale invariant broken: {this:?}")
        }

        match self.render_resolution / self.icon_scale {
            Some(size) => size,
            None => panic_invalid_icon_size(self),
        }
    }

    /// Returns the [`IconRow`]s, which specify where in the block to render its inventory.
    ///
    /// If this is empty, then none of the inventory is rendered, and the other icon configuration
    /// has no effect.
    #[inline]
    pub fn icon_rows(&self) -> &[IconRow] {
        match self.icon_rows {
            Some(ref arc_rows) => arc_rows,
            None => &[],
        }
    }

    /// Returns which inventory slots should be rendered as icons, and the cubical bounds in which
    /// those icons should be placed.
    ///
    /// `inventory_size` should be the size of the inventory to be rendered, which will be used
    /// to filter out nonexistent slots and limit the amount of computation performed to match
    /// the inventory.
    pub(crate) fn icon_positions(
        &self,
        inventory_size: Ix,
    ) -> impl Iterator<Item = (Ix, GridAab)> + '_ {
        let icon_size: GridSize = GridSize::splat(
            (self.render_resolution / self.icon_scale).unwrap_or(Resolution::R1).into(),
        );

        self.icon_rows().iter().flat_map(move |row| {
            (0..row.count)
                .map_while(move |sub_index| {
                    let slot_index = row.first_slot.checked_add(sub_index)?;
                    if slot_index >= inventory_size {
                        return None;
                    }
                    let index_coord = GridCoordinate::from(sub_index);
                    let lower_bounds: GridPoint = transpose_point_option(
                        row.origin
                            .to_vector()
                            .zip(row.stride, |origin_c, stride_c| {
                                origin_c.checked_add(stride_c.checked_mul(index_coord)?)
                            })
                            .to_point(),
                    )?;
                    Some((slot_index, lower_bounds))
                })
                .filter_map(move |(slot_index, lower_bounds)| {
                    let bounds = GridAab::checked_from_lower_size(lower_bounds, icon_size).ok()?;

                    // Filter out all slots whose icons’ bounds don’t actually intersect the block.
                    //
                    // TODO: it would be more efficient to stop looping exactly when the next icon
                    // can’t possibly intersect.
                    if bounds
                        .intersection_cubes(GridAab::for_block(self.render_resolution))
                        .is_some()
                    {
                        Some((slot_index, bounds))
                    } else {
                        None
                    }
                })
        })
    }

    /// Combine the two inputs to form one which has the size and display of both.
    pub(crate) fn concatenate(self, other: InvInBlock) -> InvInBlock {
        if self.inventory_size == 0 {
            other
        } else {
            Self::new(
                self.inventory_size.saturating_add(other.inventory_size),
                // TODO(inventory): scale and resolution need adaptation
                self.icon_scale,
                self.render_resolution,
                iter::chain(
                    self.icon_rows().iter().cloned(),
                    other.icon_rows().iter().filter_map(|row| {
                        let mut row = row.clone();
                        row.first_slot = row.first_slot.checked_add(self.inventory_size)?;
                        Some(row)
                    }),
                ),
            )
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
            inventory_size: _,
            icon_scale: _,
            render_resolution: _,
            icon_rows: _,
        } = self;
    }
}

impl crate::block::BlRotate for InvInBlock {
    fn rotationally_symmetric(&self) -> bool {
        // If it doesn't display any icons, then it's symmetric.
        self.icon_rows().is_empty()
    }

    fn rotate(self, rotation: GridRotation) -> Self {
        let Self {
            inventory_size: size,
            icon_scale,
            render_resolution,
            icon_rows: _,
        } = self;
        let transform = rotation.to_positive_octant_transform(render_resolution.into());
        let icon_size =
            GridCoordinate::from((render_resolution / icon_scale).unwrap_or(Resolution::R1));
        Self::new(
            size,
            icon_scale,
            render_resolution,
            self.icon_rows()
                .iter()
                .filter_map(|row| row.clone().rotate(transform, icon_size)),
        )
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
    // TODO: impl Into is for core::ops→core::range migration and should be removed after
    pub fn new(
        slot_range: impl Into<core::range::Range<Ix>>,
        origin: GridPoint,
        stride: GridVector,
    ) -> Self {
        let slot_range = slot_range.into();
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

    /// Returns the range of inventory slots to be rendered.
    #[inline]
    pub fn slot_range(&self) -> core::range::Range<Ix> {
        core::range::Range {
            start: self.first_slot,
            end: self.first_slot.wrapping_add(self.count), // cannot overflow
        }
    }

    /// Returns the lower corner of the location within the voxels where the first slot
    /// in [`slot_range`][Self::slot_range] is to be displayed.
    ///
    /// The scale of these coordinates is determined by the [`InvInBlock::render_resolution()`].
    pub fn origin(&self) -> GridPoint {
        self.origin
    }

    /// Returns the translation between adjacent slots in this row’s range.
    ///
    /// The scale of these coordinates is determined by the [`InvInBlock::render_resolution()`].
    pub fn stride(&self) -> GridVector {
        self.stride
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

impl fmt::Debug for InvInBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let InvInBlock {
            inventory_size,
            icon_scale,
            render_resolution,
            icon_rows: _,
        } = self;
        f.debug_struct("InvInBlock")
            .field("inventory_size", inventory_size)
            .field("icon_scale", icon_scale)
            .field("render_resolution", render_resolution)
            .field("icon_rows", &self.icon_rows())
            .finish()
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

#[cfg(feature = "save")]
mod serde {
    use crate::inv;
    use crate::save::schema;

    impl From<&inv::InvInBlock> for schema::InvInBlockSerV1 {
        fn from(value: &inv::InvInBlock) -> Self {
            let inv::InvInBlock {
                inventory_size,
                icon_scale,
                render_resolution,
                icon_rows: _,
            } = *value;
            schema::InvInBlockSerV1 {
                size: inventory_size,
                icon_scale,
                icon_resolution: render_resolution,
                icon_rows: value.icon_rows().iter().map(schema::IconRowSerV1::from).collect(),
            }
        }
    }

    impl From<schema::InvInBlockSerV1> for inv::InvInBlock {
        fn from(value: schema::InvInBlockSerV1) -> Self {
            let schema::InvInBlockSerV1 {
                size: inventory_size,
                icon_scale,
                icon_resolution: render_resolution,
                icon_rows,
            } = value;
            inv::InvInBlock::new(
                inventory_size,
                icon_scale,
                render_resolution,
                icon_rows.into_iter().map(inv::IconRow::from),
            )
        }
    }

    impl From<&inv::IconRow> for schema::IconRowSerV1 {
        fn from(value: &inv::IconRow) -> Self {
            let inv::IconRow {
                first_slot,
                count,
                origin,
                stride,
            } = *value;
            schema::IconRowSerV1 {
                first_slot,
                count,
                origin: origin.into(),
                stride: stride.into(),
            }
        }
    }

    impl From<schema::IconRowSerV1> for inv::IconRow {
        fn from(value: schema::IconRowSerV1) -> Self {
            let schema::IconRowSerV1 {
                first_slot,
                count,
                origin,
                stride,
            } = value;
            inv::IconRow {
                first_slot,
                count,
                origin: origin.into(),
                stride: stride.into(),
            }
        }
    }
}

// Manual implementation of `Arbitrary` because, currently, if we don't then the
// size hint will be missing, because the `euclid` vector types don't give one.
#[cfg(feature = "arbitrary")]
mod impl_arbitrary {
    use super::*;
    use arbitrary::size_hint::and_all;

    impl<'a> arbitrary::Arbitrary<'a> for InvInBlock {
        fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            let render_resolution: Resolution = u.arbitrary()?;
            let icon_scale: Resolution = u.choose_iter(Resolution::iter_in_range(
                Resolution::R1..=render_resolution,
            ))?;
            Ok(Self::new(
                u.arbitrary()?,
                icon_scale,
                render_resolution,
                <alloc::vec::Vec<IconRow>>::arbitrary(u)?,
            ))
        }

        fn arbitrary_take_rest(mut u: arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
            Self::arbitrary(&mut u)
        }

        fn size_hint(depth: usize) -> (usize, Option<usize>) {
            and_all(&[
                Ix::size_hint(depth),
                Resolution::size_hint(depth),
                Resolution::size_hint(depth),
                <alloc::vec::Vec<IconRow>>::size_hint(depth),
            ])
        }
    }

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
            and_all(&[
                usize::size_hint(depth),
                usize::size_hint(depth),
                <[GridCoordinate; 3]>::size_hint(depth),
                <[GridCoordinate; 3]>::size_hint(depth),
            ])
        }
    }
}

fn checked_add_point_vector(p: GridPoint, v: GridVector) -> Option<GridPoint> {
    Some(GridPoint::new(
        p.x.checked_add(v.x)?,
        p.y.checked_add(v.y)?,
        p.z.checked_add(v.z)?,
    ))
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::Resolution::*;
    use alloc::format;
    use alloc::{vec, vec::Vec};
    use euclid::{point3, vec3};
    use pretty_assertions::assert_eq;

    #[cfg(fmt_debug = "full")]
    #[test]
    fn inv_in_block_debug() {
        let iib = InvInBlock::new(
            9,
            R4,
            R16,
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
                    inventory_size: 9,
                    icon_scale: 4,
                    render_resolution: 16,
                    icon_rows: [
                        IconRow(0..3 @ (+1, +1, +1) + (+5, +0, +0)),
                        IconRow(3..6 @ (+1, +1, +6) + (+5, +0, +0)),
                        IconRow(6..9 @ (+1, +1, +11) + (+5, +0, +0)),
                    ],
                }"
            },
        );
    }

    fn cubic_aab(
        size: Resolution,
        x: GridCoordinate,
        y: GridCoordinate,
        z: GridCoordinate,
    ) -> GridAab {
        GridAab::from_lower_size([x, y, z], GridSize::splat(size.into()))
    }

    #[test]
    fn icon_positions_output() {
        let iib = InvInBlock::new(
            9,
            R4,
            R16,
            vec![
                IconRow::new(0..3, point3(1, 1, 1), vec3(5, 0, 0)),
                IconRow::new(3..6, point3(1, 1, 6), vec3(5, 0, 0)),
                IconRow::new(6..9, point3(1, 1, 11), vec3(5, 0, 0)),
            ],
        );
        assert_eq!(
            iib.icon_positions(999).take(100).collect::<Vec<_>>(),
            vec![
                (0, cubic_aab(R4, 1, 1, 1)),
                (1, cubic_aab(R4, 6, 1, 1)),
                (2, cubic_aab(R4, 11, 1, 1)),
                (3, cubic_aab(R4, 1, 1, 6)),
                (4, cubic_aab(R4, 6, 1, 6)),
                (5, cubic_aab(R4, 11, 1, 6)),
                (6, cubic_aab(R4, 1, 1, 11)),
                (7, cubic_aab(R4, 6, 1, 11)),
                (8, cubic_aab(R4, 11, 1, 11)),
            ]
        );
    }

    #[test]
    fn icon_positions_are_truncated_to_inventory_size() {
        let iib = InvInBlock {
            inventory_size: 1,
            icon_scale: R4,
            render_resolution: R16,
            icon_rows: Some(Arc::new([
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
            ])),
        };
        assert_eq!(
            iib.icon_positions(3).take(100).collect::<Vec<_>>(),
            vec![
                (0, cubic_aab(R4, 0, 0, 0)),
                (1, cubic_aab(R4, 5, 0, 0)),
                (2, cubic_aab(R4, 10, 0, 0)),
                (1, cubic_aab(R4, 0, 0, 5)),
                (2, cubic_aab(R4, 5, 0, 5)),
            ]
        );
    }

    #[test]
    fn icon_positions_are_truncated_to_block_bounds() {
        let iib = InvInBlock::new(
            1,
            R4,
            R8,
            [IconRow {
                first_slot: 0,
                count: 100,
                origin: GridPoint::new(-4, 0, 0),
                stride: GridVector::new(1, 0, 0),
            }],
        );

        assert_eq!(iib.icon_size_in_resolution(), R2, "assumption check");
        assert_eq!(
            iib.icon_positions(999).take(100).collect::<Vec<_>>(),
            vec![
                // we skip slot 0 at -4..-2, slot 1 at -3..-1, and slot 2 at -2..0
                (3, cubic_aab(R2, -1, 0, 0)),
                (4, cubic_aab(R2, 0, 0, 0)),
                (5, cubic_aab(R2, 1, 0, 0)),
                (6, cubic_aab(R2, 2, 0, 0)),
                (7, cubic_aab(R2, 3, 0, 0)),
                (8, cubic_aab(R2, 4, 0, 0)),
                (9, cubic_aab(R2, 5, 0, 0)),
                (10, cubic_aab(R2, 6, 0, 0)),
                (11, cubic_aab(R2, 7, 0, 0)),
                // 8..10 would be outside of block bounds, so no more slots appear
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
