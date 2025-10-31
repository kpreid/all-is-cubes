use alloc::sync::Arc;
use alloc::vec::Vec;
use core::ops;

use euclid::{Vector3D, vec3};

use crate::block::{self, Block, Resolution};
use crate::math::{Axis, Cube, Face6, FaceMap, GridAab, GridCoordinate, GridPoint, GridRotation};
use crate::{space, universe};

// Bits in [`BoxPart`] and indexes of [`BoxStyle::parts`].
const LOWER: u8 = 1;
const UPPER: u8 = 2;

/// Set of blocks used to draw 3D boxes of any size, allowing corners and edges to have
/// different blocks than faces and the interior.
///
/// Boxes have walls exactly 1 cube thick. Thus, a `BoxStyle` has 64 parts (blocks),
/// identified by the [`BoxPart`] type:
///
/// * interior volume,
/// * 6 faces,
/// * 12 edges,
/// * 8 corners, and
/// * 38 more parts for the case where the box to be drawn is only one block thick,
///   thus requiring a block to be both faces simultaneously on one or more axes.
///
/// Each part of the box can be absent ([`None`]), which means that when drawn, the existing
/// block in that place is unchanged.
///
/// This can be considered a 3-dimensional analogue of the “9-patch image” concept,
/// or as a voxel “tile set” (but one with no “inside corner” pieces).
///
/// `BoxStyle`’s data is kept behind a reference-counted pointer,
/// so `BoxStyle` is small and cheap to clone.
#[derive(Clone, Debug)]
pub struct BoxStyle {
    /// Contains every type of block that can appear in a box.
    /// 3D array `.parts[x][y][z]` indexed by two bitflags:
    ///
    /// * Bit 0 (1) = the block is on an lower edge.
    /// * Bit 1 (2) = the block is on an upper edge.
    ///
    /// So the sequence on each axis is [interior, lower, upper, lower & upper].
    ///
    /// `Arc`ed to allow it to be passed around cheaply even though it contains 64 `Block`s
    /// which would be 128 × pointer size.
    parts: Arc<[[[Option<Block>; 4]; 4]; 4]>,
}

impl BoxStyle {
    // TODO: Several of these constructors (not `from_fn()`) are overly specific;
    // figure out better API to offer so callers can do those things easily.

    /// Construct a `BoxStyle` by asking a function for each part.
    ///
    /// The order of calls to the function is unspecified and subject to change.
    pub fn from_fn<F>(mut f: F) -> Self
    where
        F: FnMut(BoxPart) -> Option<Block>,
    {
        // We care a lot more about keeping build time and binary size low by not duplicating code,
        // than about the relatively tiny possible savings from inlining and monomorphizing
        // this code that only participates in expensive rare worldgen operations.
        #[inline(never)]
        fn inner(f: &mut dyn FnMut(BoxPart) -> Option<Block>) -> BoxStyle {
            use core::array::from_fn;
            // TODO: deduplicate identical Blocks to reduce total number of allocations present
            BoxStyle {
                parts: Arc::new(from_fn(|x| {
                    from_fn(|y| from_fn(|z| f(BoxPart(vec3(x as u8, y as u8, z as u8)))))
                })),
            }
        }

        inner(&mut f)
    }

    /// Construct a `BoxStyle` from a 2D 4×4×1 multiblock whose components on each axis
    /// are in the order `[lower, middle, upper, lower & upper]`; that is, this layout
    /// of blocks:
    ///
    /// ```text
    /// ┊ ┏━━━━━━━━━━━━━┓┏━━━┓
    /// 3 ┃             ┃┃   ┃
    /// ┊ ┗━━━━━━━━━━━━━┛┗━━━┛
    /// ┃ ┏━━━━━━━━━━━━━┓┏━━━┓
    /// 2 ┃             ┃┃   ┃
    /// ┃ ┃             ┃┃   ┃
    /// ┊ ┃             ┃┃   ┃
    /// 1 ┃             ┃┃   ┃
    /// ┊ ┃             ┃┃   ┃
    /// ┃ ┃             ┃┃   ┃
    /// 0 ┃             ┃┃   ┃
    /// ┃ ┗━━━━━━━━━━━━━┛┗━━━┛
    ///   ━━0━━┈┈1┈┈━━2━━┈┈3┈┈
    /// ```
    ///
    /// The resolution is one quarter of the input.
    /// The Z axis is filled with all the same blocks. TODO: Make it sensitive to the input
    /// dimensions as defined by multiblock metadata instead.
    // TODO: figure out if this is good public api
    pub fn from_nine_and_thin(multiblock: &Block) -> Self {
        use core::array::from_fn;
        fn twiddle(coord: usize) -> GridCoordinate {
            [1, 0, 2, 3][coord]
        }

        Self {
            parts: Arc::new(from_fn(|x| {
                from_fn(|y| {
                    from_fn(|_z| {
                        Some(multiblock.clone().with_modifier(block::Zoom::new(
                            Resolution::R4,
                            GridPoint::new(twiddle(x), twiddle(y), 0),
                        )))
                    })
                })
            })),
        }
    }

    /// Returns the [`GridAab`]s giving the bounds of the individual regions that should be
    /// drawn in voxels to create the multiblock input to [`BoxStyle::from_nine_and_thin()`],
    /// and the single [`GridAab`] bounding all of thewm.
    ///
    /// This function is a convenience for setup and not mandatory.
    //---
    // TODO: This currently isn't used and it's unclear whether it is good API
    #[expect(dead_code)]
    fn nine_boxes(
        resolution: Resolution,
    ) -> impl Iterator<Item = GridAab> + Clone + Send + 'static {
        let resolution = GridCoordinate::from(resolution);
        let ranges = [0..resolution * 3, resolution * 3..resolution * 4];

        ranges.into_iter().flat_map(move |y_range| {
            ranges
                .into_iter()
                .map(move |x_range| GridAab::from_ranges([x_range, y_range, 0..resolution]))
        })
    }

    /// Construct a `BoxStyle` from block types for walls, floor, ceiling, and corners.
    /// The one-block corners will be left blank.
    /// without anything to handle corners of one-block walls.
    ///
    /// `corner` should be oriented so as to join up with +X and +Z walls, and will be
    /// rotated about the Y axis to suit other corners.
    ///
    /// TODO: this is a quick kludge to migrate some other worldgen code, and doesn't
    /// have a necessarily logical combination of parameters or the best rotation choices.
    ///
    /// TODO: wall blocks should be rotated
    #[doc(hidden)] // TODO: arguments and behavior need refinement
    pub fn from_whole_blocks_for_walls(
        wall: Option<Block>,
        floor: Option<Block>,
        ceiling: Option<Block>,
        corner: Option<Block>,
    ) -> Self {
        let up = Face6::PY;

        let corner_px_nz = corner.clone().map(|block| block.rotate(up.clockwise()));
        let corner_px_pz = corner.clone().map(|block| block.rotate(GridRotation::RxYz));
        let corner_nx_pz = corner.clone().map(|block| block.rotate(up.counterclockwise()));
        let corner_nx_nz = corner;
        let unsupported = || None;

        Self {
            parts: Arc::new([
                [
                    // X interior
                    [None, wall.clone(), wall.clone(), wall.clone()], // Y interior
                    [floor.clone(), wall.clone(), wall.clone(), wall.clone()], // Y floor
                    [ceiling.clone(), wall.clone(), wall.clone(), wall.clone()], // Y ceiling
                    [
                        // Y floorceiling
                        floor.or(ceiling), // Y floorceiling XZ interior
                        wall.clone(),
                        wall.clone(),
                        wall.clone(),
                    ],
                ],
                [
                    // X lower wall
                    [
                        // Y interior
                        wall.clone(),
                        corner_nx_nz.clone(),
                        corner_nx_pz.clone(),
                        unsupported(),
                    ],
                    [
                        // Y floor
                        wall.clone(),
                        corner_nx_nz.clone(),
                        corner_nx_pz.clone(),
                        wall.clone(),
                    ],
                    [
                        // Y ceiling
                        wall.clone(),
                        corner_nx_nz.clone(),
                        corner_nx_pz.clone(),
                        wall.clone(),
                    ],
                    [
                        // Y floorceiling
                        wall.clone(),
                        corner_nx_nz,
                        corner_nx_pz,
                        wall.clone(),
                    ],
                ],
                [
                    // X upper wall
                    [
                        // Y interior
                        wall.clone(),
                        corner_px_nz.clone(),
                        corner_px_pz.clone(),
                        unsupported(),
                    ],
                    [
                        // Y floor
                        wall.clone(),
                        corner_px_nz.clone(),
                        corner_px_pz.clone(),
                        wall.clone(),
                    ],
                    [
                        // Y ceiling
                        wall.clone(),
                        corner_px_nz.clone(),
                        corner_px_pz.clone(),
                        wall.clone(),
                    ],
                    [
                        // Y floorceiling
                        wall.clone(),
                        corner_px_nz,
                        corner_px_pz,
                        wall.clone(),
                    ],
                ],
                [
                    // X both walls
                    [wall.clone(), unsupported(), unsupported(), unsupported()], // Y interior
                    [wall.clone(), unsupported(), unsupported(), unsupported()], // Y floor
                    [wall.clone(), unsupported(), unsupported(), unsupported()], // Y ceiling
                    [wall, unsupported(), unsupported(), unsupported()],         // Y floorceiling
                ],
            ]),
        }
    }

    /// Construct a `BoxStyle` made up of one type of corner block and one type of edge
    /// block.
    ///
    /// * `corner_block` will be composited with the line sections at all eight
    ///   corners of the box. It should be oriented as the `lower_bounds` corner of the box;
    ///   the other seven corners will be mirrored across the relevant axis.
    /// * `line_section_block` should be a block which is a line segment at the origin and
    ///   extending in the [`+Z`](crate::math::Face6::PZ) direction.
    ///   It should be symmetric about the X-Y=0 plane, and will be rotated and mirrored
    ///   to make the other forms.
    #[expect(clippy::needless_pass_by_value, reason = "consistency")]
    pub fn from_composited_corner_and_edge(corner_block: Block, line_section_block: Block) -> Self {
        #![expect(non_snake_case)]

        let corner_x = corner_block.clone().rotate(GridRotation::RxYZ);
        let corner_y = corner_block.clone().rotate(GridRotation::RXyZ);
        let corner_z = corner_block.clone().rotate(GridRotation::RXYz);
        let corner_xy = corner_block.clone().rotate(GridRotation::RxyZ);
        let corner_xz = corner_block.clone().rotate(GridRotation::RxYz);
        let corner_yz = corner_block.clone().rotate(GridRotation::RXyz);
        let corner_xyz = corner_block.clone().rotate(GridRotation::Rxyz);

        // Lines parallel to the X axis
        let line_x_YZ = line_section_block.clone().rotate(GridRotation::RZYX);
        let line_x_yZ = line_x_YZ.clone().rotate(GridRotation::RXyZ);
        let line_x_Yz = line_x_YZ.clone().rotate(GridRotation::RXYz);
        let line_x_yz = line_x_YZ.clone().rotate(GridRotation::RXyz);
        // Lines parallel to the Y axis
        let line_y_XZ = line_section_block.clone().rotate(GridRotation::RXZY);
        let line_y_xZ = line_y_XZ.clone().rotate(GridRotation::RxYZ);
        let line_y_Xz = line_y_XZ.clone().rotate(GridRotation::RXYz);
        let line_y_xz = line_y_XZ.clone().rotate(GridRotation::RxYz);
        // Lines parallel to the Z axis
        let line_z_XY = line_section_block;
        let line_z_xY = line_z_XY.clone().rotate(GridRotation::RxYZ);
        let line_z_Xy = line_z_XY.clone().rotate(GridRotation::RXyZ);
        let line_z_xy = line_z_XY.clone().rotate(GridRotation::RxyZ);

        Self::from_fn(|part| {
            let on_face = part.on_faces();
            let mut blocks: Vec<&Block> = Vec::new();

            // Four Z lines
            if on_face.nx && on_face.ny {
                blocks.push(&line_z_XY);
            }
            if on_face.px && on_face.ny {
                blocks.push(&line_z_xY);
            }
            if on_face.nx && on_face.py {
                blocks.push(&line_z_Xy);
            }
            if on_face.px && on_face.py {
                blocks.push(&line_z_xy);
            }
            // Four X lines
            if on_face.nz && on_face.ny {
                blocks.push(&line_x_YZ);
            }
            if on_face.pz && on_face.ny {
                blocks.push(&line_x_Yz);
            }
            if on_face.nz && on_face.py {
                blocks.push(&line_x_yZ);
            }
            if on_face.pz && on_face.py {
                blocks.push(&line_x_yz);
            }
            // Four Y lines
            if on_face.nz && on_face.nx {
                blocks.push(&line_y_XZ);
            }
            if on_face.pz && on_face.nx {
                blocks.push(&line_y_Xz);
            }
            if on_face.nz && on_face.px {
                blocks.push(&line_y_xZ);
            }
            if on_face.pz && on_face.px {
                blocks.push(&line_y_xz);
            }

            // Eight corners (after the edges so that they can override)
            if on_face.nx && on_face.ny && on_face.nz {
                blocks.push(&corner_block);
            }
            if on_face.nx && on_face.ny && on_face.pz {
                blocks.push(&corner_z);
            }
            if on_face.nx && on_face.py && on_face.nz {
                blocks.push(&corner_y);
            }
            if on_face.nx && on_face.py && on_face.pz {
                blocks.push(&corner_yz);
            }
            if on_face.px && on_face.ny && on_face.nz {
                blocks.push(&corner_x);
            }
            if on_face.px && on_face.ny && on_face.pz {
                blocks.push(&corner_xz);
            }
            if on_face.px && on_face.py && on_face.nz {
                blocks.push(&corner_xy);
            }
            if on_face.px && on_face.py && on_face.pz {
                blocks.push(&corner_xyz);
            }

            if !blocks.is_empty() {
                Some(block::Composite::stack(
                    block::AIR,
                    blocks.drain(..).map(|block| {
                        block::Composite::new(block.clone(), block::CompositeOperator::Over)
                    }),
                ))
            } else {
                None
            }
        })
    }

    /// Replace a single part of this and return the modified style.
    #[must_use]
    pub fn with(mut self, part: BoxPart, block: Option<Block>) -> Self {
        let parts = Arc::make_mut(&mut self.parts);
        parts[usize::from(part.0.x)][usize::from(part.0.y)][usize::from(part.0.z)] = block;
        self
    }

    /// Applies the given function to every block present in this.
    /// Does not affect absent blocks.
    #[must_use]
    pub fn map(mut self, mut block_fn: impl FnMut(BoxPart, Block) -> Block) -> Self {
        let parts = Arc::make_mut(&mut self.parts);
        for (x, plane) in parts.iter_mut().enumerate() {
            for (y, row) in plane.iter_mut().enumerate() {
                for (z, block) in row.iter_mut().enumerate() {
                    if let Some(old_block) = block.take() {
                        let part = BoxPart(vec3(x as u8, y as u8, z as u8));
                        *block = Some(block_fn(part, old_block));
                    }
                }
            }
        }
        self
    }

    /// Returns a transaction that places an axis-aligned box of blocks from this [`BoxStyle`],
    /// within and up to the bounds of the given [`GridAab`].
    ///
    /// * The lines will lie just inside of `bounds`.
    //
    // TODO: allow specifying what happens to existing blocks.
    pub fn create_box(&self, bounds: GridAab) -> space::SpaceTransaction {
        // TODO: this could be sometimes more efficiently done as up to 27 fill operations
        // rather than one big one
        space::SpaceTransaction::filling(bounds, |cube| {
            // TODO: give transactions the ability to compose with already-present blocks
            // so that this can be no-precondition but also no-overwrite? will need
            // allowed-composition-rule work.
            space::CubeTransaction::replacing(None, self.cube_at(bounds, cube).cloned())
        })
    }

    /// Returns the block that is the part of this box at the specified `cube`
    /// when the box bounds are `bounds`.
    pub fn cube_at(&self, bounds: GridAab, cube: Cube) -> Option<&Block> {
        self[BoxPart::from_cube(bounds, cube)?].as_ref()
    }
}

impl ops::Index<BoxPart> for BoxStyle {
    type Output = Option<Block>;

    fn index(&self, index: BoxPart) -> &Self::Output {
        &self.parts[usize::from(index.0.x)][usize::from(index.0.y)][usize::from(index.0.z)]
    }
}
impl ops::IndexMut<BoxPart> for BoxStyle {
    fn index_mut(&mut self, index: BoxPart) -> &mut Self::Output {
        &mut Arc::make_mut(&mut self.parts)[usize::from(index.0.x)][usize::from(index.0.y)]
            [usize::from(index.0.z)]
    }
}

impl universe::VisitHandles for BoxStyle {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self { parts } = self;
        parts.visit_handles(visitor);
    }
}

/// Identifies one of the 64 parts of a [`BoxStyle`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct BoxPart(
    /// These positions/bit-flags follow the same indexing scheme as for the [`BoxStyle::parts`]
    /// field. Only values 0, 1, 2, and 3 are valid.
    ///
    /// We could make this representation denser by using a single `u8` bitfield, but that would be
    /// cryptic to no great gain.
    Vector3D<u8, ()>,
);

impl BoxPart {
    /// The part that is the interior of the box.
    pub const INTERIOR: Self = Self(vec3(0, 0, 0));

    /// The part that is the whole box when its size is 1×1×1.
    pub const UNIT: Self = Self(vec3(LOWER | UPPER, LOWER | UPPER, LOWER | UPPER));

    /// Returns the part which is the given face of the box.
    ///
    /// Note that this does not include the edges and corners of that face.
    ///
    /// This function is the inverse of [`BoxPart::to_face()`].
    #[must_use]
    pub const fn face(face: Face6) -> Self {
        match face {
            Face6::NX => Self(vec3(LOWER, 0, 0)),
            Face6::NY => Self(vec3(0, LOWER, 0)),
            Face6::NZ => Self(vec3(0, 0, LOWER)),
            Face6::PX => Self(vec3(UPPER, 0, 0)),
            Face6::PY => Self(vec3(0, UPPER, 0)),
            Face6::PZ => Self(vec3(0, 0, UPPER)),
        }
    }

    /// Returns the `BoxPart` that denotes the part of `bounds` the given `cube` is on,
    /// or [`None`] if `cube` is outside `bounds`.
    pub fn from_cube(bounds: GridAab, cube: Cube) -> Option<Self> {
        if bounds.contains_cube(cube) {
            Some(Self::from_on_faces({
                let lb = bounds.lower_bounds();
                let ub = bounds.upper_bounds();
                FaceMap {
                    nx: lb.x == cube.x,
                    ny: lb.y == cube.y,
                    nz: lb.z == cube.z,
                    px: ub.x.checked_sub(1) == Some(cube.x),
                    py: ub.y.checked_sub(1) == Some(cube.y),
                    pz: ub.z.checked_sub(1) == Some(cube.z),
                }
            }))
        } else {
            None
        }
    }

    fn from_on_faces(on_faces: FaceMap<bool>) -> Self {
        Self(
            on_faces.negatives().map(u8::from)
                + on_faces.positives().map(|p: bool| u8::from(p) * 2),
        )
    }

    /// Returns whether this part is a face, in the polyhedron sense
    /// (i.e. touches only one side of the box, or one side and its opposite).
    ///
    /// Equivalent to `self.to_face().is_some()`.
    pub fn is_face(self) -> bool {
        // Note: the aarch64 machine code for this is much shorter than `.to_face().is_some()`.
        self.count_touched_axes() == 1
    }

    /// If this part is a face, in the polyhedron sense
    /// (i.e. touches only one side of the box, or one side and its opposite),
    /// then return that face.
    ///
    /// This function is the inverse of [`BoxPart::face()`].
    #[rustfmt::skip]
    pub fn to_face(self) -> Option<Face6> {
        match self.0 {
            Vector3D { x: LOWER, y: 0, z: 0, .. } => Some(Face6::NX),
            Vector3D { x: 0, y: LOWER, z: 0, .. } => Some(Face6::NY),
            Vector3D { x: 0, y: 0, z: LOWER, .. } => Some(Face6::NZ),
            Vector3D { x: UPPER, y: 0, z: 0, .. } => Some(Face6::PX),
            Vector3D { x: 0, y: UPPER, z: 0, .. } => Some(Face6::PY),
            Vector3D { x: 0, y: 0, z: UPPER, .. } => Some(Face6::PZ),
            _ => None,
        }
    }

    /// Returns whether this part is an edge, in the polyhedron sense
    /// (i.e. touches two adjacent sides of the box, or the perimeter of a flat box).
    pub fn is_edge(self) -> bool {
        self.count_touched_axes() == 2
    }

    /// Returns whether this part is a corner, in the polyhedron sense
    /// (i.e. touches three adjacent sides of the box
    pub fn is_corner(self) -> bool {
        self.count_touched_axes() == 3
    }

    /// Returns whether this part touches the specified face.
    ///
    /// This includes [`BoxPart::face(face)`](BoxPart::face)
    /// but also all the adjacent edges and corners,
    /// and the “both faces” case used when the box is 1 block thick on some axis.
    #[must_use]
    pub fn is_on_face(self, face: Face6) -> bool {
        self.0[face.axis()] & if face.is_negative() { LOWER } else { UPPER } != 0
    }

    /// Returns the same as [`BoxPart::is_on_face()`] but for all faces.
    pub const fn on_faces(self) -> FaceMap<bool> {
        let Vector3D { x, y, z, .. } = self.0;
        FaceMap {
            nx: x & LOWER != 0,
            ny: y & LOWER != 0,
            nz: z & LOWER != 0,
            px: x & UPPER != 0,
            py: y & UPPER != 0,
            pz: z & UPPER != 0,
        }
    }

    /// Returns the part which lies in the given direction on that axis and is the
    /// same as this part on other axes.
    ///
    /// This may be used to find an edge or corner starting from a face,
    /// or to find ends starting from [`UNIT`](Self::UNIT).
    #[must_use]
    pub fn push(mut self, direction: Face6) -> Self {
        self.0[direction.axis()] = if direction.is_negative() {
            LOWER
        } else {
            UPPER
        };
        self
    }

    /// Returns the part which is centered on the given axis and is the
    /// same as this part on other axes.
    ///
    /// This may be used to find an edge or corner starting from a face,
    /// or to find ends starting from [`UNIT`](Self::UNIT).
    #[must_use]
    pub fn centered_on(mut self, axis: Axis) -> Self {
        self.0[axis] = 0;
        self
    }

    /// Counts how many box faces this part touches, such that a face and its opposite face
    /// only counts once.
    fn count_touched_axes(self) -> u8 {
        u8::from(self.0.x != 0) + u8::from(self.0.y != 0) + u8::from(self.0.z != 0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test that funny box sizes (e.g. zero in some axis) don't cause panics.
    #[test]
    fn box_smoke_test() {
        let style = BoxStyle::from_composited_corner_and_edge(
            block::from_color!(1., 0., 0., 1.),
            block::from_color!(0., 0., 1., 1.),
        );
        for aab in [
            GridAab::ORIGIN_CUBE,
            GridAab::from_lower_size([0, 0, 0], [0, 10, 0]),
            GridAab::from_lower_size([-20, 3, 5], [1, 1, 10]),
        ] {
            let txn = style.create_box(aab);
            assert_eq!(txn.bounds(), if aab.is_empty() { None } else { Some(aab) });
        }
    }

    #[test]
    pub fn part_face_relationships() {
        for face in Face6::ALL {
            assert!(BoxPart::face(face).is_on_face(face), "{face:?} is_on_face");
            assert_eq!(
                BoxPart::face(face),
                BoxPart::INTERIOR.push(face),
                "{face:?} push"
            );
            assert_eq!(
                BoxPart::face(face).to_face(),
                Some(face),
                "{face:?} to_face"
            );
        }
    }
}
