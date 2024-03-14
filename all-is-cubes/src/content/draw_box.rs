use alloc::vec::Vec;

use crate::block::{self, Block, Resolution};
use crate::math::{Cube, FaceMap, GridAab, GridCoordinate, GridPoint, GridRotation};
use crate::space;

/// Set of blocks used to draw 3D boxes of any size, allowing corners and edges to have
/// different blocks than faces and the interior.
///
/// This can be considered a 3-dimensional analogue of the “9-patch image” concept,
/// or as a voxel “tile set” (but one with no “inside corner” pieces).
#[derive(Clone, Debug)]
pub struct BoxStyle {
    /// Contains every type of block that can appear in a box.
    /// 3D array `.parts[x][y][z]` indexed by two bitflags:
    /// * Bit 0 (1) = the block is on an lower edge.
    /// * Bit 1 (2) = the block is on an upper edge.
    /// So the sequence on each axis is [interior, lower, upper, lower & upper].
    parts: [[[Option<Block>; 4]; 4]; 4],
}

impl BoxStyle {
    // TODO: Figure out a less complex set of constructors. Allow replacing individual parts.

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
        fn twiddle(coord: usize) -> usize {
            [1, 0, 2, 3][coord]
        }

        Self {
            parts: from_fn(|x| {
                from_fn(|y| {
                    from_fn(|_z| {
                        Some(multiblock.clone().with_modifier(block::Zoom::new(
                            Resolution::R4,
                            GridPoint::new(twiddle(x) as i32, twiddle(y) as i32, 0),
                        )))
                    })
                })
            }),
        }
    }

    /// Returns the [`GridAab`]s giving the bounds of the individual regions that should be
    /// drawn in voxels to create the multiblock input to [`BoxStyle::from_nine_and_thin()`],
    /// and the single [`GridAab`] bounding all of thewm.
    ///
    /// This function is a convenience for setup and not mandatory.
    #[doc(hidden)] // TODO: good public api? or ad-hoc?
    pub fn nine_boxes(
        resolution: Resolution,
    ) -> impl Iterator<Item = GridAab> + Clone + Send + 'static {
        let resolution = GridCoordinate::from(resolution);
        let ranges = [0..resolution * 3, resolution * 3..resolution * 4];

        ranges.clone().into_iter().flat_map(move |y_range| {
            ranges
                .clone()
                .into_iter()
                .map(move |x_range| GridAab::from_ranges([x_range, y_range.clone(), 0..resolution]))
        })
    }

    /// Construct a `BoxStyle` that uses the given blocks for
    /// interior, faces, edges, and corners, never rotated.
    pub fn from_geometric_categories(
        interior: Option<Block>,
        face: Option<Block>,
        edge: Option<Block>,
        corner: Option<Block>,
    ) -> Self {
        fn level<T: Clone>(inner: T, outer: T) -> [T; 4] {
            [inner, outer.clone(), outer.clone(), outer]
        }
        Self {
            parts: level(
                level(
                    level(interior, face.clone()),
                    level(face.clone(), edge.clone()),
                ),
                level(level(face, edge.clone()), level(edge, corner)),
            ),
        }
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
        let corner_px_nz = corner
            .clone()
            .map(|block| block.rotate(GridRotation::CLOCKWISE));
        let corner_px_pz = corner.clone().map(|block| block.rotate(GridRotation::RxYz));
        let corner_nx_pz = corner
            .clone()
            .map(|block| block.rotate(GridRotation::COUNTERCLOCKWISE));
        let corner_nx_nz = corner;
        let unsupported = || None;

        Self {
            parts: [
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
            ],
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
    pub fn from_composited_corner_and_edge(corner_block: Block, line_section_block: Block) -> Self {
        #![allow(non_snake_case)]

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

        use core::array::from_fn;
        Self {
            parts: from_fn(|x| {
                from_fn(|y| {
                    from_fn(|z| {
                        // Decode coordinate index encoding to a more legible form
                        // (this is just a way of giving names to the conditions).
                        let on_face = FaceMap {
                            nx: x & 1 != 0,
                            ny: y & 1 != 0,
                            nz: z & 1 != 0,
                            px: x & 2 != 0,
                            py: y & 2 != 0,
                            pz: z & 2 != 0,
                        };

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
                                    block::Composite::new(
                                        block.clone(),
                                        block::CompositeOperator::Over,
                                    )
                                }),
                            ))
                        } else {
                            None
                        }
                    })
                })
            }),
        }
    }

    #[must_use]
    pub fn with_interior(mut self, interior: Option<Block>) -> Self {
        self.parts[0][0][0] = interior;
        self
    }

    /// Applies the given function to every block present in this.
    #[must_use]
    pub fn map_blocks(mut self, mut block_fn: impl FnMut(Block) -> Block) -> Self {
        for block in self.parts.iter_mut().flatten().flatten() {
            if let Some(old_block) = block.take() {
                *block = Some(block_fn(old_block));
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

    /// Returns the block that is the part of this box at the specified `cube`.
    pub fn cube_at(&self, bounds: GridAab, cube: Cube) -> Option<&Block> {
        let on_face = on_faces(bounds, cube);
        self.parts[usize::from(on_face.nx) + usize::from(on_face.px) * 2]
            [usize::from(on_face.ny) + usize::from(on_face.py) * 2]
            [usize::from(on_face.nz) + usize::from(on_face.pz) * 2]
            .as_ref()
    }
}

/// Returns whether the cube lies within the outermost layer of `bounds`, for each face.
///
/// TODO: This seems generally useful; should it be a method on [`GridAab`]?
fn on_faces(bounds: GridAab, cube: Cube) -> FaceMap<bool> {
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
}

#[cfg(test)]
mod tests {
    use crate::math::Rgba;

    use super::*;

    /// Test that funny box sizes (e.g. zero in some axis) don't cause panics.
    #[test]
    fn box_smoke_test() {
        let style = BoxStyle::from_composited_corner_and_edge(
            Block::from(Rgba::new(1., 0., 0., 1.)),
            Block::from(Rgba::new(0., 0., 1., 1.)),
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
}
