use crate::block::{self, Block};
use crate::math::{FaceMap, GridAab, GridRotation};
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

    /// Construct a `BoxStyle` from a row-major 2D array whose elements are arranged in
    /// the order `[lower, middle, upper, lower & upper]`.
    /// The Z axis is filled with all the same blocks.
    // TODO: figure out if this is good public api
    #[doc(hidden)]
    pub fn from_nine_and_thin(blocks: [[Block; 4]; 4]) -> Self {
        use std::array::from_fn;
        fn twiddle(coord: usize) -> usize {
            [1, 0, 2, 3][coord]
        }

        Self {
            parts: from_fn(|x| {
                from_fn(|y| from_fn(|_z| Some(blocks[twiddle(y)][twiddle(x)].clone())))
            }),
        }
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

        use std::array::from_fn;
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

    /// Returns a transaction that places an axis-aligned box of blocks from this [`BoxStyle`],
    /// within and up to the bounds of the given [`GridAab`].
    ///
    /// * The lines will lie just inside of `bounds`.
    //
    // TODO: allow specifying what happens to existing blocks.
    pub fn create_box(&self, bounds: GridAab) -> space::SpaceTransaction {
        // Compute the regions which are the faces of the box.
        let face_aabs = FaceMap::from_fn(|face| bounds.abut(face, -1).unwrap());

        let mut txn = space::SpaceTransaction::default();
        // TODO: skip the interior if it has nothing to do, rather than iterating
        // over the entire volume.
        for cube in bounds.interior_iter() {
            let on_face = face_aabs.map(|_, aab| aab.contains_cube(cube));
            let part = &self.parts[usize::from(on_face.nx) + usize::from(on_face.px) * 2]
                [usize::from(on_face.ny) + usize::from(on_face.py) * 2]
                [usize::from(on_face.nz) + usize::from(on_face.pz) * 2];

            if let Some(part) = part {
                // TODO: give transactions the ability to compose with already-present blocks
                // so that this can be no-precondition but also no-overwrite? will need
                // allowed-composition-rule work.
                txn.set(cube, None, Some(part.clone())).unwrap();
            }
        }

        txn
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
