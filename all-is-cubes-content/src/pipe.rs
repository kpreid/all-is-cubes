//! Tools for building pipelines — blocks that are positioned to be connected end-to-end.

use core::num::NonZero;

use hashbrown::HashMap;
use itertools::Itertools;

use all_is_cubes::euclid::{point3, vec3};
use all_is_cubes::math::{Cube, Face6, GridRotation};
use all_is_cubes::op::Operation;
use all_is_cubes::time;
use all_is_cubes::{
    block::{
        self, Block,
        Resolution::{R4, R32},
    },
    inv,
    math::GridAab,
};

use crate::{BoxPart, BoxStyle};

/// A pipe block and definition of what connections it has.
///
/// Create these to describe your pipe blocks, then put them into [`Kit`].
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Debug, PartialEq)]
pub struct Descriptor {
    /// The pipe block.
    pub block: Block,
    /// Which face of the block is to be connected backward along the pipeline.
    pub from_face: Face6,
    /// Which face of the block is to be connected forward along the pipeline.
    pub to_face: Face6,
}

/// Complete set of pipe blocks usable to assemble a pipeline using [`Kit::fit()`].
#[derive(Clone, Debug, PartialEq)]
pub struct Kit(
    /// Must be complete (except that there is no such thing as a U-turn pipe).
    HashMap<(Face6, Face6), Block>,
);

impl Kit {
    /// Take a set of pipe blocks and expand it to include rotations of those blocks.
    ///
    /// One straight and one right-angle pipe is sufficient input to make all possible connections.
    pub fn new_with_rotations(pipes: impl IntoIterator<Item = Descriptor>) -> Self {
        let mut table: HashMap<(Face6, Face6), Block> = HashMap::new();
        for pipe in pipes {
            // TODO: Allow caller — or the individual pipes — to rank preferred and dispreferred
            // rotations more finely than "prefer IDENTITY".
            for rotation in GridRotation::ALL_BUT_REFLECTIONS {
                let faces = (
                    rotation.transform(pipe.from_face),
                    rotation.transform(pipe.to_face),
                );
                if rotation == GridRotation::IDENTITY {
                    // Unconditionally insert.
                    table.insert(faces, pipe.block.clone().rotate(rotation));
                } else {
                    // Insert only if we don't have one already.
                    table.entry(faces).or_insert_with(|| pipe.block.clone().rotate(rotation));
                }
            }
        }
        Kit(table)
    }

    /// Rotate and position pipe blocks from `self` to follow `path`.
    ///
    /// The first and last elements of `path` are excluded from the output, and
    /// indicate the starting and ending faces the pipes should connect to.
    ///
    /// `pipes` should contain a straight pipe and a right-angle pipe.
    /// If `pipes` contains multiple elements with the same angle, the one which does not require
    /// rotation will be preferred.
    ///
    /// Panics if any element of `path` is not adjacent to the previous element.
    /// Panics if `self` does not contain a pipe shape that is needed.
    #[track_caller]
    pub fn fit(
        &self,
        path: impl IntoIterator<Item = Cube>,
    ) -> impl Iterator<Item = (Cube, &Block)> {
        path.into_iter().tuple_windows().map(|(cube_behind, cube_here, cube_ahead)| {
            let face_behind = Face6::try_from(cube_behind - cube_here)
                .expect("path must consist of adjacent cubes");
            let face_ahead = Face6::try_from(cube_ahead - cube_here)
                .expect("path must consist of adjacent cubes");
            (cube_here, &self.0[&(face_behind, face_ahead)])
        })
    }
}

// TODO: return value isn't specific enough to be good public API yet
pub(crate) fn make_pipe_blocks(
    txn: &mut all_is_cubes::universe::UniverseTransaction,
) -> (Block, Block) {
    // TODO: Move this to `DemoBlocks`?

    let pipe_corner_material = block::from_color!(0.6, 0.2, 0.2);
    let pipe_side_pattern_material = block::from_color!(0.3, 0.1, 0.1);
    let pipe_side_glass = block::from_color!(0.4, 0.4, 0.4, 0.2);
    let pipe_box_style = BoxStyle::from_fn(|part| {
        if part.is_corner() || part.is_edge() {
            Some(pipe_corner_material.clone())
        } else if part.is_face() && !part.is_on_face(Face6::NZ) && !part.is_on_face(Face6::PZ) {
            // replaced in actual use
            Some(block::from_color!(0.1, 0.1, 0.1))
        } else {
            None
        }
    });

    let resolution = R32;
    let pipe_scale = R4;
    let pipe_slots = inv::Ix::from(pipe_scale) * 2 - 1;
    let straight_pipe_box = GridAab::from_lower_upper([11, 11, 0], [21, 21, 32]);

    let straight_voxels_fn = |cube| {
        let Some(part) = BoxPart::from_cube(straight_pipe_box, cube) else {
            return &block::AIR;
        };
        if part.is_face() && !part.is_on_face(Face6::NZ) && !part.is_on_face(Face6::PZ) {
            // Sides of the pipe get an arrow pattern
            let xy_diamond_radius = (cube.x * 2 - 31).abs().min((cube.y * 2 - 31).abs());
            if (cube.z + xy_diamond_radius + 2).rem_euclid(32) < 12 {
                &pipe_side_pattern_material
            } else {
                &pipe_side_glass
            }
        } else {
            pipe_box_style[part].as_ref().unwrap_or(&block::AIR)
        }
    };

    let straight_pipe = Block::builder()
        .display_name("Pipe")
        .voxels_fn(resolution, straight_voxels_fn)
        .unwrap()
        .inventory_config(inv::InvInBlock::new(
            pipe_slots,
            pipe_scale,
            resolution,
            // pipe slots overlap by half to allow a smoothish movement
            vec![inv::IconRow::new(
                0..pipe_slots,
                point3(12, 12, 0),
                vec3(0, 0, 4),
            )],
        ))
        .tick_action(block::TickAction {
            operation: Operation::Alt(
                [
                    Operation::MoveInventory {
                        transfer_into_adjacent: Some(Face6::PZ),
                    },
                    // This turns failures to move into do-nothing successes.
                    // TODO: there should be a simple empty operation.
                    Operation::Neighbors([].into()),
                ]
                .into(),
            ),
            schedule: time::Schedule::from_period(NonZero::new(6).unwrap()),
        })
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .build_txn(txn);

    let elbow_pipe = Block::builder()
        .display_name("Pipe")
        .voxels_fn(resolution, |mut cube| {
            // Bevel-join the pipe so that it connects NZ to NX instead of PZ.
            if cube.x < cube.z {
                (cube.x, cube.z) = (cube.z, 31 - cube.x);
            }
            straight_voxels_fn(cube)
        })
        .unwrap()
        .inventory_config(inv::InvInBlock::new(
            pipe_slots,
            pipe_scale,
            R32,
            // pipe slots overlap by half to allow a smoothish movement
            vec![
                inv::IconRow::new(0..(pipe_slots / 2), point3(12, 12, 0), vec3(0, 0, 4)),
                inv::IconRow::new(
                    (pipe_slots / 2)..pipe_slots,
                    point3(12, 12, 12),
                    vec3(-4, 0, 0),
                ),
            ],
        ))
        .tick_action(block::TickAction {
            operation: Operation::Alt(
                [
                    Operation::MoveInventory {
                        transfer_into_adjacent: Some(Face6::NX),
                    },
                    // This turns failures to move into do-nothing successes.
                    // TODO: there should be a simple empty operation.
                    Operation::Neighbors([].into()),
                ]
                .into(),
            ),
            schedule: time::Schedule::from_period(NonZero::new(6).unwrap()),
        })
        .animation_hint(block::AnimationHint::replacement(
            block::AnimationChange::Shape,
        ))
        .build_txn(txn);

    (straight_pipe, elbow_pipe)
}
