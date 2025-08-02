//! Tests for [`Modifier::Rotate`].
//!
//! The modifier implementation itself is so simple that it does not have its own file.

use super::*;
use crate::block::{
    BlockAttributes, BlockCollision, EvaluatedBlock, Evoxel, Primitive, Resolution::R2, TickAction,
};
use crate::content::make_some_voxel_blocks;
use crate::math::{Cube, Face6, FaceMap, GridAab, OpacityCategory, Rgb, Rgba};
use crate::op::Operation;
use crate::universe::Universe;
use pretty_assertions::assert_eq;

#[test]
fn rotate_evaluation() {
    let resolution = R2;
    let block_bounds = GridAab::for_block(resolution);
    let rotation = GridRotation::RYXZ;
    let mut universe = Universe::new();
    let [replacement] = make_some_voxel_blocks(&mut universe);
    let color_fn = |cube: Cube| {
        Rgba::new(
            cube.x as f32,
            cube.y as f32,
            cube.z as f32,
            if cube.y == 0 { 1.0 } else { 0.0 },
        )
    };
    let rotated_color_fn = |cube: Cube| {
        color_fn(
            rotation
                .to_positive_octant_transform(resolution.into())
                .transform_cube(cube),
        )
    };
    let block = Block::builder()
        .display_name("foo")
        .voxels_fn(resolution, |cube| {
            // Construct a lower half block with all voxels distinct
            Block::from(color_fn(cube))
        })
        .unwrap()
        .rotation_rule(block::RotationPlacementRule::Attach { by: Face6::PX })
        .tick_action(Some(TickAction::from(Operation::Become(
            replacement.clone(),
        ))))
        .build_into(&mut universe);
    let be = block.evaluate(universe.read_ticket()).unwrap();

    let rotated = block.clone().rotate(rotation);
    let re = rotated.evaluate(universe.read_ticket()).unwrap();

    assert_eq!(
        re,
        EvaluatedBlock {
            block: rotated.clone(),
            voxels: Evoxels::from_many(
                R2,
                Vol::from_fn(block_bounds, |cube| {
                    Evoxel {
                        color: rotated_color_fn(cube),
                        emission: Rgb::ZERO,
                        selectable: true,
                        collision: BlockCollision::Hard,
                    }
                })
            ),
            attributes: BlockAttributes {
                display_name: "foo".into(),
                tick_action: Some(TickAction::from(Operation::Become(
                    replacement.rotate(rotation).clone()
                ))),
                rotation_rule: block::RotationPlacementRule::Attach { by: Face6::PY },
                ..BlockAttributes::default()
            },
            cost: block::Cost {
                components: 3,           // Primitive + display_name + Rotate
                voxels: 2u32.pow(3) * 2, // original + rotation
                recursion: 0
            },
            derived: block::Derived {
                color: be.color(),
                face_colors: be.face_colors().rotate(rotation),
                light_emission: Rgb::ZERO,
                opaque: FaceMap::splat(false).with(rotation.transform(Face6::NY), true),
                visible: true,
                uniform_collision: Some(BlockCollision::Hard),
                voxel_opacity_mask: block::VoxelOpacityMask::new_raw(
                    resolution,
                    Vol::from_fn(block_bounds, |cube| {
                        if cube.x == 0 {
                            OpacityCategory::Opaque
                        } else {
                            OpacityCategory::Invisible
                        }
                    })
                ),
            },
        }
    );
}

/// Check that [`Block::rotate`]'s pre-composition is consistent with the interpretation
/// used by evaluating [`Modifier::Rotate`].
#[test]
fn rotate_rotated_consistency() {
    let mut universe = Universe::new();
    let [block] = make_some_voxel_blocks(&mut universe);
    assert!(matches!(block.primitive(), Primitive::Recur { .. }));

    // Two rotations not in the same plane, so they are not commutative.
    let rotation_1 = GridRotation::RyXZ;
    let rotation_2 = GridRotation::RXyZ;

    let rotated_twice = block.clone().rotate(rotation_1).rotate(rotation_2);
    let mut two_rotations = block.clone();
    two_rotations
        .modifiers_mut()
        .extend([Modifier::Rotate(rotation_1), Modifier::Rotate(rotation_2)]);
    assert_ne!(rotated_twice, two_rotations, "Oops; test is ineffective");

    let ev_rotated_twice = rotated_twice.evaluate(universe.read_ticket()).unwrap();
    let ev_two_rotations = two_rotations.evaluate(universe.read_ticket()).unwrap();

    assert_eq!(
        EvaluatedBlock {
            block: rotated_twice,
            cost: ev_rotated_twice.cost,
            ..ev_two_rotations
        },
        ev_rotated_twice,
    );
}

/// `.rotate(IDENTITY)` does nothing.
#[test]
fn rotate_by_identity() {
    let universe = &mut Universe::new();
    let [block] = make_some_voxel_blocks(universe);
    assert_eq!(block, block.clone().rotate(GridRotation::IDENTITY));
    // prove that the test didn't trivially pass by applying to a symmetric block
    assert_ne!(block, block.clone().rotate(Face6::PY.clockwise()));
}
