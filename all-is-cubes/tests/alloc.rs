//! Tests of memory allocation behavior.
//!
//! In a separate test crate to avoid modifying the global allocator elsewhere.

use all_is_cubes::inv;
use all_is_cubes::op::Operation;
use allocation_counter::{measure, AllocationInfo};

use all_is_cubes::block::{self, BlockAttributes};
use all_is_cubes::universe::Universe;

#[test]
fn clone_block_attributes() {
    let original = BlockAttributes {
        // These fields are refcounted or `Copy` and will not allocate when cloned
        display_name: arcstr::literal!("hello"),
        selectable: true,
        animation_hint: block::AnimationHint::UNCHANGING,

        placement_action: Some(block::PlacementAction {
            operation: Operation::Become(block::AIR),
            in_front: false,
        }),
        tick_action: Some(block::TickAction::from(Operation::Become(block::AIR))),
        activation_action: Some(Operation::Become(block::AIR)),

        // TODO(inventory): This field will allocate when cloned if it is nonempty,
        // and we should fix that and test it.
        inventory: inv::InvInBlock::default(),

        // These fields currently will never allocate when cloned
        rotation_rule: block::RotationPlacementRule::Never,
    };
    let mut clone = None;

    assert_no_alloc(|| {
        clone = Some(original.clone());
    });
}

/// Test that cloning an `EvaluatedBlock`, with voxels, allocates nothing (except
/// `BlockAttributes` as tested above).
#[test]
fn clone_evaluated_block() {
    let universe = &mut Universe::new();
    let [block] = all_is_cubes::content::make_some_voxel_blocks(universe);

    let original = block.evaluate().unwrap();
    let mut clone = None;

    assert_no_alloc(|| {
        clone = Some(original.clone());
    });
}

// TODO: Test cloning of `Operation`

#[track_caller]
fn assert_no_alloc(f: impl FnOnce()) {
    assert_eq!(measure(f), AllocationInfo::default());
}
