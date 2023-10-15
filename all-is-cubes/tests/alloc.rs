//! Tests of memory allocation behavior.
//!
//! In a separate test crate to avoid modifying the global allocator elsewhere.

use std::borrow::Cow;

use all_is_cubes::universe::Universe;
use allocation_counter::{measure, AllocationInfo};

use all_is_cubes::block::{self, BlockAttributes};

/// TODO: Make the name refcounted so this count goes to zero.
#[test]
fn clone_block_attributes() {
    let display_name: Cow<'static, str> = String::from("hello").into();
    let original = BlockAttributes {
        // This field will allocate when cloned
        display_name: display_name.clone(),

        // TODO: This field could allocate when cloned but we're not worrying about that now
        tick_action: None,

        // These fields currently will never allocate when cloned
        selectable: true,
        rotation_rule: block::RotationPlacementRule::Never,
        animation_hint: block::AnimationHint::UNCHANGING,
    };
    let mut clone = None;

    assert_eq!(
        measure(|| {
            clone = Some(original.clone());
        }),
        AllocationInfo {
            count_total: 1,
            count_current: 1,
            count_max: 1,
            bytes_total: display_name.len() as u64,
            bytes_current: display_name.len() as i64,
            bytes_max: display_name.len() as u64
        }
    )
}

/// Test that cloning an `EvaluatedBlock`, with voxels, allocates nothing (except
/// BlockAttributes as tested above).
#[test]
fn clone_evaluated_block() {
    let universe = &mut Universe::new();
    let [block] = all_is_cubes::content::make_some_voxel_blocks(universe);

    let original = block.evaluate().unwrap();
    // TODO: should be zero
    let expected_bytes = original.attributes.display_name.len() as u64;
    let mut clone = None;

    assert_eq!(
        measure(|| {
            clone = Some(original.clone());
        }),
        AllocationInfo {
            count_total: 1,
            count_current: 1,
            count_max: 1,
            bytes_total: expected_bytes,
            bytes_current: expected_bytes as i64,
            bytes_max: expected_bytes
        }
    )
}
