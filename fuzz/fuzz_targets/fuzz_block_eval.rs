#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::{Block, BlockAttributes, Modifier, Resolution};
use all_is_cubes::math::Rgba;
use all_is_cubes::space::Space;
use all_is_cubes::universe::Universe;

fuzz_target!(|input: (
    BlockAttributes,
    Rgba,
    Vec<Modifier>,
    Option<(Resolution, Space)>
)| {
    let (attributes, color, modifiers, voxels) = input;

    let mut universe = Universe::new();

    let builder = Block::builder().attributes(attributes);
    let mut block = if let Some((resolution, space)) = voxels {
        builder
            .voxels_ref(resolution, universe.insert_anonymous(space))
            .build()
    } else {
        builder.color(color).build()
    };
    block.modifiers_mut().extend(modifiers);

    // Currently, no evaluation possible from this fuzz input should ever fail.
    // (That might change in the future.)
    let evaluated = block.evaluate().unwrap();

    evaluated.consistency_check();
});
