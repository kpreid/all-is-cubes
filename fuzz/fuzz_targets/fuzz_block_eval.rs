#![no_main]

use libfuzzer_sys::fuzz_target;

use pretty_assertions::assert_eq;

use all_is_cubes::block::{self, Block};
use all_is_cubes::math::GridRotation;
use all_is_cubes::universe::{self, ReadTicket, VisitHandles as _};

fuzz_target!(|block: Block| check_block(block));

fn check_block(block: Block) {
    // TODO: The `Block` will have pending `Handle`s (not inserted in a Universe).
    // Put them in a universe once that is possible.

    let rotationally_symmetric = block.rotationally_symmetric();

    let sink = all_is_cubes::listen::Sink::new();
    match block.evaluate_and_listen(ReadTicket::stub(), sink.listener()) {
        Ok(evaluated) => {
            evaluated.consistency_check();

            if rotationally_symmetric {
                assert!(
                    evaluated.rotationally_symmetric(),
                    "block said symmetric but evaluation did not"
                );

                // Test against an *actual* rotation
                let mut rotated = block.clone();
                rotated
                    .modifiers_mut()
                    .push(block::Modifier::Rotate(GridRotation::CLOCKWISE));
                if let Ok(ev_rotated) = rotated.evaluate(ReadTicket::stub()) {
                    assert_eq!(
                        (ev_rotated.attributes(), ev_rotated.voxels()),
                        (evaluated.attributes(), evaluated.voxels()),
                        "block said symmetric but evaluation differed"
                    );
                }
            }
        }
        Err(_) => {
            // Errors are an expected possibility; this fuzz test is looking for no panic.
        }
    }

    // Exercise visit_handles(), because it is a recursive operation on the block + modifiers.
    // Unfortunately there's not much we can predict about it.
    block.visit_handles(&mut |_: &dyn universe::ErasedHandle| {});
}
