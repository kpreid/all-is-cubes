#![no_main]

use libfuzzer_sys::fuzz_target;

use pretty_assertions::assert_eq;

use all_is_cubes::block::{self, Block};
use all_is_cubes::math::Face6;
use all_is_cubes::universe::{self, ReadTicket, VisitHandles as _};

fuzz_target!(
    |input: all_is_cubes::universe::ArbitraryWithUniverse<Block>| {
        let read_ticket = input.universe.read_ticket();
        check_block(read_ticket, &input.contents);
    }
);

fn check_block(read_ticket: ReadTicket<'_>, block: &Block) {
    let rotationally_symmetric = block.rotationally_symmetric();

    let log = all_is_cubes::listen::Log::new();
    match block.evaluate_and_listen(read_ticket, log.listener()) {
        Ok(evaluated) => {
            evaluated.consistency_check();

            if rotationally_symmetric {
                assert!(
                    evaluated.rotationally_symmetric(),
                    "block said symmetric but evaluation did not"
                );

                // Test against an *actual* rotation
                let mut rotated = block.clone();
                rotated.modifiers_mut().push(block::Modifier::Rotate(Face6::PY.clockwise()));
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
