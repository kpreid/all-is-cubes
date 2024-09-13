#![no_main]

use libfuzzer_sys::fuzz_target;

extern crate all_is_cubes;
use all_is_cubes::block::Block;
use all_is_cubes::universe::{self, VisitHandles as _};

fuzz_target!(|block: Block| {
    // TODO: The `Block` will have pending `Handle`s (not inserted in a Universe).
    // Put them in a universe once that is possible.

    let sink = all_is_cubes::listen::Sink::new();

    match block.evaluate_and_listen(sink.listener()) {
        Ok(evaluated) => {
            evaluated.consistency_check();
        }
        Err(_) => {
            // Errors are an expected possibility; this fuzz test is looking for no panic.
        }
    }

    // Exercise visit_handles(), because it is a recursive operation on the block + modifiers.
    // Unfortunately there's not much we can predict about it.
    block.visit_handles(&mut |_: &dyn universe::ErasedHandle| {});
});
