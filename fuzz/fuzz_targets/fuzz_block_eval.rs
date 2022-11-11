#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::block::Block;

fuzz_target!(|block: Block| {
    // TODO: The `Block` will have pending URefs (not inserted in a Universe).
    // Put them in a universe once that is possible.

    let sink = all_is_cubes::listen::Sink::new();

    match (block.evaluate(), block.listen(sink.listener())) {
        (Ok(evaluated), Ok(())) => {
            evaluated.consistency_check();
        }
        (Err(_), Err(_) | Ok(())) => {
            // Errors are an expected possibility; this fuzz test is looking for no panic.
            //
            // It should never be the case that listen() fails when evaluate() succeeds.
            // (The case when evaluate() only fails is when a `BlockDef` has a broken ref
            // inside it.)

            // TODO: something like assert_eq!(eval_err, listen_err)
            // (but for now there are differences in _which_ URef is checked first)
        }
        (eval_result, listen_result) => {
            panic!("Bad outcome:\neval {eval_result:?}\nlisten {listen_result:?}");
        }
    }

    // Exercise visit_refs(), because it is a recursive operation on the block + modifiers.
    // TODO: To do this we need a handy visitor, but I think it should be simplified...
    // block.visit_refs(visitor);
});
