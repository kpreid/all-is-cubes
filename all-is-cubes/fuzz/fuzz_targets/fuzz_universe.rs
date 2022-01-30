#![no_main]
extern crate all_is_cubes;

use std::time::Instant;

use futures_executor::block_on;

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::character::Character;
use all_is_cubes::space::Space;

use libfuzzer_sys::{arbitrary::Arbitrary, fuzz_target};

#[derive(Arbitrary, Debug)]
struct FuzzUniverseTemplate {
    space: Space,
}

fuzz_target!(|input: FuzzUniverseTemplate| {
    let mut app = block_on(AllIsCubesAppState::new());

    // TODO: add some of all kinds of universe objects
    let space = app.universe_mut().insert_anonymous(input.space);
    // TODO: arbitrary-ize character except for the ref
    let _character = app
        .universe_mut()
        .insert_anonymous(Character::spawn_default(space));

    // TODO: need to be able to insert a character into the app state for testing the input interactions

    for _ in 1..100 {
        // TODO: give arbitrary "user" inputs to the input processor
        app.frame_clock.advance_to(Instant::now());
        app.maybe_step_universe();
    }
});
