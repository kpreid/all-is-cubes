#![no_main]

use std::time::{Duration, Instant};

use all_is_cubes::character::Character;
use all_is_cubes::space::Space;
use all_is_cubes::time;
use all_is_cubes::universe::{ArbitraryWithUniverse, StrongHandle};

use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: ArbitraryWithUniverse<StrongHandle<Space>>| {
    let mut universe = input.universe;
    let space_handle = input.contents;

    // TODO: this condition is a kludge because Character::spawn_default is panicky
    if space_handle.read(universe.read_ticket()).is_ok() {
        // TODO: arbitrary-ize character except for the space ref
        let _character = StrongHandle::from(universe.insert_anonymous(Character::spawn_default(
            universe.read_ticket(),
            space_handle.to_weak(),
        )));
    }

    for _ in 1..100 {
        // TODO: give arbitrary "user" inputs to the character and other universe manipulations
        universe.step(
            false,
            time::Deadline::At(Instant::now() + Duration::from_millis(1)),
        );
    }
});
