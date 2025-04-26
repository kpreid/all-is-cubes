#![no_main]
extern crate all_is_cubes;

use std::time::{Duration, Instant};

use all_is_cubes::character::Character;
use all_is_cubes::space::Space;
use all_is_cubes::time;
use all_is_cubes::universe::Universe;

use libfuzzer_sys::{arbitrary::Arbitrary, fuzz_target};

#[derive(Arbitrary, Debug)]
struct FuzzUniverseTemplate {
    space: Space,
}

fuzz_target!(|input: FuzzUniverseTemplate| {
    let mut universe = Universe::new();

    // TODO: add some of all kinds of universe objects
    let space = universe.insert_anonymous(input.space);
    // TODO: arbitrary-ize character except for the ref
    let _character =
        universe.insert_anonymous(Character::spawn_default(universe.read_ticket(), space));

    for _ in 1..100 {
        // TODO: give arbitrary "user" inputs to the character and other universe manipulations
        universe.step(
            false,
            time::DeadlineStd::At(Instant::now() + Duration::from_millis(1)),
        );
    }
});
