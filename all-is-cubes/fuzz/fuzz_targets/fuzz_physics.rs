#![no_main]
extern crate all_is_cubes;

use all_is_cubes::apps::Tick;
use all_is_cubes::cgmath::{InnerSpace, Point3, Vector3};
use all_is_cubes::character::Character;
use all_is_cubes::math::{Aab, FreeCoordinate, NotNan};
use all_is_cubes::space::Space;
use all_is_cubes::universe::Universe;
use all_is_cubes::util::{ConciseDebug, CustomFormat};

use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: ([FreeCoordinate; 3], [FreeCoordinate; 3], Space)| {
    let (position, velocity, space) = input;

    let interesting_bounds_aab = Aab::from(space.grid()).enlarge(10.0);

    // TODO: write a proper Arbitrary impl on a wrapper
    let position: Point3<FreeCoordinate> = position.into();
    let velocity: Vector3<FreeCoordinate> = velocity.into();
    if space.physics().gravity.map(NotNan::into_inner).magnitude() > 100. {
        return;
    }

    println!(
        "{} {}",
        position.custom_format(ConciseDebug),
        velocity.custom_format(ConciseDebug)
    );

    let mut universe = Universe::new();
    let space_ref = universe.insert_anonymous(space);
    let mut character = Character::spawn_default(space_ref);
    character.body.position = position;
    character.body.velocity = velocity;
    for i in 0..5000 {
        if !interesting_bounds_aab.contains(character.body.position) {
            // Flying out of bounds is not interesting.
            return;
        }

        // dbg!((i, character.body.position));
        let (info, _tx) = character.step(None, Tick::arbitrary());
        // dbg!(info);

        // Check for no push out, but not on the first step, which might have been due to initial
        // placement in a bad location.
        if i != 0 {
            assert_eq!(
                info.expect("should be making body steps").push_out,
                None,
                "triggered push_out"
            );
        }
    }
});
