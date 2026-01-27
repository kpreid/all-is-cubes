#![no_main]

use all_is_cubes::character::Character;
use all_is_cubes::euclid::Vector3D;
use all_is_cubes::math::{self, FreeCoordinate, NotNan, ps64};
use all_is_cubes::space::Space;
use all_is_cubes::time;
use all_is_cubes::universe::{ArbitraryWithUniverse, StrongHandle, Universe};

use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: (
    [NotNan<FreeCoordinate>; 3],
    [NotNan<FreeCoordinate>; 3],
    ArbitraryWithUniverse<Space>,
)| {
    let (
        position,
        velocity,
        ArbitraryWithUniverse {
            contents: space, ..
        },
    ) = input;

    let interesting_bounds_aab = space.bounds().to_free().expand(ps64(10.0));

    // TODO: write a proper Arbitrary impl on a wrapper
    let position: math::FreePoint = position.map(NotNan::into_inner).into();
    let velocity: Vector3D<_, _> = velocity.map(NotNan::into_inner).into();
    if space.physics().gravity.map(NotNan::into_inner).length() > 100. {
        return;
    }

    // println!(
    //     "{} {}",
    //     position.refmt(&ConciseDebug),
    //     velocity.refmt(&ConciseDebug)
    // );

    // TODO(ecs): When `Character` and `Body` have been disentangled, make this work solely on
    // `Body`.

    let mut universe = Universe::new();
    let space_handle = universe.insert_anonymous(space);
    let mut character = Character::spawn_default(universe.read_ticket(), space_handle).unwrap();
    character.body.set_position(position);
    character.body.add_velocity(velocity);
    let character = StrongHandle::from(universe.insert_anonymous(character));

    for i in 0..5000 {
        if !interesting_bounds_aab
            .contains(character.read(universe.read_ticket()).unwrap().body().position())
        {
            // Flying out of bounds is not interesting.
            return;
        }

        // dbg!((i, character.body.position));
        universe.step(false, time::Deadline::Whenever);
        let body_info = character
            .read(universe.read_ticket())
            .unwrap()
            .physics()
            .unwrap()
            .last_step_info;
        // dbg!(info);

        // Check for no push out, but not on the first step, which might have been due to initial
        // placement in a bad location.
        if i != 0 {
            assert_eq!(
                body_info.expect("should be making body steps").push_out,
                None,
                "triggered push_out"
            );
        }
    }
});
