#![no_main]
extern crate all_is_cubes;

use all_is_cubes::apps::Tick;
use all_is_cubes::cgmath::{EuclideanSpace, InnerSpace, Point3, Vector3};
use all_is_cubes::content::UniverseTemplate;
use all_is_cubes::math::FreeCoordinate;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|input: ([FreeCoordinate; 3], [FreeCoordinate; 3])| {
    let position: Point3<FreeCoordinate> = input.0.into();
    let velocity: Vector3<FreeCoordinate> = input.1.into();
    // TODO: write a proper Arbitrary impl on a wrapper
    if !position.to_vec().magnitude2().is_finite() {
        return;
    }
    if !velocity.magnitude2().is_finite() {
        return;
    }

    // Use DemoCity as a complex world to exercise physics in.
    // TODO: Instead, take fuzzer input data to build a just-complex-enough one.
    let universe = UniverseTemplate::DemoCity.build().unwrap();
    let character_ref = universe.get_default_character().unwrap();
    character_ref
        .try_modify(|character_mut| {
            character_mut.body.position = position;
            character_mut.body.velocity = velocity;

            for i in 0..5000 {
                let (info, _tx) = character_mut.step(None, Tick::arbitrary());

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
        })
        .unwrap();
});
