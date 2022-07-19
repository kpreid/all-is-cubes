#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::math::GridAab;
use all_is_cubes::raycast::Raycaster;

const PRINT: bool = false;

fuzz_target!(|input: (bool, [f64; 3], [f64; 3])| {
    if PRINT {
        println!("Inputs: {input:?}");
    }
    let (use_bounds, origin, direction) = input;

    let bounds = GridAab::from_lower_upper([-10, -20, -30], [10, 20, 30]);
    let raycaster = if use_bounds {
        Raycaster::new(origin, direction).within(bounds)
    } else {
        Raycaster::new(origin, direction)
    };

    if PRINT {
        dbg!(&raycaster);
    }

    for step in raycaster.take(10) {
        if PRINT {
            println!("Step: {step:?}");
        }
        if use_bounds {
            assert!(bounds.contains_cube(step.cube_ahead()));
        }
    }
});
