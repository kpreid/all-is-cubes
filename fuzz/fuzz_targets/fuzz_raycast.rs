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

    let mut prev_cube = None;
    let mut prev_t = 0.0;
    for step in raycaster.take(10) {
        if PRINT {
            println!("Step: {step:?}");
        }

        // Check that t_distance increases monotonically.
        assert!(step.t_distance() >= prev_t);
        prev_t = step.t_distance();

        // Check that steps are always adjacent to each other:
        // no jumping over diagonals or anything else.
        if let Some(prev_cube) = prev_cube {
            assert_eq!(step.cube_behind(), prev_cube);
        }
        prev_cube = Some(step.cube_ahead());

        // Check that within() is applied correctly.
        if use_bounds {
            assert!(bounds.contains_cube(step.cube_ahead()));
        }

        // TODO: Check that intersection_point() is good (see existing unit test).
    }
});
