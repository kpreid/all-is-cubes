#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::math::Grid;
use all_is_cubes::raycast::Raycaster;

const PRINT: bool = false;

fuzz_target!(|input: (bool, [f64; 3], [f64; 3])| {
    if PRINT {
        println!("Inputs: {input:?}");
    }
    let (use_grid, origin, direction) = input;

    let grid = Grid::from_lower_upper([-10, -20, -30], [10, 20, 30]);
    let raycaster = if use_grid {
        Raycaster::new(origin, direction).within_grid(grid)
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
        if use_grid {
            assert!(grid.contains_cube(step.cube_ahead()));
        }
    }
});
