#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::raycast::Raycaster;
use all_is_cubes::space::Grid;

fuzz_target!(|input: (bool, [f64; 3], [f64; 3])| {
    let (use_grid, origin, direction) = input;

    if use_grid {
        let grid = Grid::from_lower_upper([-10, -20, -30], [10, 20, 30]);
        for step in Raycaster::new(origin, direction).within_grid(grid).take(10) {
            assert!(grid.contains_cube(step.cube_ahead()));
        }
    } else {
        for _step in Raycaster::new(origin, direction).take(10) {
            // println!("{:?}", step);
        }
    }
});
