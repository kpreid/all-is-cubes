#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate all_is_cubes;

use all_is_cubes::math::{Cube, Face7, GridAab};
use all_is_cubes::raycast;

const PRINT: bool = false;

#[derive(Debug, arbitrary::Arbitrary)]
struct Input {
    use_bounds: bool,
    kind: Kind,
}
#[derive(Debug, arbitrary::Arbitrary)]
enum Kind {
    General {
        origin: [f64; 3],
        direction: [f64; 3],
    },
    AxisAligned {
        origin: Cube,
        direction: Face7,
    },
}

fuzz_target!(|input: Input| {
    if PRINT {
        println!("Inputs: {input:?}");
    }

    let Input { use_bounds, kind } = input;

    let bounds = use_bounds.then(|| GridAab::from_lower_upper([-10, -20, -30], [10, 20, 30]));

    match kind {
        Kind::General { origin, direction } => {
            let mut raycaster = raycast::Raycaster::new(origin, direction);
            if let Some(bounds) = bounds {
                raycaster = raycaster.within(bounds)
            }
            exercise(raycaster, bounds)
        }
        Kind::AxisAligned { origin, direction } => {
            let mut raycaster = raycast::AxisAlignedRaycaster::new(origin, direction);
            if let Some(bounds) = bounds {
                raycaster = raycaster.within(bounds)
            }
            exercise(raycaster, bounds)
        }
    }
});

fn exercise(
    raycaster: impl Iterator<Item = raycast::RaycastStep> + std::fmt::Debug,
    bounds: Option<GridAab>,
) {
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
        if let Some(bounds) = bounds {
            assert!(bounds.contains_cube(step.cube_ahead()));
        }

        // TODO: Check that intersection_point() is good (see existing unit test).
    }
}
