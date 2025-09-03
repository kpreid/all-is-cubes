#![no_main]

use all_is_cubes::math::{Cube, Face7, FaceMap, GridAab};
use all_is_cubes::raycast;

const PRINT: bool = false;

#[derive(Debug, arbitrary::Arbitrary)]
struct Input {
    use_bounds: bool,
    include_exit: bool,
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

libfuzzer_sys::fuzz_target!(|input: Input| {
    if PRINT {
        println!("Inputs: {input:?}");
    }

    let Input {
        use_bounds,
        include_exit,
        kind,
    } = input;

    let bounds = use_bounds.then(|| GridAab::from_lower_upper([-10, -20, -30], [10, 20, 30]));

    match kind {
        Kind::General { origin, direction } => {
            let mut raycaster = raycast::Raycaster::new(origin, direction);
            if let Some(bounds) = bounds {
                raycaster = raycaster.within(bounds, include_exit)
            }
            exercise(raycaster, bounds, include_exit)
        }
        Kind::AxisAligned { origin, direction } => {
            let mut raycaster = raycast::AaRay::new(origin, direction).cast();
            if let Some(bounds) = bounds {
                raycaster = raycaster.within(bounds, include_exit)
            }
            exercise(raycaster, bounds, include_exit)
        }
    }
});

fn exercise(
    raycaster: impl Iterator<Item = raycast::RaycastStep> + std::fmt::Debug,
    bounds: Option<GridAab>,
    expect_exit: bool,
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
        if let Some(bounds) = bounds
            && !bounds.contains_cube(step.cube_ahead())
            && !(expect_exit
                && bounds
                    .expand(FaceMap::splat(1))
                    .contains_cube(step.cube_ahead()))
        {
            panic!("out of bounds {bounds:?}: step {step:#?}");
        }

        // TODO: Check that intersection_point() is good (see existing unit test).
    }
}
