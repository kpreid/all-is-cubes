#![no_main]

use all_is_cubes::block::Resolution::R4;
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
            let ray = raycast::Ray::new(origin, direction);
            let mut raycaster = ray.cast();
            if let Some(bounds) = bounds {
                raycaster = raycaster.within(bounds, include_exit)
            }
            exercise(ray, raycaster, bounds, include_exit)
        }
        Kind::AxisAligned { origin, direction } => {
            let ray = raycast::AaRay::new(origin, direction);
            let mut raycaster = ray.cast();
            if let Some(bounds) = bounds {
                raycaster = raycaster.within(bounds, include_exit)
            }
            exercise(ray.into(), raycaster, bounds, include_exit)
        }
    }
});

fn exercise(
    ray: raycast::Ray,
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
            && !(expect_exit && bounds.expand(FaceMap::splat(1)).contains_cube(step.cube_ahead()))
        {
            panic!("out of bounds {bounds:?}: step {step:#?}");
        }

        // TODO: Check that intersection_point() is good (see existing unit test).

        // TODO: This test doesn't work, but it should.
        // Fixing it will require a new approach to `recursive_raycast` more aware of what should
        // be guaranteed based on the outer raycast step.
        if false {
            // Check `recursive_raycast` hits at least one cube ... if the direction vector is not
            // so wildly out of scale that the arithmetic might not work anyway.
            // (We consider it okay to return nothing in those edge cases.)
            if (1e-50..1e50).contains(&ray.direction.length()) {
                let (mut r_raycaster, _r_ray) =
                    step.recursive_raycast(ray, R4, GridAab::for_block(R4));
                assert_ne!(r_raycaster.next(), None);
            }
        }
    }
}
