use super::*;
use crate::math::{Aab, FaceMap};
use alloc::vec::Vec;
use euclid::{Point3D, point3, vec3};
use rand::SeedableRng as _;

/// Alternative to [`RaycastStep`] which contains optional data so partial assertions
/// can be written, and contains 'final' values rather than ones used for calculation.
#[derive(Clone, Copy, Debug, PartialEq)]
struct TestStep {
    cube: Cube,
    face: Face7,
    t_distance: Option<FreeCoordinate>,
    intersection_point: Option<FreePoint>,
}

impl TestStep {
    fn from(step: &RaycastStep, ray: Ray) -> Self {
        Self {
            cube: step.cube_ahead(),
            face: step.face(),
            t_distance: Some(step.t_distance()),
            intersection_point: Some(step.intersection_point(ray)),
        }
    }

    fn matches(self, step: &RaycastStep) -> bool {
        self.cube == step.cube_ahead()
            && self.face == step.face()
            && self.t_distance.is_none_or(|td| step.t_distance() == td)
    }
}

#[track_caller]
fn assert_steps_option<T: IntoIterator<Item = Option<TestStep>>>(r: &mut Raycaster, steps: T) {
    for (i, expected_step) in steps.into_iter().enumerate() {
        let r_backup = r.clone(); // save for diagnostics
        let actual_step = r.next();
        let matches = match (expected_step, actual_step) {
            (Some(e), Some(a)) if e.matches(&a) => true,
            (None, None) => true,
            _ => false,
        };
        if !matches {
            panic!(
                "step {i}\n\
                    expected: {expected_step:?}\n\
                    found:    {actual_ts:?}\n\
                    actual:   {actual_step:?}\n\
                    before: {r_backup:?}\n\
                    after:  {r:?}\n",
                actual_ts = actual_step.map(|s| TestStep::from(&s, r.state.param.ray)),
            );
        }
    }
}
#[track_caller]
fn assert_steps<T: IntoIterator<Item = TestStep>>(r: &mut Raycaster, steps: T) {
    assert_steps_option(r, steps.into_iter().map(Some))
}
#[track_caller]
fn assert_only_one_step(r: &mut Raycaster, step: TestStep) {
    assert_steps_option(r, vec![Some(step), None, None]);
}

#[track_caller]
fn assert_no_steps(mut raycaster: Raycaster) {
    assert_steps(&mut raycaster, vec![]);
}

/// Helper to construct steps
fn step(
    x: GridCoordinate,
    y: GridCoordinate,
    z: GridCoordinate,
    face: Face7,
    t_distance: FreeCoordinate,
) -> TestStep {
    TestStep {
        cube: Cube::new(x, y, z),
        face,
        t_distance: Some(t_distance),
        intersection_point: None,
    }
}

#[test]
fn simple_almost_1d() {
    // Testing all six directions to ensure the axis selection logic picks the correct one
    assert_steps(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), vec3(0.01, 0.0001, 0.0001)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(11, 20, 30, Face7::NX, 50.0),
            step(12, 20, 30, Face7::NX, 150.0),
        ],
    );
    assert_steps(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), vec3(-0.01, 0.0001, 0.0001)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(9, 20, 30, Face7::PX, 50.0),
            step(8, 20, 30, Face7::PX, 150.0),
        ],
    );
    assert_steps(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), vec3(0.0001, 0.01, 0.0001)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(10, 21, 30, Face7::NY, 50.0),
            step(10, 22, 30, Face7::NY, 150.0),
        ],
    );
    assert_steps(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), vec3(0.0001, -0.01, 0.0001)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(10, 19, 30, Face7::PY, 50.0),
            step(10, 18, 30, Face7::PY, 150.0),
        ],
    );
    assert_steps(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), vec3(0.0001, 0.0001, 0.01)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(10, 20, 31, Face7::NZ, 50.0),
            step(10, 20, 32, Face7::NZ, 150.0),
        ],
    );
    assert_steps(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), vec3(0.0001, 0.0001, -0.01)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(10, 20, 29, Face7::PZ, 50.0),
            step(10, 20, 28, Face7::PZ, 150.0),
        ],
    );
}

#[test]
fn simple_exactly_1d() {
    // Not testing all six directions because other tests cover that
    assert_steps(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), vec3(0.01, 0.0, 0.0)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(11, 20, 30, Face7::NX, 50.0),
            step(12, 20, 30, Face7::NX, 150.0),
        ],
    );
    assert_steps(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), vec3(-0.01, 0.0, 0.0)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(9, 20, 30, Face7::PX, 50.0),
            step(8, 20, 30, Face7::PX, 150.0),
        ],
    );
}

#[test]
fn direction_zero_produces_origin_cube_only() {
    assert_only_one_step(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), Vector3D::zero()),
        step(10, 20, 30, Face7::Within, 0.0),
    );
}

#[test]
fn direction_negative_zero_produces_origin_cube_only() {
    assert_only_one_step(
        &mut Raycaster::new(point3(10.5, 20.5, 30.5), Vector3D::zero()),
        step(10, 20, 30, Face7::Within, 0.0),
    );
}

#[test]
fn direction_nan_produces_origin_cube_only() {
    assert_only_one_step(
        &mut Raycaster::new(
            point3(10.5, 20.5, 30.5),
            vec3(1.0, 2.0, FreeCoordinate::NAN),
        ),
        step(10, 20, 30, Face7::Within, 0.0),
    );
}

/// Test the case where the starting point is exactly on a cube border
/// and the ray is mostly aligned with that axis.
#[test]
fn start_on_cube_edge_parallel() {
    // Positive origin, positive direction
    assert_steps(
        &mut Raycaster::new(point3(10.0, 20.5, 30.5), vec3(2.0, 0.1, 0.1)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(11, 20, 30, Face7::NX, 0.5),
            step(12, 20, 30, Face7::NX, 1.0),
        ],
    );
    // Positive origin, negative direction
    assert_steps(
        &mut Raycaster::new(point3(10.0, 20.5, 30.5), vec3(-2.0, 0.1, 0.1)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(9, 20, 30, Face7::PX, 0.5),
            step(8, 20, 30, Face7::PX, 1.0),
        ],
    );
    // Negative origin, positive direction
    assert_steps(
        &mut Raycaster::new(point3(-10.0, 20.5, 30.5), vec3(2.0, 0.1, 0.1)),
        vec![
            step(-10, 20, 30, Face7::Within, 0.0),
            step(-9, 20, 30, Face7::NX, 0.5),
            step(-8, 20, 30, Face7::NX, 1.0),
        ],
    );
    // Negative origin, negative direction
    assert_steps(
        &mut Raycaster::new(point3(-10.0, 20.5, 30.5), vec3(-2.0, 0.1, 0.1)),
        vec![
            step(-10, 20, 30, Face7::Within, 0.0),
            step(-11, 20, 30, Face7::PX, 0.5),
            step(-12, 20, 30, Face7::PX, 1.0),
        ],
    );
}

/// Test the case where the starting point is exactly on a cube border
/// and the ray is mostly perpendicular to that axis.
#[test]
fn start_on_cube_edge_perpendicular() {
    // Positive origin, positive direction
    assert_steps(
        &mut Raycaster::new(point3(10.0, 20.5, 30.5), vec3(0.125, 1.0, 0.0)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(10, 21, 30, Face7::NY, 0.5),
            step(10, 22, 30, Face7::NY, 1.5),
        ],
    );
    // Positive origin, negative direction
    assert_steps(
        &mut Raycaster::new(point3(10.0, 20.5, 30.5), vec3(-0.125, -1.0, 0.0)),
        vec![
            step(10, 20, 30, Face7::Within, 0.0),
            step(10, 19, 30, Face7::PY, 0.5),
            step(10, 18, 30, Face7::PY, 1.5),
        ],
    );
    // Negative origin, positive direction
    assert_steps(
        &mut Raycaster::new(point3(-10.0, -20.5, 30.5), vec3(0.125, 1.0, 0.0)),
        vec![
            step(-10, -21, 30, Face7::Within, 0.0),
            step(-10, -20, 30, Face7::NY, 0.5),
            step(-10, -19, 30, Face7::NY, 1.5),
        ],
    );
    // Negative origin, negative direction
    assert_steps(
        &mut Raycaster::new(point3(-10.0, -20.5, 30.5), vec3(-0.125, -1.0, 0.0)),
        vec![
            step(-10, -21, 30, Face7::Within, 0.0),
            step(-10, -22, 30, Face7::PY, 0.5),
            step(-10, -23, 30, Face7::PY, 1.5),
        ],
    );
}

#[test]
fn start_outside_of_integer_range() {
    assert_no_steps(Raycaster::new(
        [0.5, 0.5, FreeCoordinate::from(GridCoordinate::MAX) + 0.5],
        [0.0, 0.0, -1.0],
    ));
    assert_no_steps(Raycaster::new(
        [0.5, 0.5, FreeCoordinate::from(GridCoordinate::MAX) + 1.5],
        [0.0, 0.0, -1.0],
    ));
    assert_no_steps(Raycaster::new(
        [0.5, 0.5, FreeCoordinate::from(GridCoordinate::MIN) - 0.5],
        [0.0, 0.0, -1.0],
    ));
    assert_no_steps(Raycaster::new(
        [0.5, 0.5, FreeCoordinate::from(GridCoordinate::MIN) - 1.5],
        [0.0, 0.0, -1.0],
    ));
}

/// Regression test (found by fuzzing) for being outside of integer
/// range while also using `within()`.
#[test]
fn start_outside_of_integer_range_with_bounds() {
    let bounds = GridAab::from_lower_size([0, 0, 0], [10, 10, 10]);
    assert_no_steps(Raycaster::new(point3(0., 1e303, 0.), vec3(0., -1e303, 0.)).within(bounds));
}

/// If we start inside the range of `GridCoordinate`s and exit, this should
/// stop (as if we were `within()` the entire space) rather than panicking.
#[test]
fn exiting_integer_range() {
    // `MAX` is excluded because `Cube::grid_aab()` would panic in that case,
    // and such a cube exists in zero `GridAab`s, so it is not very useful.
    let highest = GridCoordinate::MAX - 1;
    assert_steps_option(
        &mut Raycaster::new(
            [0.5, 0.5, FreeCoordinate::from(highest) + 0.5],
            [0.0, 0.0, 1.0],
        ),
        vec![Some(step(0, 0, highest, Face7::Within, 0.0)), None],
    );
    assert_steps_option(
        &mut Raycaster::new(
            [0.5, 0.5, FreeCoordinate::from(GridCoordinate::MIN) + 0.5],
            [0.0, 0.0, -1.0],
        ),
        vec![
            Some(step(0, 0, GridCoordinate::MIN, Face7::Within, 0.0)),
            None,
        ],
    );
}

#[test]
fn within_bounds() {
    // Ray oriented diagonally on the -X side of bounds that are short on the X axis.
    let mut r = Raycaster::new(point3(0.0, -0.25, -0.5), vec3(1.0, 1.0, 1.0))
        .within(GridAab::from_lower_size([2, -10, -10], [2, 20, 20]));
    assert_steps_option(
        &mut r,
        vec![
            Some(step(2, 1, 1, Face7::NX, 2.0)),
            Some(step(2, 2, 1, Face7::NY, 2.25)),
            Some(step(2, 2, 2, Face7::NZ, 2.5)),
            Some(step(3, 2, 2, Face7::NX, 3.0)),
            Some(step(3, 3, 2, Face7::NY, 3.25)),
            Some(step(3, 3, 3, Face7::NZ, 3.5)),
            None,
        ],
    );

    // Verify that extra next()s don't modify the state and potentially cause overflow if continued.
    let mut r2 = r.clone();
    r2.next();
    // Compare the Debug strings, since the state is otherwise private.
    assert_eq!(format!("{r:?}"), format!("{r2:?}"));
}

/// An example of an axis-aligned ray that wasn't working.
#[test]
fn regression_test_1() {
    assert_steps(
        &mut Raycaster::new(
            point3(4.833333333333334, 4.666666666666666, -3.0),
            vec3(0.0, 0.0, 10.0),
        ),
        vec![
            step(4, 4, -3, Face7::Within, 0.0),
            step(4, 4, -2, Face7::NZ, 0.1),
            step(4, 4, -1, Face7::NZ, 0.2),
        ],
    );
}

/// `within()` wasn't working for axis-aligned rays that don't intersect the world,
/// which should produce zero steps.
#[test]
fn regression_test_2() {
    let bounds = GridAab::from_lower_size(GridPoint::new(0, 0, 0), [10, 10, 10]);
    assert_steps_option(
        &mut Raycaster::new(
            point3(18.166666666666668, 4.666666666666666, -3.0),
            vec3(0.0, 0.0, 16.0),
        )
        .within(bounds),
        vec![None],
    );
}

/// Regression test from a fuzz test case where `fast_forward` would perform poorly,
/// requiring a large number of steps. Note that this test is not intended to
/// detect the poor performance, but to confirm that the _fix_ doesn't change the
/// behavior.
#[test]
fn regression_long_distance_fast_forward() {
    assert_steps(
        &mut Raycaster::new(
            point3(
                6.749300603672869e-67,
                6.750109954921438e-67,
                -85891558.96000093,
            ),
            vec3(1.1036366354256313e-305, 0.0, 8589152896.000092),
        )
        .within(GridAab::from_lower_upper([-10, -20, -30], [10, 20, 30])),
        vec![step(0, 0, -30, Face7::NZ, 0.010000000000000002)],
    );
}

#[test]
fn intersection_point_positive_face() {
    let ray = Ray::new([0.5, 0.5, 0.5], [-1.0, 0.0, 0.0]);
    let mut raycaster = ray.cast();
    let mut next = || raycaster.next().unwrap().intersection_point(ray);

    assert_eq!(next(), Point3D::new(0.5, 0.5, 0.5));
    assert_eq!(next(), Point3D::new(0.0, 0.5, 0.5));
    assert_eq!(next(), Point3D::new(-1.0, 0.5, 0.5));
}

#[test]
fn intersection_point_random_test() {
    // A one-cube box, so that all possible rays should either intersect
    // exactly this cube, or none at all.
    let bounds = GridAab::from_lower_size([0, 0, 0], [1, 1, 1]);
    let ray_origins: Aab = bounds.expand(FaceMap::splat(1)).to_free();

    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
    for _ in 0..1000 {
        let ray = Ray::new(
            ray_origins.random_point(&mut rng),
            Aab::new(-1., 1., -1., 1., -1., 1.)
                .random_point(&mut rng)
                .to_vector(),
        );
        let steps: Vec<RaycastStep> = ray.cast().within(bounds).collect();
        match &steps[..] {
            [] => {}
            [step] => {
                let point = step.intersection_point(ray);
                let mut surfaces = 0;
                let mut interiors = 0;
                for axis in Axis::ALL {
                    if point[axis] == 0.0 || point[axis] == 1.0 {
                        surfaces += 1;
                    } else if point[axis] > 0.0 && point[axis] < 1.0 {
                        interiors += 1;
                    }
                }
                assert!(
                    surfaces + interiors == 3 && (surfaces > 0 || step.face() == Face7::Within),
                    "ray {ray:?} produced invalid point {point:?}",
                );
            }
            steps => {
                panic!("Raycaster should not have produced multiple steps {steps:?}",);
            }
        }
    }
}

#[test]
fn recursive_simple() {
    let ray = Ray::new([-1., 10.125, 0.125], [1.0, 0.0, 0.0]);
    let resolution = Resolution::R4;

    let outer_step = ray.cast().nth(1).unwrap();
    assert_eq!(outer_step.cube_ahead(), Cube::new(0, 10, 0));

    let (mut inner_raycaster, inner_ray) =
        outer_step.recursive_raycast(ray, resolution, GridAab::for_block(resolution));

    assert_eq!(inner_ray, Ray::new([-4., 0.5, 0.5], [1.0, 0.0, 0.0]));
    assert_steps_option(
        &mut inner_raycaster,
        vec![
            Some(step(0, 0, 0, Face7::NX, 4.0)),
            Some(step(1, 0, 0, Face7::NX, 5.0)),
            Some(step(2, 0, 0, Face7::NX, 6.0)),
            Some(step(3, 0, 0, Face7::NX, 7.0)),
            None,
        ],
    );
}

#[test]
fn scale_to_integer_step_basics() {
    assert_eq!(scale_to_integer_step(1.25, 0.25), 3.0);
    assert_eq!(scale_to_integer_step(1.25, -0.25), 1.0);
    assert_eq!(scale_to_integer_step(-1.25, 0.25), 1.0);
    assert_eq!(scale_to_integer_step(-1.25, -0.25), 3.0);
}

#[test]
fn scale_to_integer_step_positive_and_negative_zero() {
    assert_eq!(scale_to_integer_step(1.5, 0.0), FreeCoordinate::INFINITY);
    assert_eq!(scale_to_integer_step(1.5, -0.0), FreeCoordinate::INFINITY);
    assert_eq!(scale_to_integer_step(0.0, 0.0), FreeCoordinate::INFINITY);
    assert_eq!(scale_to_integer_step(0.0, -0.0), FreeCoordinate::INFINITY);
    assert_eq!(scale_to_integer_step(-0.0, 0.0), FreeCoordinate::INFINITY);
}

#[test]
fn scale_to_integer_step_starting_on_integer() {
    assert_eq!(scale_to_integer_step(3.0, 0.5), 2.0);
    assert_eq!(scale_to_integer_step(3.0, -0.5), 2.0);
    assert_eq!(scale_to_integer_step(-3.0, 0.5), 2.0);
    assert_eq!(scale_to_integer_step(-3.0, -0.5), 2.0);
}

#[test]
fn scale_to_integer_step_nan_propagation() {
    assert!(scale_to_integer_step(1.5, FreeCoordinate::NAN).is_nan());
    assert!(scale_to_integer_step(FreeCoordinate::NAN, 1.0).is_nan());
    assert!(scale_to_integer_step(FreeCoordinate::NAN, 0.0).is_nan());
}

/// Edge case found by fuzzing.
#[test]
fn scale_to_integer_step_small_offset() {
    assert_eq!(
        scale_to_integer_step(-0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000019656826074480345, 0.0),
        FreeCoordinate::INFINITY,
    );
}
