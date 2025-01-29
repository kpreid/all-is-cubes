use alloc::vec::Vec;
use std::collections::VecDeque;

use euclid::{point3, vec3, Vector3D};
use ordered_float::NotNan;
use rand::prelude::SliceRandom as _;
use rand::{Rng as _, SeedableRng as _};

use crate::block::{
    Resolution::{self, R2},
    AIR,
};
use crate::content::{make_slab, make_some_blocks};
use crate::math::{Aab, Cube, CubeFace, Face7, FreeCoordinate, GridAab};
use crate::physics::{Body, Contact, Velocity, POSITION_EPSILON, VELOCITY_MAGNITUDE_LIMIT};
use crate::space::{Space, SpacePhysics};
use crate::time::Tick;
use crate::universe::Universe;

fn collision_noop(_: Contact) {}

fn test_body() -> Body {
    Body::new_minimal([0., 2., 0.], Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5))
}

#[test]
fn freefall_no_gravity() {
    let mut body = test_body();
    body.set_velocity(vec3(2.0, 0.0, 0.0));
    body.flying = true;

    body.step(Tick::from_seconds(1.5), None, collision_noop);
    assert_eq!(body.position(), point3(3.0, 2.0, 0.0));
    body.step(Tick::from_seconds(1.5), None, collision_noop);
    assert_eq!(body.position(), point3(6.0, 2.0, 0.0));
}

#[test]
fn freefall_with_gravity() {
    let mut space = Space::empty_positive(1, 1, 1);
    space.set_physics(SpacePhysics {
        gravity: vec3(0, -20, 0).map(NotNan::from),
        ..SpacePhysics::default()
    });
    let mut body = test_body();
    body.set_velocity(vec3(2.0, 0.0, 0.0));
    body.flying = false;

    body.step(Tick::from_seconds(1.5), Some(&space), collision_noop);
    assert_eq!(body.position(), point3(3.0, -43.0, 0.0));
    body.step(Tick::from_seconds(1.5), Some(&space), collision_noop);
    assert_eq!(body.position(), point3(6.0, -133.0, 0.0));
}

#[test]
fn paused_does_not_move() {
    let mut body = test_body();
    body.set_velocity(vec3(2.0, 0.0, 0.0));
    body.flying = false;

    body.step(Tick::from_seconds(1.5).pause(), None, collision_noop);

    assert_eq!(body.position(), test_body().position());
}

#[test]
fn falling_collision() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(1, 1, 1);
    space.set([0, 0, 0], &block).unwrap();
    let mut body = test_body();
    body.set_velocity(vec3(2.0, 0.0, 0.0));
    body.flying = false;

    let mut contacts = Vec::new();
    body.step(Tick::from_seconds(1.0), Some(&space), |c| contacts.push(c));

    assert_eq!(body.position().x, 2.0);
    assert_eq!(body.position().z, 0.0);
    assert!(
        (body.position().y - 1.5).abs() < 1e-6,
        "{:?}",
        body.position()
    );
    assert_eq!(
        contacts,
        vec![Contact::Block(CubeFace::new([0, 0, 0], Face7::PY))]
    );
}

#[test]
fn falling_collision_partial_block() {
    const RES: Resolution = Resolution::R4;
    let x_velocity = 0.2;

    let u = &mut Universe::new();
    let block = make_slab(u, RES.halve().unwrap().into(), RES);

    let mut space = Space::empty_positive(1, 1, 1);
    space.set([0, 0, 0], &block).unwrap();
    let mut body = test_body();
    body.set_velocity(vec3(x_velocity, 0.0, 0.0));
    body.flying = false;

    let mut contacts = Vec::new();
    dbg!(body.step(Tick::from_seconds(1.0), Some(&space), |c| contacts.push(c)));

    dbg!(body.collision_box_abs());

    assert_eq!(body.position().x, x_velocity);
    assert_eq!(body.position().z, 0.0);
    assert!(
        (body.position().y - 1.0).abs() < 1e-6,
        "not touching surface on first step{:?}",
        body.position()
    );
    assert!(
        matches!(
            contacts[0],
            Contact::Voxel {
                cube: Cube::ORIGIN,
                resolution: RES,
                voxel: CubeFace {
                    cube: _,
                    face: Face7::PY
                },
            }
        ),
        "contact not as expected {:?}",
        contacts[0]
    );

    // Remove horizontal velocity, then let time proceed and see if any falling through happens.
    body.set_velocity(Vector3D {
        x: 0.0,
        ..body.velocity()
    });

    for t in 1..=1000 {
        eprintln!("--- step {t}");
        body.step(Tick::from_seconds(1.0), Some(&space), |_| {});
        assert!(
            (body.position().y - 1.0).abs() < 1e-6,
            "not touching surface on step {:?}: {:?}",
            t,
            body.position()
        );
    }
}

#[test]
fn push_out_simple() {
    let [block] = make_some_blocks();
    let mut space = Space::empty_positive(1, 1, 1);
    space.set([0, 0, 0], &block).unwrap();
    let mut body = test_body();
    body.set_position(point3(1.25, 0.5, 0.5)); // intersection of 0.25
    body.set_velocity(Vector3D::zero());
    body.flying = true;

    let mut contacts = Vec::new();
    let info = body.step(Tick::from_seconds(1.0), Some(&space), |c| contacts.push(c));
    dbg!(info);

    assert_eq!(body.position(), point3(1.5 + POSITION_EPSILON, 0.5, 0.5));
    assert_eq!(body.velocity(), Vector3D::zero());
    // TODO: push out should create and report contacts just like normal collision
    // assert_eq!(contacts, vec![CubeFace::new((0, 0, 0), Face7::PY)]);
}

#[test]
#[ignore = "TODO: enable this test when push_out() works as intended"]
fn push_out_voxels() {
    let u = &mut Universe::new();
    let block = make_slab(u, 1, R2);
    let mut space = Space::empty_positive(1, 1, 1);
    space.set([0, 0, 0], &block).unwrap();
    let mut body = test_body();
    body.set_position(point3(0.5, 0.75, 0.5)); // intersection of 0.25 in the Y axis
    body.flying = true;

    let mut contacts = Vec::new();
    let info = body.step(Tick::from_seconds(1.0), Some(&space), |c| contacts.push(c));
    dbg!(info);

    assert_eq!(body.position(), point3(0.5, 1.0 + POSITION_EPSILON, 0.5));
    assert_eq!(body.velocity(), Vector3D::zero());
    // TODO: push out should create and report contacts just like normal collision
    // assert_eq!(contacts, vec![CubeFace::new((0, 0, 0), Face7::PY)]);
}

#[test]
fn no_passing_through_blocks() {
    // Construct cubical box. TODO: worldgen utilities for this?
    let mut space = Space::empty(GridAab::from_lower_size([-1, -1, -1], [3, 3, 3]));
    let [wall_block] = make_some_blocks();
    space.fill_uniform(space.bounds(), &wall_block).unwrap();
    space.set([0, 0, 0], &AIR).unwrap();

    let one_test = |velocity: Vector3D<FreeCoordinate, Velocity>| {
        print!("Velocity {velocity:?}... ");
        let start = point3(0.5, 0.5, 0.5);
        let box_radius = 0.375; // use an exact float to minimize complications
        let mut body = Body::new_minimal(
            start,
            Aab::new(
                -box_radius,
                box_radius,
                -box_radius,
                box_radius,
                -box_radius,
                box_radius,
            ),
        );
        body.flying = true;

        let mut iterations = 0;
        let mut position_history = VecDeque::new();
        loop {
            iterations += 1;
            // TODO: We'd like to consider this a failure, but some cases get stuck in a loop of jitter.
            // assert!(
            //     iterations < 5000,
            //     "didn't terminate after {:?} iterations; reached {:#?}",
            //     iterations,
            //     position_history.iter().rev().collect::<Vec<_>>(),
            // );
            if iterations >= 5000 {
                return;
            }
            // Reset velocity every frame as an approximation of the effect of player input.
            body.set_velocity(velocity);
            position_history.push_front(body.position());
            body.step(Tick::from_seconds(1.0 / 60.0), Some(&space), |_contact| {});

            let distance_from_start = max_norm(body.position() - start);
            assert!(
                distance_from_start < 0.5,
                "escaped to {:?}",
                body.position()
            );
            if position_history.contains(&body.position()) {
                // Reached steady state. Ish.
                break;
            }
            position_history.truncate(10);
        }
        println!("{iterations:?} iterations to {:?}", body.position());
        let distance_from_start = max_norm(body.position() - start);
        assert!(
            distance_from_start > 0.09,
            "didn't move away from origin: {distance_from_start}"
        );
    };

    for case in [[1.0, 1.0, 1.0], [1.0, 0.1, 0.1], [0.1, -0.1, -0.047]] {
        let case = Vector3D::from(case);
        for &variant in &[case, -case] {
            one_test(variant);
        }
    }

    // Randomly generate test cases
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(1);
    for _ in 0..100 {
        let random_velocity = Vector3D::<f32 /* dummy */, _>::zero().map(|_| {
            // Generate vector components which are not too close to zero
            // to finish the test promptly
            rng.gen_range(0.04..=1.) * [-1., 1.].choose(&mut rng).unwrap()
        });
        if random_velocity.length() < 0.05 {
            // Too slow
            continue;
        }
        one_test(random_velocity);
    }
}

// TODO: Ignoring NaNs is an interim solution.
// Eventually they should be just prohibited by type, but with a solution more convenient to use
// than `Vector3D<NotNan<f64>>`.
#[test]
fn position_nan_ignored() {
    let mut body = test_body();
    body.set_position(point3(FreeCoordinate::NAN, 0., 0.));
    assert_eq!(body.position(), point3(0., 2., 0.));
}
#[test]
fn velocity_nan_ignored() {
    let mut body = test_body();
    body.set_velocity(vec3(1., FreeCoordinate::NAN, 0.));
    assert_eq!(body.velocity(), vec3(0., 0., 0.));
}

#[test]
fn velocity_limit() {
    let mut body = test_body();
    body.set_position(point3(0., 0., 0.));
    body.set_velocity(vec3(1e7, 0., 0.));

    body.step(Tick::from_seconds(2.0), None, collision_noop);

    // Velocity is capped and *then* applied to position
    assert_eq!(body.velocity(), vec3(VELOCITY_MAGNITUDE_LIMIT, 0., 0.));
    assert_eq!(
        body.position(),
        point3(2. * VELOCITY_MAGNITUDE_LIMIT, 0., 0.)
    );
}

/// Takes the maximum length on all coordinate axes; all points forming a cube
/// centered on the origin will have the same value for this norm.
///
/// See also <https://en.wikipedia.org/wiki/Uniform_norm>
fn max_norm<S: num_traits::real::Real, U>(v: Vector3D<S, U>) -> S {
    v.x.abs().max(v.y.abs()).max(v.z.abs())
}

// TODO: test collision more
// TODO: test having all 3 move segments
