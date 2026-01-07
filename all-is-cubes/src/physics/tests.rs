use alloc::boxed::Box;
use itertools::Itertools;
use std::collections::VecDeque;

use bevy_ecs::prelude as ecs;
use euclid::{Vector3D, point3, vec3};
use ordered_float::NotNan;
use rand::prelude::IndexedRandom as _;
use rand::{Rng as _, SeedableRng as _};

use crate::block::{
    AIR,
    Resolution::{self, R2},
};
use crate::character::ParentSpace;
use crate::content::{make_slab, make_some_blocks};
use crate::math::{Aab, Cube, CubeFace, Face7, FreeCoordinate, GridAab, chebyshev_length};
use crate::physics::step::PhysicsOutputs;
use crate::physics::{
    Body, BodyStepDetails, Contact, ContactSet, POSITION_EPSILON, Velocity,
    step::VELOCITY_MAGNITUDE_LIMIT,
};
use crate::space::{Space, SpacePhysics};
use crate::time;
use crate::universe::{self, Universe};

// -------------------------------------------------------------------------------------------------

/// Sets up the environment to test a physics body.
pub(crate) struct BodyTester {
    universe: Box<Universe>,

    /// We need this `StrongHandle` because GC doesn't look at our body entity.
    /// (If we start using non-`Character` bodies for non-test purposes, we’ll need to fix that.)
    #[expect(dead_code)]
    space: universe::StrongHandle<Space>,

    body_entity: ecs::Entity,
}

impl BodyTester {
    pub fn new(space: Space, body: Body, schedule: time::TickSchedule) -> Self {
        let mut universe = Universe::new();
        universe.set_clock(time::Clock::new(schedule, 0));
        let space_handle = universe.insert_anonymous(space);
        let body_entity = universe.ecs_mut().spawn((body, ParentSpace(space_handle.clone()))).id();
        BodyTester {
            universe,
            space: universe::StrongHandle::new(space_handle),
            body_entity,
        }
    }

    pub fn step(&mut self) -> (ContactSet, BodyStepDetails) {
        self.universe.step(false, time::Deadline::Whenever);

        let outputs = self
            .universe
            .ecs()
            .get::<PhysicsOutputs>(self.body_entity)
            .expect("PhysicsOutputs missing");

        (
            outputs.colliding_cubes.clone(),
            outputs.last_step_info.expect("last_step_info missing"),
        )
    }

    pub fn body(&self) -> &Body {
        self.universe.ecs().get::<Body>(self.body_entity).unwrap()
    }

    pub fn body_mut(&mut self) -> &mut Body {
        self.universe.ecs_mut().get_mut::<Body>(self.body_entity).unwrap().into_inner()
    }
}

// -------------------------------------------------------------------------------------------------

fn test_body() -> Body {
    Body::new_minimal([0., 2., 0.], Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5))
}

#[rstest::rstest]
fn freefall(#[values(false, true)] gravity: bool) {
    let mut tester = BodyTester::new(
        {
            let mut space = Space::empty_positive(1, 1, 1);
            space.set_physics(SpacePhysics {
                gravity: vec3(0, -20, 0).map(NotNan::from),
                ..SpacePhysics::default()
            });
            space
        },
        {
            let mut body = test_body();
            body.set_velocity(vec3(2.0, 0.0, 0.0));
            body.flying = !gravity;
            body
        },
        time::TickSchedule::per_second(4),
    );

    tester.step();
    let p1 = tester.body().position();
    tester.step();
    let p2 = tester.body().position();

    assert_eq!(
        [p1, p2],
        [
            point3(0.5, if gravity { 0.75 } else { 2.0 }, 0.0),
            point3(1.0, if gravity { -1.75 } else { 2.0 }, 0.0)
        ]
    );
}

#[test]
fn paused_does_not_move() {
    let mut tester = BodyTester::new(
        Space::empty_positive(1, 1, 1),
        {
            let mut body = test_body();
            body.set_velocity(vec3(2.0, 0.0, 0.0));
            body.flying = false;
            body
        },
        time::TickSchedule::per_second(4),
    );

    tester.universe.step(true, time::Deadline::Whenever);

    assert_eq!(tester.body().position(), test_body().position());
}

#[test]
fn falling_collision() {
    let [block] = make_some_blocks();
    let mut tester = BodyTester::new(
        {
            Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
                .filled_with(block.clone())
                .build()
        },
        {
            let mut body = test_body();
            body.set_velocity(vec3(2.0, 0.0, 0.0));
            body.flying = false;
            body
        },
        time::TickSchedule::per_second(1),
    );

    let (contacts, _info) = tester.step();

    assert_eq!(tester.body().position().x, 2.0);
    assert_eq!(tester.body().position().z, 0.0);
    assert!(
        (tester.body().position().y - 1.5).abs() < 1e-6,
        "{:?}",
        tester.body().position()
    );
    assert_eq!(
        contacts.into_iter().collect_vec(),
        vec![Contact::Block(CubeFace::new([0, 0, 0], Face7::PY))]
    );
}

#[test]
fn falling_collision_partial_block() {
    const RES: Resolution = Resolution::R4;
    let x_velocity = 0.2;

    let u = &mut Universe::new();
    let block = make_slab(u, RES.halve().unwrap().into(), RES);

    let mut tester = BodyTester::new(
        Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .read_ticket(u.read_ticket())
            .filled_with(block.clone())
            .build(),
        {
            let mut body = test_body();
            body.set_velocity(vec3(x_velocity, 0.0, 0.0));
            body.flying = false;
            body
        },
        time::TickSchedule::per_second(1),
    );

    let (contacts, _info) = dbg!(tester.step());

    dbg!(tester.body().collision_box_abs());

    assert_eq!(tester.body().position().x, x_velocity);
    assert_eq!(tester.body().position().z, 0.0);
    assert!(
        (tester.body().position().y - 1.0).abs() < 1e-6,
        "not touching surface on first step{:?}",
        tester.body().position()
    );
    assert!(
        matches!(
            contacts.iter().exactly_one().unwrap(),
            Contact::Voxel {
                cube: Cube::ORIGIN,
                resolution: RES,
                voxel: CubeFace {
                    cube: _,
                    face: Face7::PY
                },
            }
        ),
        "contact not as expected {contacts:?}",
    );

    // Remove horizontal velocity, then let time proceed and see if any falling through happens.
    {
        let new_velocity = Vector3D {
            x: 0.0,
            ..tester.body().velocity()
        };
        tester.body_mut().set_velocity(new_velocity);
    }

    for t in 1..=1000 {
        eprintln!("--- step {t}");
        tester.step();
        assert!(
            (tester.body().position().y - 1.0).abs() < 1e-6,
            "not touching surface on step {:?}: {:?}",
            t,
            tester.body().position()
        );
    }
}

#[test]
fn push_out_simple() {
    let [block] = make_some_blocks();
    let mut tester = BodyTester::new(
        Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .filled_with(block.clone())
            .build(),
        {
            let mut body = test_body();
            body.set_position(point3(1.25, 0.5, 0.5)); // intersection of 0.25
            body.set_velocity(Vector3D::zero());
            body.flying = true;
            body
        },
        time::TickSchedule::per_second(1),
    );

    let (_contacts, info) = dbg!(tester.step());
    dbg!(info);

    assert_eq!(
        tester.body().position(),
        point3(1.5 + POSITION_EPSILON, 0.5, 0.5)
    );
    assert_eq!(tester.body().velocity(), Vector3D::zero());
    // TODO: push out should create and report contacts just like normal collision
    // assert_eq!(contacts, vec![CubeFace::new((0, 0, 0), Face7::PY)]);
}

#[test]
#[ignore = "TODO: enable this test when push_out() works as intended"]
fn push_out_voxels() {
    let u = &mut Universe::new();
    let block = make_slab(u, 1, R2);
    let mut tester = BodyTester::new(
        Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .read_ticket(u.read_ticket())
            .filled_with(block.clone())
            .build(),
        {
            let mut body = test_body();
            body.set_position(point3(0.5, 0.75, 0.5)); // intersection of 0.25 in the Y axis
            body.flying = true;
            body
        },
        time::TickSchedule::per_second(1),
    );

    dbg!(tester.step());

    assert_eq!(
        tester.body().position(),
        point3(0.5, 1.0 + POSITION_EPSILON, 0.5)
    );
    assert_eq!(tester.body().velocity(), Vector3D::zero());
    // TODO: push out should create and report contacts just like normal collision
    // assert_eq!(contacts, vec![CubeFace::new((0, 0, 0), Face7::PY)]);
}

#[test]
fn no_passing_through_blocks() {
    // Construct cubical box. TODO: worldgen utilities for this?
    let [wall_block] = make_some_blocks();
    let one_test = |velocity: Vector3D<FreeCoordinate, Velocity>| {
        print!("Velocity {velocity:?}... ");
        let start = point3(0.5, 0.5, 0.5);
        let box_radius = 0.375; // use an exact float to minimize complications

        let mut tester = BodyTester::new(
            Space::builder(GridAab::from_lower_size([-1, -1, -1], [3, 3, 3]))
                .build_and_mutate(|m| {
                    m.fill_all_uniform(&wall_block).unwrap();
                    m.set([0, 0, 0], &AIR).unwrap();
                    Ok(())
                })
                .unwrap(),
            {
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
                body
            },
            time::TickSchedule::per_second(60),
        );

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
            tester.body_mut().set_velocity(velocity);
            position_history.push_front(tester.body().position());
            tester.step();

            let distance_from_start = chebyshev_length(tester.body().position() - start);
            assert!(
                distance_from_start < 0.5,
                "escaped to {:?}",
                tester.body().position()
            );
            if position_history.contains(&tester.body().position()) {
                // Reached steady state. Ish.
                break;
            }
            position_history.truncate(10);
        }
        println!(
            "{iterations:?} iterations to {:?}",
            tester.body().position()
        );
        let distance_from_start = chebyshev_length(tester.body().position() - start);
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
            rng.random_range(0.04..=1.) * [-1., 1.].choose(&mut rng).unwrap()
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
    let schedule = time::TickSchedule::per_second(2);
    let mut tester = BodyTester::new(
        Space::empty_positive(1, 1, 1),
        {
            let mut body = test_body();
            body.set_position(point3(0., 0., 0.));
            body.set_velocity(vec3(1e7, 0., 0.));
            body.flying = true;
            body
        },
        schedule,
    );

    tester.step();

    // Velocity is capped and *then* applied to position
    assert_eq!(
        tester.body().velocity(),
        vec3(VELOCITY_MAGNITUDE_LIMIT, 0., 0.)
    );
    assert_eq!(
        tester.body().position(),
        point3(
            schedule.delta_t().as_secs_f64() * VELOCITY_MAGNITUDE_LIMIT,
            0.,
            0.
        )
    );
}

// TODO: test collision more
// TODO: test having all 3 move segments
