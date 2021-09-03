// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Continuously moving objects and collision.

use crate::math::FreeCoordinate;

mod body;
pub use body::*;
mod collision;
pub use collision::*;

/// Close-but-not-intersecting objects are set to this separation.
pub(crate) const POSITION_EPSILON: FreeCoordinate = 1e-6 * 1e-6;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::apps::Tick;
    use crate::block::{Block, BlockCollision, AIR};
    use crate::content::make_some_blocks;
    use crate::math::{Aab, CubeFace, Face};
    use crate::space::{Grid, Space, SpacePhysics};
    use crate::universe::Universe;
    use cgmath::{InnerSpace as _, Point3, Vector3, Zero as _};
    use rand::prelude::SliceRandom as _;
    use rand::{Rng as _, SeedableRng as _};
    use std::collections::VecDeque;

    fn collision_noop(_: Contact) {}

    fn test_body() -> Body {
        Body {
            flying: false,
            noclip: false,
            ..Body::new_minimal((0., 2., 0.), Aab::new(-0.5, 0.5, -0.5, 0.5, -0.5, 0.5))
        }
    }

    #[test]
    fn freefall_no_gravity() {
        let mut body = Body {
            velocity: Vector3::new(2.0, 0.0, 0.0),
            flying: true,
            ..test_body()
        };
        body.step(Tick::from_seconds(1.5), None, collision_noop);
        assert_eq!(body.position, Point3::new(3.0, 2.0, 0.0));
        body.step(Tick::from_seconds(1.5), None, collision_noop);
        assert_eq!(body.position, Point3::new(6.0, 2.0, 0.0));
    }

    #[test]
    fn freefall_with_gravity() {
        let mut space = Space::empty_positive(1, 1, 1);
        space.set_physics(SpacePhysics {
            gravity: Vector3::new(notnan!(0.0), notnan!(-20.0), notnan!(0.0)),
            ..SpacePhysics::default()
        });
        let mut body = Body {
            velocity: Vector3::new(2.0, 0.0, 0.0),
            flying: false,
            ..test_body()
        };
        body.step(Tick::from_seconds(1.5), Some(&space), collision_noop);
        assert_eq!(body.position, Point3::new(3.0, -43.0, 0.0));
        body.step(Tick::from_seconds(1.5), Some(&space), collision_noop);
        assert_eq!(body.position, Point3::new(6.0, -133.0, 0.0));
    }

    #[test]
    fn paused_does_not_move() {
        let mut body = Body {
            velocity: Vector3::new(2.0, 0.0, 0.0),
            flying: false,
            ..test_body()
        };
        body.step(Tick::from_seconds(1.5).pause(), None, collision_noop);
        assert_eq!(body.position, test_body().position);
    }

    #[test]
    fn falling_collision() {
        let [block] = make_some_blocks();
        let mut space = Space::empty_positive(1, 1, 1);
        space.set((0, 0, 0), &block).unwrap();
        let mut body = Body {
            velocity: Vector3::new(2.0, 0.0, 0.0),
            flying: false,
            ..test_body()
        };

        let mut contacts = Vec::new();
        body.step(Tick::from_seconds(1.0), Some(&space), |c| contacts.push(c));

        assert_eq!(body.position.x, 2.0);
        assert_eq!(body.position.z, 0.0);
        assert!((body.position.y - 1.5).abs() < 1e-6, "{:?}", body.position);
        assert_eq!(contacts, vec![CubeFace::new((0, 0, 0), Face::PY)]);
    }

    #[test]
    #[ignore] // TODO: Voxel-level collision is not yet implemented; this test will not pass
    fn falling_collision_partial_block() {
        let u = &mut Universe::new();
        let [voxel] = make_some_blocks();
        let block = Block::builder()
            .collision(BlockCollision::Hard)
            .voxels_fn(u, 2, |p| if p.y > 0 { &AIR } else { &voxel })
            .unwrap()
            .build();

        let mut space = Space::empty_positive(1, 1, 1);
        space.set((0, 0, 0), &block).unwrap();
        let mut body = Body {
            velocity: Vector3::new(2.0, 0.0, 0.0),
            flying: false,
            ..test_body()
        };

        let mut contacts = Vec::new();
        body.step(Tick::from_seconds(1.0), Some(&space), |c| contacts.push(c));

        assert_eq!(body.position.x, 2.0);
        assert_eq!(body.position.z, 0.0);
        assert!((body.position.y - 1.0).abs() < 1e-6, "{:?}", body.position);
        assert_eq!(contacts, vec![CubeFace::new((0, 0, 0), Face::PY)]);
    }

    #[test]
    fn push_out_simple() {
        let [block] = make_some_blocks();
        let mut space = Space::empty_positive(1, 1, 1);
        space.set((0, 0, 0), &block).unwrap();
        let mut body = Body {
            position: Point3::new(1.25, 0.5, 0.5), // intersection of 0.25
            velocity: Vector3::zero(),
            flying: true,
            ..test_body()
        };

        let mut contacts = Vec::new();
        let info = body.step(Tick::from_seconds(1.0), Some(&space), |c| contacts.push(c));
        dbg!(info);

        assert_eq!(body.position, Point3::new(1.5 + POSITION_EPSILON, 0.5, 0.5));
        assert_eq!(body.velocity, Vector3::zero());
        // TODO: push out should create report contacts just like normal collision
        // assert_eq!(contacts, vec![CubeFace::new((0, 0, 0), Face::PY)]);
    }

    #[test]
    fn no_passing_through_blocks() {
        // Construct cubical box. TODO: worldgen utilities for this?
        let mut space = Space::empty(Grid::new((-1, -1, -1), (3, 3, 3)));
        let [wall_block] = make_some_blocks();
        space.fill_uniform(space.grid(), &wall_block).unwrap();
        space.set([0, 0, 0], &AIR).unwrap();

        let one_test = |velocity: Vector3<FreeCoordinate>| {
            print!("Velocity {:?}... ", velocity);
            let start = Point3::new(0.5, 0.5, 0.5);
            let box_radius = 0.375; // use an exact float to minimize complications
            let mut body = Body {
                flying: true,
                position: start,
                collision_box: Aab::new(
                    -box_radius,
                    box_radius,
                    -box_radius,
                    box_radius,
                    -box_radius,
                    box_radius,
                ),
                ..test_body()
            };
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
                body.velocity = velocity;
                position_history.push_front(body.position);
                body.step(Tick::from_seconds(1.0 / 60.0), Some(&space), |_contact| {});

                let distance_from_start = max_norm(body.position - start);
                assert!(distance_from_start < 0.5, "escaped to {:?}", body.position);
                if position_history.contains(&body.position) {
                    // Reached steady state. Ish.
                    break;
                }
                position_history.truncate(10);
            }
            println!("{:?} iterations to {:?}", iterations, body.position);
            let distance_from_start = max_norm(body.position - start);
            assert!(
                distance_from_start > 0.09,
                "didn't move away from origin: {}",
                distance_from_start
            );
        };

        for case in (&[[1.0, 1.0, 1.0], [1.0, 0.1, 0.1], [0.1, -0.1, -0.047]])
            .iter()
            .copied()
            .map(Vector3::from)
        {
            for &variant in &[case, -case] {
                one_test(variant);
            }
        }

        // Randomly generate test cases
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(1);
        for _ in 0..100 {
            let random_velocity = Vector3::<f32 /* dummy */>::zero().map(|_| {
                // Generate vector components which are not too close to zero
                // to finish the test promptly
                rng.gen_range(0.04..=1.) * [-1., 1.].choose(&mut rng).unwrap()
            });
            if random_velocity.magnitude() < 0.05 {
                // Too slow
                continue;
            }
            one_test(random_velocity);
        }
    }

    #[test]
    fn position_nan() {
        let space = Space::empty_positive(1, 1, 1);
        let mut body = Body {
            position: Point3::new(FreeCoordinate::NAN, 0., 0.),
            velocity: Vector3::new(1., 0., 0.),
            ..test_body()
        };
        body.step(Tick::from_seconds(2.0), Some(&space), collision_noop);
        // TODO: We would like to have some recovery strategy.
        // For now, this is just a "doesn't panic" test.
    }

    #[test]
    fn velocity_nan() {
        let space = Space::empty_positive(1, 1, 1);
        let mut body = Body {
            position: Point3::new(1., 0., 0.),
            velocity: Vector3::new(1., FreeCoordinate::NAN, 0.),
            ..test_body()
        };
        body.step(Tick::from_seconds(2.0), Some(&space), collision_noop);

        // Velocity is zeroed and position is unchanged.
        assert_eq!(body.velocity, Vector3::new(0., 0., 0.));
        assert_eq!(body.position, Point3::new(1., 0., 0.));
    }

    #[test]
    fn velocity_limit() {
        let mut body = Body {
            position: Point3::new(0., 0., 0.),
            velocity: Vector3::new(1e7, 0., 0.),
            ..test_body()
        };
        body.step(Tick::from_seconds(2.0), None, collision_noop);

        // Velocity is capped and *then* applied to position
        assert_eq!(body.velocity, Vector3::new(1e5, 0., 0.));
        assert_eq!(body.position, Point3::new(2e5, 0., 0.));
    }

    /// Takes the maximum length on all coordinate axes; all points forming a cube
    /// centered on the origin will have the same value for this norm.
    ///
    /// https://en.wikipedia.org/wiki/Uniform_norm
    fn max_norm<S: num_traits::real::Real>(v: Vector3<S>) -> S {
        v[0].abs().max(v[1].abs()).max(v[2].abs())
    }

    // TODO: test collision more
    // TODO: test having all 3 move segments
}
