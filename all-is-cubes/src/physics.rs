// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Continuously moving objects and collision.

use cgmath::{EuclideanSpace as _, Vector3, Zero as _};
use std::collections::HashSet;

use crate::block::BlockCollision;
use crate::math::{Aab, CubeFace, Face, FreeCoordinate, Geometry as _, GridPoint};
use crate::raycast::{Ray, RaycastStep};
use crate::space::Space;

mod body;
pub use body::*;
mod collision;
pub use collision::*;

/// Close-but-not-intersecting objects are set to this separation.
const POSITION_EPSILON: FreeCoordinate = 1e-6 * 1e-6;

/// An individual collision contact.
pub type Contact = CubeFace;

/// Result of [`collide_along_ray`] which specifies a collision point possibly inside the cube.
#[derive(Debug)]
struct CollisionRayEnd {
    /// Non-colliding length of the provided ray.
    t_distance: FreeCoordinate,
    /// Cube in the provided space collided with, and the orientation of the surface collided with.
    cube_face: CubeFace,
}

fn collide_along_ray<CC>(
    space: &Space,
    ray: Ray,
    aab: Aab,
    mut collision_callback: CC,
) -> Option<CollisionRayEnd>
where
    CC: FnMut(Contact),
{
    let mut already_colliding: HashSet<Contact> = HashSet::new();

    // Note: no `.within_grid()` because that would not work when the leading
    // corner is not within the grid.
    for (ray_step, step_aab) in aab_raycast(aab, ray, false) {
        if ray_step.t_distance() >= 1.0 {
            // Movement is unobstructed in this timestep.
            break;
        }
        if ray_step.face() == Face::Within {
            // If we are intersecting a block, we are allowed to leave it; pretend
            // it doesn't exist. (Ideally, `push_out()` would have fixed this, but
            // maybe there's no clear direction.)
            for box_cube in find_colliding_cubes(&space, step_aab) {
                let contact = Contact {
                    cube: box_cube,
                    face: ray_step.face(),
                };
                already_colliding.insert(contact);
            }
            continue;
        }

        // Loop over all the cubes that our AAB is just now intersecting and check if
        // any of them are solid.
        let mut hit_something = false;
        for box_cube in find_colliding_cubes(&space, step_aab) {
            let contact = Contact {
                cube: box_cube,
                face: ray_step.face(),
            };
            if !already_colliding.contains(&contact) {
                hit_something = true;
                collision_callback(contact);
            }
        }

        // Now that we've found _all_ the contacts, report the collision.
        if hit_something {
            return Some(CollisionRayEnd {
                t_distance: ray_step.t_distance(),
                cube_face: ray_step.cube_face(),
            });
        }
    }

    None
}

/// Returns an iterator over all blocks in `space` which intersect `aab`, accounting for
/// collision options.
fn find_colliding_cubes(space: &Space, aab: Aab) -> impl Iterator<Item = GridPoint> + '_ {
    aab.round_up_to_grid().interior_iter().filter(move |&cube| {
        // TODO: change this from `==` to `match` to allow for expansion of the enum
        space.get_evaluated(cube).attributes.collision == BlockCollision::Hard
    })
}

/// Given a ray describing movement of the origin of an AAB, perform a raycast to find
/// the positions where the AAB moves into new cubes.
///
/// If `reversed` is true, find positions where it leaves cubes.
fn aab_raycast(
    aab: Aab,
    origin_ray: Ray,
    reversed: bool,
) -> impl Iterator<Item = (RaycastStep, Aab)> {
    let (leading_corner, trailing_box) = aab.leading_corner_trailing_box(if reversed {
        -origin_ray.direction
    } else {
        origin_ray.direction
    });
    let leading_ray = origin_ray.translate(leading_corner);
    leading_ray.cast().map(move |step| {
        // TODO: The POSITION_EPSILON is a quick kludge to get a result that
        // *includes* the cubes we are advancing towards. Replace it with something
        // more precisely what we need.
        let nudge = if step.face() != Face::Within {
            origin_ray.direction * POSITION_EPSILON
        } else {
            Vector3::zero()
        };
        (
            step,
            trailing_box.translate((step.intersection_point(leading_ray) + nudge).to_vec()),
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::apps::Tick;
    use crate::block::AIR;
    use crate::content::make_some_blocks;
    use crate::space::{Grid, Space};
    use cgmath::{InnerSpace as _, Point3};
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
        let mut body = Body {
            velocity: Vector3::new(2.0, 0.0, 0.0),
            flying: false,
            ..test_body()
        };
        body.step(Tick::from_seconds(1.5), None, collision_noop);
        assert_eq!(body.position, Point3::new(3.0, -43.0, 0.0));
        body.step(Tick::from_seconds(1.5), None, collision_noop);
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

        assert_eq!(body.position, Point3::new(1.5, 0.5, 0.5));
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
