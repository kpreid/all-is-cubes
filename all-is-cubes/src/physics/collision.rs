// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for collision detection with [`Space`](crate::space::Space)s.

use cgmath::{EuclideanSpace as _, Vector3, Zero as _};
use std::collections::HashSet;

use super::POSITION_EPSILON;
use crate::block::BlockCollision;
use crate::math::{Aab, CubeFace, Face, FreeCoordinate, Geometry as _, GridPoint};
use crate::raycast::{Ray, RaycastStep};
use crate::space::Space;

/// An individual collision contact.
pub type Contact = CubeFace;

/// Result of [`collide_along_ray`] which specifies a collision point possibly inside the cube.
#[derive(Debug)]
pub(crate) struct CollisionRayEnd {
    /// Non-colliding length of the provided ray.
    pub t_distance: FreeCoordinate,
    /// Cube in the provided space collided with, and the orientation of the surface collided with.
    pub cube_face: CubeFace,
}

pub(crate) fn collide_along_ray<CC>(
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
pub(crate) fn find_colliding_cubes(
    space: &Space,
    aab: Aab,
) -> impl Iterator<Item = GridPoint> + '_ {
    aab.round_up_to_grid().interior_iter().filter(move |&cube| {
        // TODO: change this from `==` to `match` to allow for expansion of the enum
        space.get_evaluated(cube).attributes.collision == BlockCollision::Hard
    })
}

/// Given a ray describing movement of the origin of an AAB, perform a raycast to find
/// the positions where the AAB moves into new cubes.
///
/// If `reversed` is true, find positions where it leaves cubes.
pub(crate) fn aab_raycast(
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
