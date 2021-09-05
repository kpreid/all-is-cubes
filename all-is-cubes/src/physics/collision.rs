// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for collision detection with [`Space`](crate::space::Space)s.

use cgmath::EuclideanSpace as _;
use std::collections::HashSet;

use super::POSITION_EPSILON;
use crate::block::{BlockCollision, EvaluatedBlock, Evoxel};
use crate::math::{Aab, CubeFace, Face, FreeCoordinate, Geometry as _, GridPoint};
use crate::raycast::{Ray, Raycaster};
use crate::space::{GridArray, Space};

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

/// Move `aab`'s origin along the line segment from `ray.origin` to `ray.origin + ray.direction`,
/// and find the first point at which it collides with `space`'s collidable blocks.
///
/// The return value specifies the distance achieved and the normal (face) of the surface collided
///  with; if [`None`], then no obstacles were met along the full length of the line segment.
///
/// `collision_callback` is called once for each colliding cube — any one of them would have been
/// sufficient to stop the ray, but all are reported.
pub(crate) fn collide_along_ray<Sp, CC>(
    space: &Sp,
    ray: Ray,
    aab: Aab,
    mut collision_callback: CC,
) -> Option<CollisionRayEnd>
where
    Sp: CollisionSpace,
    CC: FnMut(Contact),
{
    let mut already_colliding: HashSet<Contact> = HashSet::new();

    // Note: no `.within_grid()` because that would not work when the leading corner is
    // not within the grid. We could expand the grid slightly (while considering overflow
    // cases), but this would be an optimization which only affects the unusual case of
    // being out of bounds, so it's not worth doing unless we specifically expect to have
    // many bodies outside a space and occasionally inside.
    for ray_step in aab_raycast(aab, ray, false) {
        let step_aab = aab.translate(
            ray.origin.to_vec() + ray.direction * (ray_step.t_distance() + POSITION_EPSILON),
        );
        if ray_step.t_distance() >= 1.0 {
            // Movement is unobstructed in this timestep.
            break;
        }
        if ray_step.face() == Face::Within {
            // If we are intersecting a block, we are allowed to leave it; pretend
            // it doesn't exist. (Ideally, `push_out()` would have fixed this, but
            // maybe there's no clear direction.)
            for box_cube in find_colliding_cubes(space, step_aab) {
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
        // TODO: Useful optimization for large AABs would be skipping all the interior
        // cubes that must have been detected in the _previous_ step.
        let mut hit_something = false;
        for box_cube in find_colliding_cubes(space, step_aab) {
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
pub(crate) fn find_colliding_cubes<Sp>(space: &Sp, aab: Aab) -> impl Iterator<Item = GridPoint> + '_
where
    Sp: CollisionSpace,
{
    aab.round_up_to_grid().interior_iter().filter(move |&cube| {
        // TODO: change this from `==` to `match` to allow for expansion of the enum
        Sp::collision(space.get_cell(cube)) == BlockCollision::Hard
    })
}

/// Abstraction over voxel arrays that the collision detection algorithm can use,
/// i.e. [`Space`] and `GridArray<Evoxel>`.
pub(crate) trait CollisionSpace {
    type Cell;

    /// Retrieve a cell value from the grid.
    /// Should return a non-colliding value if the point is out of bounds.
    fn get_cell(&self, cube: GridPoint) -> &Self::Cell;

    /// Retrieve a cell's collision behavior option.
    fn collision(cell: &Self::Cell) -> BlockCollision;
}

impl CollisionSpace for Space {
    type Cell = EvaluatedBlock;

    #[inline]
    fn get_cell(&self, cube: GridPoint) -> &Self::Cell {
        self.get_evaluated(cube)
    }

    #[inline]
    fn collision(cell: &Self::Cell) -> BlockCollision {
        cell.attributes.collision
    }
}

// TODO: This impl is not yet used; it will be used for voxel block collision.
// It exists now as a piece of incremental progress and to prove that `CollisionSpace`
// *can* be implemented for EvaluatedBlock.
impl CollisionSpace for GridArray<Evoxel> {
    type Cell = Evoxel;

    #[inline]
    fn get_cell(&self, cube: GridPoint) -> &Self::Cell {
        self.get(cube).unwrap_or(&Evoxel::AIR)
    }

    #[inline]
    fn collision(cell: &Self::Cell) -> BlockCollision {
        cell.collision
    }
}

/// Given a ray describing movement of the origin of an AAB, perform a raycast to find
/// the positions where the AAB moves into new cubes. The returned ray steps' `t_distance`
/// values report how far to move the AAB to meet the edge.
///
/// If `reversed` is true, find positions where it leaves cubes.
///
/// Note that due to the nature of floating-point arithmetic, it is uncertain whether the
/// resulting new AAB position will have the AAB's forward face land before, after, or
/// exactly on the boundary. The caller must compute an appropriate nudge (using TODO:
/// provide a function for this) to serve its needs.
pub(crate) fn aab_raycast(aab: Aab, origin_ray: Ray, reversed: bool) -> Raycaster {
    let leading_corner = aab.leading_corner(if reversed {
        -origin_ray.direction
    } else {
        origin_ray.direction
    });
    let leading_ray = origin_ray.translate(leading_corner);
    leading_ray.cast()
}
