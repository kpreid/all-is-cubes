// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for collision detection with [`Space`](crate::space::Space)s.

use std::collections::HashSet;

use cgmath::{EuclideanSpace as _, InnerSpace as _, Zero as _};

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

    debug_assert!(
        ray.direction.magnitude2() < super::body::VELOCITY_MAGNITUDE_LIMIT_SQUARED * 2.,
        "Attempting to collide_along_ray a very long distance: {:?}",
        ray
    );

    // Note: no `.within_grid()` because that would not work when the leading corner is
    // not within the grid. We could expand the grid slightly (while considering overflow
    // cases), but this would be an optimization which only affects the unusual case of
    // being out of bounds, so it's not worth doing unless we specifically expect to have
    // many bodies outside a space and occasionally inside.
    for ray_step in aab_raycast(aab, ray, false) {
        let offset_segment = ray.scale_direction(ray_step.t_distance() + POSITION_EPSILON);
        let step_aab = aab.translate(offset_segment.unit_endpoint().to_vec());
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

/// Given an [`Aab`] and a [`Ray`] such that the given face of
/// `aab.translate(segment.unit_endpoint().to_vec())`
/// lands almost but not quite on a unit cell boundary along the `plane` axis,
/// nudge the endpoint of the ray
/// infinitesimally so that it lands definitely beyond (or before if `backward`)
/// the cell boundary.
///
/// Note that the required `face` is the opposite of the face produced by a raycast.
/// (This may be confusing but we feel that it would be more confusing to use a face
/// other than the relevant face of the [`Aab`]).
pub(crate) fn nudge_on_ray(aab: Aab, segment: Ray, face: Face, backward: bool) -> Ray {
    if segment.direction.is_zero() || face == Face::Within {
        return segment;
    }

    let fc = aab
        .translate(segment.unit_endpoint().to_vec())
        .face_coordinate(face);
    // This is the depth by which the un-nudged face penetrates the plane, which we are going to subtract.
    let penetration_depth = fc - fc.round();
    debug_assert!(penetration_depth.abs() <= 0.5);

    // This is the length of the direction vector projected along the face normal — thus, the reciprocal of the scale factor by which to adjust the distance.
    let direction_projection = face.normal_vector().dot(segment.direction);

    debug_assert!(direction_projection != 0.0);

    // `translation` is how far we want to translate the AAB linearly along the face normal.
    let epsilon_nudge = if backward {
        -POSITION_EPSILON
    } else {
        POSITION_EPSILON
    };
    let translation = epsilon_nudge - penetration_depth;

    if false {
        dbg!(
            face,
            aab.face_coordinate(face),
            penetration_depth,
            translation
        );
    }
    segment.scale_direction(1.0 + translation / direction_projection)
}

#[cfg(test)]
mod tests {
    use crate::math::GridCoordinate;
    use crate::space::Grid;

    use super::*;
    use rand::{Rng, SeedableRng as _};

    #[test]
    fn nudge_random_test() {
        let moving_aab = Aab::new(-0.345, 0.489, -0.118, 0.0325, -0.319, 0.2252);
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
        for case_number in 0..1000 {
            // Prepare test data
            let ray = Ray::new(
                Aab::new(-2., 2., -2., 2., -2., 2.).random_point(&mut rng),
                Aab::new(-1., 1., -1., 1., -1., 1.)
                    .random_point(&mut rng)
                    .to_vec(),
            );
            let step = aab_raycast(moving_aab, ray, false)
                .nth(rng.gen_range(1..10))
                .unwrap();
            let axis = step.face().axis_number().expect("should have an axis");
            let segment = ray.scale_direction(step.t_distance()); // TODO: this should be a function? Should aab_raycast return a special step type with these features?
            let unnudged_aab = moving_aab.translate(segment.unit_endpoint().to_vec());
            let face_to_nudge = step.face().opposite();

            for backward in [true, false] {
                // Perform nudge
                let adjusted_segment = nudge_on_ray(moving_aab, segment, face_to_nudge, backward);
                let nudged_aab = moving_aab.translate(adjusted_segment.unit_endpoint().to_vec());

                // Check that the nudge was not too big
                let nudge_length = (adjusted_segment.direction - segment.direction).magnitude();
                assert!(
                    nudge_length <= 0.001,
                    "nudge moved from {:?} to {:?}, distance of {}",
                    segment,
                    adjusted_segment,
                    nudge_length
                );

                // Check that the nudge was not backwards of the ray
                assert!(
                    adjusted_segment.direction.dot(segment.direction) >= 0.0,
                    "nudge went backwards to {:?} from starting position {:?}",
                    adjusted_segment,
                    segment,
                );

                // Check expected position properties
                let position_on_axis = nudged_aab.face_coordinate(face_to_nudge);
                let fraction = position_on_axis - position_on_axis.round();
                assert!(
                    fraction.abs() > POSITION_EPSILON / 2.,
                    "{:?} coord {:?} shouldn't be at surface",
                    face_to_nudge,
                    position_on_axis,
                );
                assert!(
                    fraction.abs() < POSITION_EPSILON * 2.,
                    "{:?} coord {:?} shouldn't be so large",
                    face_to_nudge,
                    position_on_axis,
                );

                let enclosing = nudged_aab.round_up_to_grid();
                if backward {
                    let expected_enclosing = Grid::single_cube(
                        segment.unit_endpoint().map(|p| p.floor() as GridCoordinate),
                    );
                    assert_eq!(
                        enclosing.axis_range(axis),
                        expected_enclosing.axis_range(axis),
                        "\ncase {:?}\nface {:?}\nsegment {:?}\nunnudged_aab {:#?}\nnudged {:#?}\n",
                        case_number,
                        face_to_nudge,
                        segment,
                        unnudged_aab,
                        nudged_aab,
                    );
                } else {
                    // TODO check the forward cases
                }
            }
        }
    }
}
