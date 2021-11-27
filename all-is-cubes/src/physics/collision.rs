// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for collision detection with [`Space`](crate::space::Space)s.

use std::collections::HashSet;
use std::fmt;

use cgmath::{EuclideanSpace as _, InnerSpace as _, Point3, Vector3, Zero as _};

use super::POSITION_EPSILON;
use crate::block::{BlockCollision, EvaluatedBlock, Evoxel, Resolution};
use crate::math::{Aab, CubeFace, Face, FreeCoordinate, Geometry, GridCoordinate, GridPoint, Rgba};
use crate::raycast::{Ray, Raycaster};
use crate::space::{GridArray, Space};
use crate::util::{ConciseDebug, CustomFormat, MapExtend};

/// An individual collision contact; something in a [`Space`] that a moving [`Aab`]
/// collided with.
///
/// This type is designed to be comparable/hashable to deduplicate contacts.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_enums)] // any change will probably be breaking anyway
pub enum Contact {
    /// Contact with a fully solid block; the [`CubeFace`] specifies the block position
    /// and the side of it that was collided with (hence also the contact normal).
    Block(CubeFace),
    Voxel {
        /// The “outer” cube in the [`Space`].
        cube: GridPoint,
        /// The voxel resolution of the block; that is, the factor by which voxel
        /// coordinates are smaller than `cube` coordinates.
        resolution: Resolution,
        /// The voxel position in the block (each coordinate lies between `0` and
        /// `resolution - 1`) and the face of that voxel collided with, which is also
        /// the contact normal.
        voxel: CubeFace,
    },
}

impl Contact {
    pub fn cube(&self) -> GridPoint {
        match *self {
            Contact::Block(CubeFace { cube, .. }) => cube,
            Contact::Voxel { cube, .. } => cube,
        }
    }

    /// Returns the contact normal: the direction in which the colliding box should be
    /// pushed back.
    ///
    /// Note that this may be equal to [`Face::Within`] in case the box was already
    /// intersecting before any movement.
    pub fn normal(&self) -> Face {
        match *self {
            Contact::Block(CubeFace { face, .. }) => face,
            Contact::Voxel {
                voxel: CubeFace { face, .. },
                ..
            } => face,
        }
    }

    pub fn resolution(&self) -> Resolution {
        match *self {
            Contact::Block(_) => 1,
            Contact::Voxel { resolution, .. } => resolution,
        }
    }

    /// Return a copy where the contact normal is replaced with [`Face::Within`].
    fn without_normal(&self) -> Self {
        let mut result = *self;
        match result {
            Contact::Block(CubeFace { ref mut face, .. }) => *face = Face::Within,
            Contact::Voxel {
                voxel: CubeFace { ref mut face, .. },
                ..
            } => *face = Face::Within,
        }
        result
    }
}

impl fmt::Debug for Contact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Block(CubeFace { cube, face }) => {
                write!(f, "{:?} of {}", face, cube.custom_format(ConciseDebug))
            }
            Self::Voxel {
                cube,
                resolution,
                voxel: CubeFace { cube: voxel, face },
            } => write!(
                f,
                "{:?} of {} {}/{}",
                face,
                cube.custom_format(ConciseDebug),
                voxel.custom_format(ConciseDebug),
                resolution
            ),
        }
    }
}

impl Geometry for Contact {
    type Coord = GridCoordinate;

    fn translate(mut self, offset: impl Into<Vector3<Self::Coord>>) -> Self {
        let offset = offset.into();
        match &mut self {
            Contact::Block(CubeFace { mut cube, .. }) => cube += offset,
            Contact::Voxel { mut cube, .. } => cube += offset,
        }
        self
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<(Point3<FreeCoordinate>, Option<Rgba>)>,
    {
        match self {
            Contact::Block(cube_face) => cube_face.wireframe_points(output),
            Contact::Voxel {
                cube,
                resolution,
                voxel,
            } => {
                let resolution: FreeCoordinate = (*resolution).into();
                voxel.wireframe_points(&mut MapExtend::new(
                    output,
                    |mut vert: (Point3<FreeCoordinate>, Option<Rgba>)| {
                        vert.0 = vert.0 / resolution + cube.to_vec().map(FreeCoordinate::from);
                        vert
                    },
                ))
            }
        }
    }
}

/// Result of [`collide_along_ray`] which specifies a collision point possibly inside the cube.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CollisionRayEnd {
    /// Non-colliding length of the provided ray.
    pub t_distance: FreeCoordinate,
    pub contact: Contact,
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
    ignore_already_colliding: bool,
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
        let offset_segment = nudge_on_ray(
            aab,
            ray.scale_direction(ray_step.t_distance()),
            ray_step.face().opposite(),
            1,
            false,
        );
        let step_aab = aab.translate(offset_segment.unit_endpoint().to_vec());
        if ray_step.t_distance() >= 1.0 {
            // Movement is unobstructed in this timestep.
            break;
        }

        // Loop over all the cubes that our AAB is just now intersecting and check if
        // any of them are solid, and if so, how far into their volume is a hit.
        // TODO: Useful optimization for large AABs would be skipping all the interior
        // cubes that must have been detected in the _previous_ step.
        let mut something_hit = None;
        for cube in step_aab.round_up_to_grid().interior_iter() {
            let cell = space.get_cell(cube);
            let full_cube_end = CollisionRayEnd {
                t_distance: ray_step.t_distance(),
                contact: Contact::Block(CubeFace {
                    cube,
                    face: ray_step.face(),
                }),
            };
            let found_end = match Sp::collision(cell) {
                BlockCollision::None => {
                    // No collision for this block
                    continue;
                }
                BlockCollision::Hard => full_cube_end,
                BlockCollision::Recur => {
                    if let Some(found_end) = Sp::recurse(
                        full_cube_end.clone(),
                        aab,
                        ray,
                        cell,
                        ignore_already_colliding && ray_step.face() != Face::Within,
                    ) {
                        found_end
                    } else {
                        // No collision
                        continue;
                    }
                }
            };

            if ignore_already_colliding {
                if found_end.contact.normal() == Face::Within {
                    // If we start intersecting a block, we are allowed to leave it; pretend
                    // it doesn't exist. (Ideally, `push_out()` would have fixed this, but
                    // maybe there's no clear direction.)
                    already_colliding.insert(found_end.contact);
                    collision_callback(found_end.contact);
                    continue;
                } else if already_colliding.contains(&found_end.contact.without_normal()) {
                    continue;
                }
            }

            // TODO: We need to buffer contacts instead of calling this callback, in case something else is closer
            collision_callback(found_end.contact);

            let nearest_so_far = match something_hit {
                Some(CollisionRayEnd { t_distance, .. }) => t_distance,
                None => FreeCoordinate::INFINITY,
            };
            if found_end.t_distance < nearest_so_far {
                something_hit = Some(found_end);
            }
        }

        // Now that we've found _all_ the contacts for this step, report the collision.
        if let Some(end) = something_hit {
            return Some(end);
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
    // TODO: rework interfaces to avoid allocating a vector
    // TODO: don't throw out the contact details
    let mut points = Vec::new();
    collide_along_ray(
        space,
        Ray::new([0., 0., 0.], [0., 0., 0.]),
        aab,
        |contact| {
            points.push(contact.cube());
        },
        false,
    );
    points.into_iter()
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

    /// TODO: document
    fn get_voxels(cell: &Self::Cell) -> Option<(Resolution, &GridArray<Evoxel>)>;

    /// TODO: document
    ///
    /// * `entry_end`: the endpoint we would return if the recursion stopped here — entering the given cell.
    fn recurse(
        entry_end: CollisionRayEnd,
        aab: Aab,
        ray: Ray,
        cell: &Self::Cell,
        ignore_already_colliding: bool,
    ) -> Option<CollisionRayEnd>;
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

    #[inline]
    fn get_voxels(evaluated: &EvaluatedBlock) -> Option<(Resolution, &GridArray<Evoxel>)> {
        evaluated.voxels.as_ref().map(|v| (evaluated.resolution, v))
    }

    #[inline]
    fn recurse(
        entry_end: CollisionRayEnd,
        space_aab: Aab,
        space_ray: Ray,
        evaluated: &EvaluatedBlock,
        ignore_already_colliding: bool,
    ) -> Option<CollisionRayEnd> {
        match &evaluated.voxels {
            // Plain non-recursive collision
            None => Some(entry_end),
            Some(voxels) => {
                let cube_translation = entry_end
                    .contact
                    .cube()
                    .to_vec()
                    .map(|s| -FreeCoordinate::from(s));
                let scale = FreeCoordinate::from(evaluated.resolution);
                // Transform our original AAB and ray so that it is in the coordinate system of the block voxels.
                // Note: aab is not translated since it's relative to the ray anyway.
                let voxel_aab = space_aab.scale(scale);
                let voxel_ray = space_ray.translate(cube_translation).scale_all(scale);
                if let Some(hit_voxel) = collide_along_ray(
                    voxels,
                    voxel_ray,
                    voxel_aab,
                    |_| {},
                    ignore_already_colliding,
                ) {
                    let CollisionRayEnd {
                        t_distance: voxel_t_distance,
                        contact,
                    } = hit_voxel;

                    match contact {
                        Contact::Block(voxel) => Some(CollisionRayEnd {
                            t_distance: voxel_t_distance,
                            contact: Contact::Voxel {
                                cube: entry_end.contact.cube(),
                                resolution: evaluated.resolution,
                                voxel,
                            },
                        }),
                        Contact::Voxel { .. } => panic!("encountered 3-level voxel recursion"),
                    }
                } else {
                    // Didn't hit anything within this block.
                    None
                }
            }
        }
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

    #[inline]
    fn get_voxels(_cell: &Self::Cell) -> Option<(Resolution, &GridArray<Evoxel>)> {
        None
    }
    #[inline(always)]
    fn recurse(
        entry_end: CollisionRayEnd,
        _aab: Aab,
        _local_ray: Ray,
        _cell: &Self::Cell,
        _ignore_already_colliding: bool,
    ) -> Option<CollisionRayEnd> {
        Some(entry_end)
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
pub(crate) fn nudge_on_ray(
    aab: Aab,
    segment: Ray,
    face: Face,
    subdivision: Resolution,
    backward: bool,
) -> Ray {
    if segment.direction.is_zero() || face == Face::Within {
        return segment;
    }

    // This is the depth by which the un-nudged face penetrates the plane, which we are going to subtract.
    let penetration_depth = {
        let subdivision = FreeCoordinate::from(subdivision);
        let fc_scaled = aab
            .translate(segment.unit_endpoint().to_vec())
            .face_coordinate(face)
            * subdivision;
        (fc_scaled - fc_scaled.round()) / subdivision
    };

    // This is the length of the direction vector projected along the face normal — thus, the reciprocal of the scale factor by which to adjust the distance.
    let direction_projection = face.dot(segment.direction);

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
    use crate::block::{Block, AIR};
    use crate::content::{make_slab, make_some_blocks};
    use crate::math::GridCoordinate;
    use crate::raytracer::print_space;
    use crate::space::Grid;
    use crate::universe::Universe;

    use super::*;
    use rand::{Rng, SeedableRng as _};
    #[test]
    fn collide_along_ray_with_opaque_block() {
        collide_along_ray_tester(
            1.5,
            |_u| {
                let [block] = make_some_blocks();
                [AIR, block]
            },
            Some(CollisionRayEnd {
                t_distance: 0.25, // half of a unit cube, quarter of a ray with magnitude 2
                contact: Contact::Block(CubeFace::new([1, 0, 0], Face::PY)),
            }),
        );
    }

    #[test]
    fn collide_along_ray_recursive_from_outside() {
        collide_along_ray_tester(
            1.5,
            |u| [AIR, make_slab(u, 1, 2)],
            Some(CollisionRayEnd {
                t_distance: 0.5, // half of a ray with magnitude 2
                contact: Contact::Voxel {
                    cube: GridPoint::new(1, 0, 0),
                    resolution: 2,
                    // TODO: the voxel reported here is arbitrary, so this test is fragile
                    voxel: CubeFace::new([0, 0, 0], Face::PY),
                },
            }),
        );
    }

    #[test]
    fn collide_along_ray_recursive_from_inside() {
        collide_along_ray_tester(
            0.75,
            |u| [AIR, make_slab(u, 1, 2)],
            Some(CollisionRayEnd {
                t_distance: 0.125,
                contact: Contact::Voxel {
                    cube: GridPoint::new(1, 0, 0),
                    resolution: 2,
                    // TODO: the voxel reported here is arbitrary, so this test is fragile
                    voxel: CubeFace::new([0, 0, 0], Face::PY),
                },
            }),
        );
    }

    /// Check that colliding against two recursive blocks correctly picks the taller one,
    /// in either ordering.
    #[test]
    fn collide_along_ray_two_recursive() {
        collide_along_ray_tester(
            0.75,
            |u| [make_slab(u, 1, 3), make_slab(u, 1, 2)],
            Some(CollisionRayEnd {
                t_distance: 0.125,
                contact: Contact::Voxel {
                    cube: GridPoint::new(1, 0, 0), // second of 2 blocks is taller
                    resolution: 2,
                    voxel: CubeFace::new([0, 0, 0], Face::PY),
                },
            }),
        );
        collide_along_ray_tester(
            0.75,
            // Opposite ordering of the other test
            |u| [make_slab(u, 1, 2), make_slab(u, 1, 3)],
            Some(CollisionRayEnd {
                t_distance: 0.125,
                contact: Contact::Voxel {
                    cube: GridPoint::new(0, 0, 0), // first of 2 blocks is taller
                    resolution: 2,
                    voxel: CubeFace::new([1, 0, 0], Face::PY),
                },
            }),
        );
    }

    fn collide_along_ray_tester(
        initial_y: FreeCoordinate,
        block_gen: fn(&mut Universe) -> [Block; 2],
        expected_end: Option<CollisionRayEnd>,
    ) {
        let u = &mut Universe::new();
        let blocks = block_gen(u);
        let mut space = Space::empty_positive(2, 1, 1);
        space.set([0, 0, 0], &blocks[0]).unwrap();
        space.set([1, 0, 0], &blocks[1]).unwrap();
        print_space(&space, [1., 1., 1.]);

        // Set up to collide with the block such that the ray doesn't pass through it, to
        // make sure the right cube is returned.
        // The block is at [1, 0, 0] and we're "dropping" a block-shaped AAB onto it
        // with lower corner [0.5, 1.5, 0] so it should contact the "left" half of
        // the `block` after moving a distance of 0.5 plus whatever penetration
        // depth into a recursive block applies.
        let aab = Aab::from_cube(GridPoint::new(0, 0, 0));
        let ray = Ray::new([0.5, initial_y, 0.], [0., -2., 0.]);

        let result = collide_along_ray(&space, ray, aab, |_| {}, true);
        assert_eq!(result, expected_end);
    }

    /// Test reporting of being already inside a block at the start of the ray,
    /// particularly with a trailing AAB bigger than a block.
    #[test]
    fn already_colliding() {
        let mut u = Universe::new();
        let mut space = Space::empty_positive(2, 1, 1);
        let [block] = make_some_blocks();
        let half_block = make_slab(&mut u, 1, 2);
        space.set([0, 0, 0], &block).unwrap();
        space.set([1, 0, 0], &half_block).unwrap();

        let aab = Aab::from_lower_upper([-1., -1., -1.], [2., 1., 1.]);
        let ray = Ray::new([0.5, 0.0, 0.5], [1., 0., 0.]);

        let mut contacts = Vec::new();
        let result = collide_along_ray(&space, ray, aab, |c| contacts.push(c), true);
        assert_eq!(
            (result, contacts),
            (
                None,
                vec![
                    Contact::Block(CubeFace::new([0, 0, 0], Face::Within)),
                    Contact::Voxel {
                        cube: GridPoint::new(1, 0, 0),
                        resolution: 2,
                        // TODO: the voxel reported here is arbitrary, so this test is fragile
                        voxel: CubeFace::new([0, 0, 0], Face::Within),
                    }
                ]
            )
        )
    }

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
                // TODO: test non-1 resolution
                let adjusted_segment =
                    nudge_on_ray(moving_aab, segment, face_to_nudge, 1, backward);
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
