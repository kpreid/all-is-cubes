// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Algorithms for collision detection with [`Space`](crate::space::Space)s.

use std::collections::HashSet;

use cgmath::{EuclideanSpace as _, InnerSpace as _, Point3, Vector3, Zero as _};

use super::POSITION_EPSILON;
use crate::block::{BlockCollision, EvaluatedBlock, Evoxel, Resolution};
use crate::math::{Aab, CubeFace, Face, FreeCoordinate, Geometry, GridCoordinate, GridPoint, Rgba};
use crate::raycast::{Ray, Raycaster};
use crate::space::{GridArray, Space};
use crate::util::MapExtend;

/// An individual collision contact; something in a [`Space`] that a moving [`Aab`]
/// collided with.
///
/// This type is designed to be comparable/hashable to deduplicate contacts.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
        if ray_step.face() == Face::Within {
            // If we are intersecting a block, we are allowed to leave it; pretend
            // it doesn't exist. (Ideally, `push_out()` would have fixed this, but
            // maybe there's no clear direction.)
            for box_cube in find_colliding_cubes(space, step_aab) {
                let contact = Contact::Block(CubeFace {
                    cube: box_cube,
                    face: ray_step.face(),
                });
                already_colliding.insert(contact);
            }
            continue;
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
            match Sp::collision(cell) {
                // Note: This match must be in sync with find_colliding_cubes, the non-in-motion version of this.
                BlockCollision::None => {
                    // No collision for this block
                    continue;
                }
                BlockCollision::Hard => {
                    let contact = full_cube_end.contact;
                    if !already_colliding.contains(&contact) {
                        collision_callback(contact);
                        something_hit = Some(full_cube_end);
                    }
                }
                BlockCollision::Recur => {
                    if let Some(did_end) = Sp::recurse(full_cube_end, aab, ray, cell) {
                        if !already_colliding.contains(&did_end.contact) {
                            collision_callback(did_end.contact);
                            something_hit = Some(did_end);
                        }
                    }
                }
            }
        }

        // Now that we've found _all_ the contacts, report the collision.
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
    aab.round_up_to_grid().interior_iter().filter(move |&cube| {
        let cell = space.get_cell(cube);
        match Sp::collision(cell) {
            BlockCollision::None => false,
            BlockCollision::Hard => true,
            BlockCollision::Recur => match Sp::get_voxels(cell) {
                None => true,
                Some((resolution, voxels)) => {
                    let voxel_aab = aab
                        .translate(cube.to_vec().map(|s| -FreeCoordinate::from(s)))
                        .scale(FreeCoordinate::from(resolution));
                    let voxel_aab_grid = voxel_aab.round_up_to_grid().intersection(voxels.grid());
                    if let Some(g) = voxel_aab_grid {
                        g.interior_iter()
                            .any(|subcube| match voxels[subcube].collision {
                                BlockCollision::None => false,
                                BlockCollision::Hard | BlockCollision::Recur => true,
                            })
                    } else {
                        false
                    }
                }
            },
        }
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
                if let Some(hit_voxel) = collide_along_ray(voxels, voxel_ray, voxel_aab, |_| {}) {
                    let CollisionRayEnd {
                        t_distance: voxel_t_distance,
                        contact,
                    } = hit_voxel;

                    match contact {
                        Contact::Block(voxel) => Some(CollisionRayEnd {
                            t_distance: voxel_t_distance * scale,
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
    use crate::block::{Block, AIR};
    use crate::content::make_some_blocks;
    use crate::math::GridCoordinate;
    use crate::raytracer::print_space;
    use crate::space::Grid;
    use crate::universe::Universe;

    use super::*;
    use rand::{Rng, SeedableRng as _};
    #[test]
    fn collide_along_ray_with_opaque_block() {
        collide_along_ray_tester(
            || {
                let [block] = make_some_blocks();
                block
            },
            Some(CollisionRayEnd {
                t_distance: 0.5,
                contact: Contact::Block(CubeFace::new([1, 0, 0], Face::PY)),
            }),
        );
    }

    #[test]
    #[ignore] // TODO: enable test when recursive collision is implemented
    fn collide_along_ray_with_recursive_block() {
        collide_along_ray_tester(
            || {
                // Construct a lower half block ▄.
                let u = &mut Universe::new();
                let [voxel] = make_some_blocks();
                Block::builder()
                    .collision(BlockCollision::Recur)
                    .display_name("H")
                    .voxels_fn(u, 2, |p| if p.y > 0 { &AIR } else { &voxel })
                    .unwrap()
                    .build()
            },
            Some(CollisionRayEnd {
                t_distance: 0.5,
                contact: Contact::Voxel {
                    cube: GridPoint::new(0, 0, 0),
                    resolution: 2,
                    voxel: CubeFace::new([0, 0, 0], Face::PY),
                },
            }),
        );
    }

    fn collide_along_ray_tester(block_gen: fn() -> Block, expected_end: Option<CollisionRayEnd>) {
        let block = block_gen();
        let mut space = Space::empty_positive(2, 1, 1);
        space.set([1, 0, 0], &block).unwrap();
        print_space(&space, [1., 1., 1.]);

        // Set up to collide with the block such that the ray doesn't pass through it, to
        // make sure the right cube is returned.
        // The block is at [1, 0, 0] and we're "dropping" a block-shaped AAB onto it
        // with lower corner [-0.5, 0, 0] so it should contact the "left" half of
        // the recursive block.
        let aab = Aab::from_cube(GridPoint::new(0, 0, 0));
        let ray = Ray::new([0.5, 1., 0.], [0., -2., 0.]); // TODO: ray shouldn't need to be this long; debug

        let result = collide_along_ray(&space, ray, aab, |_| {});
        assert_eq!(result, expected_end);
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
