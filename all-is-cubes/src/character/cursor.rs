//! [`Cursor`] type and related items.
//!
//! TODO: It's unclear what the scope of this module should be.

use std::fmt;

use cgmath::{EuclideanSpace, InnerSpace as _, Matrix4, Point3, Transform as _};

use crate::block::{recursive_raycast, Block, EvaluatedBlock};
use crate::content::palette;
use crate::math::{
    Aab, Face7, FreeCoordinate, Geometry, GridCoordinate, GridPoint, GridVector, Rgba,
};
use crate::raycast::Ray;
use crate::space::{PackedLight, Space};
use crate::universe::URef;
use crate::util::MapExtend;

/// Find the first selectable block the ray strikes and express the result in a [`Cursor`]
/// value, or [`None`] if nothing was struck within the distance limit.
pub fn cursor_raycast(
    mut ray: Ray,
    space_ref: &URef<Space>,
    maximum_distance: FreeCoordinate,
) -> Option<Cursor> {
    ray.direction = ray.direction.normalize();
    let space = space_ref.try_borrow().ok()?;
    for step in ray.cast().within(space.bounds()) {
        if step.t_distance() > maximum_distance {
            break;
        }

        let cube = step.cube_ahead();
        let evaluated = space.get_evaluated(cube);

        // Check intersection with recursive block
        if let Some(voxels) = &evaluated.voxels {
            if !recursive_raycast(ray, step.cube_ahead(), evaluated.resolution)
                .flat_map(|voxel_step| voxels.get(voxel_step.cube_ahead()))
                .any(|v| v.selectable)
            {
                continue;
            }
        }

        if evaluated.attributes.selectable {
            return Some(Cursor {
                space: space_ref.clone(),
                face_entered: step.face(),
                point_entered: step.intersection_point(ray),
                distance_to_point: step.t_distance(),
                hit: CubeSnapshot {
                    position: cube,
                    block: space[cube].clone(),
                    evaluated: evaluated.clone(),
                    light: space.get_lighting(cube),
                },
                preceding: if step.face() != Face7::Within {
                    let pcube = step.cube_behind();
                    Some(CubeSnapshot {
                        position: pcube,
                        block: space[pcube].clone(),
                        evaluated: space.get_evaluated(pcube).clone(),
                        light: space.get_lighting(pcube),
                    })
                } else {
                    None
                },
            });
        }
    }
    None
}
/// Data collected by [`cursor_raycast`] about the blocks struck by the ray; intended to be
/// sufficient for various player interactions with blocks.
///
/// TODO: Should carry information about both the struck and preceding cubes.
#[derive(Clone, Debug, PartialEq)]
pub struct Cursor {
    /// The space the selected cube is in.
    space: URef<Space>,

    /// The face that the cursor ray entered the cube via.
    ///
    /// Note that this is not necessarily the same as “the face of the block” in the case
    /// where the block occupies less than the full volume.
    face_entered: Face7,

    /// Intersection point where the ray entered the cube.
    point_entered: Point3<FreeCoordinate>,

    /// Distance from ray origin (viewpoint) to `point_entered`.
    distance_to_point: FreeCoordinate,

    /// Data about the cube the cursor selected/hit.
    hit: CubeSnapshot,

    /// Data about the cube the cursor ray was in before it hit [`Self::hit`],
    /// if there was one, or `None` if the cursor ray started in the cube it hit.
    preceding: Option<CubeSnapshot>,
}

/// Snapshot of the contents of one cube of a [`Space`], independent of the [`Space`].
///
/// TODO: Can we find a cleaner name for this class?
#[derive(Clone, Debug, PartialEq)]
#[non_exhaustive]
pub struct CubeSnapshot {
    pub position: GridPoint,
    pub block: Block,
    pub evaluated: EvaluatedBlock,
    pub light: PackedLight,
}

impl Cursor {
    /// The space the selected cube is in.
    #[inline]
    pub fn space(&self) -> &URef<Space> {
        &self.space
    }

    /// Which cube of the space that the cursor ray selected/hit.
    pub fn cube(&self) -> GridPoint {
        self.hit.position
    }

    pub fn preceding_cube(&self) -> GridPoint {
        self.cube() + self.face_entered.normal_vector()
    }

    pub fn face_selected(&self) -> Face7 {
        // TODO: this should reflect the face of the block hit, not the cube
        self.face_entered
    }

    /// Returns data about the cube the cursor selected/hit.
    #[inline]
    pub fn hit(&self) -> &CubeSnapshot {
        &self.hit
    }

    // TODO: Preceding data is actually unused except for debug info via fmt::Display...
    // Should we remove it? Tools do care about the preceding space but not quite this way.
    // I think there was some use-case for having the preceding/selected-adjacent block's
    // EvaluatedBlock data, though.
    //
    // /// Returns data about the cube the cursor ray passed through just before it hit anything.
    // /// If the ray started from within the cube it hit, returns the same as [`hit()`](Self::hit).
    // #[inline]
    // pub fn preceding_or_within(&self) -> &CubeSnapshot {
    //     self.preceding.as_ref().unwrap_or(&self.hit)
    // }
}

// TODO: this probably shouldn't be Display any more, but Debug or ConciseDebug
// — or just a regular method.
impl fmt::Display for Cursor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Block at {c:?} face {f:?}\n{ev:#?}\nLighting within {la:?}, behind {lb:?}",
            c = self.cube(),
            f = self.face_entered,
            ev = self.hit().evaluated,
            la = self.hit().light,
            lb = self.preceding.as_ref().map(|s| s.light),
        )
    }
}

/// TODO: This implementation exists because it was convenient to support drawing;
/// eventually we will probably want cursor rendering to be its own more elaborate
/// thing.
impl Geometry for Cursor {
    type Coord = GridCoordinate;

    /// Not implemented for [`Cursor`].
    fn translate(self, _offset: GridVector) -> Self {
        unimplemented!()
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<(Point3<FreeCoordinate>, Option<crate::math::Rgba>)>,
    {
        // Compute an approximate offset that will prevent Z-fighting.
        let offset_from_surface = 0.001 * self.distance_to_point;

        // Draw box
        Aab::from_cube(self.hit().position)
            .expand(offset_from_surface)
            .wireframe_points(&mut MapExtend::new(
                output,
                |(p, _): (Point3<FreeCoordinate>, Option<Rgba>)| (p, Some(palette::CURSOR_OUTLINE)),
            ));

        let face_transform_full =
            Matrix4::from_translation(self.hit().position.map(FreeCoordinate::from).to_vec())
                * self.face_entered.matrix(1).to_free();

        // Frame the selected face with a square
        let inset = 1. / 128.;
        for &p in [
            Point3::new(inset, inset, -offset_from_surface),
            Point3::new(inset, 1. - inset, -offset_from_surface),
            Point3::new(1. - inset, 1. - inset, -offset_from_surface),
            Point3::new(1. - inset, inset, -offset_from_surface),
            Point3::new(inset, inset, -offset_from_surface),
        ]
        .windows(2)
        .flatten()
        {
            let p = face_transform_full.transform_point(p);
            output.extend([(p, Some(palette::CURSOR_OUTLINE))]);
        }

        // Frame the cursor intersection point with a diamond.
        // TODO: This addition is experimental and we may or may not want to keep it.
        // For now, it visualizes the intersection and face information.
        let face_transform_axes_only = self.face_entered.matrix(0).to_free();
        for f in [Face7::PX, Face7::PY, Face7::NX, Face7::NY, Face7::PX]
            .windows(2)
            .flatten()
        {
            let p = self.point_entered
                + self.face_entered.normal_vector() * offset_from_surface
                + face_transform_axes_only.transform_vector(f.normal_vector() * (1.0 / 32.0));
            output.extend([(p, Some(palette::CURSOR_OUTLINE))]);
        }
    }
}

/// These are tests of [`cursor_raycast()`] and the data it returns.
/// For tests of behavior when actually _using_ a [`Cursor`] to invoke a tool,
/// see [`crate::character::tests`] and [`crate::inv`].
#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Resolution::*, AIR};
    use crate::content::{make_slab, make_some_blocks};
    use crate::math::GridAab;
    use crate::universe::Universe;
    use cgmath::Vector3;

    fn test_space<const N: usize>(universe: &mut Universe, blocks: [&Block; N]) -> URef<Space> {
        let mut space =
            Space::builder(GridAab::from_lower_size([0, 0, 0], [N as i32, 1, 1])).build();
        space
            .fill(space.bounds(), |p| Some(blocks[p.x as usize]))
            .unwrap();
        universe.insert_anonymous(space)
    }

    /// A [`Ray`] aligned with the X axis, such that it starts in cube [-1, 0, 0] and hits
    /// [0, 0, 0], [1, 0, 0], [2, 0, 0], et cetera, and just slightly above the midpoint.
    const X_RAY: Ray = Ray {
        origin: Point3::new(-0.5, 0.500001, 0.500001),
        direction: Vector3::new(1., 0., 0.),
    };

    #[test]
    fn simple_hit_after_air() {
        let universe = &mut Universe::new();
        let [block] = make_some_blocks();
        let space_ref = test_space(universe, [&AIR, &block]);

        let cursor = cursor_raycast(X_RAY, &space_ref, f64::INFINITY).unwrap();
        assert_eq!(cursor.hit().block, block);
        assert_eq!(cursor.cube(), GridPoint::new(1, 0, 0));
        assert_eq!(cursor.face_selected(), Face7::NX);
    }

    #[test]
    fn maximum_distance_too_short() {
        let universe = &mut Universe::new();
        let [block] = make_some_blocks();
        let space_ref = test_space(universe, [&AIR, &block]);

        assert_eq!(cursor_raycast(X_RAY, &space_ref, 1.0), None);
    }

    #[test]
    fn ignores_not_selectable_atom() {
        let universe = &mut Universe::new();
        let [block] = make_some_blocks();
        let not_selectable = Block::builder()
            .color(Rgba::WHITE)
            .selectable(false)
            .build();
        let space_ref = test_space(universe, [&not_selectable, &block]);

        let cursor = cursor_raycast(X_RAY, &space_ref, f64::INFINITY).unwrap();
        // If the non-selectable block was hit, this would be [0, 0, 0]
        assert_eq!(cursor.cube(), GridPoint::new(1, 0, 0));
        assert_eq!(cursor.hit().block, block);
    }

    #[test]
    fn ignores_not_selectable_voxels() {
        let universe = &mut Universe::new();
        let [block] = make_some_blocks();
        let not_selectable = make_slab(universe, 1, R2); // Upper half is nonselectable air
        let space_ref = test_space(universe, [&not_selectable, &block]);

        let cursor = cursor_raycast(X_RAY, &space_ref, f64::INFINITY).unwrap();
        assert_eq!(cursor.cube(), GridPoint::new(1, 0, 0));
        assert_eq!(cursor.hit().block, block);
    }

    #[test]
    fn hits_selectable_voxels() {
        let universe = &mut Universe::new();
        let [other_block] = make_some_blocks();
        let selectable_voxels = make_slab(universe, 3, R4);
        let space_ref = test_space(universe, [&AIR, &selectable_voxels, &other_block]);

        let cursor = cursor_raycast(X_RAY, &space_ref, f64::INFINITY).unwrap();
        assert_eq!(cursor.cube(), GridPoint::new(1, 0, 0));
        assert_eq!(cursor.hit().block, selectable_voxels);
    }
}
