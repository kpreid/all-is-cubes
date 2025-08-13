//! [`Cursor`] type and related items.
//!
//! TODO: It's unclear what the scope of this module should be.

use core::fmt;

use euclid::point3;

use crate::block::{Block, EvaluatedBlock, Evoxel};
use crate::content::palette;
use crate::math::{
    Cube, Face6, Face7, FreeCoordinate, FreePoint, FreeVector, LineVertex, colorize_lines,
};
use crate::raycast::Ray;
use crate::space::{PackedLight, Space};
use crate::universe::{Handle, HandleError, ReadTicket};

/// Find the first selectable block the ray strikes and express the result in a [`Cursor`]
/// value, or [`None`] if nothing was struck within the distance limit.
pub fn cursor_raycast(
    read_ticket: ReadTicket<'_>,
    mut ray: Ray,
    space_handle: &Handle<Space>,
    maximum_distance: FreeCoordinate,
) -> Result<Option<Cursor>, HandleError> {
    ray.direction = ray.direction.normalize();
    let space = space_handle.read(read_ticket)?;
    for step in ray.cast().within(space.bounds()) {
        if step.t_distance() > maximum_distance {
            break;
        }

        let cube = step.cube_ahead();
        let evaluated = space.get_evaluated(cube);
        let mut face_selected = None;

        if !evaluated.attributes().selectable {
            continue;
        }

        // Check intersection with recursive block
        match evaluated.voxels().single_voxel() {
            Some(evoxel) => {
                if !evoxel.selectable {
                    continue;
                }
                face_selected = Some(step.face());
            }
            None => {
                let voxels = evaluated.voxels().as_vol_ref();
                let recursive_hit: Option<(Cube, &Evoxel)> = step
                    .recursive_raycast(ray, evaluated.resolution(), voxels.bounds())
                    .0
                    .filter_map(|voxel_step| {
                        if face_selected.is_none() {
                            // Set the selected face to the first face we hit, which
                            // will be the face of the bounding box we hit.
                            // TODO: Either don't rely on the bounding box (perhaps
                            // only take faces of selectable voxels) or change block
                            // evaluation to make the bounding box guaranteed tight.
                            face_selected = Some(voxel_step.face());
                        }
                        voxels
                            .get(voxel_step.cube_ahead())
                            .map(|v| (voxel_step.cube_ahead(), v))
                    })
                    .find(|(_, v)| v.selectable);
                if recursive_hit.is_none() {
                    continue;
                }
            }
        }

        return Ok(Some(Cursor {
            space: space_handle.clone(),
            face_entered: step.face(),
            face_selected: face_selected.expect("failed to determine face_selected"),
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
        }));
    }
    Ok(None)
}
/// Data collected by [`cursor_raycast`] about the blocks struck by the ray; intended to be
/// sufficient for various player interactions with blocks.
#[derive(Clone, Debug, PartialEq)]
pub struct Cursor {
    /// The space the selected cube is in.
    space: Handle<Space>,

    /// The face that the cursor ray entered the cube via.
    ///
    /// Note that this is not necessarily the same as “the face of the block” in the case
    /// where the block occupies less than the full volume.
    face_entered: Face7,

    /// The face of the block that is being selected.
    face_selected: Face7,

    /// Intersection point where the ray entered the cube.
    point_entered: FreePoint,

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
#[allow(missing_docs, reason = "TODO")]
pub struct CubeSnapshot {
    pub position: Cube,
    pub block: Block,
    pub evaluated: EvaluatedBlock,
    pub light: PackedLight,
}

impl Cursor {
    /// The space the selected cube is in.
    #[inline]
    pub fn space(&self) -> &Handle<Space> {
        &self.space
    }

    /// Which cube of the space that the cursor ray selected/hit.
    pub fn cube(&self) -> Cube {
        self.hit.position
    }

    /// The cube the ray passed through immediately before the selected cube.
    ///
    /// This may be the same cube if the ray started there.
    pub fn preceding_cube(&self) -> Cube {
        self.cube() + self.face_entered.normal_vector()
    }

    /// Which face of the block the cursor ray selected/hit.
    ///
    /// Note that this is not necessarily the same as the face of the enclosing cube,
    /// in the case where the block occupies less than the full volume; rather it is
    /// intended to make sense to the human who does not get to see the cube grid.
    /// It is currently defined to be the hit face of the bounding box of the block data
    /// (which is often but not always tightly bounding the visible voxels, so this will
    /// have the unsurprising value for any box-shaped block).
    ///
    /// Will be [`Face7::Within`] if the ray started inside the block.
    pub fn face_selected(&self) -> Face7 {
        self.face_selected
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

impl crate::math::Wireframe for Cursor {
    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<LineVertex>,
    {
        let evaluated = &self.hit().evaluated;

        // Compute an approximate offset that will prevent Z-fighting.
        let offset_from_surface = 0.001 * self.distance_to_point;

        // AABB of the block's actual content. We use this rather than the full extent of
        // the cube so that it feels more accurate.
        //
        // TODO: voxels_bounds() is not really an intentionally designed selection box and
        // will often be oversized.
        // (But maybe we should guarantee it is right-sized within block evaluation?)
        // (Perhaps a better box would be the bounds of all `selectable` voxels?)
        let block_aabb = evaluated
            .voxels_bounds()
            .to_free()
            .scale(FreeCoordinate::from(evaluated.resolution()).recip())
            .translate(
                self.cube()
                    .lower_bounds()
                    .map(FreeCoordinate::from)
                    .to_vector(),
            );

        // Add wireframe of the block.
        block_aabb
            .expand(offset_from_surface)
            .wireframe_points(&mut colorize_lines(output, palette::CURSOR_OUTLINE));

        // Frame the selected face with a square.
        // TODO: Position this frame relative to block_aabb.
        if let Ok(face) = Face6::try_from(self.face_selected()) {
            let face_transform_full = face.face_transform(1).to_matrix().to_free().then(
                &self
                    .hit()
                    .position
                    .lower_bounds()
                    .map(FreeCoordinate::from)
                    .to_vector()
                    .to_transform(),
            );

            let inset = 1. / 128.;
            for &p in [
                point3(inset, inset, -offset_from_surface),
                point3(inset, 1. - inset, -offset_from_surface),
                point3(1. - inset, 1. - inset, -offset_from_surface),
                point3(1. - inset, inset, -offset_from_surface),
                point3(inset, inset, -offset_from_surface),
            ]
            .windows(2)
            .flatten()
            {
                let position = face_transform_full.transform_point3d(p).unwrap();
                output.extend([LineVertex {
                    position,
                    color: Some(palette::CURSOR_OUTLINE),
                }]);
            }
        }

        // Frame the cursor intersection point with a diamond.
        // TODO: This addition is experimental and we may or may not want to keep it.
        // For now, it visualizes the intersection and face information.
        if let Ok(face) = Face6::try_from(self.face_entered) {
            let face_transform_axes_only = face.rotation_from_nz().to_rotation_matrix().to_free();
            for f in [Face7::PX, Face7::PY, Face7::NX, Face7::NY, Face7::PX]
                .windows(2)
                .flatten()
            {
                let tip: FreeVector = face_transform_axes_only
                    .transform_vector3d(f.normal_vector::<_, Cube>() * (1.0 / 32.0));
                let position = self.point_entered
                    + self.face_entered.normal_vector() * offset_from_surface
                    + tip;
                output.extend([LineVertex {
                    position,
                    color: Some(palette::CURSOR_OUTLINE),
                }]);
            }
        }
    }
}

/// These are tests of [`cursor_raycast()`] and the data it returns.
/// For tests of behavior when actually _using_ a [`Cursor`] to invoke a tool,
/// see [`crate::character::tests`] and [`crate::inv`].
#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{AIR, Resolution::*};
    use crate::content::{make_slab, make_some_blocks};
    use crate::math::{GridAab, Rgba};
    use crate::universe::Universe;
    use euclid::{Point3D, Vector3D, vec3};

    fn test_space<const N: usize>(universe: &mut Universe, blocks: [&Block; N]) -> Handle<Space> {
        let mut space =
            Space::builder(GridAab::from_lower_size([0, 0, 0], vec3(N, 1, 1).to_u32())).build();
        space.mutate(universe.read_ticket(), |m| {
            m.fill_all(|p| Some(blocks[p.x as usize])).unwrap();
        });
        universe.insert_anonymous(space)
    }

    /// A [`Ray`] aligned with the X axis, such that it starts in cube [-1, 0, 0] and hits
    /// [0, 0, 0], [1, 0, 0], [2, 0, 0], et cetera, and just slightly above the midpoint.
    const X_RAY: Ray = Ray {
        origin: Point3D::new(-0.5, 0.500001, 0.500001),
        direction: Vector3D::new(1., 0., 0.),
    };

    #[test]
    fn simple_hit_after_air() {
        let universe = &mut Universe::new();
        let [block] = make_some_blocks();
        let space_handle = test_space(universe, [&AIR, &block]);

        let cursor = cursor_raycast(universe.read_ticket(), X_RAY, &space_handle, f64::INFINITY)
            .unwrap()
            .unwrap();
        assert_eq!(cursor.hit().block, block);
        assert_eq!(cursor.cube(), Cube::new(1, 0, 0));
        assert_eq!(cursor.face_selected(), Face7::NX);
    }

    #[test]
    fn maximum_distance_too_short() {
        let universe = &mut Universe::new();
        let [block] = make_some_blocks();
        let space_handle = test_space(universe, [&AIR, &block]);

        assert_eq!(
            cursor_raycast(universe.read_ticket(), X_RAY, &space_handle, 1.0),
            Ok(None)
        );
    }

    #[test]
    fn ignores_not_selectable_atom() {
        let universe = &mut Universe::new();
        let [block] = make_some_blocks();
        let not_selectable = Block::builder()
            .color(Rgba::WHITE)
            .selectable(false)
            .build();
        let space_handle = test_space(universe, [&not_selectable, &block]);

        let cursor = cursor_raycast(universe.read_ticket(), X_RAY, &space_handle, f64::INFINITY)
            .unwrap()
            .unwrap();
        // If the non-selectable block was hit, this would be [0, 0, 0]
        assert_eq!(cursor.cube(), Cube::new(1, 0, 0));
        assert_eq!(cursor.hit().block, block);
    }

    #[test]
    fn ignores_not_selectable_voxels() {
        let universe = &mut Universe::new();
        let [block] = make_some_blocks();
        let not_selectable = make_slab(universe, 1, R2); // Upper half is nonselectable air
        let space_handle = test_space(universe, [&not_selectable, &block]);

        let cursor = cursor_raycast(universe.read_ticket(), X_RAY, &space_handle, f64::INFINITY)
            .unwrap()
            .unwrap();
        assert_eq!(cursor.cube(), Cube::new(1, 0, 0));
        assert_eq!(cursor.hit().block, block);
    }

    #[test]
    fn hits_selectable_voxels() {
        let universe = &mut Universe::new();
        let [other_block] = make_some_blocks();
        let selectable_voxels = make_slab(universe, 3, R4);
        let space_handle = test_space(universe, [&AIR, &selectable_voxels, &other_block]);

        let cursor = cursor_raycast(universe.read_ticket(), X_RAY, &space_handle, f64::INFINITY)
            .unwrap()
            .unwrap();
        assert_eq!(cursor.cube(), Cube::new(1, 0, 0));
        assert_eq!(cursor.hit().block, selectable_voxels);
    }

    /// A [`Ray`] which will pass through the left face and then the middle Y plane of a
    /// block located at [0, 0, 0].
    ///
    /// ```text
    /// 1 +----•-+------+
    ///   |     \|      |
    ///   |      \      |
    ///   |      |\     |
    ///   |      | \    |
    ///   |      |  \   |
    ///   |      |   ↘  |
    /// 0 +------+------+
    ///  -1      0      1
    /// ```
    const SLOPING_RAY: Ray = Ray {
        origin: Point3D::new(-0.25, 1.0, 0.5),
        direction: Vector3D::new(1.0, -1.0, 0.0),
    };

    /// Testing the “normal” case in contrast to `slope_hits_face_different_from_entered`.
    #[test]
    fn slope_hits_face_of_full_block() {
        let universe = &mut Universe::new();
        let [block] = make_some_blocks();
        let space_handle = test_space(universe, [&block]);

        let cursor = cursor_raycast(
            universe.read_ticket(),
            SLOPING_RAY,
            &space_handle,
            f64::INFINITY,
        )
        .unwrap()
        .unwrap();
        assert_eq!(cursor.face_entered, Face7::NX);
        assert_eq!(cursor.face_selected(), Face7::NX);
    }

    /// Test the case where the face of the block the cursor ray hits is not equal to the
    /// face of the cube the ray entered.
    #[test]
    fn slope_hits_face_different_from_entered() {
        let universe = &mut Universe::new();
        let slab = make_slab(universe, 1, R2);
        let space_handle = test_space(universe, [&slab]);

        let cursor = cursor_raycast(
            universe.read_ticket(),
            SLOPING_RAY,
            &space_handle,
            f64::INFINITY,
        )
        .unwrap()
        .unwrap();
        dbg!(&cursor);
        assert_eq!(cursor.face_entered, Face7::NX);
        assert_eq!(cursor.face_selected(), Face7::PY);
    }
}
