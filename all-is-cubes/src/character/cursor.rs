// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! [`Cursor`] type and related items.
//!
//! TODO: It's unclear what the scope of this module should be.

use std::fmt;

use cgmath::{InnerSpace as _, Point3, Transform};

use crate::block::{recursive_raycast, Block, EvaluatedBlock};
use crate::content::palette;
use crate::math::{
    Aab, CubeFace, Face7, FreeCoordinate, Geometry, GridCoordinate, GridVector, Rgba,
};
use crate::raycast::Ray;
use crate::space::{PackedLight, Space};
use crate::universe::URef;
use crate::util::{ConciseDebug, CustomFormat as _, MapExtend};

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
        let lighting_ahead = space.get_lighting(cube);
        let lighting_behind = space.get_lighting(step.cube_behind());

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
                place: step.cube_face(),
                point: step.intersection_point(ray),
                distance: step.t_distance(),
                block: space[cube].clone(),
                evaluated: evaluated.clone(),
                lighting_ahead,
                lighting_behind,
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
#[non_exhaustive]
pub struct Cursor {
    pub space: URef<Space>,
    /// The cube the cursor is at and which face was hit.
    pub place: CubeFace,
    pub point: Point3<FreeCoordinate>,
    /// Distance from viewpoint to intersection point.
    pub distance: FreeCoordinate,
    /// The block that was found in the given cube.
    pub block: Block,
    /// The EvaluatedBlock data for the block.
    pub evaluated: EvaluatedBlock,
    pub lighting_ahead: PackedLight,
    pub lighting_behind: PackedLight,
}

// TODO: this probably shouldn't be Display any more, but Debug or ConciseDebug
// — or just a regular method.
impl fmt::Display for Cursor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Block at {:?}\n{:#?}\nLighting within {:?}, behind {:?}",
            self.place,
            self.evaluated.custom_format(ConciseDebug),
            self.lighting_ahead,
            self.lighting_behind,
        )
    }
}

/// TODO: This implementation exists because it was convenient to support drawing;
/// eventually we will probably want cursor rendering to be its own more elaborate
/// thing.
impl Geometry for Cursor {
    type Coord = GridCoordinate;

    fn translate(mut self, offset: GridVector) -> Self {
        self.place.cube += offset;
        self.point += offset.map(FreeCoordinate::from);
        self
    }

    fn wireframe_points<E>(&self, output: &mut E)
    where
        E: Extend<(Point3<FreeCoordinate>, Option<crate::math::Rgba>)>,
    {
        // Compute an approximate offset that will prevent Z-fighting.
        let offset_from_surface = 0.001 * self.distance;

        // TODO: Maybe highlight the selected face's rectangle
        Aab::from_cube(self.place.cube)
            .expand(offset_from_surface)
            .wireframe_points(&mut MapExtend::new(
                output,
                |(p, _): (Point3<FreeCoordinate>, Option<Rgba>)| (p, Some(palette::CURSOR_OUTLINE)),
            ));

        // Frame the cursor intersection point with a diamond.
        // TODO: This addition is experimental and we may or may not want to keep it.
        // For now, it visualizes the intersection and face information.
        let face_frame = self.place.face.matrix(0).to_free();
        for f in [
            Face7::PX,
            Face7::PY,
            Face7::PY,
            Face7::NX,
            Face7::NX,
            Face7::NY,
            Face7::NY,
            Face7::PX,
        ] {
            let p = self.point
                + self.place.face.normal_vector() * offset_from_surface
                + face_frame.transform_vector(f.normal_vector() * (1.0 / 32.0));
            output.extend([(p, Some(palette::CURSOR_OUTLINE))]);
        }
    }
}
