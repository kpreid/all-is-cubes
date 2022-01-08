// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! [`Cursor`] type and related items.
//!
//! TODO: It's unclear what the scope of this module should be.

use std::fmt;

use cgmath::{InnerSpace as _, Point3};

use crate::block::{recursive_raycast, Block, EvaluatedBlock};
use crate::math::FreeCoordinate;
use crate::raycast::{CubeFace, Ray};
use crate::space::{PackedLight, Space};
use crate::universe::URef;
use crate::util::{ConciseDebug, CustomFormat as _};

/// Find the first selectable block the ray strikes and express the result in a [`Cursor`]
/// value, or [`None`] if nothing was struck within the distance limit.
pub fn cursor_raycast(
    mut ray: Ray,
    space_ref: &URef<Space>,
    maximum_distance: FreeCoordinate,
) -> Option<Cursor> {
    ray.direction = ray.direction.normalize();
    let space = space_ref.try_borrow().ok()?;
    for step in ray.cast().within_grid(space.grid()) {
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
