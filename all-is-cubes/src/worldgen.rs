// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Procedural world generation.

use cgmath::{Point3, Vector3, Vector4};
use std::borrow::Cow;
use std::convert::TryFrom;

use crate::block::Block;
use crate::math::{FreeCoordinate, GridCoordinate, Rgb, Rgba};
use crate::raycast::{Face, Raycaster};
use crate::space::{Grid, Space};

/// Draw the world axes as lines of blocks centered on (0, 0, 0).
///
/// ```
/// use all_is_cubes::block::AIR;
/// use all_is_cubes::space::{Grid, Space};
/// use all_is_cubes::worldgen::axes;
///
/// let mut space = Space::empty(Grid::new((-10, -10, -10), (21, 21, 21)));
/// axes(&mut space);
///
/// assert!(space[(10, 0, 0)] != AIR);
/// assert!(space[(0, 10, 0)] != AIR);
/// assert!(space[(0, 0, 10)] != AIR);
/// assert!(space[(-10, 0, 0)] != AIR);
/// assert!(space[(0, -10, 0)] != AIR);
/// assert!(space[(0, 0, -10)] != AIR);
/// ```
pub fn axes(space: &mut Space) {
    for &face in Face::ALL_SIX {
        let axis = face.axis_number();
        let direction = face.normal_vector::<GridCoordinate>()[axis];
        let raycaster = Raycaster::new((0.5, 0.5, 0.5), face.normal_vector::<FreeCoordinate>())
            .within_grid(space.grid());
        for step in raycaster {
            let i = step.cube_ahead()[axis] * direction; // always positive
            let mut color = Vector4::new(0.0, 0.0, 0.0, 1.0);
            let mut light = Vector3::new(0.0, 0.0, 0.0);
            let mut display_name: Cow<'static, str> = (i % 10).to_string().into();
            if i % 2 == 0 {
                color[axis] = if direction > 0 { 1.0 } else { 0.9 };
            } else {
                if direction > 0 {
                    color = Vector4::new(1.0, 1.0, 1.0, 1.0);
                    display_name = ["X", "Y", "Z"][axis].into();
                } else {
                    display_name = ["x", "y", "z"][axis].into();
                };
            }
            light[axis] = 3.0;
            space
                .set(
                    step.cube_ahead(),
                    Block::builder()
                        .display_name(display_name)
                        .light_emission(Rgb::try_from(light).unwrap())
                        .color(Rgba::try_from(color).expect("axes() color generation failed"))
                        .build(),
                )
                .unwrap();
        }
    }
}

/// Generate a space which is both completely enclosed and has a convenient flat surface
/// for adding stuff to and, as the name suggests, testing physics.
///
/// TODO: This is not actually used yet. Don't declare it "stable" until we've proven
/// that this design is useful.
/// TODO: Add some lights.
/// TODO: Define exactly what the radii mean so users can build things on surfaces.
#[allow(unused)]
fn physics_lab(shell_radius: u16, planet_radius: u16) -> Space {
    assert!(shell_radius > planet_radius);
    let space_radius = shell_radius + 1; // TODO check off-by-one consistency
    let mut space = Space::empty(Grid::new(
        Point3::new(-1, -1, -1) * space_radius.into(),
        Vector3::new(1, 1, 1) * (space_radius * 2 + 1).into(),
    ));
    let shell_radius: GridCoordinate = shell_radius.into();
    let planet_radius: GridCoordinate = planet_radius.into();

    let outer_wall_block = Block::builder()
        .display_name("Celestial Cube")
        .color(Rgba::new(0.2, 0.2, 0.2, 1.0))
        .build();
    let floor_1 = Block::builder()
        .display_name("Floor")
        .color(Rgba::new(0.5, 0.5, 0.5, 1.0))
        .build();
    let floor_2 = Block::builder()
        .display_name("Floor")
        .color(Rgba::new(0.95, 0.95, 0.95, 1.0))
        .build();

    // Outer walls
    // TODO: Build some utilities for symmetric systematic constructions so we don't have to handcode this.
    let mut place_outer_wall = |p| {
        // TODO: add some random stars
        space.set(p, &outer_wall_block).unwrap();
    };
    for x in -shell_radius..=shell_radius {
        for y in -shell_radius..=shell_radius {
            if x.abs() == shell_radius || y.abs() == shell_radius {
                for z in -shell_radius..=shell_radius {
                    place_outer_wall((x, y, z));
                }
            } else {
                place_outer_wall((x, y, shell_radius));
                place_outer_wall((x, y, -shell_radius));
            }
        }
    }

    // Inner surface.
    for x in -planet_radius..=planet_radius {
        for y in -planet_radius..=planet_radius {
            for z in -planet_radius..=planet_radius {
                let block = if (x + y + z).rem_euclid(2) == 0 {
                    &floor_1
                } else {
                    &floor_2
                };
                space.set((x, y, z), block).unwrap();
            }
        }
    }

    space
}
