// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! First-run game content. (Well, all runs, since we don't have saving yet.)

use all_is_cubes::block::Block;
use all_is_cubes::cgmath::Point3;
use all_is_cubes::character::Character;
use all_is_cubes::linking::{GenError, InGenError};
use all_is_cubes::math::{FreeCoordinate, GridCoordinate, GridPoint, GridVector, Rgb, Rgba};
use all_is_cubes::space::{Grid, LightPhysics, Space};
use all_is_cubes::universe::{Name, Universe, UniverseIndex};

use crate::{atrium::atrium, demo_city, dungeon::demo_dungeon, install_demo_blocks};

/// Selection of initial content for constructing a new [`Universe`].
//
// TODO: Stop using strum, because we will eventually want parameterized templates.
#[derive(
    Clone,
    Debug,
    Eq,
    Hash,
    PartialEq,
    strum::Display,
    strum::EnumString,
    strum::EnumIter,
    strum::IntoStaticStr,
)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub enum UniverseTemplate {
    Blank,
    /// Always produces an error, for testing error-handling functionality.
    Fail,
    DemoCity,
    Dungeon,
    Atrium,
    CornellBox,
    PhysicsLab,
    LightingBench,
    // TODO: add an "nothing, you get a blank editor" option once we have enough editing support.
}

impl UniverseTemplate {
    pub fn build(self) -> Result<Universe, GenError> {
        use UniverseTemplate::*;
        match self {
            Blank => Ok(Universe::new()),
            Fail => Err(GenError::failure(
                InGenError::Other("the Fail template always fails to generate".into()),
                "space".into(), // TODO: should be able to not provide a dummy name
            )),
            DemoCity => new_universe_with_space_setup(demo_city),
            Dungeon => new_universe_with_space_setup(demo_dungeon),
            Atrium => new_universe_with_space_setup(atrium),
            CornellBox => new_universe_with_space_setup(cornell_box),
            PhysicsLab => new_universe_with_space_setup(|_| physics_lab(50, 16)),
            LightingBench => {
                new_universe_with_space_setup(all_is_cubes::content::testing::lighting_bench_space)
            }
        }
    }
}

impl Default for UniverseTemplate {
    fn default() -> Self {
        Self::DemoCity
    }
}

#[rustfmt::skip]
fn cornell_box(_universe: &mut Universe) -> Result<Space, InGenError> {
    // Coordinates are set up based on this dimension because, being blocks, we're not
    // going to *exactly* replicate the original data, but we might want to adjust the
    // scale to something else entirely.
    let box_size = 55;
    // Add one block to all sides for wall thickness.
    let grid = Grid::new(
        (-1, -1, -1),
        GridVector::new(1, 1, 1) * box_size + GridVector::new(2, 2, 2),
    );
    let mut space = Space::builder(grid)
        // There shall be no light but that which we make for ourselves!
        .sky_color(Rgb::ZERO)
        .light_physics(LightPhysics::Rays {
            maximum_distance: (box_size * 2).try_into().unwrap_or(u16::MAX),
        })
        .spawn_position(Point3::<FreeCoordinate>::new(0.5, 0.5, 1.6) * box_size.into())
        .build_empty();

    let white: Block = Rgba::new(1.0, 1.0, 1.0, 1.0).into();
    let red: Block = Rgba::new(0.57, 0.025, 0.025, 1.0).into();
    let green: Block = Rgba::new(0.025, 0.236, 0.025, 1.0).into();
    let light: Block = Block::builder()
        .display_name("Light")
        .light_emission(Rgb::new(40., 40., 40.))
        .color(Rgba::new(1.0, 1.0, 1.0, 1.0))
        .build();

    // Floor.
    space.fill_uniform(Grid::new((0, -1, 0), (box_size, 1, box_size)), &white)?;
    // Ceiling.
    space.fill_uniform(Grid::new((0, box_size, 0), (box_size, 1, box_size)), &white)?;
    // Light in ceiling.
    space.fill_uniform(Grid::from_lower_upper((21, box_size, 23), (34, box_size + 1, 33)), &light)?;
    // Back wall.
    space.fill_uniform(Grid::new((0, 0, -1), (box_size, box_size, 1)), &white)?;
    // Right wall (green).
    space.fill_uniform(Grid::new((box_size, 0, 0), (1, box_size, box_size)), &green)?;
    // Left wall (red).
    space.fill_uniform(Grid::new((-1, 0, 0), (1, box_size, box_size)), &red)?;

    // Block #1
    space.fill_uniform(Grid::new((29, 0, 36), (16, 16, 15)), &white)?;
    // Block #2
    space.fill_uniform(Grid::new((10, 0, 13), (18, 33, 15)), &white)?;

    // TODO: Explicitly define camera position (needs a means to do so).

    Ok(space)
}

fn new_universe_with_space_setup<F>(space_fn: F) -> Result<Universe, GenError>
where
    F: FnOnce(&mut Universe) -> Result<Space, InGenError>,
{
    let mut universe = Universe::new();
    install_demo_blocks(&mut universe)?;

    let space_name1: Name = "space".into();
    let space_name2 = space_name1.clone();
    let space: Space = space_fn(&mut universe).map_err(|e| GenError::failure(e, space_name1))?;
    let space_ref = universe.insert(space_name2, space)?;

    // TODO: "character" is a special default name used for finding the character the
    // player actually uses, and we should replace that or handle it more formally.
    universe.insert("character".into(), Character::spawn_default(space_ref))?;

    Ok(universe)
}

/// Generate a space which is both completely enclosed and has a convenient flat surface
/// for adding stuff to and, as the name suggests, testing physics.
///
/// TODO: The original premise of this was that it would be useful as a default setup for
/// physics tests. But it hasn't actually been used for that. Evaluate whether it was ever
/// a good idea.
/// TODO: Add some lights.
/// TODO: Define exactly what the radii mean so users can build things on surfaces.
fn physics_lab(shell_radius: u16, planet_radius: u16) -> Result<Space, InGenError> {
    assert!(shell_radius > planet_radius);
    let space_radius = shell_radius + 1; // TODO check off-by-one consistency
    let mut space = Space::empty(Grid::new(
        GridPoint::new(-1, -1, -1) * space_radius.into(),
        GridVector::new(1, 1, 1) * (space_radius * 2 + 1).into(),
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
        space.set(p, &outer_wall_block)
    };
    for x in -shell_radius..=shell_radius {
        for y in -shell_radius..=shell_radius {
            if x.abs() == shell_radius || y.abs() == shell_radius {
                for z in -shell_radius..=shell_radius {
                    place_outer_wall((x, y, z))?;
                }
            } else {
                place_outer_wall((x, y, shell_radius))?;
                place_outer_wall((x, y, -shell_radius))?;
            }
        }
    }

    // Inner surface.
    space.fill(
        Grid::from_lower_upper(
            GridPoint::new(-1, -1, -1) * planet_radius,
            GridPoint::new(1, 1, 1) * planet_radius,
        ),
        |GridPoint { x, y, z }| {
            Some(if (x + y + z).rem_euclid(2) == 0 {
                &floor_1
            } else {
                &floor_2
            })
        },
    )?;

    let spawn = space.spawn_mut();
    spawn.set_eye_position(Point3::new(
        0.,
        FreeCoordinate::from(planet_radius) + 2.,
        0.,
    ));
    spawn.set_flying(false);

    Ok(space)
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::apps::Tick;
    use strum::IntoEnumIterator as _;

    #[test]
    pub fn template_smoke_test() {
        for template in UniverseTemplate::iter() {
            eprintln!("{:?}", template);

            let result = template.clone().build();
            if matches!(template, UniverseTemplate::Fail) {
                result.unwrap_err();
                continue;
            }
            let mut u = result.unwrap();

            if template != UniverseTemplate::Blank {
                let _ = u.get_default_character().unwrap().borrow();
            }
            u.step(Tick::arbitrary());
        }
    }
}
