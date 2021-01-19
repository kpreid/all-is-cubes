// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! First-run game content. (Well, all runs, since we don't have saving yet.)

use std::error::Error;

use cgmath::{
    Basis2, EuclideanSpace as _, InnerSpace as _, Rad, Rotation, Rotation2, Vector2, Vector3,
};
use embedded_graphics::fonts::Font8x16;
use embedded_graphics::fonts::Text;
use embedded_graphics::geometry::Point;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::style::TextStyleBuilder;
use ordered_float::NotNan;

use crate::block::{space_to_blocks, Block, BlockAttributes, BlockCollision, AIR};
use crate::camera::Camera;
use crate::content::axes;
use crate::content::blocks::{install_demo_blocks, DemoBlocks};
use crate::content::landscape::{wavy_landscape, LandscapeBlocks};
use crate::content::logo_text;
use crate::drawing::{draw_to_blocks, VoxelBrush};
use crate::linking::BlockProvider;
use crate::math::{
    Face, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridRotation, GridVector, Rgb,
    Rgba,
};
use crate::raycast::Raycaster;
use crate::space::{Grid, Space};
use crate::tools::Tool;
use crate::universe::{Name, Universe, UniverseIndex};

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
    DemoCity,
    CornellBox,
    PhysicsLab,
    // TODO: add an "nothing, you get a blank editor" option once we have enough editing support.
}

impl UniverseTemplate {
    pub fn build(self) -> Universe {
        use UniverseTemplate::*;
        match self {
            DemoCity => new_universe_with_space_setup(demo_city),
            CornellBox => new_universe_with_space_setup(cornell_box),
            PhysicsLab => new_universe_with_space_setup(|_| physics_lab(50, 16)),
        }
    }
}

fn demo_city(universe: &mut Universe) -> Space {
    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe).unwrap();
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe).unwrap();
    use DemoBlocks::*;

    // Layout parameters
    let road_radius = 2;
    let lamp_position_radius = road_radius + 2;
    let exhibit_front_radius = lamp_position_radius + 2;
    let lamp_spacing = 20;
    let sky_height = 30;
    let ground_depth = 30; // TODO: wavy_landscape is forcing us to have extra symmetry here
    let radius_xz = 60;

    // Prepare brushes.
    let lamp_brush = VoxelBrush::new(vec![
        ((0, 0, 0), &*demo_blocks[Lamppost]),
        ((0, 1, 0), &*demo_blocks[Lamppost]),
        ((0, 2, 0), &*demo_blocks[Lamppost]),
        ((0, 3, 0), &*demo_blocks[Lamp]),
    ]);

    // Construct space.
    let mut space = Space::empty(Grid::from_lower_upper(
        (-radius_xz, -ground_depth, -radius_xz),
        (radius_xz, sky_height, radius_xz),
    ));
    space.set_sky_color(Rgb::new(0.9, 0.9, 1.4));

    // Fill in flat ground
    space
        .fill(
            Grid::from_lower_upper(
                (-radius_xz, -ground_depth, -radius_xz),
                (radius_xz, 0, radius_xz),
            ),
            |_| Some(landscape_blocks[LandscapeBlocks::Stone].clone()), // TODO: fix design so no clone needed
        )
        .unwrap();
    space
        .fill(
            Grid::from_lower_upper((-radius_xz, 0, -radius_xz), (radius_xz, 1, radius_xz)),
            |_| Some(landscape_blocks[LandscapeBlocks::Grass].clone()), // TODO: fix design so no clone needed
        )
        .unwrap();

    // Roads and lamps
    for &face in &[Face::PX, Face::NX, Face::PZ, Face::NZ] {
        let forward: GridVector = face.normal_vector();
        let raycaster = Raycaster::new((0.5, 0.5, 0.5), face.normal_vector::<FreeCoordinate>())
            .within_grid(space.grid());
        for (i, step) in raycaster.enumerate() {
            let i = i as GridCoordinate;
            let perpendicular: GridVector = forward.cross(Face::PY.normal_vector());
            for p in -road_radius..=road_radius {
                space
                    .set(step.cube_ahead() + perpendicular * p, &*demo_blocks[Road])
                    .unwrap();
            }
            if i > road_radius {
                // Curbs
                for (side, &p) in [-(road_radius + 1), road_radius + 1].iter().enumerate() {
                    // TODO: blocks should have a .rotate() method
                    let mut rotation =
                        GridRotation::from_basis([face.cross(Face::PY), Face::PY, face]);
                    if side == 0 {
                        rotation =
                            rotation * GridRotation::from_basis([Face::NX, Face::PY, Face::NZ]);
                    }
                    let r = Block::Rotated(rotation, Box::new((*demo_blocks[Curb]).clone()));
                    space
                        .set(
                            step.cube_ahead() + perpendicular * p + GridVector::unit_y(),
                            r,
                        )
                        .unwrap();
                }
            }

            if (i - lamp_position_radius) % lamp_spacing == 0 {
                for p in &[-lamp_position_radius, lamp_position_radius] {
                    lamp_brush
                        .paint(
                            &mut space,
                            step.cube_ahead() + GridVector::new(0, 1, 0) + perpendicular * *p,
                        )
                        .unwrap();
                }
            }
        }
    }

    axes(&mut space);

    // Landscape filling one quadrant
    let landscape_region = Grid::from_lower_upper(
        [-radius_xz, -ground_depth * 8 / 10, -radius_xz],
        [-exhibit_front_radius, sky_height, -exhibit_front_radius],
    );
    space.fill(landscape_region, |_| Some(&AIR)).unwrap();
    wavy_landscape(landscape_region, &mut space, &landscape_blocks, 1.0);

    // Exhibits
    // TODO: Generalize this so it can use all directions and sides of roads
    let mut exhibit_x = exhibit_front_radius + 1;
    for exhibit in DEMO_CITY_EXHIBITS {
        // Align lower bound of footprint with starting point
        exhibit_x -= exhibit.footprint.lower_bounds().x;
        let translation = GridVector::new(
            exhibit_x,
            1,
            -exhibit_front_radius - exhibit.footprint.upper_bounds().z,
        );
        let translated_footprint = exhibit.footprint.translate(translation);

        // Mark the exhibit bounds
        // TODO: Design a unique block for this
        let enclosure = Grid::from_lower_upper(
            translated_footprint.lower_bounds().map(|x| x - 1),
            [
                translated_footprint.upper_bounds().x + 1,
                1,
                translated_footprint.upper_bounds().z + 1,
            ],
        );
        space
            .fill(enclosure, |_| {
                Some(&*landscape_blocks[LandscapeBlocks::Stone])
            })
            .unwrap();

        // Write exhibit content
        let exhibit_space = (exhibit.factory)(exhibit, universe)
            .expect("TODO: place an error marker and continue instead");
        space
            .fill(translated_footprint, |p| {
                Some(&exhibit_space[p - translation])
            })
            .expect("TODO: place an error marker and continue instead");

        // Line up for the next exhibit
        exhibit_x += exhibit.footprint.upper_bounds().x + 3;
    }

    logo_text(
        GridMatrix::from_translation([0, 12, -radius_xz]),
        &mut space,
    );

    space
}

#[allow(clippy::type_complexity)]
struct Exhibit {
    name: &'static str,
    footprint: Grid,
    factory: fn(&Exhibit, &mut Universe) -> Result<Space, Box<dyn Error>>,
}

static DEMO_CITY_EXHIBITS: &[Exhibit] = &[
    Exhibit {
        name: "Knot",
        footprint: Grid::new_c([-2, -2, -1], [5, 5, 3]),
        factory: |this, universe| {
            let resolution = 16;
            let toroidal_radius = 24.;
            let knot_split_radius = 9.;
            let strand_radius = 6.;
            let twists = 2.5;

            let mut drawing_space = Space::empty(this.footprint.multiply(resolution));
            let paint = Block::from(Rgba::new(0.9, 0.9, 0.9, 1.0));
            drawing_space.fill(drawing_space.grid(), |p| {
                // Measure from midpoint of odd dimension space
                let p = p - Vector3::new(1, 1, 1) * (resolution / 2);
                // Work in floating point
                let p = p.map(FreeCoordinate::from);

                let cylindrical = Vector2::new((p.x.powi(2) + p.y.powi(2)).sqrt(), p.z);
                let torus_cross_section = cylindrical - Vector2::new(toroidal_radius, 0.);
                let angle = Rad(p.x.atan2(p.y));
                let rotated_cross_section =
                    Basis2::from_angle(angle * twists).rotate_vector(torus_cross_section);
                let knot_center_1 = rotated_cross_section - Vector2::new(knot_split_radius, 0.);
                let knot_center_2 = rotated_cross_section + Vector2::new(knot_split_radius, 0.);

                if knot_center_1.magnitude() < strand_radius
                    || knot_center_2.magnitude() < strand_radius
                {
                    Some(&paint)
                } else {
                    None
                }
            })?;
            let space = space_to_blocks(
                16,
                BlockAttributes {
                    display_name: this.name.into(),
                    collision: BlockCollision::None,
                    ..BlockAttributes::default()
                },
                universe.insert_anonymous(drawing_space),
            )?;
            Ok(space)
        },
    },
    Exhibit {
        name: "Text",
        footprint: Grid::new_c([0, 0, 0], [9, 1, 1]),
        factory: |_, universe| {
            let space = draw_to_blocks(
                universe,
                16,
                Text::new("Hello block world", Point::new(0, -16)).into_styled(
                    TextStyleBuilder::new(Font8x16)
                        .text_color(Rgb888::new(120, 100, 200))
                        .build(),
                ),
            )?;
            Ok(space)
        },
    },
    Exhibit {
        name: "Resolutions",
        footprint: Grid::new_c([0, 0, 0], [5, 2, 3]),
        factory: |this, universe| {
            let mut space = Space::empty(this.footprint);

            for (i, &resolution) in [1, 2, 3, 8, 16, 32].iter().enumerate() {
                let i = i as GridCoordinate;
                let location = GridPoint::new(i.rem_euclid(3) * 2, 0, i.div_euclid(3) * 2);
                space.set(
                    location,
                    Block::builder()
                        .voxels_fn(universe, resolution, |p| {
                            if p.x + p.y + p.z >= GridCoordinate::from(resolution) {
                                return AIR.clone();
                            }
                            let rescale = if resolution > 8 { 4 } else { 1 };
                            let color = Rgb::from(p.to_vec().map(|s| {
                                NotNan::new(
                                    (s / GridCoordinate::from(rescale)) as f32
                                        / f32::from(resolution / rescale - 1).max(1.),
                                )
                                .unwrap()
                            }));
                            Block::from(color)
                        })?
                        .build(),
                )?;

                space.set(
                    location + GridVector::unit_y(),
                    &draw_to_blocks(
                        universe,
                        16,
                        Text::new(&resolution.to_string(), Point::new(0, -16)).into_styled(
                            TextStyleBuilder::new(Font8x16)
                                .text_color(Rgb888::new(10, 10, 10))
                                .build(),
                        ),
                    )?[GridPoint::origin()],
                )?;
            }

            Ok(space)
        },
    },
];

#[rustfmt::skip]
fn cornell_box(_universe: &mut Universe) -> Space {
    // Coordinates are set up based on this dimension because, being blocks, we're not
    // going to *exactly* replicate the original data, but we might want to adjust the
    // scale to something else entirely.
    let box_size = 55;
    // Add one block to all sides for wall thickness.
    let grid = Grid::new(
        (-1, -1, -1),
        GridVector::new(1, 1, 1) * box_size + GridVector::new(2, 2, 2),
    );
    let mut space = Space::empty(grid);
    // There shall be no light but that which we make for ourselves!
    space.set_sky_color(Rgb::new(0., 0., 0.));

    let white: Block = Rgba::new(1.0, 1.0, 1.0, 1.0).into();
    let red: Block = Rgba::new(0.57, 0.025, 0.025, 1.0).into();
    let green: Block = Rgba::new(0.025, 0.236, 0.025, 1.0).into();
    let light: Block = Block::builder()
        .display_name("Light")
        .light_emission(Rgb::new(100., 100., 100.))
        .color(Rgba::new(1.0, 1.0, 1.0, 1.0))
        .build();

    // Floor.
    space.fill(Grid::new((0, -1, 0), (box_size, 1, box_size)), |_| Some(&white)).unwrap();
    // Ceiling.
    space.fill(Grid::new((0, box_size, 0), (box_size, 1, box_size)), |_| Some(&white)).unwrap();
    // Light in ceiling.
    space.fill(Grid::from_lower_upper((21, box_size, 23), (34, box_size + 1, 33)), |_| Some(&light)).unwrap();
    // Back wall.
    space.fill(Grid::new((0, 0, -1), (box_size, box_size, 1)), |_| Some(&white)).unwrap();
    // Right wall (green).
    space.fill(Grid::new((box_size, 0, 0), (1, box_size, box_size)), |_| Some(&green)).unwrap();
    // Left wall (red).
    space.fill(Grid::new((-1, 0, 0), (1, box_size, box_size)), |_| Some(&red)).unwrap();

    // Block #1
    space.fill(Grid::new((29, 0, 36), (16, 16, 15)), |_| Some(&white)).unwrap();
    // Block #2
    space.fill(Grid::new((10, 0, 13), (18, 33, 15)), |_| Some(&white)).unwrap();

    // TODO: Explicitly define camera.

    space
}

fn new_universe_with_space_setup<F>(space_fn: F) -> Universe
where
    F: FnOnce(&mut Universe) -> Space,
{
    let mut universe = Universe::new();
    install_demo_blocks(&mut universe).unwrap();

    let space: Space = space_fn(&mut universe);
    // TODO: this position should be configurable; it makes some sense to have a "spawn point" per Space
    let position = space.grid().center() + Vector3::new(0.5, 2.91, 8.5);
    let space_ref = universe.insert("space".into(), space).unwrap();

    //let camera = Camera::looking_at_space(space_ref, Vector3::new(0.5, 0.5, 1.0));
    let mut camera = Camera::new(space_ref, position);
    // Copy all named block defs into inventory.
    for (name, block_def_ref) in universe.iter_by_type() {
        if matches!(name, Name::Anonym(_)) {
            continue;
        }
        match camera.try_add_item(Tool::PlaceBlock(Block::Indirect(block_def_ref))) {
            Ok(()) => {}
            Err(_) => {
                // Out of space
                break;
            }
        }
    }
    universe.insert("camera".into(), camera).unwrap();

    universe
}

/// Generate a space which is both completely enclosed and has a convenient flat surface
/// for adding stuff to and, as the name suggests, testing physics.
///
/// TODO: The original premise of this was that it would be useful as a default setup for
/// physics tests. But it hasn't actually been used for that. Evaluate whether it was ever
/// a good idea.
/// TODO: Add some lights.
/// TODO: Define exactly what the radii mean so users can build things on surfaces.
fn physics_lab(shell_radius: u16, planet_radius: u16) -> Space {
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
    space
        .fill(
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
        )
        .unwrap();

    space
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;
    use strum::IntoEnumIterator as _;

    #[test]
    pub fn template_smoke_test() {
        for template in UniverseTemplate::iter() {
            let mut u = template.build();
            let _ = u.get_default_camera().borrow();
            let _ = u.get_default_space().borrow();
            u.step(Duration::from_millis(10));
        }
    }
}
