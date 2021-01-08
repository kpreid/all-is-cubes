// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! First-run game content. (Well, all runs, since we don't have saving yet.)

use std::error::Error;

use cgmath::{Basis2, InnerSpace, Rad, Rotation, Rotation2, Vector2, Vector3};
use embedded_graphics::fonts::Font8x16;
use embedded_graphics::fonts::Text;
use embedded_graphics::geometry::Point;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::style::TextStyleBuilder;

use crate::block::{space_to_blocks, Block, BlockAttributes, BlockCollision, AIR};
use crate::blockgen::{BlockGen, LandscapeBlocks};
use crate::camera::Camera;
use crate::content::blocks::{install_demo_blocks, DemoBlocks};
use crate::content::logo_text;
use crate::drawing::{draw_to_blocks, VoxelBrush};
use crate::linking::BlockProvider;
use crate::math::{Face, FreeCoordinate, GridCoordinate, GridPoint, GridVector, Rgb, Rgba};
use crate::raycast::Raycaster;
use crate::space::{Grid, Space};
use crate::tools::Tool;
use crate::universe::{Name, Universe, UniverseIndex};
use crate::worldgen::{axes, wavy_landscape};

/// Creates a [`Universe`] with some content for a "new game", as much as that can exist.
pub fn new_universe_with_stuff() -> Universe {
    new_universe_with_space_setup(demo_city)
}

fn demo_city(universe: &mut Universe) -> Space {
    let bg = BlockGen {
        universe,
        resolution: 16,
    };
    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(bg.universe).unwrap();
    let demo_blocks = BlockProvider::<DemoBlocks>::using(bg.universe).unwrap();
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
    for face in &[Face::PX, Face::NX, Face::PZ, Face::NZ] {
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

    logo_text(GridPoint::new(0, 12, -radius_xz), &mut space);

    space
}

struct Exhibit {
    name: &'static str,
    footprint: Grid,
    factory: fn(&Exhibit, &mut Universe) -> Result<Space, Box<dyn Error>>,
}

static DEMO_CITY_EXHIBITS: &[Exhibit] = &[
    Exhibit {
        name: "Placeholder",
        footprint: Grid::new_c([0, 0, 0], [1, 1, 1]),
        factory: |_, universe| {
            let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
            let mut space = Space::empty_positive(1, 1, 1);
            space.set([0, 0, 0], demo_blocks[DemoBlocks::Lamp].clone())?;
            Ok(space)
        },
    },
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
                &mut BlockGen {
                    universe,
                    resolution: 16,
                },
                Text::new("Hello block world", Point::new(0, -16)).into_styled(
                    TextStyleBuilder::new(Font8x16)
                        .text_color(Rgb888::new(120, 100, 200))
                        .build(),
                ),
            )?;
            Ok(space)
        },
    },
];

#[rustfmt::skip]
#[allow(unused)]  // TODO: Make a scene selector menu somehow so this can be used without recompiling.
fn cornell_box(universe: &mut Universe) -> Space {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    /// Check that it doesn't panic, at least.
    #[test]
    pub fn new_universe_smoke_test() {
        let mut u = new_universe_with_stuff();
        let _ = u.get_default_camera().borrow();
        let _ = u.get_default_space().borrow();
        u.step(Duration::from_millis(10));
    }
}
