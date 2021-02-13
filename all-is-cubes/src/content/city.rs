// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! A space with miscellaneous demonstrations/tests of functionality.
//! The individual buildings/exhibits are defined in [`DEMO_CITY_EXHIBITS`].

use std::error::Error;

use cgmath::{EuclideanSpace as _, One as _, Transform as _};
use embedded_graphics::fonts::{Font8x16, Text};
use embedded_graphics::geometry::Point;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::style::TextStyleBuilder;

use crate::block::{BlockAttributes, BlockCollision, AIR};
use crate::content::{
    axes, logo_text, wavy_landscape, DemoBlocks, LandscapeBlocks, DEMO_CITY_EXHIBITS,
};
use crate::drawing::{draw_to_blocks, VoxelBrush};
use crate::linking::BlockProvider;
use crate::math::{
    Face, FaceMap, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridRotation, GridVector,
    Rgb,
};
use crate::raycast::Raycaster;
use crate::space::{Grid, SetCubeError, Space};
use crate::universe::Universe;

pub(crate) fn demo_city(universe: &mut Universe) -> Space {
    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe).unwrap();
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe).unwrap();
    use DemoBlocks::*;

    // Layout parameters
    // TODO: move to CityPlanner
    let road_radius = CityPlanner::ROAD_RADIUS;
    let lamp_position_radius = CityPlanner::LAMP_POSITION_RADIUS;
    let exhibit_front_radius = CityPlanner::PLOT_FRONT_RADIUS;
    let lamp_spacing = 20;
    let sky_height = 30;
    let ground_depth = 30; // TODO: wavy_landscape is forcing us to have extra symmetry here
    let radius_xz = 60;
    let grid = Grid::from_lower_upper(
        (-radius_xz, -ground_depth, -radius_xz),
        (radius_xz, sky_height, radius_xz),
    );

    let mut planner = CityPlanner::new(grid);

    // Prepare brushes.
    let lamp_brush = VoxelBrush::new(vec![
        ((0, 0, 0), &*demo_blocks[Lamppost]),
        ((0, 1, 0), &*demo_blocks[Lamppost]),
        ((0, 2, 0), &*demo_blocks[Lamppost]),
        ((0, 3, 0), &*demo_blocks[Lamp]),
    ]);

    // Construct space.
    let mut space = Space::empty(grid);
    space.set_sky_color(Rgb::new(0.9, 0.9, 1.4));

    // Fill in flat ground
    space
        .fill_uniform(
            Grid::from_lower_upper(
                (-radius_xz, -ground_depth, -radius_xz),
                (radius_xz, 0, radius_xz),
            ),
            &*landscape_blocks[LandscapeBlocks::Stone],
        )
        .unwrap();
    space
        .fill_uniform(
            Grid::from_lower_upper((-radius_xz, 0, -radius_xz), (radius_xz, 1, radius_xz)),
            &*landscape_blocks[LandscapeBlocks::Grass],
        )
        .unwrap();

    // Roads and lamps
    for &face in &[Face::PX, Face::NX, Face::PZ, Face::NZ] {
        let forward: GridVector = face.normal_vector();
        let perpendicular: GridVector = forward.cross(Face::PY.normal_vector());
        let raycaster = Raycaster::new((0.5, 0.5, 0.5), face.normal_vector::<FreeCoordinate>())
            .within_grid(space.grid());
        let curb_y = GridVector::unit_y();
        for (i, step) in raycaster.enumerate() {
            let i = i as GridCoordinate;
            for p in -road_radius..=road_radius {
                space
                    .set(step.cube_ahead() + perpendicular * p, &*demo_blocks[Road])
                    .unwrap();
            }
            if i > road_radius {
                // Curbs
                for (side, &p) in [-(road_radius + 1), road_radius + 1].iter().enumerate() {
                    // TODO: should be able to express this in look-at terms.
                    let mut curb = (*demo_blocks[Curb])
                        .clone()
                        .rotate(GridRotation::from_basis([
                            face.cross(Face::PY),
                            Face::PY,
                            face,
                        ]));
                    if side == 0 {
                        curb =
                            curb.rotate(GridRotation::from_basis([Face::NX, Face::PY, Face::NZ]));
                    }
                    space
                        .set(step.cube_ahead() + perpendicular * p + curb_y, curb)
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

        // Patch up curb corners
        for &p in &[-(road_radius + 1), road_radius + 1] {
            space
                .set(
                    GridPoint::origin() + curb_y + forward * (road_radius + 1) + perpendicular * p,
                    &*demo_blocks[CurbCorner],
                )
                .unwrap();
        }
    }

    axes(&mut space);

    // Landscape filling one quadrant
    let landscape_region = Grid::from_lower_upper(
        [-radius_xz, -ground_depth * 8 / 10, -radius_xz],
        [-exhibit_front_radius, sky_height, -exhibit_front_radius],
    );
    space.fill_uniform(landscape_region, AIR).unwrap();
    wavy_landscape(landscape_region, &mut space, &landscape_blocks, 1.0);
    planner.occupied_plots.push(landscape_region);

    // Exhibits
    for exhibit in DEMO_CITY_EXHIBITS.iter() {
        let exhibit_space = (exhibit.factory)(exhibit, universe)
            .expect("exhibit generation failure. TODO: place an error marker and continue instead");
        let exhibit_footprint = exhibit_space.grid();

        let enclosure_footprint = exhibit_footprint.expand(FaceMap::generate(|_| 1));

        let plot_transform = planner
            .find_plot(enclosure_footprint)
            .expect("Out of city space!");
        let plot = exhibit_footprint.transform(plot_transform).unwrap();

        // Mark the exhibit bounds
        // TODO: Design a unique block for this
        let enclosure = Grid::from_lower_upper(
            plot.lower_bounds().map(|x| x - 1),
            [plot.upper_bounds().x + 1, 1, plot.upper_bounds().z + 1],
        );
        space
            .fill_uniform(enclosure, &*landscape_blocks[LandscapeBlocks::Stone])
            .unwrap();

        // TODO: Add "entrances" so it's clear what the "front" of the exhibit is supposed to be.

        // Draw exhibit name
        let name_transform = GridMatrix::from_translation([
            exhibit_footprint.lower_bounds().x - 1,
            0,
            exhibit_footprint.upper_bounds().z + 2,
        ]) * GridRotation::from_basis([Face::PX, Face::NZ, Face::PY])
            .to_rotation_matrix();
        let name_blocks = draw_to_blocks(
            universe,
            16, // TODO: use a higher resolution once it's supported
            0,
            BlockAttributes {
                display_name: format!("Exhibit name {:?}", exhibit.name).into(),
                collision: BlockCollision::None,
                ..BlockAttributes::default()
            },
            Text::new(exhibit.name, Point::new(0, -16)).into_styled(
                TextStyleBuilder::new(Font8x16)
                    .text_color(Rgb888::new(0, 0, 0))
                    .build(),
            ),
        )
        .expect("name drawing failure");
        // TODO: give this an offset of a suitable rotation that won't collide...
        space_to_space_copy(
            &name_blocks,
            name_blocks.grid(),
            &mut space,
            plot_transform * name_transform,
        )
        .expect("name drawing failure");

        // Place exhibit content
        space_to_space_copy(
            &exhibit_space,
            exhibit_footprint,
            &mut space,
            plot_transform,
        )
        .expect("copy failure. TODO: place an error marker and continue instead");
    }

    logo_text(
        GridMatrix::from_translation([0, 12, -radius_xz]),
        &mut space,
    );

    space
}

// TODO: move this since it is a generally useful utility
fn space_to_space_copy(
    src: &Space,
    src_grid: Grid,
    dst: &mut Space,
    src_to_dst_transform: GridMatrix,
) -> Result<(), SetCubeError> {
    // TODO: don't panic
    let dst_to_src_transform = src_to_dst_transform.inverse_transform().unwrap();
    let (block_rotation, _) = dst_to_src_transform
        .decompose()
        .expect("could not decompose transform");
    dst.fill(src_grid.transform(src_to_dst_transform).unwrap(), |p| {
        Some(
            src[dst_to_src_transform.transform_cube(p)]
                .clone()
                .rotate(block_rotation),
        )
    })
}

#[allow(clippy::type_complexity)]
pub(crate) struct Exhibit {
    pub name: &'static str,
    pub factory: fn(&Exhibit, &mut Universe) -> Result<Space, Box<dyn Error>>,
}

/// Tracks available land while the city is being generated.
#[derive(Clone, Debug, PartialEq)]
struct CityPlanner {
    space_grid: Grid,
    /// Count of blocks beyond the origin that are included in the city space.
    city_radius: GridCoordinate, // TODO redundant with grid
    /// Each plot/exhibit that has already been placed. (This could be a spatial data structure but we're not that big yet.)
    occupied_plots: Vec<Grid>,
}

impl CityPlanner {
    const ROAD_RADIUS: GridCoordinate = 2;
    const LAMP_POSITION_RADIUS: GridCoordinate = Self::ROAD_RADIUS + 2;
    const PLOT_FRONT_RADIUS: GridCoordinate = Self::LAMP_POSITION_RADIUS + 2;
    const GAP_BETWEEN_PLOTS: GridCoordinate = 1;

    pub fn new(space_grid: Grid) -> Self {
        let city_radius = space_grid.upper_bounds().x; // TODO: compare everything and take the max

        let mut occupied_plots = Vec::new();
        let road = Grid::from_lower_upper(
            [-Self::LAMP_POSITION_RADIUS, 0, -city_radius],
            [Self::LAMP_POSITION_RADIUS + 1, 2, city_radius + 1],
        );
        occupied_plots.push(road);
        occupied_plots.push(
            road.transform(GridRotation::CLOCKWISE.to_rotation_matrix())
                .unwrap(),
        );
        Self {
            space_grid,
            city_radius,
            occupied_plots,
        }
    }

    pub fn find_plot(&mut self, plot_shape: Grid) -> Option<GridMatrix> {
        // TODO: We'd like to resume the search from _when we left off_, but that's tricky since a
        // smaller plot might fit where a large one didn't. So, quadratic search it is for now.
        for d in 0..=self.city_radius {
            for street_axis in GridRotation::COUNTERCLOCKWISE.iterate() {
                // TODO exercising opposite sides logic
                'search: for &left_side in &[false, true] {
                    // The translation is expressed along the +X axis street, so
                    // "left" is -Z and "right" is +Z.
                    let mut transform = GridMatrix::from_translation(GridVector::new(
                        d,
                        1,
                        if left_side {
                            -Self::PLOT_FRONT_RADIUS - plot_shape.upper_bounds().z
                        } else {
                            Self::PLOT_FRONT_RADIUS + plot_shape.upper_bounds().z
                        },
                    )) * if left_side {
                        GridMatrix::one()
                    } else {
                        (GridRotation::COUNTERCLOCKWISE * GridRotation::COUNTERCLOCKWISE)
                            .to_rotation_matrix()
                    };
                    // Rotate to match street
                    transform = street_axis.to_rotation_matrix() * transform;

                    let transformed = plot_shape
                        .transform(transform)
                        .expect("can't happen: city plot transformation failure");

                    if !self.space_grid.contains_grid(transformed) {
                        continue 'search;
                    }

                    let for_occupancy_check =
                        transformed.expand(FaceMap::generate(|_| Self::GAP_BETWEEN_PLOTS));

                    for occupied in self.occupied_plots.iter() {
                        if occupied.intersection(for_occupancy_check).is_some() {
                            continue 'search;
                        }
                    }

                    self.occupied_plots.push(transformed);
                    return Some(transform);
                }
            }
        }
        None
    }
}
