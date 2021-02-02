// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! A space with a lot of lot of demonstrations of functionality.

use std::error::Error;

use cgmath::{
    Basis2, EuclideanSpace as _, InnerSpace as _, One as _, Rad, Rotation as _, Rotation2,
    Transform as _, Vector2, Vector3,
};
use embedded_graphics::fonts::{Font8x16, Text};
use embedded_graphics::geometry::Point;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::style::TextStyleBuilder;
use ordered_float::NotNan;

use crate::block::{space_to_blocks, Block, BlockAttributes, BlockCollision, AIR};
use crate::content::{axes, logo_text, wavy_landscape, DemoBlocks, LandscapeBlocks};
use crate::drawing::{draw_to_blocks, VoxelBrush};
use crate::linking::BlockProvider;
use crate::math::{
    Face, FaceMap, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridRotation, GridVector,
    Rgb, Rgba,
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
    space.fill(landscape_region, |_| Some(&AIR)).unwrap();
    wavy_landscape(landscape_region, &mut space, &landscape_blocks, 1.0);
    planner.occupied_plots.push(landscape_region);

    // Exhibits
    for exhibit in DEMO_CITY_EXHIBITS.iter() {
        let enclosure_footprint = exhibit.footprint.expand(FaceMap::generate(|_| 1));

        let plot_transform = planner
            .find_plot(enclosure_footprint)
            .expect("Out of city space!");
        let plot = exhibit.footprint.transform(plot_transform).unwrap();

        // Mark the exhibit bounds
        // TODO: Design a unique block for this
        let enclosure = Grid::from_lower_upper(
            plot.lower_bounds().map(|x| x - 1),
            [plot.upper_bounds().x + 1, 1, plot.upper_bounds().z + 1],
        );
        space
            .fill(enclosure, |_| {
                Some(&*landscape_blocks[LandscapeBlocks::Stone])
            })
            .unwrap();

        // TODO: Add "entrances" so it's clear what the "front" of the exhibit is supposed to be.

        // Draw exhibit name
        let name_transform = GridMatrix::from_translation([
            exhibit.footprint.lower_bounds().x - 1,
            0,
            exhibit.footprint.upper_bounds().z + 2,
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
        let exhibit_space = (exhibit.factory)(exhibit, universe)
            .expect("exhibit generation failure. TODO: place an error marker and continue instead");
        space_to_space_copy(
            &exhibit_space,
            exhibit.footprint,
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
struct Exhibit {
    name: &'static str,
    footprint: Grid,
    factory: fn(&Exhibit, &mut Universe) -> Result<Space, Box<dyn Error>>,
}

static DEMO_CITY_EXHIBITS: &[Exhibit] = &[
    Exhibit {
        name: "Transparency WIP",
        footprint: Grid::new_c([-3, 0, -3], [7, 5, 7]),
        factory: |this, _universe| {
            let mut space = Space::empty(this.footprint);

            let glass = Block::from(Rgba::new(0.9, 0.9, 0.9, 0.25));
            for rot in GridRotation::CLOCKWISE.iterate() {
                let windowpane = Grid::from_lower_upper([-1, 0, 3], [2, 5, 4]);
                space.fill(
                    windowpane
                        .transform(rot.to_positive_octant_matrix(1))
                        .unwrap(),
                    |_| Some(&glass),
                )?;
            }

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
                universe,
                16,
                8,
                BlockAttributes::default(),
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
                        0,
                        BlockAttributes {
                            display_name: resolution.to_string().into(),
                            collision: BlockCollision::None,
                            ..BlockAttributes::default()
                        },
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
