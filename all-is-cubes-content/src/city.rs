// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! A space with miscellaneous demonstrations/tests of functionality.
//! The individual buildings/exhibits are defined in [`DEMO_CITY_EXHIBITS`].

use futures_core::future::LocalBoxFuture;
use instant::Instant;
use noise::Seedable as _;

use all_is_cubes::cgmath::{EuclideanSpace as _, One as _, Transform as _, Vector3};
use all_is_cubes::drawing::embedded_graphics::{
    geometry::Point,
    mono_font::{iso_8859_1::FONT_9X18_BOLD, MonoTextStyle},
    prelude::{Dimensions, Transform},
    text::{Baseline, Text},
};

use all_is_cubes::block::{Block, Resolution};
use all_is_cubes::block::{BlockAttributes, BlockCollision, AIR};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::palette;
use all_is_cubes::drawing::{draw_to_blocks, VoxelBrush};
use all_is_cubes::inv::Slot;
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{
    Face, FaceMap, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridRotation, GridVector,
    NoiseFnExt as _, Rgb,
};
use all_is_cubes::raycast::Raycaster;
use all_is_cubes::space::{Grid, LightPhysics, SetCubeError, Space, SpacePhysics};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::{
    logo_text, logo_text_extent, wavy_landscape, DemoBlocks, LandscapeBlocks, DEMO_CITY_EXHIBITS,
};

pub(crate) async fn demo_city(
    universe: &mut Universe,
    p: YieldProgress,
) -> Result<Space, InGenError> {
    let start_city_time = Instant::now();

    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    use DemoBlocks::*;
    use LandscapeBlocks::*;

    // Layout parameters
    // TODO: move to CityPlanner
    let road_radius = CityPlanner::ROAD_RADIUS;
    let lamp_position_radius = CityPlanner::LAMP_POSITION_RADIUS;
    let exhibit_front_radius = CityPlanner::PLOT_FRONT_RADIUS;
    let lamp_spacing = 20;
    let sky_height = 30;
    let ground_depth = 30; // TODO: wavy_landscape is forcing us to have extra symmetry here
    let underground_floor_y = -5;
    let radius_xz = 60;
    let grid = Grid::from_lower_upper(
        (-radius_xz, -ground_depth, -radius_xz),
        (radius_xz, sky_height, radius_xz),
    );

    let mut planner = CityPlanner::new(grid);

    // Prepare brushes.
    let lamp_brush = VoxelBrush::new(vec![
        ((0, 0, 0), &demo_blocks[Lamppost]),
        ((0, 1, 0), &demo_blocks[Lamppost]),
        ((0, 2, 0), &demo_blocks[Lamppost]),
        ((0, 3, 0), &demo_blocks[Lamp]),
    ]);

    // Construct space.
    let mut space = Space::builder(grid)
        .sky_color(Rgb::new(0.9, 0.9, 1.4))
        .light_physics(LightPhysics::None) // disable until we are done with bulk updates
        .spawn({
            // TODO: Add incremental spawn configuration to SpaceBuilder?
            let mut spawn = Spawn::default_for_new_space(grid);
            spawn.set_eye_position(grid.center() + Vector3::new(0.5, 2.91, 8.5));
            // Initial inventory contents. TODO: Make a better list.
            let mut inventory = vec![
                Tool::RemoveBlock { keep: true }.into(),
                Tool::Jetpack { active: false }.into(),
            ];
            for block in [
                &landscape_blocks[Grass],
                &landscape_blocks[Stone],
                &demo_blocks[Lamp],
                &demo_blocks[Arrow],
            ] {
                inventory.push(Slot::stack(40, Tool::Block(block.clone())));
            }
            spawn.set_inventory(inventory);
            spawn
        })
        .build_empty();

    // Fill basic layers, underground and top
    space.fill_uniform(planner.y_range(-ground_depth, 0), &landscape_blocks[Stone])?;
    p.progress(0.1).await;
    space.fill_uniform(planner.y_range(0, 1), &landscape_blocks[Grass])?;
    p.progress(0.2).await;

    // Stray grass
    let grass_noise_v = noise::OpenSimplex::new().set_seed(0x21b5cc6b);
    let grass_noise = noise::ScaleBias::new(&grass_noise_v)
        .set_bias(0.0)
        .set_scale(4.0);
    let grass_threshold = 1.2;
    space.fill(
        Grid::from_lower_upper((-radius_xz, 1, -radius_xz), (radius_xz, 2, radius_xz)),
        |cube| {
            if cube.x.abs() <= road_radius || cube.z.abs() <= road_radius {
                return None;
            }
            if grass_noise.at_cube(cube) > grass_threshold * 2. {
                Some(&landscape_blocks[GrassBlades2])
            } else if grass_noise.at_cube(cube) > grass_threshold {
                Some(&landscape_blocks[GrassBlades1])
            } else {
                None
            }
        },
    )?;
    p.progress(0.3).await;

    // Roads and lamps
    for &face in &[Face::PX, Face::NX, Face::PZ, Face::NZ] {
        let forward: GridVector = face.normal_vector();
        let perpendicular: GridVector = GridRotation::CLOCKWISE.transform(face).normal_vector();
        let road_aligned_rotation = GridRotation::from_to(Face::NZ, face, Face::PY).unwrap();
        let other_side_of_road =
            GridRotation::from_basis([Face::NX, Face::PY, Face::NZ]) * road_aligned_rotation;
        let rotations = [other_side_of_road, road_aligned_rotation];
        let raycaster = Raycaster::new((0.5, 0.5, 0.5), face.normal_vector::<FreeCoordinate>())
            .within_grid(space.grid());
        let curb_y = GridVector::unit_y();
        for (i, step) in raycaster.enumerate() {
            let i = i as GridCoordinate;
            // Road surface
            for p in -road_radius..=road_radius {
                space.set(step.cube_ahead() + perpendicular * p, &demo_blocks[Road])?;
            }

            // Curbs
            if i > road_radius {
                for (side, &p) in [-(road_radius + 1), road_radius + 1].iter().enumerate() {
                    space.set(
                        step.cube_ahead() + perpendicular * p + curb_y,
                        demo_blocks[Curb].clone().rotate(rotations[side]),
                    )?;
                }
            }

            // Lampposts
            if (i - lamp_position_radius) % lamp_spacing == 0 {
                for p in &[-lamp_position_radius, lamp_position_radius] {
                    lamp_brush.paint(
                        &mut space,
                        step.cube_ahead() + GridVector::new(0, 1, 0) + perpendicular * *p,
                    )?;
                }
            }

            // Dig underground passages
            // TODO: They need a connection to the surface
            for p in -road_radius..=road_radius {
                for z in underground_floor_y..0 {
                    space.set(
                        step.cube_ahead() + perpendicular * p + Vector3::unit_y() * z,
                        &AIR,
                    )?;
                }
            }

            // Underground lighting
            if (i - lamp_position_radius) % 7 == 0 {
                // Underground lamps
                for (side, &p) in [-road_radius, road_radius].iter().enumerate() {
                    space.set(
                        step.cube_ahead() + GridVector::new(0, -2, 0) + perpendicular * p,
                        demo_blocks[Sconce]
                            .clone()
                            .rotate(GridRotation::CLOCKWISE * rotations[side]),
                    )?;
                }
            }
        }

        // Patch up curb corners
        for &p in &[-(road_radius + 1), road_radius + 1] {
            space.set(
                GridPoint::origin() + curb_y + forward * (road_radius + 1) + perpendicular * p,
                &demo_blocks[CurbCorner],
            )?;
        }
    }
    p.progress(0.4).await;

    let blank_city_time = Instant::now();
    log::trace!(
        "Blank city took {:.3} s",
        blank_city_time
            .duration_since(start_city_time)
            .as_secs_f32()
    );

    // Landscape filling one quadrant
    let landscape_region = Grid::from_lower_upper(
        [-radius_xz, -ground_depth * 8 / 10, -radius_xz],
        [-exhibit_front_radius, sky_height, -exhibit_front_radius],
    );
    space.fill_uniform(landscape_region, AIR)?;
    p.progress(0.5).await;
    wavy_landscape(landscape_region, &mut space, &landscape_blocks, 1.0)?;
    planner.occupied_plots.push(landscape_region);

    // TODO: Integrate logging and YieldProgress
    let landscape_time = Instant::now();
    log::trace!(
        "Landscape took {:.3} s",
        landscape_time.duration_since(blank_city_time).as_secs_f32()
    );
    let [exhibits_progress, final_progress] = p.finish_and_cut(0.6).await.split(0.8);

    // Exhibits
    for (exhibit, exhibit_progress) in DEMO_CITY_EXHIBITS
        .iter()
        .zip(exhibits_progress.split_evenly(DEMO_CITY_EXHIBITS.len()))
    {
        let start_exhibit_time = Instant::now();
        let exhibit_space = (exhibit.factory)(exhibit, universe)
            .await
            .expect("exhibit generation failure. TODO: place an error marker and continue instead");
        exhibit_progress.progress(0.7).await;

        let exhibit_footprint = exhibit_space.grid();

        let enclosure_footprint = exhibit_footprint.expand(FaceMap::repeat(1));

        let plot_transform = planner
            .find_plot(enclosure_footprint)
            .expect("Out of city space!");
        let (plot_rotation, _) = plot_transform.decompose().unwrap();
        let plot = exhibit_footprint.transform(plot_transform).unwrap();

        // Mark the exhibit bounds
        let enclosure = Grid::from_lower_upper(
            plot.lower_bounds().map(|x| x - 1),
            [
                plot.upper_bounds().x + 1,
                1.max(plot.lower_bounds()[1]), // handles case where plot is floating
                plot.upper_bounds().z + 1,
            ],
        );
        space.fill_uniform(enclosure, &demo_blocks[ExhibitBackground])?;
        exhibit_progress.progress(0.9).await;

        // TODO: Add "entrances" so it's clear what the "front" of the exhibit is supposed to be.

        // Draw exhibit name
        let name_transform = GridMatrix::from_translation([
            exhibit_footprint.lower_bounds().x - 1,
            0,
            exhibit_footprint.upper_bounds().z + 1,
        ]);
        let name_block_resolution = 32;
        let font = &FONT_9X18_BOLD;
        let name_bottom_y =
            (name_block_resolution - font.character_size.height as GridCoordinate) / 2;
        let name_text = Text::with_baseline(
            exhibit.name,
            Point::new(0, -name_bottom_y),
            MonoTextStyle::new(font, palette::ALMOST_BLACK),
            Baseline::Bottom,
        );
        // TODO: This is an awful lot of code to benerate "text is centered on a number of whole blocks"
        let name_width = name_text.bounding_box().size.width as GridCoordinate;
        let name_width_in_blocks: GridCoordinate =
            (name_width + name_block_resolution - 1) / name_block_resolution; // rounding up
        let name_blocks = draw_to_blocks(
            universe,
            name_block_resolution as Resolution,
            0,
            0..1,
            BlockAttributes {
                display_name: format!("Exhibit name {:?}", exhibit.name).into(),
                collision: BlockCollision::Recur,
                ..BlockAttributes::default()
            },
            &name_text.translate(Point::new(
                ((name_width_in_blocks * name_block_resolution) - name_width) / 2,
                0,
            )),
        )
        .expect("name drawing failure");
        // Truncate name to not overrun the exhibit itself
        let truncated_name_grid = name_blocks
            .grid()
            .intersection(Grid::new([0, 0, 0], [exhibit_footprint.size().x + 3, 1, 1]))
            .unwrap();
        space_to_space_copy(
            &name_blocks,
            truncated_name_grid,
            &mut space,
            plot_transform * name_transform,
        )?; // TODO: on failure, place an error marker and continue
        space.fill_uniform(
            truncated_name_grid
                .transform(
                    plot_transform * name_transform * GridMatrix::from_translation([0, 0, -1]),
                )
                .unwrap(),
            demo_blocks[Signboard].clone().rotate(plot_rotation),
        )?;

        // Place exhibit content
        space_to_space_copy(
            &exhibit_space,
            exhibit_footprint,
            &mut space,
            plot_transform,
        )?; // TODO: on failure, place an error marker and continue

        // Log build time
        let exhibit_time = Instant::now().duration_since(start_exhibit_time);
        log::trace!(
            "{:?} took {:.3} s",
            exhibit.name,
            exhibit_time.as_secs_f32()
        );

        exhibit_progress.progress(1.0).await;
    }

    if false {
        // A visual test of logo_text_extent().
        // TODO: Transplant this to an automated test.
        space.fill_uniform(
            logo_text_extent()
                .transform(GridMatrix::from_translation([0, 12, -radius_xz]))
                .unwrap(),
            Block::from(Rgb::ONE),
        )?;
    }
    logo_text(
        GridMatrix::from_translation([0, 12, -radius_xz]),
        &mut space,
    )?;
    final_progress.progress(0.5).await;

    // Enable light computation
    space.set_physics({
        let mut p = space.physics().clone();
        p.light = SpacePhysics::default().light;
        p
    });
    final_progress.progress(1.0).await;

    Ok(space)
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
    let (block_rotation, _) = src_to_dst_transform
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
    pub factory:
        for<'a> fn(&'a Exhibit, &'a mut Universe) -> LocalBoxFuture<'a, Result<Space, InGenError>>,
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
                        transformed.expand(FaceMap::repeat(Self::GAP_BETWEEN_PLOTS));

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

    fn y_range(&self, lower_y: GridCoordinate, upper_y: GridCoordinate) -> Grid {
        let mut lower = self.space_grid.lower_bounds();
        let mut upper = self.space_grid.upper_bounds();
        lower.y = lower_y;
        upper.y = upper_y;
        Grid::from_lower_upper(lower, upper)
    }
}
