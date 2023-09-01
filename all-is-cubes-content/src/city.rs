//! A space with miscellaneous demonstrations/tests of functionality.
//! The individual buildings/exhibits are defined in [`DEMO_CITY_EXHIBITS`].

use std::sync::Arc;

use futures_core::future::BoxFuture;
use instant::Instant;
use rand::{Rng, SeedableRng as _};

use all_is_cubes::cgmath::Vector3;
use all_is_cubes::drawing::embedded_graphics::{
    mono_font::iso_8859_1 as font,
    text::{Alignment, Baseline, TextStyleBuilder},
};

use all_is_cubes::block::{self, Block, BlockAttributes, Resolution::*, AIR};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::palette;
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::inv::Slot;
use all_is_cubes::inv::Tool;
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{
    Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridRotation, GridVector, Gridgid, Rgb,
};
use all_is_cubes::raycast::Raycaster;
use all_is_cubes::space::{LightPhysics, Space, SpaceBuilder, SpacePhysics};
use all_is_cubes::transaction::{self, Transaction};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;
use all_is_cubes_ui::logo::logo_text;
use all_is_cubes_ui::vui::{
    install_widgets, widgets, Align, Gravity, LayoutGrant, LayoutTree, WidgetTree,
};

use crate::{
    clouds::clouds, exhibits::DEMO_CITY_EXHIBITS, noise::NoiseFnExt, space_to_space_copy,
    wavy_landscape, DemoBlocks, LandscapeBlocks,
};

pub(crate) async fn demo_city(
    universe: &mut Universe,
    mut p: YieldProgress,
    params: crate::TemplateParameters,
) -> Result<Space, InGenError> {
    let start_city_time = Instant::now();

    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    use DemoBlocks::*;
    use LandscapeBlocks::*;

    // Also install blocks some exhibits want.
    // We do this once so that if multiple exhibits end up wanting them there are no conflicts.
    // TODO: We want a "module loading" system that allows expressing dependencies.
    let ui_blocks_progress = p.start_and_cut(0.05, "UiBlocks").await;
    all_is_cubes_ui::vui::blocks::UiBlocks::new(universe, ui_blocks_progress)
        .await
        .install(universe)
        .unwrap();
    let icons_blocks_progress = p.start_and_cut(0.05, "Icons").await;
    all_is_cubes::inv::Icons::new(universe, icons_blocks_progress)
        .await
        .install(universe)
        .unwrap();

    // Layout parameters
    // TODO: move to CityPlanner
    let road_radius = CityPlanner::ROAD_RADIUS;
    let lamp_position_radius = CityPlanner::LAMP_POSITION_RADIUS;
    let exhibit_front_radius = CityPlanner::PLOT_FRONT_RADIUS;
    let lamp_spacing = 20;
    let sky_height = 30;
    let ground_depth = 30; // TODO: wavy_landscape is forcing us to have extra symmetry here
    let underground_floor_y = -5;
    let space_size = params.size.unwrap_or(Vector3::new(160, 60, 160));
    let bounds = GridAab::from_lower_upper(
        [-space_size.x / 2, -ground_depth, -space_size.z / 2],
        [space_size.x / 2, sky_height, space_size.z / 2],
    );

    let mut planner = CityPlanner::new(bounds);

    // Prepare brushes.
    let lamp_brush = VoxelBrush::new([
        ([0, 0, 0], &demo_blocks[LamppostBase]),
        ([0, 1, 0], &demo_blocks[LamppostSegment]),
        ([0, 2, 0], &demo_blocks[LamppostTop]),
        ([0, 3, 0], &demo_blocks[Lamp]),
    ]);

    // Construct space.
    let mut space = Space::builder(bounds)
        .sky_color(Rgb::new(0.9, 0.9, 1.4))
        .light_physics(LightPhysics::None) // disable until we are done with bulk updates
        .spawn({
            // TODO: Add incremental spawn configuration to SpaceBuilder?
            let mut spawn = Spawn::default_for_new_space(bounds);
            spawn.set_bounds(GridAab::from_lower_upper(
                [-road_radius, 1, 0],
                [road_radius + 1, sky_height, 17],
            ));
            //spawn.set_eye_position(bounds.center() + Vector3::new(0.5, 2.91, 8.5));
            // Initial inventory contents. TODO: Make a better list.
            let mut inventory = vec![
                Tool::RemoveBlock { keep: true }.into(),
                Tool::Jetpack { active: false }.into(),
                Tool::PushPull.into(),
            ];
            for block in [
                &landscape_blocks[Stone],
                &demo_blocks[GlassBlock],
                &demo_blocks[Lamp],
                &demo_blocks[Arrow],
            ] {
                inventory.push(Slot::stack(40, Tool::Block(block.clone())));
            }
            inventory.push(Slot::stack(
                1,
                Tool::InfiniteBlocks(demo_blocks[Explosion(0)].clone()),
            ));
            spawn.set_inventory(inventory);
            spawn
        })
        .build();

    // Fill basic layers, underground and top
    space.fill_uniform(planner.y_range(-ground_depth, 0), &landscape_blocks[Stone])?;
    p.progress(0.1).await;
    space.fill_uniform(planner.y_range(0, 1), &landscape_blocks[Grass])?;
    p.progress(0.2).await;

    // Stray grass
    {
        let grass_noise = noise::ScaleBias::new(noise::OpenSimplex::new(0x21b5cc6b))
            .set_bias(0.0)
            .set_scale(8.0);
        let grass_threshold = 1.2;
        space.fill(
            GridAab::from_lower_upper(
                [bounds.lower_bounds().x, 1, bounds.lower_bounds().z],
                [bounds.upper_bounds().x, 2, bounds.upper_bounds().z],
            ),
            |cube| {
                if cube.x.abs() <= road_radius || cube.z.abs() <= road_radius {
                    return None;
                }
                if grass_noise.at_cube(cube) > grass_threshold * 2. {
                    Some(&landscape_blocks[GrassBlades { variant: true }])
                } else if grass_noise.at_cube(cube) > grass_threshold {
                    Some(&landscape_blocks[GrassBlades { variant: false }])
                } else {
                    None
                }
            },
        )?;
    }
    p.progress(0.3).await;

    // Roads and lamps
    for face in [Face6::PX, Face6::NX, Face6::PZ, Face6::NZ] {
        let perpendicular: GridVector = GridRotation::CLOCKWISE.transform(face).normal_vector();
        let road_aligned_rotation = GridRotation::from_to(Face6::NZ, face, Face6::PY).unwrap();
        let other_side_of_road =
            GridRotation::from_basis([Face6::NX, Face6::PY, Face6::NZ]) * road_aligned_rotation;
        let rotations = [other_side_of_road, road_aligned_rotation];
        let raycaster = Raycaster::new([0.5, 0.5, 0.5], face.normal_vector::<FreeCoordinate>())
            .within(space.bounds());
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
                    let position = step.cube_ahead() + perpendicular * p + curb_y;

                    // Place curb and combine it with other curb blocks .
                    let mut to_compose_with = space[position].clone();
                    // TODO: .unspecialize() is a maybe expensive way to make this test, and
                    // this isn't the first time this has come up. Benchmark a "block view"
                    // to cheaply filter out modifiers.
                    if to_compose_with.clone().unspecialize() != vec![demo_blocks[Curb].clone()] {
                        to_compose_with = AIR;
                    }
                    space.set(
                        position,
                        block::Composite::new(
                            demo_blocks[Curb].clone().rotate(rotations[side]),
                            block::CompositeOperator::Over,
                        )
                        .with_disassemblable()
                        .compose_or_replace(to_compose_with),
                    )?;
                }
            }

            // Lampposts
            if (i - lamp_position_radius).rem_euclid(lamp_spacing) == 0 {
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
            if (i - lamp_position_radius).rem_euclid(7) == 0 {
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
    let landscape_progress = p.start_and_cut(0.4, "Landscape").await;
    landscape_progress.progress(0.0).await;
    let landscape_region = GridAab::from_lower_upper(
        [
            bounds.lower_bounds().x,
            -ground_depth * 8 / 10,
            bounds.lower_bounds().z,
        ],
        [-exhibit_front_radius, sky_height, -exhibit_front_radius],
    );
    space.fill_uniform(landscape_region, AIR)?;
    landscape_progress.progress(0.5).await;
    wavy_landscape(landscape_region, &mut space, &landscape_blocks, 1.0)?;
    planner.occupied_plots.push(landscape_region);
    landscape_progress.finish().await;

    // Clouds (needs to be done after landscape to not be overwritten)
    // TODO: Enable this once transparency rendering is better.
    if false {
        clouds(planner.y_range(sky_height - 2, sky_height), &mut space, 0.1)?;
    }

    // TODO: Integrate logging and YieldProgress
    let landscape_time = Instant::now();
    log::trace!(
        "Landscape took {:.3} s",
        landscape_time.duration_since(blank_city_time).as_secs_f32()
    );
    let [exhibits_progress, final_progress] = p.split(0.8);

    // All is Cubes logo
    let logo_location = space
        .bounds()
        .abut(Face6::NZ, -3)
        .unwrap()
        .abut(Face6::PY, -(sky_height - 12))
        .unwrap();
    install_widgets(
        LayoutGrant::new(logo_location),
        &LayoutTree::leaf(logo_text()),
    )?
    .execute(&mut space, &mut transaction::no_outputs)?;
    planner.occupied_plots.push(logo_location);

    // Exhibits
    'exhibit: for (exhibit, mut exhibit_progress) in DEMO_CITY_EXHIBITS
        .iter()
        .zip(exhibits_progress.split_evenly(DEMO_CITY_EXHIBITS.len()))
    {
        exhibit_progress.set_label(format!("Exhibit “{name}”", name = exhibit.name));
        exhibit_progress.progress(0.0).await;
        let start_exhibit_time = Instant::now();

        // Execute the exhibit factory function.
        // TODO: Factory should be given a YieldProgress.
        let exhibit_space = match (exhibit.factory)(exhibit, universe).await {
            Ok(s) => s,
            Err(error) => {
                // TODO: put the error on a sign in place of the exhibit
                log::error!(
                    "Exhibit generation failure.\nExhibit: {name}\nError: {error}",
                    name = exhibit.name,
                    error = all_is_cubes::util::ErrorChain(&error),
                );
                continue 'exhibit;
            }
        };
        exhibit_progress.progress(0.33).await;

        // Now that we know the size of the exhibit, find a place for it.
        let exhibit_footprint = exhibit_space.bounds();

        let enclosure_footprint = exhibit_footprint.expand(FaceMap::repeat(1));

        let Some(plot_transform) = planner.find_plot(enclosure_footprint) else {
            log::error!("Out of city space!");
            break 'exhibit;
        };
        let plot = exhibit_footprint.transform(plot_transform).unwrap();
        let enclosure_at_plot = enclosure_footprint.transform(plot_transform).unwrap();

        // Create enclosure, with a block replacing all ground blocks 1 block around,
        // and everything else cleared
        let enclosure = GridAab::from_lower_upper(
            enclosure_at_plot.lower_bounds(),
            [
                enclosure_at_plot.upper_bounds().x,
                1.max(plot.lower_bounds()[1]), // handles case where plot is floating
                enclosure_at_plot.upper_bounds().z,
            ],
        );
        space.fill_uniform(enclosure_at_plot, &AIR)?;
        space.fill_uniform(enclosure, &demo_blocks[ExhibitBackground])?;
        exhibit_progress.progress(0.4).await;

        // TODO: Add "entrances" so it's clear what the "front" of the exhibit is supposed to be.

        // Draw exhibit info
        {
            let info_resolution = R64;
            let exhibit_info_space = draw_exhibit_info(exhibit)?; // TODO: on failure, place an error marker and continue ... or at least produce a GenError for context

            // Enforce maximum width (TODO: this should be done inside draw_exhibit_info instead)
            let bounds_for_info_voxels = GridAab::from_lower_size(
                exhibit_info_space.bounds().lower_bounds(),
                GridVector {
                    x: exhibit_info_space
                        .bounds()
                        .size()
                        .x
                        .min(enclosure_footprint.size().x * i32::from(info_resolution)),
                    ..exhibit_info_space.bounds().size()
                },
            );

            let info_voxels_widget: WidgetTree = Arc::new(LayoutTree::Stack {
                direction: Face6::PZ,
                children: vec![
                    LayoutTree::leaf(widgets::Frame::with_block(demo_blocks[Signboard].clone())),
                    LayoutTree::leaf(Arc::new(widgets::Voxels::new(
                        bounds_for_info_voxels,
                        universe.insert_anonymous(exhibit_info_space),
                        info_resolution,
                        BlockAttributes {
                            display_name: "Exhibit Name".into(),
                            ..Default::default()
                        },
                    ))),
                ],
            });

            // Install the signboard-and-text widgets in a space.
            let info_sign_space = info_voxels_widget
                .to_space(
                    SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
                    Gravity::new(Align::Center, Align::Center, Align::Low),
                )
                .unwrap();

            let sign_position_in_plot_coordinates = {
                GridVector {
                    // extending right from left edge
                    x: enclosure_footprint.lower_bounds().x,
                    // at ground level
                    y: 0,
                    // minus 1 to put the Signboard blockss on the enclosure blocks
                    z: enclosure_footprint.upper_bounds().z - 1,
                }
            };
            let sign_transform =
                plot_transform * Gridgid::from_translation(sign_position_in_plot_coordinates);

            // Copy the signboard into the main space.
            // (TODO: We do this indirection because the widget system does not currently
            // support arbitrary rotation of widget trees, but that is wanted.)
            space_to_space_copy(
                &info_sign_space,
                info_sign_space.bounds(),
                &mut space,
                sign_transform,
            )?;
        }
        exhibit_progress.progress(0.66).await;

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

        exhibit_progress.finish().await;
    } // end per-exhibit loop

    final_progress.progress(0.0).await;

    // Sprinkle some trees around in the remaining space.
    {
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(params.seed.unwrap_or(0));
        let possible_tree_origins: GridAab = planner.y_range(1, 2);
        for _ in 0..60 {
            // won't get this many trees, because some will be blocked
            let tree_origin = possible_tree_origins.random_cube(&mut rng).unwrap();
            let height = rng.gen_range(1..8);
            let tree_bounds = GridAab::single_cube(tree_origin).expand(FaceMap {
                nx: height / 3,
                ny: 0,
                nz: height / 3,
                px: height / 3,
                py: height - 1,
                pz: height / 3,
            });
            if space.bounds().contains_box(tree_bounds) && !planner.is_occupied(tree_bounds) {
                crate::tree::make_tree(&landscape_blocks, &mut rng, tree_origin, tree_bounds)?
                    .execute(&mut space, &mut transaction::no_outputs)?;
            }
        }
    }

    final_progress.progress(0.5).await;

    // Enable light computation
    space.set_physics({
        let mut p = space.physics().clone();
        p.light = SpacePhysics::default().light;
        p
    });
    final_progress.finish().await;

    Ok(space)
}

#[allow(clippy::type_complexity)]
pub(crate) struct Exhibit {
    pub name: &'static str,
    pub subtitle: &'static str,
    pub factory:
        for<'a> fn(&'a Exhibit, &'a mut Universe) -> BoxFuture<'a, Result<Space, InGenError>>,
}

/// Generate a Space containing text voxels to put on the signboard for an exhibit.
///
/// The space's bounds extend upward from [0, 0, 0].
fn draw_exhibit_info(exhibit: &Exhibit) -> Result<Space, InGenError> {
    let info_widgets: WidgetTree = Arc::new(LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            LayoutTree::leaf(Arc::new(widgets::LargeText {
                text: exhibit.name.into(),
                font: || &font::FONT_9X18_BOLD,
                brush: VoxelBrush::single(Block::from(palette::ALMOST_BLACK)),
                text_style: TextStyleBuilder::new()
                    .alignment(Alignment::Center)
                    .baseline(Baseline::Middle)
                    .build(),
            })),
            LayoutTree::leaf(Arc::new(widgets::LargeText {
                text: {
                    let t = exhibit.subtitle;
                    // TODO: rectangle_to_aab() fails when the empty string is drawn;
                    // when that's fixed, remove this condition
                    if t.is_empty() {
                        " "
                    } else {
                        t
                    }
                }
                .into(),
                font: || &font::FONT_5X8,
                brush: VoxelBrush::single(Block::from(palette::ALMOST_BLACK)),
                text_style: TextStyleBuilder::new()
                    .alignment(Alignment::Center)
                    .baseline(Baseline::Middle)
                    .build(),
            })),
        ],
    });

    // TODO: give it a maximum size, instead of what we currently do which is truncating later
    let space = info_widgets.to_space(
        SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
        Gravity::new(Align::Low, Align::Low, Align::Low),
    )?;
    Ok(space)
}

/// Tracks available land while the city is being generated.
#[derive(Clone, Debug, PartialEq)]
struct CityPlanner {
    space_bounds: GridAab,
    /// Count of blocks beyond the origin that are included in the city space.
    city_radius: GridCoordinate, // TODO redundant with space_bounds
    /// Each plot/exhibit that has already been placed. (This could be a spatial data structure but we're not that big yet.)
    occupied_plots: Vec<GridAab>,
}

impl CityPlanner {
    const ROAD_RADIUS: GridCoordinate = 2;
    /// Distance from the center cube to the line of cubes where lampposts are placed.
    const LAMP_POSITION_RADIUS: GridCoordinate = Self::ROAD_RADIUS + 2;
    /// Distance from the center cube to the line of the front of each placed exhibit.
    /// TODO: The units of this isn't being consistent, since it is actually + 2 from the lamps
    const PLOT_FRONT_RADIUS: GridCoordinate = Self::LAMP_POSITION_RADIUS + 1;
    const GAP_BETWEEN_PLOTS: GridCoordinate = 1;

    pub fn new(space_bounds: GridAab) -> Self {
        let city_radius = space_bounds.upper_bounds().x; // TODO: compare everything and take the max

        let mut occupied_plots = Vec::new();
        let road = GridAab::from_lower_upper(
            [-Self::LAMP_POSITION_RADIUS, 0, -city_radius],
            [Self::LAMP_POSITION_RADIUS + 1, 2, city_radius + 1],
        );
        occupied_plots.push(road);
        occupied_plots.push(road.transform(GridRotation::CLOCKWISE.into()).unwrap());
        Self {
            space_bounds,
            city_radius,
            occupied_plots,
        }
    }

    pub fn find_plot(&mut self, plot_shape: GridAab) -> Option<Gridgid> {
        // TODO: We'd like to resume the search from _when we left off_, but that's tricky since a
        // smaller plot might fit where a large one didn't. So, quadratic search it is for now.
        for d in 0..=self.city_radius {
            for street_axis in GridRotation::CLOCKWISE.iterate() {
                // TODO exercising opposite sides logic
                'search: for &left_side in &[false, true] {
                    // The translation is expressed along the +X axis street, so
                    // "left" is -Z and "right" is +Z.
                    //
                    // The rotations are about the origin cube (GridAab::ORIGIN_CUBE), so
                    // they are done with .to_positive_octant_matrix(1).

                    let mut transform = Gridgid::from_translation(GridVector::new(
                        d,
                        1,
                        if left_side {
                            -Self::PLOT_FRONT_RADIUS - plot_shape.upper_bounds().z
                        } else {
                            Self::PLOT_FRONT_RADIUS + plot_shape.upper_bounds().z
                        },
                    )) * if left_side {
                        Gridgid::IDENTITY
                    } else {
                        (GridRotation::COUNTERCLOCKWISE * GridRotation::COUNTERCLOCKWISE)
                            .to_positive_octant_transform(1)
                    };
                    // Rotate to match street
                    transform = street_axis.to_positive_octant_transform(1) * transform;

                    let transformed = plot_shape
                        .transform(transform)
                        .expect("can't happen: city plot transformation failure");

                    if !self.space_bounds.contains_box(transformed) {
                        continue 'search;
                    }

                    let for_occupancy_check =
                        transformed.expand(FaceMap::repeat(Self::GAP_BETWEEN_PLOTS));

                    if self.is_occupied(for_occupancy_check) {
                        continue 'search;
                    }

                    self.occupied_plots.push(transformed);
                    return Some(transform);
                }
            }
        }
        None
    }

    fn is_occupied(&self, query: GridAab) -> bool {
        for occupied in self.occupied_plots.iter() {
            if occupied.intersection(query).is_some() {
                return true;
            }
        }
        false
    }

    fn y_range(&self, lower_y: GridCoordinate, upper_y: GridCoordinate) -> GridAab {
        let mut lower = self.space_bounds.lower_bounds();
        let mut upper = self.space_bounds.upper_bounds();
        lower.y = lower_y;
        upper.y = upper_y;
        GridAab::from_lower_upper(lower, upper)
    }
}
