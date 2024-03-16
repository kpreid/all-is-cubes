//! A space with miscellaneous demonstrations/tests of functionality.
//! The individual buildings/exhibits are defined in [`DEMO_CITY_EXHIBITS`].

use all_is_cubes::block::text::Font;
use alloc::{sync::Arc, vec::Vec};

use itertools::Itertools;
use rand::{Rng, SeedableRng as _};

use all_is_cubes::block::{self, Block, BlockAttributes, Resolution::*, AIR};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::palette;
use all_is_cubes::drawing::embedded_graphics::text::{Alignment, Baseline, TextStyleBuilder};
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::inv::{Slot, Tool};
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{
    Cube, Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridRotation, GridSize,
    GridVector, Gridgid,
};
use all_is_cubes::raycast::Raycaster;
use all_is_cubes::space::{LightPhysics, Space, SpaceBuilder, SpacePhysics};
use all_is_cubes::time::Instant;
use all_is_cubes::transaction::{self, Transaction};
use all_is_cubes::universe::Universe;
use all_is_cubes::universe::UniverseTransaction;
use all_is_cubes::util::YieldProgress;
use all_is_cubes_ui::{logo::logo_text, vui, vui::widgets};

use crate::alg::{space_to_space_copy, walk};
use crate::{clouds::clouds, wavy_landscape, DemoBlocks, LandscapeBlocks};

mod exhibit;
use exhibit::{Context, Exhibit, Placement};
mod exhibits;
use exhibits::DEMO_CITY_EXHIBITS;

pub(crate) async fn demo_city<I: Instant>(
    universe: &mut Universe,
    mut p: YieldProgress,
    params: crate::TemplateParameters,
) -> Result<Space, InGenError> {
    let start_city_time = I::now();

    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    use DemoBlocks::*;
    use LandscapeBlocks::*;

    // Also install blocks some exhibits want.
    // We do this once so that if multiple exhibits end up wanting them there are no conflicts.
    // TODO: We want a "module loading" system that allows expressing dependencies.
    let mut install_txn = UniverseTransaction::default();
    let widget_theme_progress = p.start_and_cut(0.05, "WidgetTheme").await;
    let widget_theme = widgets::WidgetTheme::new(&mut install_txn, widget_theme_progress)
        .await
        .unwrap();
    let ui_blocks_progress = p.start_and_cut(0.05, "UiBlocks").await;
    vui::blocks::UiBlocks::new(&mut install_txn, ui_blocks_progress)
        .await
        .install(&mut install_txn)
        .unwrap();
    let icons_blocks_progress = p.start_and_cut(0.05, "Icons").await;
    all_is_cubes::inv::Icons::new(&mut install_txn, icons_blocks_progress)
        .await
        .install(&mut install_txn)
        .unwrap();
    install_txn.execute(universe, &mut transaction::no_outputs)?;

    // Layout parameters
    // TODO: move to CityPlanner
    let road_directions = [Face6::PX, Face6::NX, Face6::PZ, Face6::NZ];
    let road_radius = CityPlanner::ROAD_RADIUS;
    let lamp_position_radius = CityPlanner::LAMP_POSITION_RADIUS;
    let exhibit_front_radius = CityPlanner::PLOT_FRONT_RADIUS;
    let lamp_spacing = 20;
    let sky_height = 30;
    let ground_depth = 30; // TODO: wavy_landscape is forcing us to have extra symmetry here
    let space_size = params.size.unwrap_or(GridSize::new(160, 60, 160));
    let bounds = GridAab::from_lower_upper(
        [-space_size.width / 2, -ground_depth, -space_size.depth / 2],
        [space_size.width / 2, sky_height, space_size.depth / 2],
    );

    let mut planner = CityPlanner::new(bounds);

    // Construct space.
    let mut space = Space::builder(bounds)
        .sky(crate::landscape::sky_with_grass(palette::DAY_SKY_COLOR))
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
                Tool::InfiniteBlocks(demo_blocks[Explosion(-10)].clone()),
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
        let grass_at = crate::landscape::grass_placement_function(0x21b5cc6b);
        space.fill(planner.y_range(1, 2), |cube| {
            if cube.x.abs() <= road_radius || cube.z.abs() <= road_radius {
                return None;
            }
            grass_at(cube).map(|height| &landscape_blocks[GrassBlades { height }])
        })?;
    }
    p.progress(0.3).await;

    // Roads and lamps
    for face in road_directions {
        let perpendicular: GridVector = GridRotation::CLOCKWISE.transform(face).normal_vector();
        let road_aligned_rotation = GridRotation::from_to(Face6::NZ, face, Face6::PY).unwrap();
        let other_side_of_road =
            GridRotation::from_basis([Face6::NX, Face6::PY, Face6::NZ]) * road_aligned_rotation;
        let rotations = [other_side_of_road, road_aligned_rotation];
        let raycaster = Raycaster::new([0.5, 0.5, 0.5], face.normal_vector::<FreeCoordinate, _>())
            .within(space.bounds());
        let curb_y = GridVector::new(0, 1, 0);
        for (i, step) in raycaster.enumerate() {
            let i = i as GridCoordinate;
            // Road surface
            for p in -road_radius..=road_radius {
                space.set(step.cube_ahead() + perpendicular * p, &demo_blocks[Road])?;
            }

            // Curbs
            if i >= road_radius {
                for &(side, p) in [(1, -road_radius), (0, road_radius)].iter() {
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
                            block::CompositeOperator::In,
                        )
                        .with_disassemblable()
                        .compose_or_replace(to_compose_with),
                    )?;
                }
            }

            // Dig underground passages
            // TODO: They need a connection to the surface
            for p in -road_radius..=road_radius {
                for y in CityPlanner::UNDERGROUND_FLOOR_Y..0 {
                    space.set(
                        step.cube_ahead() + perpendicular * p + GridVector::new(0, y, 0),
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

    let blank_city_time = I::now();
    log::trace!(
        "Blank city took {:.3} s",
        blank_city_time
            .saturating_duration_since(start_city_time)
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
    space.fill_uniform(landscape_region, &AIR)?;
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
    let landscape_time = I::now();
    log::trace!(
        "Landscape took {:.3} s",
        landscape_time
            .saturating_duration_since(blank_city_time)
            .as_secs_f32()
    );
    let [exhibits_progress, mut final_progress] = p.split(0.8);

    // All is Cubes logo
    let logo_location = space
        .bounds()
        .abut(Face6::NZ, -3)
        .unwrap()
        .abut(Face6::PY, -(sky_height - 12))
        .unwrap();
    vui::install_widgets(
        vui::LayoutGrant::new(logo_location),
        &vui::leaf_widget(logo_text()),
    )?
    .execute(&mut space, &mut transaction::no_outputs)?;
    planner.occupied_plots.push(logo_location);

    // Exhibits
    place_exhibits_in_city::<I>(
        exhibits_progress,
        universe,
        &widget_theme,
        &mut planner,
        &mut space,
    )
    .await?;

    // Lampposts
    'directions: for direction in road_directions {
        let perpendicular: GridVector =
            GridRotation::CLOCKWISE.transform(direction).normal_vector();
        for distance in (lamp_position_radius..).step_by(lamp_spacing) {
            for side_of_road in [-1, 1] {
                let globe_cube = Cube::new(0, 4, 0)
                    + direction.normal_vector() * distance
                    + perpendicular * (side_of_road * lamp_position_radius);
                if !bounds.contains_cube(globe_cube) {
                    continue 'directions;
                }

                let Some(base_cube) = planner.find_cube_near(
                    globe_cube - GridVector::new(0, 3, 0),
                    &[direction, direction.opposite()],
                ) else {
                    continue;
                };
                place_lamppost(base_cube, globe_cube, &mut space, &demo_blocks)?;
            }
        }
    }

    final_progress.progress(0.0).await;

    // Sprinkle some trees around in the remaining space.
    plant_trees(
        final_progress.start_and_cut(0.5, "Trees").await,
        params,
        &mut planner,
        &mut space,
        universe,
    )
    .await?;

    // Enable light computation
    space.set_physics({
        let mut p = space.physics().clone();
        p.light = SpacePhysics::default().light;
        p
    });
    final_progress.finish().await;

    Ok(space)
}

async fn place_exhibits_in_city<I: Instant>(
    exhibits_progress: YieldProgress,
    universe: &mut Universe,
    widget_theme: &widgets::WidgetTheme,
    planner: &mut CityPlanner,
    space: &mut Space,
) -> Result<(), InGenError> {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    use DemoBlocks::*;

    'exhibit: for (exhibit, mut exhibit_progress) in DEMO_CITY_EXHIBITS
        .iter()
        .zip(exhibits_progress.split_evenly(DEMO_CITY_EXHIBITS.len()))
    {
        exhibit_progress.set_label(format!("Exhibit “{name}”", name = exhibit.name));
        exhibit_progress.progress(0.0).await;
        let start_exhibit_time = I::now();

        // Execute the exhibit factory function.
        // TODO: stop handing out mutable Universe access, so we can parallelize this loop
        let ctx = Context {
            exhibit,
            universe,
            widget_theme,
        };
        let (exhibit_space, exhibit_transaction) = match (exhibit.factory)(ctx) {
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

        // Amount by which the exhibit's own size is expanded to form walls and empty space
        // for displaying it and allowing players to move around it.
        let enclosure_thickness = match exhibit.placement {
            Placement::Surface => FaceMap::repeat(1),
            // Underground exhibits get no enclosure; they are expected to play nicely with being
            // buried in stone except for the entranceway.
            Placement::Underground => FaceMap::repeat(0),
        };

        // Now that we know the size of the exhibit, find a place for it that fits its bounds.
        let exhibit_footprint = exhibit_space.bounds();
        let enclosure_footprint = exhibit_footprint.expand(enclosure_thickness);

        let Some(plot_transform) = planner.find_plot(enclosure_footprint, exhibit.placement) else {
            log::error!("Out of city space!");
            break 'exhibit;
        };
        let plot = exhibit_footprint.transform(plot_transform).unwrap();
        let enclosure_at_plot = enclosure_footprint.transform(plot_transform).unwrap();

        // Create enclosure, with a block replacing all ground blocks 1 block around,
        // and everything else cleared
        let enclosure_lower = GridAab::from_lower_upper(
            enclosure_at_plot.lower_bounds(),
            [
                enclosure_at_plot.upper_bounds().x,
                // max()ing handles the case where the plot is floating but should still
                // have enclosure floor
                exhibit.placement.floor().max(plot.lower_bounds().y),
                enclosure_at_plot.upper_bounds().z,
            ],
        );
        space.fill_uniform(enclosure_at_plot, &AIR)?;
        space.fill_uniform(enclosure_lower, &demo_blocks[ExhibitBackground])?;
        exhibit_progress.progress(0.4).await;

        // Cut an "entranceway" into the curb and grass, or corridor wall underground
        let entranceway_height = 2; // TODO: let exhibit customize
        {
            let front_face = plot_transform.rotation.transform(Face6::PZ);
            // Compute the surface that needs to be clear for walking
            let entrance_plane = enclosure_lower
                .expand(
                    FaceMap {
                        // don't alter y
                        ny: 0,
                        py: 0,
                        ..enclosure_thickness
                    }
                    .map(|_, thick| -thick),
                )
                .abut(Face6::PY, 0)
                .unwrap()
                .abut(front_face, enclosure_thickness.pz) // re-add enclosure bounds
                .unwrap()
                .abut(
                    front_face,
                    CityPlanner::PLOT_FRONT_RADIUS - CityPlanner::ROAD_RADIUS + 1,
                )
                .unwrap();

            if let Placement::Surface = exhibit.placement {
                let walkway = entrance_plane.abut(Face6::NY, 1).unwrap();
                space.fill_uniform(walkway, &demo_blocks[Road])?;
            }

            let walking_volume = entrance_plane.abut(Face6::PY, entranceway_height).unwrap();
            space.fill_uniform(walking_volume, &AIR)?;

            planner.occupied_plots.push(walking_volume);
        }

        // Draw exhibit info
        {
            let info_resolution = R64;
            let exhibit_info_space = draw_exhibit_info(exhibit)?; // TODO: on failure, place an error marker and continue ... or at least produce a GenError for context

            // Enforce maximum width (TODO: this should be done inside draw_exhibit_info instead)
            let bounds_for_info_voxels = GridAab::from_lower_size(
                exhibit_info_space.bounds().lower_bounds(),
                GridSize {
                    width: exhibit_info_space
                        .bounds()
                        .size()
                        .width
                        .min(enclosure_footprint.size().width * i32::from(info_resolution)),
                    ..exhibit_info_space.bounds().size()
                },
            );

            let info_voxels_widget: vui::WidgetTree = Arc::new(vui::LayoutTree::Stack {
                direction: Face6::PZ,
                children: vec![
                    match exhibit.placement {
                        Placement::Surface => vui::leaf_widget(widgets::Frame::with_block(
                            demo_blocks[Signboard].clone(),
                        )),
                        Placement::Underground => vui::LayoutTree::empty(),
                    },
                    vui::leaf_widget(Arc::new(widgets::Voxels::new(
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
                    vui::Gravity::new(vui::Align::Center, vui::Align::Center, vui::Align::Low),
                )
                .unwrap();

            let sign_position_in_plot_coordinates = match exhibit.placement {
                Placement::Surface => GridVector::new(
                    // extending right from left edge
                    enclosure_footprint.lower_bounds().x,
                    // at ground level
                    0,
                    // minus 1 to put the Signboard blocks sitting on the enclosure blocks
                    enclosure_footprint.upper_bounds().z - 1,
                ),
                Placement::Underground => GridVector::new(
                    // centered horizontally
                    enclosure_footprint.lower_bounds().x
                        + (enclosure_footprint.size().width
                            - info_sign_space.bounds().size().width)
                            / 2,
                    // at above-the-entrance level
                    entranceway_height,
                    // on the surface of the corridor wall -- TODO: We don't actually know fundamentally that the corridor
                    enclosure_footprint.upper_bounds().z + 1,
                ),
            };
            let sign_transform =
                plot_transform * Gridgid::from_translation(sign_position_in_plot_coordinates);

            // Copy the signboard into the main space.
            // (TODO: We do this indirection because the widget system does not currently
            // support arbitrary rotation of widget trees, but that is wanted.)
            space_to_space_copy(
                &info_sign_space,
                info_sign_space.bounds(),
                space,
                sign_transform,
            )?;

            // Mark sign space as occupied
            planner
                .occupied_plots
                .push(info_sign_space.bounds().transform(sign_transform).unwrap());
        }
        exhibit_progress.progress(0.66).await;

        // As the last step before we actually copy the exhibit into the city,
        // execute its transaction.
        match exhibit_transaction.execute(universe, &mut transaction::no_outputs) {
            Ok(()) => {}
            Err(error) => {
                // TODO: put the error on a sign in place of the exhibit
                log::error!(
                    "Exhibit transaction failure.\nExhibit: {name}\nError: {error}",
                    name = exhibit.name,
                    error = all_is_cubes::util::ErrorChain(&error),
                );
                continue 'exhibit;
            }
        }

        // Place exhibit content
        space_to_space_copy(&exhibit_space, exhibit_footprint, space, plot_transform)?; // TODO: on failure, place an error marker and continue

        // Log build time
        let exhibit_time = I::now().saturating_duration_since(start_exhibit_time);
        log::trace!(
            "{:?} took {:.3} s",
            exhibit.name,
            exhibit_time.as_secs_f32()
        );

        exhibit_progress.finish().await;
    }

    Ok(())
}

async fn plant_trees(
    progress: YieldProgress,
    params: crate::TemplateParameters,
    planner: &mut CityPlanner,
    space: &mut Space,
    universe: &Universe,
) -> Result<(), InGenError> {
    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(params.seed.unwrap_or(0));
    let possible_tree_origins: GridAab = planner.y_range(1, 2);

    for progress in progress.split_evenly(60) {
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
                .execute(space, &mut transaction::no_outputs)?;
        }
        progress.finish().await;
    }
    Ok(())
}

fn place_lamppost(
    base_position: Cube,
    globe_position: Cube,
    space: &mut Space,
    demo_blocks: &BlockProvider<DemoBlocks>,
) -> Result<(), InGenError> {
    use DemoBlocks::*;

    fn rot_py_to(to: Face6) -> GridRotation {
        // TODO: make it possible to express this more elegantly
        // "rotate A to B by the shortest path possible, and if it is 180º then prefer this axis of rotation"
        GridRotation::from_to(Face6::PY, to, Face6::PX)
            .or_else(|| GridRotation::from_to(Face6::PY, to, Face6::PZ))
            .unwrap()
    }

    space.set(base_position, &demo_blocks[LamppostBase])?;

    for (step1, step2) in walk(base_position, globe_position).tuple_windows() {
        // let prev_cube = step1.cube;
        let prev_dir: Face6 = step1.face.try_into().unwrap();
        let this_cube = step2.cube;
        let next_dir: Face6 = step2.face.try_into().unwrap();
        let next_cube = step2.adjacent();

        let block = if next_cube == globe_position {
            demo_blocks[LamppostTop].clone().rotate(rot_py_to(next_dir))
        } else {
            demo_blocks[LamppostSegment]
                .clone()
                .rotate(rot_py_to(next_dir))
        };

        let block = if prev_dir == next_dir {
            block
        } else {
            block::Composite::new(
                demo_blocks[LamppostSegment]
                    .clone()
                    .rotate(rot_py_to(prev_dir)),
                block::CompositeOperator::Over,
            )
            .compose_or_replace(block)
        };

        space.set(this_cube, block)?;
    }

    space.set(globe_position, &demo_blocks[Lamp])?;

    Ok(())
}

/// Generate a Space containing text voxels to put on the signboard for an exhibit.
///
/// The space's bounds extend upward from [0, 0, 0].
fn draw_exhibit_info(exhibit: &Exhibit) -> Result<Space, InGenError> {
    let info_widgets: vui::WidgetTree = Arc::new(vui::LayoutTree::Stack {
        direction: Face6::NY,
        children: vec![
            vui::leaf_widget(widgets::LargeText {
                text: exhibit.name.into(),
                font: Font::System16,
                brush: VoxelBrush::single(Block::from(palette::ALMOST_BLACK)),
                text_style: TextStyleBuilder::new()
                    .alignment(Alignment::Center)
                    .baseline(Baseline::Middle)
                    .build(),
            }),
            vui::leaf_widget(widgets::LargeText {
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
                font: Font::SmallerBodyText,
                brush: VoxelBrush::single(Block::from(palette::ALMOST_BLACK)),
                text_style: TextStyleBuilder::new()
                    .alignment(Alignment::Center)
                    .baseline(Baseline::Middle)
                    .build(),
            }),
        ],
    });

    // TODO: give it a maximum size, instead of what we currently do which is truncating later
    let space = info_widgets.to_space(
        SpaceBuilder::default().physics(SpacePhysics::DEFAULT_FOR_BLOCK),
        vui::Gravity::new(vui::Align::Low, vui::Align::Low, vui::Align::Low),
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
    const LAMP_POSITION_RADIUS: GridCoordinate = Self::ROAD_RADIUS + 1;
    /// Distance from the center cube to the line of the front of each placed exhibit.
    /// TODO: The units of this isn't being consistent, since it is actually + 1 from the lamps
    const PLOT_FRONT_RADIUS: GridCoordinate = Self::LAMP_POSITION_RADIUS;
    const GAP_BETWEEN_PLOTS: GridCoordinate = 1;

    const SURFACE_Y: GridCoordinate = 1;
    const UNDERGROUND_FLOOR_Y: GridCoordinate = -10;

    pub fn new(space_bounds: GridAab) -> Self {
        let city_radius = space_bounds.upper_bounds().x; // TODO: compare everything and take the max

        let mut occupied_plots = Vec::new();
        let road = GridAab::from_lower_upper(
            [
                -Self::ROAD_RADIUS,
                Self::UNDERGROUND_FLOOR_Y - 1,
                -city_radius,
            ],
            [Self::ROAD_RADIUS + 1, Self::SURFACE_Y + 2, city_radius + 1],
        );
        occupied_plots.push(road);
        occupied_plots.push(road.transform(GridRotation::CLOCKWISE.into()).unwrap());
        Self {
            space_bounds,
            city_radius,
            occupied_plots,
        }
    }

    pub fn find_plot(&mut self, plot_shape: GridAab, placement: Placement) -> Option<Gridgid> {
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
                        placement.floor(),
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

    fn find_cube_near(&self, start_cube: Cube, directions: &[Face6]) -> Option<Cube> {
        for distance in 0.. {
            let mut some_in_bounds = false;
            for &direction in directions {
                let cube = start_cube + direction.normal_vector() * distance;
                if self.space_bounds.contains_cube(cube) {
                    some_in_bounds = true;
                } else {
                    break;
                }
                if !self.is_occupied(cube.grid_aab()) {
                    return Some(cube);
                }
                if distance == 0 {
                    // no need to try multiple equivalent directions
                    break;
                }
            }
            if !some_in_bounds {
                break;
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
        GridAab::from_ranges([
            self.space_bounds.x_range(),
            lower_y..upper_y,
            self.space_bounds.z_range(),
        ])
    }
}
