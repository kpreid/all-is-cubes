//! Miscellanous demonstrations of capability and manual test-cases.
//! The exhibits defined in this file are combined into [`crate::demo_city`].

use std::convert::TryFrom;
use std::f64::consts::PI;

use exhaust::Exhaust as _;
use rand::SeedableRng as _;

use all_is_cubes::block::{
    space_to_blocks, AnimationHint, Block, BlockAttributes, BlockCollision, Composite,
    CompositeOperator, Move,
    Resolution::{self, *},
    RotationPlacementRule, Zoom, AIR,
};
use all_is_cubes::cgmath::{
    Basis2, ElementWise, EuclideanSpace as _, InnerSpace as _, Point3, Rad, Rotation as _,
    Rotation2, Vector2, Vector3,
};
use all_is_cubes::content::load_image::{default_srgb, space_from_image};
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::drawing::{
    draw_to_blocks,
    embedded_graphics::{
        geometry::Point,
        mono_font::{
            iso_8859_1::{FONT_6X10, FONT_8X13_BOLD},
            MonoTextStyle,
        },
        pixelcolor::Rgb888,
        prelude::Size,
        primitives::{PrimitiveStyle, Rectangle, StyledDrawable},
        text::{Baseline, Text},
    },
};
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{
    Aab, Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridMatrix, GridPoint,
    GridRotation, GridVector, NotNan, Rgb, Rgba,
};
use all_is_cubes::space::{SetCubeError, Space, SpacePhysics, SpaceTransaction};
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::Universe;
use all_is_cubes::{include_image, rgb_const, rgba_const};

use crate::{
    four_walls, make_slab, make_some_blocks, make_some_voxel_blocks, palette, tree, AnimatedVoxels,
    DemoBlocks, Exhibit, Fire, LandscapeBlocks,
};

/// All exhibits which will show up in [`crate::UniverseTemplate::DemoCity`].
///
/// Ordered by distance from the center.
pub(crate) static DEMO_CITY_EXHIBITS: &[Exhibit] = &[
    KNOT,
    TRANSPARENCY_LARGE,
    TRANSPARENCY_SMALL,
    COLLISION,
    RESOLUTIONS,
    ANIMATION,
    MAKE_SOME_BLOCKS,
    DASHED_BOXES,
    COMPOSITE,
    DESTRUCTION,
    MOVED_BLOCKS,
    ROTATIONS,
    UI_BLOCKS,
    TREES,
    CHUNK_CHART,
    COLOR_LIGHTS,
    IMAGES,
    SMALLEST,
    SWIMMING_POOL,
    COLORS,
    TEXT,
    ZOOM,
];

macro_rules! exhibit {
    (
        #[exhibit($( $fields:tt )*)]
        async fn $name:ident($( $args:tt )*) {
            $( $body:tt )*
        }
    ) => {
        const $name: Exhibit = Exhibit {
            factory: |$( $args )*| Box::pin(async move { $( $body )* }),
            $( $fields )*
        };
    }
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Transparency",
    subtitle:
        "Test depth sorting and blending.\n\
        Lighting of volumes still needs work.",
)]
async fn TRANSPARENCY_LARGE(_: &Exhibit, _universe: &mut Universe) {
    let mut space = Space::empty(GridAab::from_lower_size([-3, 0, -3], [7, 5, 7]));

    let colors = [
        Rgb::new(1.0, 0.5, 0.5),
        Rgb::new(0.5, 1.0, 0.5),
        Rgb::new(0.5, 0.5, 1.0),
        Rgb::new(0.9, 0.9, 0.9),
    ];
    let alphas = [0.25, 0.5, 0.75, 0.95];
    for (rot, color) in GridRotation::CLOCKWISE.iterate().zip(&colors) {
        let windowpane =
            GridAab::from_lower_upper([-1, 0, 3], [2, alphas.len() as GridCoordinate, 4]);
        space.fill(
            windowpane
                .transform(rot.to_positive_octant_matrix(1))
                .unwrap(),
            |GridPoint { y, .. }| {
                Some(Block::from(
                    color.with_alpha(NotNan::new(alphas[y as usize]).unwrap()),
                ))
            },
        )?;
    }

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Voxel Transparency WIP",
    subtitle:
        "Transparency in complex blocks is not correctly implemented.\n\
        We also need something for surface properties.",
)]
async fn TRANSPARENCY_SMALL(_: &Exhibit, universe: &mut Universe) {
    let footprint = GridAab::from_lower_size([0, 0, 0], [7, 4, 7]);
    let pool = GridAab::from_lower_size([1, 0, 1], [5, 2, 5]);
    let mut space = Space::empty(footprint);

    let water_voxel = Block::builder()
        .color(rgba_const!(0.02, 0.04, 0.9, 0.5))
        .collision(BlockCollision::None)
        .build();
    let water_surface_voxel = Block::builder()
        .color(rgba_const!(0.5, 0.5, 0.72, 0.8))
        .collision(BlockCollision::None)
        .build();

    let water_surface_block = Block::builder()
        .voxels_fn(universe, R8, |p| match p.y {
            0..=3 => &water_voxel,
            4 => &water_surface_voxel,
            _ => &AIR,
        })?
        .build();

    let window_block = {
        let window_pane_resolution = R32;
        let depth = 3;
        let window_frame_block = Block::from(palette::ALMOST_BLACK);
        let window_glass_surface_block = Block::from(rgba_const!(0.5, 0.72, 0.5, 0.6));
        let window_glass_inner_block = Block::from(rgba_const!(0.7, 0.72, 0.7, 0.05));
        let upper = GridCoordinate::from(window_pane_resolution) - 1;

        Block::builder()
            .rotation_rule(RotationPlacementRule::Attach { by: Face6::NZ })
            .voxels_fn(universe, window_pane_resolution, |p| {
                if p.z >= depth {
                    return &AIR;
                }
                if p.x == 0 || p.y == 0 || p.x == upper || p.y == upper {
                    return &window_frame_block;
                }
                if p.z == depth - 1 || p.z == 0 {
                    if p.x == 1 || p.y == 1 || p.x == upper - 1 || p.y == upper - 1 {
                        &window_frame_block
                    } else {
                        &window_glass_surface_block
                    }
                } else {
                    &window_glass_inner_block
                }
            })?
            .build()
    };

    four_walls(
        pool.expand(FaceMap::symmetric([1, 0, 1])),
        |_origin, direction, _length, wall_excluding_corners| {
            space.fill_uniform(
                wall_excluding_corners,
                window_block
                    .clone()
                    .rotate(GridRotation::from_to(Face6::PX, direction, Face6::PY).unwrap()),
            )?;
            Ok::<(), InGenError>(())
        },
    )?;

    space.fill_uniform(
        pool.abut(Face6::NY, 0).unwrap().abut(Face6::PY, 1).unwrap(),
        &water_voxel,
    )?;
    space.fill_uniform(pool.abut(Face6::PY, -1).unwrap(), &water_surface_block)?;

    let [floater] = make_some_voxel_blocks(universe);
    space.set([3, 1, 3], floater)?;

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Knot",
    subtitle: "Complex voxel shape",
)]
async fn KNOT(this: &Exhibit, universe: &mut Universe) {
    let footprint = GridAab::from_lower_size([-2, -2, -1], [5, 5, 3]);
    let resolution = R32;
    let resf = FreeCoordinate::from(resolution);
    let toroidal_radius = resf * 1.5;
    let knot_split_radius = resf * 0.5625;
    let strand_radius = resf * 0.25;
    let twists = 2.5;

    let mut drawing_space = Space::builder(footprint.multiply(resolution.into()))
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build();
    let paint1 = Block::from(Rgba::new(0.7, 0.7, 0.7, 1.0));
    let paint2 = Block::from(Rgba::new(0.1, 0.1, 0.9, 1.0));
    let paint3 = Block::from(Rgba::new(0.9, 0.7, 0.1, 1.0));
    drawing_space.fill(drawing_space.bounds(), |p| {
        // Measure from midpoint of odd dimension space
        let p = p - Vector3::new(1, 1, 1) * (GridCoordinate::from(resolution) / 2);
        // Work in floating point
        let p = p.map(FreeCoordinate::from);

        let cylindrical = Vector2::new((p.x.powi(2) + p.y.powi(2)).sqrt(), p.z);
        let torus_cross_section = cylindrical - Vector2::new(toroidal_radius, 0.);
        let knot_center_angle = Rad(p.x.atan2(p.y));
        let rotated_cross_section =
            Basis2::from_angle(knot_center_angle * twists).rotate_vector(torus_cross_section);

        let angle_if_within_strand = |offset: Vector2<f64>| {
            let knot_center = rotated_cross_section
                .mul_element_wise(Vector2::new(1.0, 2.0_f64.sqrt().recip()))
                + offset;
            if knot_center.magnitude() < strand_radius {
                // Add center angle to add twist relative to the strands.
                Some(knot_center.x.atan2(knot_center.y) + knot_center_angle.0)
            } else {
                None
            }
        };

        // Compute stripe pattern
        // Note that the second strand is rotated by PI so they join up
        if let Some(strand_radial_angle) =
            angle_if_within_strand(Vector2::new(-knot_split_radius, 0.)).or_else(|| {
                angle_if_within_strand(Vector2::new(knot_split_radius, 0.)).map(|a| a + PI)
            })
        {
            let unit_range = (strand_radial_angle / (PI * 2.)).rem_euclid(1.0);
            Some(if unit_range < 0.25 {
                &paint2
            } else if (0.5..0.75).contains(&unit_range) {
                &paint3
            } else {
                &paint1
            })
        } else {
            None
        }
    })?;
    let space = space_to_blocks(
        resolution,
        BlockAttributes {
            display_name: this.name.into(),
            ..BlockAttributes::default()
        },
        universe.insert_anonymous(drawing_space),
    )?;
    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Text",
    subtitle: "",
)]
async fn TEXT(_: &Exhibit, universe: &mut Universe) {
    let space = draw_to_blocks(
        universe,
        R16,
        8,
        8..9,
        BlockAttributes::default(),
        &Text::with_baseline(
            "Hello block world",
            Point::new(0, 0),
            MonoTextStyle::new(&FONT_8X13_BOLD, Rgb888::new(120, 100, 200)),
            Baseline::Bottom,
        ),
    )?;
    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Animation",
    subtitle: "Blocks whose definition is animated",
)]
async fn ANIMATION(_: &Exhibit, universe: &mut Universe) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;

    let footprint = GridAab::from_lower_size([0, 0, -1], [3, 2, 3]);
    let mut space = Space::empty(footprint);

    let sweep_block = {
        let resolution = R8;
        let mut block_space = Space::for_block(resolution).build();
        // The length of this pattern is set so that the block will sometimes be fully opaque and sometimes be invisible.
        let fills = [
            AIR,
            AIR,
            AIR,
            AIR,
            AIR,
            Block::from(Rgb::new(0.0, 0.3, 0.0)),
            Block::from(Rgb::new(0.0, 0.7, 0.0)),
            Block::from(Rgb::new(0.0, 1.0, 0.0)),
            Block::from(Rgb::new(0.0, 0.7, 0.7)),
            Block::from(Rgb::new(0.0, 0.3, 1.0)),
        ];
        let repeats_per_fill = 6;
        SpaceTransaction::add_behavior(
            block_space.bounds(),
            AnimatedVoxels::new(move |p, frame| {
                let n = fills.len() as GridCoordinate * repeats_per_fill;
                let location_offset = p.x + p.y + p.z;
                let time_offset = (frame as GridCoordinate).rem_euclid(n);
                let value = location_offset.wrapping_sub(time_offset);
                fills[value
                    .div_euclid(repeats_per_fill)
                    .rem_euclid(fills.len() as GridCoordinate) as usize]
                    .clone()
            }),
        )
        .execute(&mut block_space, &mut transaction::no_outputs)?;
        Block::builder()
            .animation_hint(AnimationHint::CONTINUOUS)
            .voxels_ref(resolution, universe.insert_anonymous(block_space))
            .build()
    };

    let fire_block = {
        let fire_resolution = R8;
        Block::builder()
            .animation_hint(AnimationHint::CONTINUOUS)
            .light_emission(rgb_const!(1.4, 1.0, 0.8) * 8.0)
            .voxels_ref(fire_resolution, {
                let fire_bounds = GridAab::for_block(fire_resolution);
                let mut space = Space::for_block(fire_resolution).build();
                space.set([0, 0, 0], Rgb::ONE)?; // placeholder for not fully transparent so first pass lighting is better
                SpaceTransaction::add_behavior(fire_bounds, Fire::new(fire_bounds))
                    .execute(&mut space, &mut transaction::no_outputs)
                    .unwrap();
                universe.insert_anonymous(space)
            })
            .build()
    };

    space.set([0, 0, 0], sweep_block)?;
    space.set([2, 0, 0], fire_block)?;
    space.set([1, 1, -1], &demo_blocks[DemoBlocks::Clock])?;

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Collision",
    subtitle: "Test cases for character/world collision",
)]
async fn COLLISION(_: &Exhibit, universe: &mut Universe) {
    let half_block = make_slab(universe, 2, R4);

    let footprint = GridAab::from_lower_size([0, 0, 0], [5, 2, 4]);
    let mut space = Space::empty(footprint);

    for dx in -1..=1 {
        for dz in -1..=1 {
            let offset = GridVector::new(dx, 0, dz);
            space.set(
                GridPoint::new(1, 0, 1) + offset,
                // Rotate block so its +Y is towards the offset vector
                half_block.clone().rotate(match Face6::try_from(offset) {
                    Ok(face) => GridRotation::from_to(
                        Face6::PY,
                        face,
                        face.cross(Face6::PY).try_into().unwrap(),
                    )
                    .unwrap(),
                    Err(GridVector { x: 0, y: 0, z: 0 }) => GridRotation::RXyZ,
                    Err(_) => GridRotation::IDENTITY,
                }),
            )?;
        }
    }

    let range = footprint.z_range();
    for i in 0..(range.len() as GridCoordinate) {
        space.set(
            [4, 0, range.start + i],
            make_slab(universe, range.end - i, range.len().try_into().unwrap()),
        )?;
    }

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Resolutions",
    subtitle:
        "Voxel blocks can be subdivided into\n\
        powers of 2 from 2 to 256.",
)]
async fn RESOLUTIONS(_: &Exhibit, universe: &mut Universe) {
    let footprint = GridAab::from_lower_size([0, 0, 0], [5, 3, 3]);
    let mut space = Space::empty(footprint);

    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    for (i, &resolution) in (0i32..).zip([R1, R2, R4, R8, R16, R32].iter()) {
        stack(
            &mut space,
            GridPoint::new(i.rem_euclid(3) * 2, 0, i.div_euclid(3) * 2),
            [
                pedestal,
                &Block::builder()
                    .voxels_fn(universe, resolution, |p| {
                        if p.x + p.y + p.z >= GridCoordinate::from(resolution) {
                            return AIR.clone();
                        }
                        let rescale = if resolution > R8 { 4 } else { 1 };
                        let color = Rgb::from(p.to_vec().map(|s| {
                            NotNan::new(
                                (s / GridCoordinate::from(rescale)) as f32
                                    / f32::from(u16::from(resolution) / rescale - 1).max(1.),
                            )
                            .unwrap()
                        }));
                        Block::from(color)
                    })?
                    .build(),
                &draw_to_blocks(
                    universe,
                    R32,
                    0,
                    0..1,
                    BlockAttributes {
                        display_name: resolution.to_string().into(),
                        ..BlockAttributes::default()
                    },
                    &Text::with_baseline(
                        &resolution.to_string(),
                        Point::new(0, -1),
                        MonoTextStyle::new(&FONT_6X10, &non_colliding_text_pixel()),
                        Baseline::Bottom,
                    ),
                )?[GridPoint::origin()],
            ],
        )?;
    }

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "World's Smallest Voxel",
    subtitle: "1/128th the length of a standard block",
)]
async fn SMALLEST(_: &Exhibit, universe: &mut Universe) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    let resolution = R128;
    let rg = GridCoordinate::from(resolution);

    let block_space = Space::builder(GridAab::from_lower_size([rg / 2, 0, rg / 2], [1, 1, 1]))
        .filled_with(Block::from(palette::ALMOST_BLACK))
        .build();

    let mut exhibit_space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 2, 1])).build();
    stack(
        &mut exhibit_space,
        [0, 0, 0],
        [
            pedestal,
            &Block::builder()
                .display_name("World's Smallest Voxel")
                .voxels_ref(resolution, universe.insert_anonymous(block_space))
                .build(),
        ],
    )?;

    Ok(exhibit_space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Rotations",
    subtitle: "Rotated blocks and GridRotation::from_to()",
)]
async fn ROTATIONS(_: &Exhibit, universe: &mut Universe) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    let mut space = Space::empty(GridAab::from_lower_size([-2, 0, -2], [5, 5, 5]));

    let [_, central_block] = make_some_voxel_blocks(universe);
    let pointing_block = &demo_blocks[DemoBlocks::Arrow];

    let center = GridPoint::new(0, 0, 0);
    space.set(center, central_block)?;

    let mut place_rotated_arrow = |pos: GridPoint, rot: GridRotation| -> Result<(), InGenError> {
        stack(
            &mut space,
            pos,
            [
                &pointing_block.clone().rotate(rot),
                &draw_to_blocks(
                    universe,
                    R32,
                    0,
                    0..1,
                    BlockAttributes::default(),
                    &Text::with_baseline(
                        &format!("{rot:?}"),
                        Point::new(0, -1),
                        MonoTextStyle::new(&FONT_6X10, &non_colliding_text_pixel()),
                        Baseline::Bottom,
                    ),
                )?[GridPoint::origin()],
            ],
        )?;
        Ok(())
    };

    for face in [Face6::PX, Face6::PZ, Face6::NX, Face6::NZ] {
        place_rotated_arrow(
            center + face.normal_vector() * 2,
            GridRotation::from_to(Face6::NZ, face.opposite(), Face6::PY).unwrap(),
        )?;
    }

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Modifier::Zoom",
    subtitle: "",
)]
async fn ZOOM(_: &Exhibit, universe: &mut Universe) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;

    let specimen = &demo_blocks[DemoBlocks::LamppostBase];

    let scale = R8;
    let mut space = Space::builder(GridAab::for_block(scale)).build();

    // TODO: This algorithm should be generically available for creating Zoom instances,
    // rather than only an exhibit.
    for cube in space.bounds().interior_iter() {
        space
            .set(cube, {
                let mut zoom_block = specimen.clone();
                zoom_block
                    .modifiers_mut()
                    .push(Zoom::new(scale, cube.cast().unwrap()).into());
                zoom_block
            })
            .unwrap();
        if !space.get_evaluated(cube).visible {
            // Cancel placing useless invisible zoomed blocks.
            // Note: This is not an equivalent optimization (if the original block has
            // BlockCollision::Hard or animation).
            space.set(cube, AIR).unwrap();
        }
    }

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Modifier::Composite",
    subtitle: "",
)]
async fn COMPOSITE(_: &Exhibit, universe: &mut Universe) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;

    let sources = [
        &demo_blocks[DemoBlocks::Lamp],
        &demo_blocks[DemoBlocks::Arrow],
        &demo_blocks[DemoBlocks::Signboard],
    ];
    let destinations = [
        &demo_blocks[DemoBlocks::ExhibitBackground],
        &demo_blocks[DemoBlocks::GlassBlock],
        &demo_blocks[DemoBlocks::LamppostBase],
    ];
    let operators = [CompositeOperator::Over, CompositeOperator::In];

    let mut space = Space::empty(GridAab::from_lower_upper(
        [0, 0, 0],
        [
            destinations.len() as GridCoordinate * 2,
            operators.len() as GridCoordinate * 3,
            sources.len() as GridCoordinate * 2,
        ],
    ));
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    for (di, destination) in destinations.into_iter().enumerate() {
        for (si, source) in sources.into_iter().enumerate() {
            for (oi, operator) in operators.into_iter().enumerate() {
                let composite = destination.clone().with_modifier(Composite::new(
                    source.clone().rotate(GridRotation::CLOCKWISE),
                    operator,
                ));

                // TODO: stop using draw_to_blocks for these in favor of widgets?
                // or at least extract this to a function since 3 exhibits have them
                let label_str = format!(
                    "{s}\n{operator:?}\n{d}",
                    s = source.evaluate().unwrap().attributes.display_name,
                    d = destination.evaluate().unwrap().attributes.display_name
                );
                let label = draw_to_blocks(
                    universe,
                    R64,
                    0,
                    0..1,
                    BlockAttributes {
                        display_name: label_str.clone().into(),
                        ..BlockAttributes::default()
                    },
                    &Text::with_baseline(
                        &label_str,
                        Point::new(0, -(1 + 10 * 3)), // 3 lines above the bottom
                        MonoTextStyle::new(&FONT_6X10, &non_colliding_text_pixel()),
                        Baseline::Top,
                    ),
                )?[GridPoint::origin()]
                .clone();

                stack(
                    &mut space,
                    GridPoint::new(
                        di as GridCoordinate * 2,
                        oi as GridCoordinate * 3,
                        si as GridCoordinate * 2,
                    ),
                    [if oi == 0 { pedestal } else { &AIR }, &composite, &label],
                )?;
            }
        }
    }
    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Modifier::Move",
    subtitle: "Stationary but not animated cases.",
)]
async fn MOVED_BLOCKS(_: &Exhibit, universe: &mut Universe) {
    let mut space = Space::empty(GridAab::from_lower_upper([0, 0, -3], [16, 2, 3]));

    let blocks: [Block; 16] = make_some_voxel_blocks(universe);
    for x in 0..8 {
        for z in 0..2 {
            let i = x + z * 8;
            let distance = (i * 16).try_into().unwrap();
            let block = &blocks[i as usize];
            let [move_out, move_in] = Move::paired_move(Face6::PY, distance, 0);
            // TODO: Move should be able to spawn a "tail" on its own when animated?
            space.set(
                [x * 2, 0, (1 - z) * 2],
                block.clone().with_modifier(move_out),
            )?;
            space.set(
                [x * 2, 1, (1 - z) * 2],
                block.clone().with_modifier(move_in),
            )?;

            // Horizontal
            let [move_out, move_in] = Move::paired_move(Face6::PZ, distance, 0);
            space.set([i, 0, -2], block.clone().with_modifier(move_out))?;
            space.set([i, 0, -1], block.clone().with_modifier(move_in))?;
        }
    }
    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Colors",
    subtitle: "RGB cube of 5 linear color steps",
)]
async fn COLORS(_: &Exhibit, universe: &mut Universe) {
    let gradient_resolution = 5;
    let mut space = Space::empty(GridAab::from_lower_size(
        [0, 0, 0],
        [
            gradient_resolution * 2 - 1,
            gradient_resolution * 2,
            gradient_resolution * 2 - 1,
        ],
    ));

    space.fill(space.bounds(), |p| {
        let color_point = p / 2;
        let part_of_grid: [GridCoordinate; 3] = p.to_vec().map(|s| s.rem_euclid(2)).into();
        let color = Rgb::from(
            color_point
                .to_vec()
                .map(|s| NotNan::new(s as f32 / (gradient_resolution - 1) as f32).unwrap()),
        );
        let color_srgb = color.with_alpha_one().to_srgb8();
        let description = format!(
            "Linear\n  {:0.2}\n  {:0.2}\n  {:0.2}\nsRGB\n  #{:02x}{:02x}{:02x}",
            color.red(),
            color.green(),
            color.blue(),
            color_srgb[0],
            color_srgb[1],
            color_srgb[2]
        );
        match part_of_grid {
            [0, 0, 0] => Some(
                Block::builder()
                    .display_name(description)
                    .color(color.with_alpha_one())
                    .build(),
            ),
            [0, 1, 0] => Some({
                let resolution = R64;
                draw_to_blocks(
                    universe,
                    resolution,
                    0,
                    0..1,
                    BlockAttributes {
                        display_name: description.clone().into(),
                        ..BlockAttributes::default()
                    },
                    &Text::with_baseline(
                        &description,
                        Point::new(0, -i32::from(resolution)),
                        MonoTextStyle::new(&FONT_6X10, &non_colliding_text_pixel()),
                        Baseline::Top,
                    ),
                )
                .unwrap()[GridPoint::origin()] // TODO: Give Space an into_single_element() ?
                .clone()
                .rotate(GridRotation::RXzY)
            }),
            _ => None,
        }
    })?;

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Colored Lights",
    subtitle: "RGBCMY lights in an enclosed room",
)]
async fn COLOR_LIGHTS(_: &Exhibit, universe: &mut Universe) {
    let room_width = 11;
    let room_length = 16;
    let room_height = 7;
    let separator_width = 4; // less than room_width/2
    let brightness = 1.0;

    let interior = GridAab::from_lower_size([0, 0, 0], [room_width, room_height, room_length]);
    let mut space = Space::empty(interior.expand(FaceMap::repeat(1)));

    fn normalize(color: Rgb) -> Rgb {
        color * color.luminance().recip()
    }

    let light_colors = [
        rgb_const!(1.0, 0.0, 0.0),
        rgb_const!(1.0, 1.0, 0.0),
        rgb_const!(0.0, 1.0, 0.0),
        rgb_const!(0.0, 1.0, 1.0),
        rgb_const!(0.0, 0.0, 1.0),
        rgb_const!(1.0, 0.0, 1.0),
    ];
    let surface_colors = [
        rgb_const!(1.0, 0.0, 0.0),
        rgb_const!(1.0, 1.0, 0.0),
        rgb_const!(0.0, 1.0, 0.0),
        rgb_const!(0.0, 1.0, 1.0),
        rgb_const!(0.0, 0.0, 1.0),
        rgb_const!(1.0, 0.0, 1.0),
        rgb_const!(0.25, 0.25, 0.25),
        rgb_const!(0.75, 0.75, 0.75),
        rgb_const!(1.0, 1.0, 1.0),
    ];

    // Room wall block with test card
    let wall_color_block = Block::from(rgba_const!(0.5, 0.5, 0.5, 1.0));
    let wall_resolution = R16;
    let wall_block = {
        let colors_as_blocks: Vec<Block> =
            surface_colors.iter().copied().map(Block::from).collect();
        let mut wall_block_space = Space::for_block(wall_resolution)
            .filled_with(wall_color_block.clone())
            .build();
        for rotation in [
            GridRotation::IDENTITY,
            GridRotation::CLOCKWISE,
            GridRotation::CLOCKWISE * GridRotation::CLOCKWISE,
            GridRotation::COUNTERCLOCKWISE,
        ] {
            let mut plane = wall_block_space.draw_target(
                rotation.to_positive_octant_matrix(GridCoordinate::from(wall_resolution) - 1)
                    * GridMatrix::from_translation([4, 4, 15]),
            );
            for (i, swatch_block) in colors_as_blocks.iter().enumerate() {
                let i = i as GridCoordinate;
                Rectangle::new(
                    Point::new(i.rem_euclid(3) * 3, i.div_euclid(3) * 3),
                    Size::new(2, 2),
                )
                .draw_styled(&PrimitiveStyle::with_fill(swatch_block), &mut plane)?;
            }
        }

        Block::builder()
            .display_name("Color room wall")
            .voxels_ref(wall_resolution, universe.insert_anonymous(wall_block_space))
            .build()
    };

    // Wall corner
    let corner = Block::builder()
        .display_name("Color room wall corner")
        .rotation_rule(RotationPlacementRule::Attach { by: Face6::NZ }) // TODO: more specific
        .voxels_fn(universe, wall_resolution, |p| {
            if p.x.pow(2) + p.z.pow(2) < GridCoordinate::from(wall_resolution).pow(2) {
                &wall_color_block
            } else {
                &AIR
            }
        })?
        .build();

    // Construct room.
    crate::BoxStyle::from_whole_blocks_for_walls(
        Some(wall_block.clone()),
        Some(wall_block.clone()),
        Some(wall_block.clone()),
        Some(corner.rotate(GridRotation::RxYz)),
    )
    .create_box(interior.expand(FaceMap::repeat(1)))
    .execute(&mut space, &mut transaction::no_outputs)?;

    // Vertical separators
    space.fill_uniform(
        GridAab::from_lower_size([0, room_height / 2, 0], [separator_width, 1, room_length]),
        &wall_block,
    )?;
    space.fill_uniform(
        GridAab::from_lower_size(
            [room_width - separator_width, room_height / 2, 0],
            [separator_width, 1, room_length],
        ),
        &wall_block,
    )?;

    // Entrance door
    space.fill_uniform(
        GridAab::from_lower_size([room_width / 2 - 1, 0, room_length], [3, 2, 1]),
        &AIR,
    )?;

    // Place lights and horizontal separators
    for (i, color) in light_colors.iter().copied().enumerate() {
        let z =
            (i as GridCoordinate) * (room_length - 1) / (light_colors.len() as GridCoordinate - 1);
        let p = GridPoint::new(if i % 2 == 0 { 1 } else { room_width - 2 }, 0, z);
        space.set(
            p,
            Block::builder()
                .display_name("Colored light with colored surface")
                .color(color.with_alpha_one())
                .light_emission(normalize(color) * brightness)
                .build(),
        )?;
        space.set(
            p + GridVector::new(0, room_height - 1, 0),
            Block::builder()
                .display_name("Colored light with white surface")
                .color(Rgba::WHITE)
                .light_emission(normalize(color) * brightness)
                .build(),
        )?;

        // Separator between different light areas
        if i % 2 == 0 {
            space.fill_uniform(
                GridAab::from_lower_size(
                    [room_width - separator_width, 0, z],
                    [separator_width, room_height, 1],
                ),
                &wall_block,
            )?;
        } else {
            space.fill_uniform(
                GridAab::from_lower_size([0, 0, z], [separator_width, room_height, 1]),
                &wall_block,
            )?;
        }
    }

    // TODO: Add an RGBCMY section, and also a color-temperature section (or maybe different buildings)
    // sRGB white is D65, or approximately 6500 K.

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "ChunkChart",
    subtitle: "Volume of world chunks in view at a distance of 4.99",
)]
async fn CHUNK_CHART(_: &Exhibit, _: &mut Universe) {
    use all_is_cubes::chunking::ChunkChart;

    // TODO: Show more than one size.
    let chart = ChunkChart::<16>::new(16. * 4.99);
    Ok(chart.visualization())
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "make_some_blocks()",
    subtitle: "",
)]
async fn MAKE_SOME_BLOCKS(_: &Exhibit, universe: &mut Universe) {
    const ROWS: GridCoordinate = 5;
    fn make_both_blocks<const N: usize>(universe: &mut Universe) -> (Vec<Block>, Vec<Block>) {
        (
            Vec::from(make_some_blocks::<N>()),
            Vec::from(make_some_voxel_blocks::<N>(universe)),
        )
    }
    let rows: [(Vec<Block>, Vec<Block>); ROWS as usize] = [
        make_both_blocks::<5>(universe),
        make_both_blocks::<4>(universe),
        make_both_blocks::<3>(universe),
        make_both_blocks::<2>(universe),
        make_both_blocks::<1>(universe),
    ];
    let mut space = Space::empty_positive(3, ROWS, ROWS);
    for (y, (blocks_a, blocks_v)) in rows.into_iter().enumerate() {
        for (h, (block_a, block_v)) in blocks_a.into_iter().zip(blocks_v).enumerate() {
            space.set([0, y as GridCoordinate, h as GridCoordinate], block_a)?;
            space.set([2, y as GridCoordinate, h as GridCoordinate], block_v)?;
        }
    }
    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Dashed outline boxes",
    subtitle: "",
)]
async fn DASHED_BOXES(_: &Exhibit, universe: &mut Universe) {
    let color = Rgb::new(1.0, 0.5, 0.5);
    let brush = Block::from(color);
    let corner_brush = Block::from(color * 0.6);
    let line_segment = Block::builder()
        .display_name("Dashed Box Segment")
        .voxels_fn(universe, R16, |p| {
            let zmod = p.z.rem_euclid(4);
            if p.x == 0 && p.y == 0 && zmod > 0 && zmod < 3 {
                &brush
            } else {
                &AIR
            }
        })?
        .build();
    let corner = Block::builder()
        .display_name("Dashed Box Corner")
        .voxels_fn(universe, R16, |p| {
            if p.x < 2 && p.z < 2 && p.y < 2 {
                &corner_brush
            } else {
                &AIR
            }
        })?
        .build();
    let style = crate::BoxStyle::from_composited_corner_and_edge(corner, line_segment);

    let mut space = Space::empty_positive(7, 3, 3);
    // Unit sized box
    style
        .create_box(GridAab::from_lower_size([0, 0, 1], [1, 1, 1]))
        .execute(&mut space, &mut transaction::no_outputs)?;
    // Tall box
    style
        .create_box(GridAab::from_lower_size([2, 0, 1], [1, 3, 1]))
        .execute(&mut space, &mut transaction::no_outputs)?;
    // Large box
    style
        .create_box(GridAab::from_lower_size([4, 0, 0], [3, 3, 3]))
        .execute(&mut space, &mut transaction::no_outputs)?;

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Swimming Pool",
    subtitle: "Transparent blocks that can be passed through",
)]
async fn SWIMMING_POOL(_: &Exhibit, _: &mut Universe) {
    let width = 6;
    let depth = 6;
    let water_area = GridAab::from_lower_size([0, -depth, 0], [width, depth, width]);
    let mut space = Space::empty(water_area);
    space.fill_uniform(
        water_area,
        Block::builder()
            .display_name("Not entirely unlike water")
            .color(Rgba::new(0.96, 0.96, 1.0, 0.1))
            .collision(BlockCollision::None)
            .build(),
    )?;
    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "space_from_image()",
    subtitle: "Using rotations XYZ, XyZ, XZY, xYZ",
)]
async fn IMAGES(_: &Exhibit, universe: &mut Universe) {
    // TODO: it would be nice if this exhibit visualized the generated bounding box somehow

    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];

    let mut outer_space = Space::empty(GridAab::from_lower_size([0, 0, 0], [4, 2, 1]));

    let mut place = |position: [i32; 3], rotation: GridRotation| -> Result<(), InGenError> {
        let terrain_map_function = |pixel: image::Rgba<u8>| -> VoxelBrush<'static> {
            let image::Rgba([r, g, b, a]) = pixel;
            if (r > b || g > b) && a > 0 {
                let block = Block::from(Rgba::from_srgb8(pixel.0));
                VoxelBrush::with_thickness(block, 0..2).transform(rotation.to_rotation_matrix())
            } else {
                default_srgb(pixel)
            }
        };

        let image = include_image!("exhibits/terrain-image.png");
        let image_space = universe
            .insert_anonymous(space_from_image(image, rotation, terrain_map_function).unwrap());
        let block = Block::builder()
            .display_name(format!("{rotation:?}"))
            .voxels_ref(R16, image_space)
            .build();

        stack(&mut outer_space, position, [pedestal, &block])?;
        Ok(())
    };
    place([0, 0, 0], GridRotation::RXYZ)?;
    place([1, 0, 0], GridRotation::RXyZ)?;
    place([2, 0, 0], GridRotation::RXZY)?;
    place([3, 0, 0], GridRotation::RxYZ)?;

    Ok(outer_space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "UI Blocks",
    subtitle:
        "Blocks from the UI system (inactive)",
)]
async fn UI_BLOCKS(_: &Exhibit, universe: &mut Universe) {
    // TODO: This was designed for a render test and is still shaped for that rather than
    // any-viewpoint examination.

    use all_is_cubes_ui::vui::blocks::{ToolbarButtonState, UiBlocks};

    let icons = BlockProvider::<all_is_cubes::inv::Icons>::using(universe)?;
    let icons = icons.iter().map(|(_, block)| block.clone());

    let ui_blocks = BlockProvider::<UiBlocks>::using(universe)?;
    let ui_blocks = ui_blocks
            .iter()
            .filter(|&(key, _)| match key {
                // Filter out large number of pointer blocks
                UiBlocks::ToolbarPointer([
                    ToolbarButtonState::Unmapped,
                    ToolbarButtonState::Mapped,
                    ToolbarButtonState::Pressed
                ]) => true,
                UiBlocks::ToolbarPointer(_) => false,
                _ => true,
            })
            .map(|(_, block)| block.clone());

    let all_blocks: Vec<Block> = icons.chain(ui_blocks).collect();

    // Compute layout
    let count = all_blocks.len() as GridCoordinate;
    let row_length = 4;
    let bounds = GridAab::from_lower_upper(
        [0, 0, 0],
        [row_length, ((count + row_length - 1) / row_length), 2],
    );

    // Fill space with blocks
    let mut space = Space::builder(bounds)
        .spawn_position(Point3::new(
            FreeCoordinate::from(bounds.size().x) / 2.,
            FreeCoordinate::from(bounds.size().y) / 2.,
            FreeCoordinate::from(bounds.size().y) * 1.5,
        ))
        .build();
    for (index, block) in all_blocks.into_iter().enumerate() {
        let index = index as GridCoordinate;
        space
            .set(
                [
                    index.rem_euclid(row_length),
                    index.div_euclid(row_length),
                    0,
                ],
                block,
            )
            .unwrap();
    }

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Trees",
    subtitle: "",
)]
async fn TREES(_: &Exhibit, universe: &mut Universe) {
    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(128947981240);

    let n_x = 4;
    let n_z = 4;
    let spacing_x = 6;
    let spacing_z = 6;
    let bounds = GridAab::from_lower_upper(
        [-2, -1, -2],
        [(n_x - 1) * spacing_x + 3, 20, (n_z - 1) * spacing_z + 3],
    );
    let mut space = Space::builder(bounds).build();

    // Grassy floor
    space.fill_uniform(
        bounds.abut(Face6::NY, -1).unwrap(),
        &landscape_blocks[LandscapeBlocks::Grass],
    )?;

    for ix in 0..n_x {
        for iz in 0..n_z {
            let origin = GridPoint::new(ix * spacing_x, 0, iz * spacing_z);
            tree::make_tree(
                &landscape_blocks,
                &mut rng,
                origin,
                GridAab::single_cube(origin).expand(FaceMap {
                    nx: 2,
                    ny: 0,
                    nz: 2,
                    px: 2,
                    py: ix + iz * 2,
                    pz: 2,
                }),
            )?
            .execute(&mut space, &mut transaction::no_outputs)?;
        }
    }

    // exhibit of leaves growth stages for debugging
    for (i, g) in tree::TreeGrowth::exhaust().enumerate() {
        space.set(
            [i as GridCoordinate * 2, 0, bounds.lower_bounds().z],
            &landscape_blocks[LandscapeBlocks::Leaves(g)],
        )?;
    }

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(
    name: "Block Destruction",
    subtitle: "Animation prototype",
)]
async fn DESTRUCTION(_: &Exhibit, universe: &mut Universe) {
    let width = 7;

    let footprint = GridAab::from_lower_size([0, 0, 0], [width, 3, 1]);
    let mut space = Space::empty(footprint);

    let landscape_blocks = BlockProvider::<LandscapeBlocks>::using(universe)?;
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    let pedestal = &demo_blocks[DemoBlocks::Pedestal];
    let block_to_destroy = &landscape_blocks[LandscapeBlocks::Grass];

    fn generate_destruction_mask(
        universe: &mut Universe,
        resolution: Resolution,
        fraction: f64,
    ) -> Result<Block, InGenError> {
        let solid = Block::from(Rgba::WHITE);
        let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(3887829);
        let points: [_; 32] = std::array::from_fn(|_| {
            let free_point = Aab::from_cube(GridPoint::origin()).random_point(&mut rng);
            (
                free_point,
                if free_point.y > fraction {
                    AIR
                } else {
                    solid.clone()
                },
            )
        });
        let pattern = crate::voronoi_pattern(resolution, false, &points);

        Ok(Block::builder()
            .voxels_fn(universe, resolution, pattern)?
            .build())
    }

    for stage in 0i32..width {
        let mask = generate_destruction_mask(universe, R16, (stage as f64 + 0.5) / width as f64)?;
        let destroyed = block_to_destroy
            .clone()
            .with_modifier(Composite::new(mask, CompositeOperator::In).reversed());

        stack(
            &mut space,
            GridPoint::new(stage, 0, 0),
            [pedestal, &destroyed],
        )?;
    }

    Ok(space)
}

/// TODO: is this really the strategy we want to take?
/// It is a quick replacement for having discarded `BlockAttributes::collision`
/// that could override voxel collision.
fn non_colliding_text_pixel() -> Block {
    Block::builder()
        .color(palette::ALMOST_BLACK.with_alpha_one())
        .collision(BlockCollision::None)
        .build()
}

/// Place a series of blocks on top of each other, starting at the specified point.
///
/// TODO: think about whether this should be instead returning a `VoxelBrush` or a `SpaceTransaction` or something, for the future of composable worldgen
fn stack<'b, B>(
    space: &mut Space,
    origin: impl Into<GridPoint>,
    blocks: impl IntoIterator<Item = B>,
) -> Result<(), SetCubeError>
where
    B: Into<std::borrow::Cow<'b, Block>>,
{
    let origin = origin.into();
    for (y, block) in (0..).zip(blocks) {
        space.set(origin + GridVector::unit_y() * y, block)?;
    }
    Ok(())
}
