// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Miscellanous demonstrations of capability and manual test-cases.
//! The exhibits defined in this file are combined into [`crate::demo_city`].

use std::convert::TryFrom;
use std::f64::consts::PI;

use all_is_cubes::block::{
    space_to_blocks, AnimationHint, Block, BlockAttributes, BlockCollision, Modifier,
    Resolution::*, RotationPlacementRule, AIR,
};
use all_is_cubes::cgmath::{
    Basis2, ElementWise, EuclideanSpace as _, InnerSpace as _, Rad, Rotation as _, Rotation2,
    Vector2, Vector3,
};
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
    Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridMatrix, GridPoint, GridRotation,
    GridVector, NotNan, Rgb, Rgba,
};
use all_is_cubes::space::{Space, SpacePhysics};
use all_is_cubes::universe::Universe;
use all_is_cubes::{rgb_const, rgba_const};

use crate::{
    four_walls, make_slab, make_some_blocks, make_some_voxel_blocks, palette, AnimatedVoxels,
    DemoBlocks, Exhibit, Fire,
};

pub(crate) static DEMO_CITY_EXHIBITS: &[Exhibit] = &[
    KNOT,
    TRANSPARENCY_LARGE,
    TRANSPARENCY_SMALL,
    COLLISION,
    TEXT,
    RESOLUTIONS,
    ANIMATION,
    MAKE_SOME_BLOCKS,
    MOVED_BLOCKS,
    ROTATIONS,
    COLOR_LIGHTS,
    CHUNK_CHART,
    SWIMMING_POOL,
    COLORS,
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
#[exhibit(name: "Transparency")]
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
#[exhibit(name: "Voxel Transparency WIP")]
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
        .collision(BlockCollision::Recur)
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
            .collision(BlockCollision::Recur)
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
#[exhibit(name: "Knot")]
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
        .build_empty();
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
            collision: BlockCollision::Recur,
            ..BlockAttributes::default()
        },
        universe.insert_anonymous(drawing_space),
    )?;
    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(name: "Text")]
async fn TEXT(_: &Exhibit, universe: &mut Universe) {
    let space = draw_to_blocks(
        universe,
        R16,
        8,
        8..9,
        BlockAttributes {
            collision: BlockCollision::Recur,
            ..Default::default()
        },
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
#[exhibit(name: "Animation")]
async fn ANIMATION(_: &Exhibit, universe: &mut Universe) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;

    let footprint = GridAab::from_lower_size([0, 0, -1], [3, 2, 3]);
    let mut space = Space::empty(footprint);

    let sweep_block = {
        let resolution = R8;
        let mut block_space = Space::for_block(resolution).build_empty();
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
        block_space.add_behavior(AnimatedVoxels::new(move |p, frame| {
            let n = fills.len() as GridCoordinate * repeats_per_fill;
            let location_offset = p.x + p.y + p.z;
            let time_offset = (frame as GridCoordinate).rem_euclid(n);
            let value = location_offset.wrapping_sub(time_offset);
            fills[value
                .div_euclid(repeats_per_fill)
                .rem_euclid(fills.len() as GridCoordinate) as usize]
                .clone()
        }));
        Block::builder()
            .animation_hint(AnimationHint::CONTINUOUS)
            .collision(BlockCollision::Recur)
            .voxels_ref(resolution, universe.insert_anonymous(block_space))
            .build()
    };

    let fire_block = {
        let fire_resolution = R8;
        Block::builder()
            .animation_hint(AnimationHint::CONTINUOUS)
            .collision(BlockCollision::None)
            .light_emission(rgb_const!(1.4, 1.0, 0.8) * 8.0)
            .voxels_ref(fire_resolution, {
                let fire_bounds = GridAab::for_block(fire_resolution);
                let mut space = Space::for_block(fire_resolution).build_empty();
                space.set([0, 0, 0], Rgb::ONE)?; // placeholder for not fully transparent so first pass lighting is better
                space.add_behavior(Fire::new(fire_bounds));
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
#[exhibit(name: "Collision")]
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
#[exhibit(name: "Resolutions")]
async fn RESOLUTIONS(_: &Exhibit, universe: &mut Universe) {
    let footprint = GridAab::from_lower_size([0, 0, 0], [5, 2, 3]);
    let mut space = Space::empty(footprint);

    for (i, &resolution) in [R1, R2, R4, R8, R16, R32].iter().enumerate() {
        let i = i as GridCoordinate;
        let location = GridPoint::new(i.rem_euclid(3) * 2, 0, i.div_euclid(3) * 2);
        space.set(
            location,
            Block::builder()
                .collision(BlockCollision::Recur)
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
        )?;

        space.set(
            location + GridVector::unit_y(),
            &draw_to_blocks(
                universe,
                R32,
                0,
                0..1,
                BlockAttributes {
                    display_name: resolution.to_string().into(),
                    collision: BlockCollision::None,
                    ..BlockAttributes::default()
                },
                &Text::with_baseline(
                    &resolution.to_string(),
                    Point::new(0, -1),
                    MonoTextStyle::new(&FONT_6X10, palette::ALMOST_BLACK),
                    Baseline::Bottom,
                ),
            )?[GridPoint::origin()],
        )?;
    }

    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(name: "Rotations")]
async fn ROTATIONS(_: &Exhibit, universe: &mut Universe) {
    let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
    let mut space = Space::empty(GridAab::from_lower_size([-2, 0, -2], [5, 5, 5]));

    let [_, central_block] = make_some_voxel_blocks(universe);
    let pointing_block = &demo_blocks[DemoBlocks::Arrow];

    let center = GridPoint::new(0, 0, 0);
    space.set(center, central_block)?;

    let mut place_rotated_arrow = |pos: GridPoint, rot: GridRotation| -> Result<(), InGenError> {
        space.set(pos, pointing_block.clone().rotate(rot))?;
        space.set(
            pos + GridVector::unit_y(),
            &draw_to_blocks(
                universe,
                R32,
                0,
                0..1,
                BlockAttributes {
                    collision: BlockCollision::None,
                    ..BlockAttributes::default()
                },
                &Text::with_baseline(
                    &format!("{:?}", rot),
                    Point::new(0, -1),
                    MonoTextStyle::new(&FONT_6X10, palette::ALMOST_BLACK),
                    Baseline::Bottom,
                ),
            )?[GridPoint::origin()],
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
#[exhibit(name: "Moved Blocks")]
async fn MOVED_BLOCKS(_: &Exhibit, universe: &mut Universe) {
    let mut space = Space::empty(GridAab::from_lower_upper([0, 0, -3], [16, 2, 3]));

    let blocks: [Block; 16] = make_some_voxel_blocks(universe);
    for x in 0..8 {
        for z in 0..2 {
            let i = x + z * 8;
            let distance = (i * 16).try_into().unwrap();
            let block = &blocks[i as usize];
            let [move_out, move_in] = Modifier::paired_move(Face6::PY, distance, 0);
            // TODO: Move should be able to spawn a "tail" on its own when animated?
            space.set([x * 2, 0, (1 - z) * 2], move_out.attach(block.clone()))?;
            space.set([x * 2, 1, (1 - z) * 2], move_in.attach(block.clone()))?;

            // Horizontal
            let [move_out, move_in] = Modifier::paired_move(Face6::PZ, distance, 0);
            space.set([i, 0, -2], move_out.attach(block.clone()))?;
            space.set([i, 0, -1], move_in.attach(block.clone()))?;
        }
    }
    Ok(space)
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(name: "Colors")]
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
                        collision: BlockCollision::None,
                        ..BlockAttributes::default()
                    },
                    &Text::with_baseline(
                        &description,
                        Point::new(0, -i32::from(resolution)),
                        MonoTextStyle::new(&FONT_6X10, palette::ALMOST_BLACK),
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
#[exhibit(name: "Colored Lights")]
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
        let mut wall_block_space = Space::for_block(wall_resolution).build_empty();
        wall_block_space.fill_uniform(wall_block_space.bounds(), &wall_color_block)?;
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
                Rectangle::new(Point::new((i % 3) * 3, (i / 3) * 3), Size::new(2, 2))
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
    // Floor
    space.fill_uniform(interior.abut(Face6::NY, 1).unwrap(), &wall_block)?;
    // Ceiling
    space.fill_uniform(interior.abut(Face6::PY, 1).unwrap(), &wall_block)?;
    // Walls
    four_walls(
        space.bounds(),
        |origin, direction, _length, wall_excluding_corners| {
            // Corner pillar
            space.fill_uniform(
                GridAab::from_lower_size(origin + GridVector::unit_y(), [1, room_height + 1, 1]),
                corner
                    .clone()
                    .rotate(GridRotation::from_to(Face6::NZ, direction, Face6::PY).unwrap()),
            )?;
            // Wall face
            space.fill_uniform(wall_excluding_corners, &wall_block)?;
            Ok::<(), InGenError>(())
        },
    )?;

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
#[exhibit(name: "Visible chunk chart")]
async fn CHUNK_CHART(_: &Exhibit, _: &mut Universe) {
    use all_is_cubes::chunking::ChunkChart;

    // TODO: Show more than one size.
    let chart = ChunkChart::<16>::new(16. * 4.99);
    Ok(chart.visualization())
}

#[macro_rules_attribute::apply(exhibit!)]
#[exhibit(name: "make_some_blocks")]
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
#[exhibit(name: "Swimming Pool")]
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
