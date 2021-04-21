// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Miscellanous demonstrations of capability and manual test-cases.
//! The exhibits defined in this file are combined into [`crate::content::demo_city`].

use cgmath::{
    Basis2, EuclideanSpace as _, InnerSpace as _, Rad, Rotation as _, Rotation2, Vector2, Vector3,
};
use embedded_graphics::fonts::{Font8x16, Text};
use embedded_graphics::geometry::Point;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::style::TextStyleBuilder;
use ordered_float::NotNan;

use crate::block::{space_to_blocks, Block, BlockAttributes, BlockCollision, AIR};
use crate::content::Exhibit;
use crate::drawing::draw_to_blocks;
use crate::math::{FreeCoordinate, GridCoordinate, GridPoint, GridRotation, GridVector, Rgb, Rgba};
use crate::space::{Grid, Space};

pub(crate) static DEMO_CITY_EXHIBITS: &[Exhibit] = &[
    Exhibit {
        name: "Transparency",
        factory: |_this, _universe| {
            // TODO: Add some partial-block transparency once we're any good at implementing it
            let mut space = Space::empty(Grid::new([-3, 0, -3], [7, 5, 7]));

            let colors = [
                Rgb::new(1.0, 0.5, 0.5),
                Rgb::new(0.5, 1.0, 0.5),
                Rgb::new(0.5, 0.5, 1.0),
                Rgb::new(0.9, 0.9, 0.9),
            ];
            let alphas = [0.25, 0.5, 0.75, 0.95];
            for (rot, color) in GridRotation::CLOCKWISE.iterate().zip(&colors) {
                let windowpane =
                    Grid::from_lower_upper([-1, 0, 3], [2, alphas.len() as GridCoordinate, 4]);
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
        },
    },
    Exhibit {
        name: "Knot",
        factory: |this, universe| {
            let footprint = Grid::new([-2, -2, -1], [5, 5, 3]);
            let resolution = 16;
            let toroidal_radius = 24.;
            let knot_split_radius = 9.;
            let strand_radius = 6.;
            let twists = 2.5;

            let mut drawing_space = Space::empty(footprint.multiply(resolution));
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
        factory: |_this, universe| {
            let footprint = Grid::new([0, 0, 0], [5, 2, 3]);
            let mut space = Space::empty(footprint);

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
    {
        Exhibit {
            name: "Colors",
            factory: |_this, universe| {
                let gradient_resolution = 5;
                let mut space = Space::empty(Grid::new(
                    [0, 0, 0],
                    [
                        gradient_resolution * 2 - 1,
                        gradient_resolution * 2,
                        gradient_resolution * 2 - 1,
                    ],
                ));

                space.fill(space.grid(), |p| {
                    let color_point = p / 2;
                    let part_of_grid: [GridCoordinate; 3] =
                        p.to_vec().map(|s| s.rem_euclid(2)).into();
                    let color = Rgb::from(
                        color_point
                            .to_vec()
                            .map(|s| NotNan::new(s as f32 / (gradient_resolution - 1) as f32).unwrap()),
                    );
                    let color_srgb = color.with_alpha_one().to_srgb_32bit();
                    let description = format!(
                        "{:0.2}\n{:0.2}\n{:0.2}\n#{:02x}{:02x}{:02x} srgb",
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
                        [0, 1, 0] if false /* TODO: disabled because high-res blocks are too slow for now */ => Some(
                            draw_to_blocks(
                                universe,
                                64,
                                0,
                                BlockAttributes {
                                    display_name: description.clone().into(),
                                    collision: BlockCollision::None,
                                    ..BlockAttributes::default()
                                },
                                Text::new(
                                    &description,
                                    Point::new(0, -16 * (description.lines().count() as i32)),
                                )
                                .into_styled(
                                    TextStyleBuilder::new(Font8x16)
                                        .text_color(Rgb888::new(10, 10, 10))
                                        .build(),
                                ),
                            )
                            .unwrap()[GridPoint::origin()] // TODO: Give Space an into_single_element() ?
                            .clone()
                            .rotate(GridRotation::RXzY),
                        ),
                        _ => None,
                    }
                })?;

                Ok(space)
            },
        }
    },
    {
        Exhibit {
            name: "Visible chunk chart",
            factory: |_this, _universe| {
                use crate::chunking::ChunkChart;

                // TODO: Show more than one size.
                let chart = ChunkChart::<16>::new(16. * 4.99);
                Ok(chart.visualization())
            },
        }
    },
    {
        Exhibit {
            name: "make_some_blocks",
            factory: |_this, _universe| {
                use crate::content::make_some_blocks;
                const ROWS: GridCoordinate = 5;
                let rows: [Vec<Block>; ROWS as usize] = [
                    Vec::from(make_some_blocks::<5>()),
                    Vec::from(make_some_blocks::<4>()),
                    Vec::from(make_some_blocks::<3>()),
                    Vec::from(make_some_blocks::<2>()),
                    Vec::from(make_some_blocks::<1>()),
                ];
                let mut space = Space::empty_positive(1, ROWS, ROWS);
                for (y, blocks) in std::array::IntoIter::new(rows).enumerate() {
                    for (h, block) in blocks.into_iter().enumerate() {
                        space.set([0, y as GridCoordinate, h as GridCoordinate], block)?;
                    }
                }
                Ok(space)
            },
        }
    },
    {
        Exhibit {
            name: "Swimming Pool",
            factory: |_this, _universe| {
                let width = 6;
                let depth = 6;
                let water_area = Grid::new([0, -depth, 0], [width, depth, width]);
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
            },
        }
    },
];
