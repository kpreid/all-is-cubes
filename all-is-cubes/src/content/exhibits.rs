// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Miscellanous demonstrations of capability and manual test-cases.
//! The exhibits defined in this file are combined into [`crate::content::demo_city`].

use cgmath::{
    Basis2, EuclideanSpace as _, InnerSpace as _, Rad, Rotation as _, Rotation2, Vector2, Vector3,
};
use embedded_graphics::geometry::Point;
use embedded_graphics::mono_font::iso_8859_1::{FONT_6X10, FONT_8X13_BOLD};
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::text::{Baseline, Text};
use ordered_float::NotNan;

use crate::block::{space_to_blocks, Block, BlockAttributes, BlockCollision, AIR};
use crate::content::{palette, DemoBlocks, Exhibit};
use crate::drawing::draw_to_blocks;
use crate::linking::{BlockProvider, InGenError};
use crate::math::{
    Face, FreeCoordinate, GridCoordinate, GridPoint, GridRotation, GridVector, Rgb, Rgba,
};
use crate::space::{Grid, Space};
use crate::universe::Universe;

pub(crate) static DEMO_CITY_EXHIBITS: &[Exhibit] = &[
    TRANSPARENCY,
    KNOT,
    TEXT,
    RESOLUTIONS,
    MAKE_SOME_BLOCKS,
    ROTATIONS,
    CHUNK_CHART,
    SWIMMING_POOL,
    COLORS,
];

const TRANSPARENCY: Exhibit = Exhibit {
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
};

const KNOT: Exhibit = Exhibit {
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
};

const TEXT: Exhibit = Exhibit {
    name: "Text",
    factory: |_, universe| {
        let space = draw_to_blocks(
            universe,
            16,
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
    },
};

const RESOLUTIONS: Exhibit = Exhibit {
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
                    32,
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
    },
};

const ROTATIONS: Exhibit = Exhibit {
    name: "Rotations",
    factory: |_this, universe| {
        let demo_blocks = BlockProvider::<DemoBlocks>::using(universe)?;
        let mut space = Space::empty(Grid::new([-2, 0, -2], [5, 5, 5]));

        let [_, central_block] = crate::content::make_some_voxel_blocks(universe);
        let pointing_block = &demo_blocks[DemoBlocks::Arrow];

        let center = GridPoint::new(0, 0, 0);
        space.set(center, central_block)?;

        let mut place_rotated_arrow =
            |pos: GridPoint, rot: GridRotation| -> Result<(), InGenError> {
                space.set(pos, pointing_block.clone().rotate(rot))?;
                space.set(
                    pos + GridVector::unit_y(),
                    &draw_to_blocks(
                        universe,
                        32,
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

        for face in [Face::PX, Face::PZ, Face::NX, Face::NZ] {
            place_rotated_arrow(
                center + face.normal_vector() * 2,
                GridRotation::from_to(Face::NZ, face.opposite(), Face::PY).unwrap(),
            )?;
        }

        Ok(space)
    },
};

const COLORS: Exhibit = Exhibit {
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
            let part_of_grid: [GridCoordinate; 3] = p.to_vec().map(|s| s.rem_euclid(2)).into();
            let color = Rgb::from(
                color_point
                    .to_vec()
                    .map(|s| NotNan::new(s as f32 / (gradient_resolution - 1) as f32).unwrap()),
            );
            let color_srgb = color.with_alpha_one().to_srgb_32bit();
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
                    let resolution = 64;
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
    },
};

const CHUNK_CHART: Exhibit = Exhibit {
    name: "Visible chunk chart",
    factory: |_this, _universe| {
        use crate::chunking::ChunkChart;

        // TODO: Show more than one size.
        let chart = ChunkChart::<16>::new(16. * 4.99);
        Ok(chart.visualization())
    },
};

const MAKE_SOME_BLOCKS: Exhibit = Exhibit {
    name: "make_some_blocks",
    factory: |_this, mut universe| {
        use crate::content::{make_some_blocks, make_some_voxel_blocks};
        const ROWS: GridCoordinate = 5;
        fn make_both_blocks<const N: usize>(universe: &mut Universe) -> (Vec<Block>, Vec<Block>) {
            (
                Vec::from(make_some_blocks::<N>()),
                Vec::from(make_some_voxel_blocks::<N>(universe)),
            )
        }
        let rows: [(Vec<Block>, Vec<Block>); ROWS as usize] = [
            make_both_blocks::<5>(&mut universe),
            make_both_blocks::<4>(&mut universe),
            make_both_blocks::<3>(&mut universe),
            make_both_blocks::<2>(&mut universe),
            make_both_blocks::<1>(&mut universe),
        ];
        let mut space = Space::empty_positive(3, ROWS, ROWS);
        for (y, (blocks_a, blocks_v)) in std::array::IntoIter::new(rows).enumerate() {
            for (h, (block_a, block_v)) in blocks_a.into_iter().zip(blocks_v).enumerate() {
                space.set([0, y as GridCoordinate, h as GridCoordinate], block_a)?;
                space.set([2, y as GridCoordinate, h as GridCoordinate], block_v)?;
            }
        }
        Ok(space)
    },
};

const SWIMMING_POOL: Exhibit = Exhibit {
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
};
