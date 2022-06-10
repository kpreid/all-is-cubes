// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! A voxel reinterpretation of the famous Sponza Atrium test scene.

use std::fmt;

use exhaust::Exhaust;
use noise::Seedable;

use all_is_cubes::block::{
    space_to_blocks, Block, BlockAttributes, BlockCollision, RotationPlacementRule, AIR,
};
use all_is_cubes::cgmath::{EuclideanSpace as _, Point3, Transform, Vector3};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::{free_editing_starter_inventory, palette};
use all_is_cubes::linking::{BlockModule, BlockProvider, InGenError};
use all_is_cubes::math::{
    Face6, Face7, FaceMap, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridRotation,
    GridVector, Rgb,
};
use all_is_cubes::rgba_const;
use all_is_cubes::space::{Grid, GridArray, SetCubeError, Space, SpacePhysics};
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;

use crate::noise::array_of_noise;
use crate::{four_walls, scale_color, Fire};

/// A special name for "the thickness of a 1-block-thick wall/floor/pillar", for readability.
const WALL: GridCoordinate = 1;

pub(crate) async fn atrium(
    universe: &mut Universe,
    progress: YieldProgress,
) -> Result<Space, InGenError> {
    // TODO: subdivide progress
    let blocks = install_atrium_blocks(universe, progress).await?;

    let ceiling_height = 6;
    let between_small_arches = 3;
    let between_large_arches = between_small_arches * 2 + 1;
    let balcony_radius = 4;
    let large_arch_count = Vector3::new(1, 0, 5); // x, dummy y, z
    let floor_count = 4;

    let origin = Grid::new([0, 0, 0], [1, 1, 1]);
    let atrium_footprint = origin.expand(FaceMap::symmetric([
        ((between_large_arches + WALL) * large_arch_count.x) / 2 - WALL,
        0,
        ((between_large_arches + WALL) * large_arch_count.z) / 2 - WALL,
    ]));
    let arches_footprint = atrium_footprint.expand(FaceMap::symmetric([WALL, 0, WALL]));
    let balconies_footprint =
        arches_footprint.expand(FaceMap::symmetric([balcony_radius, 0, balcony_radius]));
    let outer_walls_footprint = balconies_footprint.expand(FaceMap::symmetric([WALL, 0, WALL]));

    let balcony_floor_pos = GridVector::new(0, ceiling_height + WALL, 0);
    let top_floor_pos = GridVector::new(0, (ceiling_height + WALL) * 2, 0);

    let space_grid = outer_walls_footprint
        .expand(FaceMap::default().with(Face7::PY, ceiling_height * floor_count));

    let floor_with_cutout = |mut p: GridPoint| {
        p.y = 0;
        if atrium_footprint.contains_cube(p) {
            None
        } else {
            Some(&blocks[AtriumBlocks::UpperFloor])
        }
    };

    let mut space = Space::builder(space_grid)
        .spawn({
            // TODO: default_for_new_space isn't really doing anything for us here
            let mut spawn = Spawn::default_for_new_space(space_grid);
            spawn.set_eye_position(Point3::new(
                0.5,
                1.91 + FreeCoordinate::from(ceiling_height + 2),
                10.0,
            ));
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .sky_color(Rgb::new(1.0, 1.0, 0.9843) * 4.0)
        .build_empty();

    // Outer walls
    four_walls(
        space_grid,
        |_origin, direction, _length, wall_excluding_corners| -> Result<(), InGenError> {
            space.fill_uniform(
                wall_excluding_corners,
                blocks[AtriumBlocks::GroundFloor]
                    .clone()
                    .rotate(GridRotation::from_to(Face6::PZ, direction, Face6::PY).unwrap()),
            )?;
            Ok(())
        },
    )?;

    // Ground floor
    space.fill_uniform(outer_walls_footprint, &blocks[AtriumBlocks::GroundFloor])?;

    // Balcony floor
    space.fill(
        outer_walls_footprint.translate(balcony_floor_pos),
        floor_with_cutout,
    )?;

    // Top floor (solid)
    space.fill(
        outer_walls_footprint
            .translate(top_floor_pos)
            .expand(FaceMap::from_fn(|f| {
                GridCoordinate::from(f == Face7::PY) * ceiling_height
            })),
        floor_with_cutout,
    )?;

    // Arches and atrium walls
    #[rustfmt::skip]
    let arches_pattern = GridArray::from_y_flipped_array([[
        *br"########", // Roof edge height
        *br"####.###",
        *br"########",
        *br"########",
        *br"########",
        *br"####.###",
        *br"########",
        *br"aaaaaaaa", // top floor height
        *br"aaaaaaaa",
        *br"aa aaa a",
        *br"|   o   ",
        *br"|   o   ", 
        *br"########",
        *br"##AAAAA#", // balcony floor height
        *br"AAAA AAA",
        *br"AAA   AA",
        *br"AA     A",
        *br"G       ",
        *br"G       ", 
        *br"G       ",
    ], [
        *br"TTTTTTTT", // roof edge height
        *br"        ",
        *br"        ",
        *br"        ",
        *br"        ",
        *br"        ",
        *br"TTTTTTTT",
        *br"    f   ", // top floor height
        *br"        ",
        *br"        ",
        *br"        ",
        *br"        ",
        *br"TTTTTTTT",
        *br"        ", // balcony floor height
        *br"        ",
        *br"        ",
        *br"        ",
        *br"        ",
        *br"        ", 
        *br"P       ",
    ]]);
    four_walls(
        arches_footprint.translate([0, WALL, 0]),
        |origin, direction, length, _box| {
            arch_row(
                &mut space,
                &blocks,
                origin,
                between_large_arches + WALL,
                length / (between_large_arches + WALL),
                direction,
                &arches_pattern,
            )
        },
    )?;

    space.fast_evaluate_light();

    Ok(space)
}

fn map_text_block(
    ascii: u8,
    blocks: &BlockProvider<AtriumBlocks>,
    cube: GridPoint,
    existing_block: Block,
) -> Block {
    match ascii {
        b' ' => existing_block,
        b'.' => AIR,
        b'#' => blocks[AtriumBlocks::SolidBricks]
            .clone()
            .rotate(GridRotation::CLOCKWISE),
        b'G' => blocks[AtriumBlocks::GroundColumn].clone(),
        b'o' => blocks[AtriumBlocks::SmallColumn].clone(),
        b'|' => blocks[AtriumBlocks::SquareColumn].clone(),
        b'A' => {
            let start_y = 3;
            let mirrored_half = cube.x >= 4;
            let block = lookup_multiblock_2d(
                blocks,
                AtriumBlocks::GroundArch,
                [
                    if mirrored_half { 8 - cube.x } else { cube.x },
                    cube.y - start_y,
                ],
            );
            // TODO: Coordinate systems in use aren't really consistent.
            // If we reorient the bricks so that they run along the X axis instead of the Z axis, that should help.
            block.rotate(
                GridRotation::CLOCKWISE
                    * if mirrored_half {
                        GridRotation::IDENTITY
                    } else {
                        GridRotation::RXYz
                    },
            )
        }
        b'a' => {
            let start_y = 10; // TODO: should be vs. ceiling_height
            let mut cube = cube;
            cube.x = cube.x.rem_euclid(4);
            let mirrored_half = cube.x >= 3;
            let block = lookup_multiblock_2d(
                blocks,
                AtriumBlocks::UpperArch,
                [
                    if mirrored_half { 4 - cube.x } else { cube.x },
                    cube.y - start_y,
                ],
            );
            // TODO: Coordinate systems in use aren't really consistent.
            // If we reorient the bricks so that they run along the X axis instead of the Z axis, that should help.
            block.rotate(
                GridRotation::CLOCKWISE
                    * if mirrored_half {
                        GridRotation::IDENTITY
                    } else {
                        GridRotation::RXYz
                    },
            )
        }
        b'T' => possibly_corner_block(
            existing_block,
            &blocks[AtriumBlocks::Molding]
                .clone()
                .rotate(GridRotation::CLOCKWISE),
            &blocks[AtriumBlocks::MoldingCorner]
                .clone()
                .rotate(GridRotation::CLOCKWISE),
        ),
        // TODO: These are supposed to be planters
        b'P' => blocks[AtriumBlocks::Firepot].clone(),
        // Not-yet-implemented decoration placeholder blocks
        b'f' => Block::from(rgba_const!(1.0, 0.5, 0.5, 1.0)),
        _ => panic!(
            "Unrecognized block character {:?}",
            std::str::from_utf8(&[ascii])
        ),
    }
}

/// Given a block in "straight" and "corner" forms, replace empty air with the straight
/// form and replace a rotated straight form with the corner form.
/// TODO: explain the corner connectivity/rotation assumption
fn possibly_corner_block(existing_block: Block, new_block: &Block, new_corner: &Block) -> Block {
    let adjacent_ccw = new_block.clone().rotate(GridRotation::COUNTERCLOCKWISE);
    let adjacent_cw = new_block.clone().rotate(GridRotation::CLOCKWISE);
    if existing_block == AIR {
        new_block.clone()
    } else if existing_block == adjacent_cw {
        new_corner.clone()
    } else if existing_block == adjacent_ccw {
        new_corner.clone().rotate(GridRotation::COUNTERCLOCKWISE)
    } else {
        // Don't overwrite
        existing_block
    }
}

#[allow(clippy::too_many_arguments)]
fn arch_row(
    space: &mut Space,
    blocks: &BlockProvider<AtriumBlocks>,
    first_column_base: GridPoint,
    section_length: GridCoordinate,
    section_count: GridCoordinate,
    parallel: Face6,
    pattern: &GridArray<u8>,
) -> Result<(), InGenError> {
    let offset = parallel.normal_vector() * section_length;
    let rotation = GridRotation::from_to(Face6::NX, parallel, Face6::PY).unwrap();
    for i in 0..section_count {
        let column_base = first_column_base + offset * (i + 1);

        fill_space_transformed(
            |p, block| map_text_block(pattern[p], blocks, p, block),
            pattern.grid(),
            space,
            GridMatrix::from_translation(column_base.to_vec())
                * rotation.to_positive_octant_matrix(1),
        )?;
    }
    Ok::<(), InGenError>(())
}

// TODO: figure out what the general version of this is and move it elsewhere
fn fill_space_transformed(
    src: impl Fn(GridPoint, Block) -> Block,
    src_grid: Grid,
    dst: &mut Space,
    src_to_dst_transform: GridMatrix,
) -> Result<(), SetCubeError> {
    // TODO: don't panic
    let dst_to_src_transform = src_to_dst_transform.inverse_transform().unwrap();
    let (block_rotation, _) = src_to_dst_transform
        .decompose()
        .expect("could not decompose transform");
    for cube in src_grid
        .transform(src_to_dst_transform)
        .unwrap()
        .interior_iter()
    {
        let existing_block = dst[cube].clone().rotate(block_rotation.inverse());
        let new_block =
            src(dst_to_src_transform.transform_cube(cube), existing_block).rotate(block_rotation);
        dst.set(cube, new_block)?;
    }
    Ok(())
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
enum AtriumBlocks {
    GroundFloor,
    UpperFloor,
    SolidBricks,
    GroundArch(UpTo5, UpTo5),
    UpperArch(UpTo5, UpTo5),
    GroundColumn,
    SquareColumn,
    SmallColumn,
    Molding,
    MoldingCorner,
    Firepot,
}
impl BlockModule for AtriumBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/atrium"
    }
}
impl fmt::Display for AtriumBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: We need a better pattern than writing these out manually
        match self {
            AtriumBlocks::GroundFloor => write!(f, "ground-floor"),
            AtriumBlocks::UpperFloor => write!(f, "upper-floor"),
            AtriumBlocks::SolidBricks => write!(f, "solid-bricks"),
            AtriumBlocks::GroundArch(x, y) => write!(f, "ground-arch/{x}-{y}"),
            AtriumBlocks::UpperArch(x, y) => write!(f, "upper-arch/{x}-{y}"),
            AtriumBlocks::GroundColumn => write!(f, "ground-column"),
            AtriumBlocks::SquareColumn => write!(f, "square-column"),
            AtriumBlocks::SmallColumn => write!(f, "small-column"),
            AtriumBlocks::Molding => write!(f, "molding"),
            AtriumBlocks::MoldingCorner => write!(f, "molding-corner"),
            AtriumBlocks::Firepot => write!(f, "firepot"),
        }
    }
}

async fn install_atrium_blocks(
    universe: &mut Universe,
    progress: YieldProgress,
) -> Result<BlockProvider<AtriumBlocks>, InGenError> {
    let resolution = 16;
    let resolution_g = GridCoordinate::from(resolution);
    let stone_base = Block::from(rgba_const!(0.53, 0.48, 0.40, 1.0));
    let heavy_grout_base = Block::from(rgba_const!(0.1, 0.1, 0.1, 1.0));
    let grout_base = Block::from(rgba_const!(0.32, 0.30, 0.28, 1.0));
    let ceiling_paint = Block::from(rgba_const!(0.975, 0.975, 0.975, 1.0));

    // TODO: This whole section is about having noise pick from a fixed set of pregenerated shades.
    // We should abstract it out if we like this style
    #[allow(clippy::cast_lossless)] // warns on 32-bit but f64::from() is not an option on 64-bit
    let stone_range: Vec<Block> = (-2..=2_isize)
        .map(|x| scale_color(stone_base.clone(), 1.0 + x as f64 * 0.08, 0.02))
        .collect();

    let stone_base_array = {
        let stone_noise_v = noise::OpenSimplex::new().set_seed(0x2e240365);
        let stone_noise_sc =
            noise::ScalePoint::new(&stone_noise_v).set_scale(4.0 / f64::from(resolution_g));

        // increased resolution to support brick offset patterning
        array_of_noise(resolution, &stone_noise_sc, |value| {
            &stone_range[(value * 8.0 + 2.5).round().clamp(0.0, 4.0) as usize]
        })
    };

    let brick_pattern = |mut p: GridPoint| {
        if (p.x.rem_euclid(resolution_g) > resolution_g / 2)
            ^ (p.y.rem_euclid(resolution_g) > resolution_g / 2)
        {
            // Create brick half-overlap offset
            p.z = (p.z + resolution_g / 2) % resolution_g;
        }
        let bricking = (p.x % 8).min(p.y % 8).min(p.z % 16);
        if bricking == 0 {
            &grout_base
        } else {
            stone_base_array[p.map(|c| c.rem_euclid(resolution_g))]
        }
    };
    let bottom_grout_pattern = |p: GridPoint| {
        if p.y == 0 {
            &grout_base
        } else {
            stone_base_array[p]
        }
    };
    let molding_fn = |p: GridPoint| {
        let shape: [GridCoordinate; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 2, 3, 4, 4, 3];
        if p.x < shape[p.y as usize] {
            brick_pattern(p)
        } else {
            &AIR
        }
    };

    let ground_floor_arch_blocks =
        generate_arch(universe, &stone_range, &brick_pattern, resolution, 7, 3)?;
    let upper_floor_arch_blocks =
        generate_arch(universe, &stone_range, &brick_pattern, resolution, 3, 2)?;

    // TODO: duplicated procgen code — figure out a good toolkit of math helpers
    let one_diagonal = GridVector::new(1, 1, 1);
    let center_point_doubled = GridPoint::from_vec(one_diagonal * resolution_g);

    BlockProvider::<AtriumBlocks>::new(progress, |key| {
        Ok(match key {
            AtriumBlocks::GroundFloor => Block::builder()
                .display_name("Atrium Ground Floor")
                .voxels_fn(universe, resolution, |p| {
                    if p.x == 0 {
                        &heavy_grout_base
                    } else {
                        brick_pattern(p)
                    }
                })?
                .build(),
            AtriumBlocks::UpperFloor => Block::builder()
                .display_name("Atrium Upper Floor")
                .voxels_fn(universe, resolution, |p| {
                    // Add a white ceiling
                    if p.y == 0 {
                        &ceiling_paint
                    } else {
                        brick_pattern(p)
                    }
                })?
                .build(),
            AtriumBlocks::SolidBricks => Block::builder()
                .display_name("Atrium Wall Bricks")
                .voxels_fn(universe, resolution, brick_pattern)?
                .build(),
            AtriumBlocks::GroundArch(x, y) => {
                ground_floor_arch_blocks[[0, y.to_int(), x.to_int()]].clone()
            }
            AtriumBlocks::UpperArch(x, y) => {
                upper_floor_arch_blocks[[0, y.to_int(), x.to_int()]].clone()
            }
            AtriumBlocks::GroundColumn => Block::builder()
                .display_name("Large Atrium Column")
                .collision(BlockCollision::Recur)
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(universe, resolution, |p| {
                    let mid = (p * 2 - center_point_doubled).map(|c| c.abs());
                    if mid.x + mid.z < resolution_g * 6 / 4 {
                        bottom_grout_pattern(p)
                    } else {
                        &AIR
                    }
                })?
                .build(),
            AtriumBlocks::SquareColumn => Block::builder()
                .display_name("Square Atrium Column")
                .collision(BlockCollision::Recur)
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(universe, resolution, |p| {
                    let mid = (p * 2 - center_point_doubled).map(|c| c.abs());
                    if mid.x.max(p.z) < resolution_g * 6 / 4 {
                        bottom_grout_pattern(p)
                    } else {
                        &AIR
                    }
                })?
                .build(),
            AtriumBlocks::SmallColumn => Block::builder()
                .display_name("Round Atrium Column")
                .collision(BlockCollision::Recur)
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(universe, resolution, |p| {
                    let mid = (p * 2 - center_point_doubled).map(|c| c.abs());
                    if mid.x.pow(2) + mid.z.pow(2) < (resolution_g * 3 / 4).pow(2) {
                        bottom_grout_pattern(p)
                    } else {
                        &AIR
                    }
                })?
                .build(),
            AtriumBlocks::Molding => Block::builder()
                .display_name("Atrium Top Edge Molding")
                .collision(BlockCollision::Recur)
                // TODO: rotation rule
                .voxels_fn(universe, resolution, molding_fn)?
                .build(),
            AtriumBlocks::MoldingCorner => Block::builder()
                .display_name("Atrium Top Edge Molding Corner")
                .collision(BlockCollision::Recur)
                .voxels_fn(universe, resolution, |mut p| {
                    if p.x > p.z {
                        p = GridRotation::COUNTERCLOCKWISE
                            .to_positive_octant_matrix(resolution.into())
                            .transform_cube(p);
                    }
                    molding_fn(p)
                })?
                .build(),
            AtriumBlocks::Firepot => Block::builder()
                .display_name("Firepot")
                // .light_emission(rgb_const!(1.4, 1.0, 0.8) * 4.0)
                .collision(BlockCollision::Recur)
                .voxels_ref(resolution, {
                    let mut space = Space::for_block(resolution).build_empty();
                    // Use a darker color to dampen the effect of interior light
                    let body_block = Block::from(palette::STEEL * 0.2);
                    space.fill(
                        Grid::from_lower_upper(
                            [0, 0, 0],
                            [resolution_g, resolution_g / 2, resolution_g],
                        ),
                        |p| {
                            let mid = (p * 2 - center_point_doubled).map(|c| c.abs());
                            if mid.x.max(mid.z) + (mid.y / 2) < resolution_g + 4 {
                                Some(&body_block)
                            } else {
                                None
                            }
                        },
                    )?;
                    let fire_inset = 2;
                    if false {
                        // TODO: Actually enable the fire. We need graphics optimizations for animation first, or this will swamp chunk updating capacity.
                        space.add_behavior(Fire::new(Grid::from_lower_upper(
                            // Vertical overlap will be overwritten, making a bowl shape
                            [fire_inset, resolution_g / 2 - 2, fire_inset],
                            [
                                resolution_g - fire_inset,
                                resolution_g,
                                resolution_g - fire_inset,
                            ],
                        )));
                    }
                    universe.insert_anonymous(space)
                })
                .build(),
        })
    })
    .await?
    .install(universe)?;
    Ok(BlockProvider::<AtriumBlocks>::using(universe)?)
}

fn generate_arch<'b>(
    universe: &mut Universe,
    stone_range: &[Block], // TODO: clarify
    brick_pattern: impl Fn(GridPoint) -> &'b Block,
    resolution: u8,
    width_blocks: GridCoordinate,
    height_blocks: GridCoordinate,
) -> Result<Space, SetCubeError> {
    let resolution_g: GridCoordinate = resolution.into();
    let space = {
        let arch_opening_width = resolution_g * width_blocks;
        let arch_opening_height = resolution_g * height_blocks;
        let arch_center_z_doubled = resolution_g * (width_blocks - 1) /* midpoint assuming odd width */
            + resolution_g * 3 /* offset by a block and a half */;
        let mut space = Space::builder(Grid::from_lower_upper(
            [0, 0, 0],
            [
                resolution_g,
                arch_opening_height + resolution_g,
                arch_center_z_doubled + resolution_g * 3,
            ],
        ))
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build_empty();
        space.fill(space.grid(), |p| {
            // Flip middle of first block, so that the arch to our left appears on it
            let z_for_arch /* but not for bricks */ = if p.z < resolution_g / 2 {
                resolution_g - 1 - p.z
            } else {
                p.z
            };
            let arch_z_doubled = z_for_arch * 2 - arch_center_z_doubled;
            let arch_y_doubled = p.y * 2;
            let distance_from_edge = p.x.min(resolution_g - 1 - p.x) as f32 / resolution_g as f32;
            let r = (arch_z_doubled as f32 / arch_opening_width as f32)
                .hypot(arch_y_doubled as f32 / (arch_opening_height as f32 * 2.));
            if r < 1.0 {
                // Empty space inside arch
                None
            } else if r < 1.04 {
                // Beveled edge
                if distance_from_edge < ((1. - r) * 2.0 + 0.1) {
                    None
                } else {
                    Some(&stone_range[3])
                }
            } else if r < 1.1 {
                // Surface
                Some(&stone_range[3])
            } else if r < 1.14 {
                // Groove
                if distance_from_edge == 0. {
                    None
                } else {
                    Some(&stone_range[4])
                }
            } else {
                // Body
                Some(brick_pattern(p))
            }
        })?;
        universe.insert_anonymous(space)
    };
    space_to_blocks(
        resolution,
        BlockAttributes {
            display_name: "Atrium Upper Floor Arch".into(),
            collision: BlockCollision::Recur,
            ..BlockAttributes::default()
        },
        space,
    )
}

fn lookup_multiblock_2d(
    blocks: &BlockProvider<AtriumBlocks>,
    ctor: fn(UpTo5, UpTo5) -> AtriumBlocks,
    xy: [GridCoordinate; 2],
) -> Block {
    match xy.map(UpTo5::new) {
        [Some(x), Some(y)] => blocks[ctor(x, y)].clone(),
        _ => Block::builder()
            .color(rgba_const!(1.0, 0.01, 0.8, 1.0))
            .display_name(format!("Out of bounds {:?}", xy))
            .build(),
    }
}

/// Kludge to allow for representing multi-block structures in AtriumBlocks.
/// This represents an integer in {0, 1, 2, 3, 4}, which is the biggest dimension
/// we have.
///
/// TODO: Replace this with built-in support for multi-block structures.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, exhaust::Exhaust)]
enum UpTo5 {
    U0,
    U1,
    U2,
    U3,
    U4,
}
impl UpTo5 {
    /// Basically a TryFrom without an error type
    fn new(input: GridCoordinate) -> Option<Self> {
        Some(match input {
            0 => UpTo5::U0,
            1 => UpTo5::U1,
            2 => UpTo5::U2,
            3 => UpTo5::U3,
            4 => UpTo5::U4,
            _ => return None,
        })
    }
    fn to_int(self) -> GridCoordinate {
        match self {
            UpTo5::U0 => 0,
            UpTo5::U1 => 1,
            UpTo5::U2 => 2,
            UpTo5::U3 => 3,
            UpTo5::U4 => 4,
        }
    }
}
impl fmt::Display for UpTo5 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_int())
    }
}
