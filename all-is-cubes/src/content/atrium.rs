// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! A voxel reinterpretation of the famous Sponza Atrium test scene.

use cgmath::{EuclideanSpace as _, Point3, Transform, Vector3};

use crate::block::{Block, BlockCollision, AIR};
use crate::content::four_walls;
use crate::linking::{BlockModule, BlockProvider, InGenError};

use crate::math::{FaceMap, GridCoordinate, GridMatrix, GridPoint, GridRotation, GridVector, Rgb};
use crate::raycast::Face;
use crate::space::{Grid, GridArray, SetCubeError, Space, SpacePhysics};
use crate::universe::Universe;

/// A special name for "the thickness of a 1-block-thick wall/floor/pillar", for readability.
const WALL: GridCoordinate = 1;

pub(crate) fn atrium(universe: &mut Universe) -> Result<Space, InGenError> {
    let blocks = install_atrium_blocks(universe)?;

    let ceiling_height = 6;
    let between_small_arches = 2;
    let between_large_arches = between_small_arches * 2 + 1;
    let balcony_radius = 3;
    let large_arch_count = Vector3::new(1, 0, 5); // x, dummy y, z

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

    let space_grid = outer_walls_footprint.expand(FaceMap::from_fn(|f| {
        (f == Face::PY) as GridCoordinate * ceiling_height * 4
    }));

    let floor_with_cutout = |mut p: GridPoint| {
        p.y = 0;
        if atrium_footprint.contains_cube(p) {
            None
        } else {
            Some(&blocks[AtriumBlocks::SolidBricks])
        }
    };

    let mut space = Space::empty(space_grid);

    // Outer walls
    four_walls(
        outer_walls_footprint.translate([0, WALL, 0]),
        |origin, direction, length| -> Result<(), InGenError> {
            // TODO: four_walls should provide this automatically as a convenience
            let wall_excluding_corners = Grid::single_cube(origin + direction.normal_vector())
                .union(Grid::single_cube(
                    origin
                        + direction.normal_vector() * (length - 2)
                        + GridVector::new(0, space_grid.size().y - 2, 0), // TODO: cryptic fudge
                ))
                .unwrap();
            space.fill_uniform(
                wall_excluding_corners,
                blocks[AtriumBlocks::GroundFloor]
                    .clone()
                    .rotate(GridRotation::from_to(Face::PZ, direction, Face::PY).unwrap()),
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
                (f == Face::PY) as GridCoordinate * ceiling_height
            })),
        floor_with_cutout,
    )?;

    // Arches and atrium walls
    #[rustfmt::skip]
    let arches_pattern = GridArray::from_y_flipped_array([[
        *br"######", // Roof edge height
        *br"###.##",
        *br"######",
        *br"######",
        *br"######",
        *br"###.##",
        *br"######",
        *br"######", // top floor height
        *br"#/\#/\",
        *br"|  o  ",
        *br"|  o  ",
        *br"|  o  ", 
        *br"######",
        *br"######", // balcony floor height
        *br"##/ \#",
        *br"#/   \",
        *br"G     ",
        *br"G     ",
        *br"G     ", 
        *br"G     ",
    ], [
        *br"      ", // roof edge height
        *br"      ",
        *br"      ",
        *br"      ",
        *br"      ",
        *br"      ",
        *br"TTTTTT",
        *br"   f  ", // top floor height
        *br"      ",
        *br"      ",
        *br"      ",
        *br"      ",
        *br"TTTTTT",
        *br"      ", // balcony floor height
        *br"      ",
        *br"      ",
        *br"      ",
        *br"      ",
        *br"      ", 
        *br"P     ",
    ]]);
    four_walls(
        arches_footprint.translate([0, WALL, 0]),
        |origin, direction, length| {
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

    // Other space setup
    space.spawn_mut().position = Point3::new(notnan!(0.5), notnan!(2.91), notnan!(10.0));
    space.set_physics(SpacePhysics {
        sky_color: Rgb::new(1.0, 1.0, 0.9843) * 4.0,
        ..SpacePhysics::default()
    });

    Ok(space)
}

fn map_text_block(
    ascii: u8,
    blocks: &BlockProvider<AtriumBlocks>,
    original_block: &Block,
) -> Block {
    match ascii {
        b' ' => original_block.clone(),
        b'.' => AIR,
        b'#' => blocks[AtriumBlocks::SolidBricks]
            .clone()
            .rotate(GridRotation::CLOCKWISE),
        b'G' => blocks[AtriumBlocks::GroundColumn].clone(),
        b'o' => blocks[AtriumBlocks::SmallColumn].clone(),
        b'|' => blocks[AtriumBlocks::SquareColumn].clone(),
        b'/' => blocks[AtriumBlocks::ArchBricks].clone(),
        b'\\' => blocks[AtriumBlocks::ArchBricks]
            .clone()
            .rotate(GridRotation::CLOCKWISE * GridRotation::CLOCKWISE),
        b'T' => blocks[AtriumBlocks::Molding]
            .clone()
            .rotate(GridRotation::CLOCKWISE),
        // Not-yet-implemented decoration placeholder blocks
        b'P' | b'f' => Block::from(rgba_const!(1.0, 0.5, 0.5, 1.0)),
        _ => panic!(
            "Unrecognized block character {:?}",
            std::str::from_utf8(&[ascii])
        ),
    }
}

#[allow(clippy::too_many_arguments)]
fn arch_row(
    space: &mut Space,
    blocks: &BlockProvider<AtriumBlocks>,
    first_column_base: GridPoint,
    section_length: GridCoordinate,
    section_count: GridCoordinate,
    parallel: Face,
    pattern: &GridArray<u8>,
) -> Result<(), InGenError> {
    let offset = parallel.normal_vector() * section_length;
    let rotation = GridRotation::from_to(Face::NX, parallel, Face::PY).unwrap();
    for i in 0..section_count {
        let column_base = first_column_base + offset * (i + 1);

        fill_space_transformed(
            |p, block| map_text_block(pattern[p], blocks, block),
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
    src: impl Fn(GridPoint, &Block) -> Block,
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
        dst.set(
            cube,
            src(dst_to_src_transform.transform_cube(cube), &dst[cube]).rotate(block_rotation),
        )?;
    }
    Ok(())
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub enum AtriumBlocks {
    GroundFloor,
    SolidBricks,
    ArchBricks,
    GroundColumn,
    SquareColumn,
    SmallColumn,
    Molding,
}
impl BlockModule for AtriumBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/atrium"
    }
}

fn install_atrium_blocks(
    universe: &mut Universe,
) -> Result<BlockProvider<AtriumBlocks>, InGenError> {
    let resolution = 16;
    let resolution_g = GridCoordinate::from(resolution);
    let stone_base = Block::from(rgba_const!(0.53, 0.48, 0.40, 1.0));
    let heavy_grout_base = Block::from(rgba_const!(0.1, 0.1, 0.1, 1.0));
    let grout_base = Block::from(rgba_const!(0.32, 0.30, 0.28, 1.0));
    let brick_pattern = |mut p: GridPoint| {
        if (p.x > resolution_g / 2) ^ (p.y > resolution_g / 2) {
            // Create brick half-overlap offset
            p.z = (p.z + resolution_g / 2) % resolution_g;
        }
        let bricking = (p.x % 8).min(p.y % 8).min(p.z % 16);
        if bricking == 0 {
            &grout_base
        } else {
            &stone_base
        }
    };
    let bottom_grout_pattern = |p: GridPoint| {
        if p.y == 0 {
            &grout_base
        } else {
            &stone_base
        }
    };

    // TODO: duplicated procgen code — figure out a good toolkit of math helpers
    let one_diagonal = GridVector::new(1, 1, 1);
    let center_point_doubled = GridPoint::from_vec(one_diagonal * resolution_g);

    BlockProvider::<AtriumBlocks>::new(|key| {
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
            AtriumBlocks::SolidBricks => Block::builder()
                .display_name("Atrium Wall Bricks")
                .voxels_fn(universe, resolution, brick_pattern)?
                .build(),
            AtriumBlocks::ArchBricks => Block::builder()
                .display_name("Atrium Arch Bricks")
                .voxels_fn(universe, resolution, |p| {
                    if p.x <= p.y {
                        brick_pattern(p)
                    } else {
                        &AIR
                    }
                })?
                .build(),
            AtriumBlocks::GroundColumn => Block::builder()
                .display_name("Large Atrium Column")
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
                .collision(BlockCollision::None) // TODO: once voxel collision is implemented, remove this
                .voxels_fn(universe, resolution, |p| {
                    let shape: [GridCoordinate; 16] =
                        [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 2, 3, 4, 4, 3];
                    if p.x < shape[p.y as usize] {
                        brick_pattern(p)
                    } else {
                        &AIR
                    }
                })?
                .build(),
        })
    })?
    .install(universe)?;
    Ok(BlockProvider::<AtriumBlocks>::using(universe)?)
}
