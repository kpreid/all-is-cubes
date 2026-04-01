//! A voxel reinterpretation of the famous Sponza Atrium test scene.

use alloc::boxed::Box;
use core::num::NonZero;


use all_is_cubes::block::{self, AIR, Block};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::{BoxStyle, free_editing_starter_inventory};
use all_is_cubes::euclid::Point3D;
use all_is_cubes::linking::{BlockProvider, InGenError};
use all_is_cubes::math::{
    Axis, Cube, Face, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridPoint, GridRotation,
    GridSizeCoord, GridVector, Gridgid, Rgba, Vol, rgb_const,
};
use all_is_cubes::space::{self, SetCubeError, Space};
use all_is_cubes::time;
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;

use crate::alg::four_walls;

// -------------------------------------------------------------------------------------------------

mod blocks;
use blocks::{AtriumBlocks, BannerColor, lookup_multiblock_2d};

// -------------------------------------------------------------------------------------------------

/// A special name for "the thickness of a 1-block-thick wall/floor/pillar", for readability.
const IWALL: GridCoordinate = 1;
const UWALL: GridSizeCoord = 1;

/// If set, create a constantly moving light source instead of a fixed one.
/// TODO: Make this a run-time controllable parameter (templates need a way to do that).
const MOVING_LIGHT: bool = false;

// Dimensions and counts
const BALCONY_RADIUS: GridSizeCoord = 4;
const CEILING_HEIGHT: u32 = 6u32;
const BETWEEN_SMALL_ARCHES: u32 = 3u32;
const BETWEEN_LARGE_ARCHES: u32 = BETWEEN_SMALL_ARCHES * 2 + 1;
const LARGE_ARCH_COUNT_X: u32 = 1;
const LARGE_ARCH_COUNT_Z: u32 = 5;
const FLOOR_COUNT: u32 = 4;
const SUN_HEIGHT: u32 = 10;

// -------------------------------------------------------------------------------------------------

pub(crate) async fn atrium(
    universe: &mut Universe,
    progress: YieldProgress,
) -> Result<Space, InGenError> {
    // TODO: subdivide and report further progress after the block generation
    let mut install_txn = UniverseTransaction::default();
    let blocks = blocks::install_atrium_blocks(&mut install_txn, progress).await?;
    install_txn.execute(universe, (), &mut transaction::no_outputs)?;

    atrium_non_async(universe, &blocks)
}

fn atrium_non_async(
    universe: &mut Universe,
    blocks: &BlockProvider<AtriumBlocks>,
) -> Result<Space, InGenError> {
    let origin = GridAab::ORIGIN_CUBE;
    let atrium_footprint = origin.expand(FaceMap::symmetric([
        ((BETWEEN_LARGE_ARCHES + UWALL) * LARGE_ARCH_COUNT_X) / 2 - UWALL,
        0,
        ((BETWEEN_LARGE_ARCHES + UWALL) * LARGE_ARCH_COUNT_Z) / 2 - UWALL,
    ]));
    let arches_footprint = atrium_footprint.expand(FaceMap::symmetric([UWALL, 0, UWALL]));
    let balconies_footprint =
        arches_footprint.expand(FaceMap::symmetric([BALCONY_RADIUS, 0, BALCONY_RADIUS]));
    let outer_walls_footprint = balconies_footprint.expand(FaceMap::symmetric([UWALL, 0, UWALL]));

    let balcony_floor_pos = GridVector::new(0, (CEILING_HEIGHT + UWALL).cast_signed(), 0);
    let top_floor_pos = GridVector::new(0, (CEILING_HEIGHT + UWALL).cast_signed() * 2, 0);

    let space_bounds = outer_walls_footprint
        .expand(FaceMap::default().with(Face::PY, SUN_HEIGHT + CEILING_HEIGHT * FLOOR_COUNT));

    let floor_with_cutout = |mut cube: Cube| {
        cube.y = 0;
        if atrium_footprint.contains_cube(cube) {
            None
        } else if arches_footprint.contains_cube(cube) {
            Some(&blocks[AtriumBlocks::GroovedBricks])
        } else {
            Some(&blocks[AtriumBlocks::UpperFloor])
        }
    };

    let mut space = Space::builder(space_bounds)
        .spawn({
            // TODO: default_for_new_space isn't really doing anything for us here
            let mut spawn = Spawn::default_for_new_space(space_bounds);
            spawn.set_eye_position(Point3D::new(
                0.5,
                1.91 + FreeCoordinate::from(CEILING_HEIGHT + 2),
                10.0,
            ));
            spawn.set_inventory(free_editing_starter_inventory(true));
            spawn
        })
        .sky_color(rgb_const!(0.242, 0.617, 0.956) * 1.0)
        .light_physics(space::LightPhysics::Rays {
            maximum_distance: space_bounds.size().to_vector().map(f64::from).length() as u8,
        })
        .build();

    space.mutate(universe.read_ticket(), |m| {
        // "Directional" sky light source
        if !MOVING_LIGHT {
            m.fill_uniform(
                m.bounds()
                    .abut(Face::PY, -1)
                    .unwrap()
                    .abut(Face::PX, -6)
                    .unwrap()
                    .abut(Face::PZ, -30) // TODO: we can shrink this once we manage to have denser ray distribution
                    .unwrap(),
                &blocks[AtriumBlocks::Sun],
            )?;
        }

        // Outer walls
        BoxStyle::from_fn(|part| {
            // TODO: this rotation of wall blocks is a slightly recurring pattern (here and in
            // the TRANSPARENCY_GLASS_AND_WATER exhibit) and perhaps BoxStyle should help with it
            // so that a function need not be written each time.
            if let Some(face) = part.to_face()
                && face.axis() != Axis::Y
            {
                Some(
                    blocks[AtriumBlocks::GroovedBricks]
                        .clone()
                        .rotate(GridRotation::from_to(Face::NX, face, Face::PY).unwrap()),
                )
            } else if part.is_edge() && !part.is_on_face(Face::PY) {
                Some(blocks[AtriumBlocks::SolidBricks].clone())
            } else {
                None
            }
        })
        .create_box(
            outer_walls_footprint
                .expand(FaceMap::default().with(Face::PY, CEILING_HEIGHT * FLOOR_COUNT)),
        )
        .execute_m(m)?;

        // Ground floor
        m.fill_uniform(outer_walls_footprint, &blocks[AtriumBlocks::GroundFloor])?;

        // Balcony floor
        m.fill(
            outer_walls_footprint.translate(balcony_floor_pos),
            floor_with_cutout,
        )?;

        // Top floor (solid)
        m.fill(
            outer_walls_footprint
                .translate(top_floor_pos)
                .expand(FaceMap::splat(0).with(Face::PY, CEILING_HEIGHT)),
            floor_with_cutout,
        )?;

        // Arches and atrium walls
        #[rustfmt::skip]
        let arches_pattern = Vol::<Box<[u8]>>::from_y_flipped_array([[
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
            *br"AAbbbbbA",
            *br"AAbbbbbA",
            *br"G bbbbb ",
            *br"G BBBBB ", 
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
        ], [
            *br"        ", // roof edge height
            *br"        ",
            *br"        ",
            *br"        ",
            *br"        ",
            *br"        ",
            *br"        ",
            *br"    f   ", // top floor height
            *br"        ",
            *br"        ",
            *br"        ",
            *br"        ",
            *br"        ",
            *br"        ", // balcony floor height
            *br"        ",
            *br"        ",
            *br"        ",
            *br"        ",
            *br"        ", 
            *br"        ",
        ]]);
        for wall in four_walls(arches_footprint.translate([0, IWALL, 0])) {
            arch_row(
                m,
                blocks,
                wall.bottom_corner,
                BETWEEN_LARGE_ARCHES + UWALL,
                wall.length / (BETWEEN_LARGE_ARCHES + UWALL),
                wall.counterclockwise_direction,
                arches_pattern.as_ref(),
            )?;
        }

        // Ceiling vaults
        // TODO: This only generates vaults along the long sides and not the short sides or corners.
        for z_section in 0..(LARGE_ARCH_COUNT_Z as i32) {
            let z_low = z_section * (BETWEEN_LARGE_ARCHES + UWALL) as i32;
            for (flip_x, x_low) in [
                (false, balconies_footprint.lower_bounds().x),
                (true, atrium_footprint.upper_bounds().x + IWALL),
            ] {
                for which_floor in 0..2 {
                    let start_y = which_floor * (CEILING_HEIGHT + UWALL) as i32 + 4;
                    let low_corner = GridPoint::new(
                        x_low,
                        start_y,
                        atrium_footprint.lower_bounds().z - IWALL + z_low,
                    );
                    // TODO: Some of these blocks are in fact completely empty air and we should
                    // be able to skip them, but `lookup_multiblock_2d` can't tell us which.
                    // We need a better multiblock system that can.
                    m.fill(
                        GridAab::from_lower_size(
                            low_corner,
                            // TODO: have constant for arch height
                            [BALCONY_RADIUS, 3, BETWEEN_LARGE_ARCHES + UWALL],
                        ),
                        |abs_cube| {
                            let rel_cube = abs_cube - low_corner.to_vector();
                            let balcony_axis_arch = lookup_multiblock_2d(
                                blocks,
                                AtriumBlocks::VaultBalcony,
                                [
                                    if flip_x {
                                        BALCONY_RADIUS as GridCoordinate - rel_cube.x
                                    } else {
                                        // +1 because this arch doesn't have its own support pillar so we skip the full pillar part of the arch shape
                                        rel_cube.x + 1
                                    },
                                    rel_cube.y,
                                ],
                            )
                            .rotate(
                                (if flip_x {
                                    GridRotation::RxYZ
                                } else {
                                    GridRotation::IDENTITY
                                }) * Face::PY.counterclockwise(),
                            );
                            let arch_axis_arch = lookup_multiblock_2d(
                                blocks,
                                AtriumBlocks::VaultArch,
                                [rel_cube.z, rel_cube.y],
                            );
                            // Combine the two arches to form a vault.
                            Some(
                                block::Composite::new(arch_axis_arch, block::CompositeOperator::In)
                                    .compose_or_replace(balcony_axis_arch),
                            )
                        },
                    )?;
                }
            }
        }

        if MOVING_LIGHT {
            let mut movement = block::Move::new(Face::PZ, 16, 16);
            movement.schedule = time::Schedule::from_period(NonZero::new(2).unwrap());
            let tick_action = block::TickAction {
                operation: crate::animation::back_and_forth_movement(movement),
                schedule: time::Schedule::from_period(NonZero::new(1).unwrap()),
            };

            m.set(
                [0, 5, -10],
                Block::builder()
                    .color(Rgba::WHITE)
                    .light_emission(rgb_const!(10., 10., 10.))
                    .tick_action(tick_action)
                    .build(),
            )?;
        }

        m.fast_evaluate_light();

        Ok::<(), InGenError>(())
    })?;

    Ok(space)
}

fn map_text_block(
    ascii: u8,
    blocks: &BlockProvider<AtriumBlocks>,
    cube: Cube,
    existing_block: Block,
    banner_color: Option<BannerColor>,
) -> Block {
    match ascii {
        b' ' => existing_block,
        b'.' => AIR,
        b'#' => blocks[AtriumBlocks::GroovedBricks].clone().rotate(Face::PY.clockwise()),
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
                Face::PY.clockwise()
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
                Face::PY.clockwise()
                    * if mirrored_half {
                        GridRotation::IDENTITY
                    } else {
                        GridRotation::RXYz
                    },
            )
        }
        b'T' => block::Composite::new(
            blocks[AtriumBlocks::Molding].clone().rotate(Face::PY.clockwise()),
            block::CompositeOperator::Over,
        )
        .with_disassemblable()
        .compose_or_replace(existing_block),

        b'b' => {
            if let Some(banner_color) = banner_color {
                blocks[AtriumBlocks::Banner(banner_color)].clone()
            } else {
                AIR
            }
        }
        b'B' => {
            // Banner with bottom edge accent
            if let Some(banner_color) = banner_color {
                block::Composite::new(
                    blocks[AtriumBlocks::BannerBottomAccent].clone(),
                    block::CompositeOperator::Atop,
                )
                .compose_or_replace(blocks[AtriumBlocks::Banner(banner_color)].clone())
            } else {
                AIR
            }
        }

        // TODO: These are supposed to be planters
        b'P' => blocks[AtriumBlocks::Brazier].clone(),
        b'f' => lookup_multiblock_2d(blocks, AtriumBlocks::Pole, [cube.z - 1, 0]),
        _ => panic!("Unrecognized block character {:?}", char::from(ascii)),
    }
}

fn arch_row(
    ctx: &mut space::Mutation<'_, '_>,
    blocks: &BlockProvider<AtriumBlocks>,
    first_column_base: Cube,
    section_length: GridSizeCoord,
    section_count: GridSizeCoord,
    parallel: Face,
    pattern: Vol<&[u8]>,
) -> Result<(), InGenError> {
    let offset = parallel.vector(section_length as GridCoordinate);
    let rotation = GridRotation::from_to(Face::NX, parallel, Face::PY).unwrap();
    for i in 0..(section_count as GridCoordinate) {
        let column_base = first_column_base + offset * (i + 1);

        let banner_color = if parallel.axis() == Axis::Z {
            match i.rem_euclid(3) {
                0 => Some(BannerColor::Red),
                1 => Some(BannerColor::Green),
                _ => Some(BannerColor::Blue),
            }
        } else {
            None
        };

        fill_space_transformed(
            |p, block| map_text_block(pattern[p], blocks, p, block, banner_color),
            pattern.bounds(),
            ctx,
            Gridgid::from_translation(column_base.lower_bounds().to_vector())
                * rotation.to_positive_octant_transform(1),
        )?;
    }
    Ok::<(), InGenError>(())
}

// TODO: figure out what the general version of this is and move it elsewhere
fn fill_space_transformed(
    src: impl Fn(Cube, Block) -> Block,
    src_bounds: GridAab,
    dst: &mut space::Mutation<'_, '_>,
    src_to_dst_transform: Gridgid,
) -> Result<(), SetCubeError> {
    // TODO: don't panic
    let dst_to_src_transform = src_to_dst_transform.inverse();
    let block_rotation = src_to_dst_transform.rotation;
    for cube in src_bounds.transform(src_to_dst_transform).unwrap().interior_iter() {
        let existing_block = dst[cube].clone().rotate(block_rotation.inverse());
        let new_block =
            src(dst_to_src_transform.transform_cube(cube), existing_block).rotate(block_rotation);
        dst.set(cube, new_block)?;
    }
    Ok(())
}
