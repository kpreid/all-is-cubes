//! A voxel reinterpretation of the famous Sponza Atrium test scene.

use alloc::boxed::Box;
use alloc::vec::Vec;
use core::f64::consts::TAU;
use core::fmt;
use core::num::NonZero;

use exhaust::Exhaust;

use all_is_cubes::block::{self, AIR, Block, Resolution, RotationPlacementRule, Zoom};
use all_is_cubes::character::Spawn;
use all_is_cubes::content::{BoxStyle, free_editing_starter_inventory, palette};
use all_is_cubes::euclid::Point3D;
use all_is_cubes::linking::{BlockModule, BlockProvider, InGenError};
use all_is_cubes::math::{
    Axis, Cube, Face6, FaceMap, FreeCoordinate, GridAab, GridCoordinate, GridPoint, GridRotation,
    GridSizeCoord, GridVector, Gridgid, Rgb, Rgb01, Rgba, Vol, rgb_const, zo32,
};
use all_is_cubes::space::{self, SetCubeError, Space, SpacePhysics, SpaceTransaction};
use all_is_cubes::time;
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{ReadTicket, Universe, UniverseTransaction};
use all_is_cubes::util::YieldProgress;

use crate::Fire;
use crate::alg::{array_of_noise, four_walls, scale_color};

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
    let blocks = install_atrium_blocks(&mut install_txn, progress).await?;
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
        .expand(FaceMap::default().with(Face6::PY, SUN_HEIGHT + CEILING_HEIGHT * FLOOR_COUNT));

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
                    .abut(Face6::PY, -1)
                    .unwrap()
                    .abut(Face6::PX, -6)
                    .unwrap()
                    .abut(Face6::PZ, -30) // TODO: we can shrink this once we manage to have denser ray distribution
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
                        .rotate(GridRotation::from_to(Face6::NX, face, Face6::PY).unwrap()),
                )
            } else if part.is_edge() && !part.is_on_face(Face6::PY) {
                Some(blocks[AtriumBlocks::SolidBricks].clone())
            } else {
                None
            }
        })
        .create_box(
            outer_walls_footprint
                .expand(FaceMap::default().with(Face6::PY, CEILING_HEIGHT * FLOOR_COUNT)),
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
                .expand(FaceMap::splat(0).with(Face6::PY, CEILING_HEIGHT)),
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
                                }) * Face6::PY.counterclockwise(),
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
            let mut movement = block::Move::new(Face6::PZ, 16, 16);
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
        b'#' => blocks[AtriumBlocks::GroovedBricks].clone().rotate(Face6::PY.clockwise()),
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
                Face6::PY.clockwise()
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
                Face6::PY.clockwise()
                    * if mirrored_half {
                        GridRotation::IDENTITY
                    } else {
                        GridRotation::RXYz
                    },
            )
        }
        b'T' => block::Composite::new(
            blocks[AtriumBlocks::Molding].clone().rotate(Face6::PY.clockwise()),
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
    parallel: Face6,
    pattern: Vol<&[u8]>,
) -> Result<(), InGenError> {
    let offset = parallel.vector(section_length as GridCoordinate);
    let rotation = GridRotation::from_to(Face6::NX, parallel, Face6::PY).unwrap();
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

// -------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
enum AtriumBlocks {
    Sun,

    GroundFloor,
    UpperFloor,
    /// Bricks used for filling solid wall volumes.
    SolidBricks,
    /// Bricks which have grooves, used only on the surfaces of walls.
    GroovedBricks,
    GroundArch,
    UpperArch,
    /// Axis of the vault shape that is as wide as the `GroundArch`es
    VaultArch,
    /// Axis of the vault shape that is as wide as the `balcony_radius`
    VaultBalcony,
    GroundColumn,
    SquareColumn,
    SmallColumn,
    Molding,

    /// Base banner color and shape
    Banner(BannerColor),
    /// Accent color to be composited atop the banner
    BannerBottomAccent,

    // Decorations
    Pole,
    Brazier,
}
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
enum BannerColor {
    Red,
    Green,
    Blue,
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
            AtriumBlocks::Sun => write!(f, "sun"),

            AtriumBlocks::GroundFloor => write!(f, "ground-floor"),
            AtriumBlocks::UpperFloor => write!(f, "upper-floor"),
            AtriumBlocks::SolidBricks => write!(f, "solid-bricks"),
            AtriumBlocks::GroovedBricks => write!(f, "grooved-bricks"),
            AtriumBlocks::GroundArch => write!(f, "ground-arch"),
            AtriumBlocks::UpperArch => write!(f, "upper-arch"),
            AtriumBlocks::VaultArch => write!(f, "vault-arch"),
            AtriumBlocks::VaultBalcony => write!(f, "vault-balcony"),
            AtriumBlocks::GroundColumn => write!(f, "ground-column"),
            AtriumBlocks::SquareColumn => write!(f, "square-column"),
            AtriumBlocks::SmallColumn => write!(f, "small-column"),
            AtriumBlocks::Molding => write!(f, "molding"),

            AtriumBlocks::Banner(c) => write!(f, "banner/{c}"),
            AtriumBlocks::BannerBottomAccent => write!(f, "banner/bottom-accent"),

            AtriumBlocks::Pole => write!(f, "pole"),
            AtriumBlocks::Brazier => write!(f, "firepot"),
        }
    }
}
impl fmt::Display for BannerColor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(match self {
            BannerColor::Red => "red",
            BannerColor::Green => "green",
            BannerColor::Blue => "blue",
        })
    }
}

impl BannerColor {
    fn color(self) -> Rgb01 {
        match self {
            BannerColor::Red => Rgb01::UNIFORM_LUMINANCE_RED,
            BannerColor::Green => Rgb01::UNIFORM_LUMINANCE_GREEN,
            BannerColor::Blue => Rgb01::UNIFORM_LUMINANCE_BLUE,
        }
    }
}

async fn install_atrium_blocks(
    txn: &mut UniverseTransaction,
    progress: YieldProgress,
) -> Result<BlockProvider<AtriumBlocks>, InGenError> {
    const RESOLUTION: Resolution = Resolution::R16;
    const RESOLUTION_G: i32 = RESOLUTION.to_grid();
    const STONE_BASE: &Block = &block::from_color!(0.53, 0.48, 0.40, 1.0);
    const HEAVY_GROUT_BASE: &Block = &block::from_color!(0.1, 0.1, 0.1, 1.0);
    const GROUT_BASE: &Block = &block::from_color!(0.32, 0.30, 0.28, 1.0);
    const CEILING_PAINT: &Block = &block::from_color!(0.975, 0.975, 0.975, 1.0);
    const POLE_COLOR: &Block = &block::from_color!(0.27, 0.20, 0.18, 1.0);
    let solid_air: &Block = &Block::from_primitive(block::Primitive::Atom(block::Atom {
        color: Rgba::TRANSPARENT,
        emission: Rgb::ZERO,
        collision: block::BlockCollision::Hard,
    }));

    // TODO: This whole section is about having noise pick from a fixed set of pregenerated shades.
    // We should abstract it out if we like this style
    #[allow(
        clippy::cast_lossless,
        reason = "warns on 32-bit but f64::from() is not an option on 64-bit"
    )]
    let stone_range: Vec<Block> = (-2..=2_isize)
        .into_iter()
        .map(|x| scale_color(STONE_BASE.clone(), 1.0 + x as f64 * 0.08, 0.02))
        .collect();

    // increased resolution to support brick offset patterning
    let stone_base_array = array_of_noise(
        RESOLUTION,
        &noise::ScalePoint::new(noise::OpenSimplex::new(0x2e240365))
            .set_scale(4.0 / f64::from(RESOLUTION_G)),
        |value| &stone_range[(value * 8.0 + 2.5).round().clamp(0.0, 4.0) as usize],
    );

    let brick_pattern = |mut p: Cube, grooves: bool| {
        if (p.x.rem_euclid(RESOLUTION_G) > RESOLUTION_G / 2)
            ^ (p.y.rem_euclid(RESOLUTION_G) > RESOLUTION_G / 2)
        {
            // Create brick half-overlap offset
            p.z = (p.z + RESOLUTION_G / 2).rem_euclid(RESOLUTION_G);
        }
        let bricking = (p.x.rem_euclid(8)).min(p.y.rem_euclid(8)).min(p.z.rem_euclid(16));
        if bricking == 0 {
            if grooves && p.x == RESOLUTION_G - 1 {
                // Recess the grout.
                // Using solid_air so as to avoid needing to invoke voxel-level collision detection
                // for these tiny gaps.
                solid_air
            } else {
                GROUT_BASE
            }
        } else {
            stone_base_array[Cube::from(p.lower_bounds().map(|c| c.rem_euclid(RESOLUTION_G)))] // TODO: add way to express this transformation on `Cube` without going through points
        }
    };
    let bottom_grout_pattern = |p: Cube| {
        if p.y == 0 {
            GROUT_BASE
        } else {
            stone_base_array[p]
        }
    };
    let molding_fn = |p: Cube| {
        let shape: [GridCoordinate; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 2, 3, 4, 4, 3];
        if p.x < shape[p.y as usize] {
            brick_pattern(p, false)
        } else {
            &AIR
        }
    };

    let pole_fn = {
        let pole_shape = Vol::<Box<[u8]>>::from_y_flipped_array([
            [
                *br"                           #####",
                *br"                      ##########",
                *br"                ############    ",
                *br"           ############         ",
                *br"#     ############              ",
                *br"#############                   ",
                *br"########                        ",
                *br"#                               ",
            ],
            [
                *br"                                ",
                *br"                         ###### ",
                *br"                    ######      ",
                *br"               ######           ",
                *br"          ######                ",
                *br"     ######                     ",
                *br"######                          ",
                *br"                                ",
            ],
        ]);
        move |p: Cube| match pole_shape
            .get(Cube {
                x: p.z,
                y: p.y,
                z: match p.x - 8 {
                    // remap ..-2 -1 0 1.. to ..1 0 0 1..
                    z @ 0.. => z,
                    z => (-1) - z,
                },
            })
            .copied()
            .unwrap_or(b' ')
        {
            b'#' => POLE_COLOR,
            _ => &AIR,
        }
    };

    // TODO: duplicated procgen code — figure out a good toolkit of math helpers
    let one_diagonal = GridVector::new(1, 1, 1);
    let center_point_doubled = (one_diagonal * RESOLUTION_G).to_point();

    Ok(BlockProvider::<AtriumBlocks>::new(progress, |key| {
        Ok(match key {
            AtriumBlocks::Sun => Block::builder()
                .display_name("Sun")
                .color(Rgba::WHITE)
                .light_emission(Rgb::new(1.0, 1.0, 0.9843) * 40.0)
                .build(),

            AtriumBlocks::GroundFloor => Block::builder()
                .display_name("Atrium Ground Floor")
                .voxels_fn(RESOLUTION, |p| {
                    if p.x == 0 {
                        HEAVY_GROUT_BASE
                    } else {
                        brick_pattern(p, false)
                    }
                })?
                .build_txn(txn),
            AtriumBlocks::UpperFloor => Block::builder()
                .display_name("Atrium Upper Floor")
                .voxels_fn(RESOLUTION, |p| {
                    // Add a white ceiling
                    if p.y == 0 {
                        CEILING_PAINT
                    } else {
                        brick_pattern(p, false)
                    }
                })?
                .build_txn(txn),
            AtriumBlocks::SolidBricks => Block::builder()
                .display_name("Atrium Wall Solid Bricks")
                .voxels_fn(RESOLUTION, |cube| brick_pattern(cube, false))?
                .build_txn(txn),
            AtriumBlocks::GroovedBricks => Block::builder()
                .display_name("Atrium Wall Grooved Bricks")
                .voxels_fn(RESOLUTION, |cube| brick_pattern(cube, true))?
                .build_txn(txn),
            AtriumBlocks::GroundArch => generate_arch(
                txn,
                &stone_range,
                |cube| brick_pattern(cube, true),
                RESOLUTION,
                ArchStyle::GroundFloor,
                7,
                3,
            )?,
            AtriumBlocks::UpperArch => generate_arch(
                txn,
                &stone_range,
                |cube| brick_pattern(cube, true),
                RESOLUTION,
                ArchStyle::UpperFloor,
                3,
                2,
            )?,
            AtriumBlocks::VaultArch => generate_arch(
                txn,
                &stone_range,
                |_| CEILING_PAINT,
                RESOLUTION,
                ArchStyle::Vault,
                7, // size same as GroundArch
                3,
            )?,
            AtriumBlocks::VaultBalcony => generate_arch(
                txn,
                &stone_range,
                |_| CEILING_PAINT,
                RESOLUTION,
                ArchStyle::Vault,
                BALCONY_RADIUS as GridCoordinate,
                3,
            )?,
            AtriumBlocks::GroundColumn => Block::builder()
                .display_name("Large Atrium Column")
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(RESOLUTION, |p| {
                    let mid = (p.lower_bounds() * 2 - center_point_doubled).map(|c| c.abs());
                    if mid.x + mid.z < RESOLUTION_G * 6 / 4 {
                        bottom_grout_pattern(p)
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),
            AtriumBlocks::SquareColumn => Block::builder()
                .display_name("Square Atrium Column")
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(RESOLUTION, |p| {
                    let mid = (p.lower_bounds() * 2 - center_point_doubled).map(|c| c.abs());
                    if mid.x.max(p.z) < RESOLUTION_G * 6 / 4 {
                        bottom_grout_pattern(p)
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),
            AtriumBlocks::SmallColumn => Block::builder()
                .display_name("Round Atrium Column")
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(RESOLUTION, |p| {
                    let mid = (p.lower_bounds() * 2 - center_point_doubled).map(|c| c.abs());
                    if mid.x.pow(2) + mid.z.pow(2) < (RESOLUTION_G * 3 / 4).pow(2) {
                        bottom_grout_pattern(p)
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),
            AtriumBlocks::Molding => Block::builder()
                .display_name("Atrium Top Edge Molding")
                // TODO: rotation rule
                .voxels_fn(RESOLUTION, molding_fn)?
                .build_txn(txn),

            AtriumBlocks::Banner(color) => Block::builder()
                .display_name(format!("Atrium Banner {color}"))
                .voxels_fn(RESOLUTION, |p| {
                    // wavy banner shape
                    let wave = (f64::from(p.x) * (TAU / f64::from(RESOLUTION))).sin() * 2.2;
                    if p.z == i32::from(RESOLUTION) / 2 + wave as i32 {
                        Block::from(color.color())
                    } else {
                        AIR
                    }
                })?
                .build_txn(txn),
            AtriumBlocks::BannerBottomAccent => {
                let accent_color = block::from_color!(0.95, 0.89, 0.05, 1.0);
                Block::builder()
                    .display_name("Banner Accent")
                    .voxels_fn(RESOLUTION, |p| {
                        if [0, 1, 2, 6, 8].contains(&p.y) {
                            &accent_color
                        } else {
                            &AIR
                        }
                    })?
                    .build_txn(txn)
            }

            AtriumBlocks::Pole => Block::builder()
                .display_name("Pole")
                .voxels_fn((Resolution::R16 * MULTIBLOCK_SCALE).unwrap(), &pole_fn)?
                .build_txn(txn),
            AtriumBlocks::Brazier => Block::builder()
                .display_name("Brazier")
                .animation_hint(block::AnimationHint::redefinition(
                    block::AnimationChange::Shape,
                ))
                .voxels_handle(RESOLUTION, {
                    // Use a darker color to dampen the effect of interior light
                    let body_block = Block::from(palette::STEEL * zo32(0.2));
                    let space = Space::for_block(RESOLUTION).build_and_mutate(|m| {
                        m.fill(
                            GridAab::from_lower_upper(
                                [0, 0, 0],
                                [RESOLUTION_G, RESOLUTION_G / 2, RESOLUTION_G],
                            ),
                            |p| {
                                let mid =
                                    (p.lower_bounds() * 2 - center_point_doubled).map(|c| c.abs());
                                if mid.x.max(mid.z) + (mid.y / 2) < RESOLUTION_G + 4 {
                                    Some(&body_block)
                                } else {
                                    None
                                }
                            },
                        )?;
                        {
                            let fire_inset = 2;
                            let bounds = GridAab::from_lower_upper(
                                // Vertical overlap will be overwritten, making a bowl shape
                                [fire_inset, RESOLUTION_G / 2 - 2, fire_inset],
                                [
                                    RESOLUTION_G - fire_inset,
                                    RESOLUTION_G,
                                    RESOLUTION_G - fire_inset,
                                ],
                            );
                            SpaceTransaction::add_behavior(bounds, Fire::new(bounds))
                                .execute_m(m)
                                .unwrap();
                        }
                        Ok(())
                    })?;

                    txn.insert_anonymous(space)
                })
                .build(),
        })
    })
    .await?
    .install(ReadTicket::stub(), txn)?)
}

// -------------------------------------------------------------------------------------------------

const MULTIBLOCK_SCALE: Resolution = Resolution::R8;

#[derive(Clone, Copy, Debug)]
enum ArchStyle {
    GroundFloor,
    UpperFloor,
    /// Smooth ceiling surface to be composited with a rotated copy of itself to form a [groin vault].
    ///
    /// [groin vault]: https://en.wikipedia.org/wiki/Groin_vault
    Vault,
}

/// Generates a multiblock (to be used with [`lookup_multiblock_2d()`]) forming an arch, whose
/// bottom support is at cube (0, 0, 0) and which extends upward and towards the +Z direction.
fn generate_arch<'b>(
    txn: &mut UniverseTransaction,
    stone_range: &[Block], // TODO: clarify
    brick_pattern: impl Fn(Cube) -> &'b Block,
    resolution: Resolution,
    style: ArchStyle,
    width_blocks: GridCoordinate,
    height_blocks: GridCoordinate,
) -> Result<Block, space::builder::Error> {
    let resolution_g: GridCoordinate = resolution.into();

    let arch_opening_width = resolution_g * width_blocks;
    let arch_opening_height = resolution_g * height_blocks;
    let arch_center_z_doubled = resolution_g * (width_blocks - 1) /* midpoint assuming odd width */
            + resolution_g * 3 /* offset by a block and a half */;

    let space = Space::builder(GridAab::from_lower_upper(
        [0, 0, 0],
        [
            resolution_g,
            arch_opening_height + resolution_g,
            arch_center_z_doubled + resolution_g * 3,
        ],
    ))
    .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
    .build_and_mutate(|m| {
        m.fill_all(|p| {
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
            match style {
                ArchStyle::GroundFloor | ArchStyle::UpperFloor => {
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
                }
                ArchStyle::Vault => {
                    if r < 1.02 {
                        // Empty space inside arch
                        None
                    } else {
                        Some(brick_pattern(p))
                    }
                }
            }
        })
    })?;

    Ok(Block::builder()
        .display_name(match style {
            ArchStyle::GroundFloor => "Atrium Ground Floor Arch",
            ArchStyle::UpperFloor => "Atrium Upper Floor Arch",
            ArchStyle::Vault => "Atrium Vault",
        })
        .voxels_handle(
            (resolution * MULTIBLOCK_SCALE).unwrap(),
            txn.insert_anonymous(space),
        )
        .build())
}

#[track_caller] // for Zoom range panics
fn lookup_multiblock_2d(
    blocks: &BlockProvider<AtriumBlocks>,
    ctor: AtriumBlocks,
    [x, y]: [GridCoordinate; 2],
) -> Block {
    blocks[ctor]
        .clone()
        .with_modifier(Zoom::new(MULTIBLOCK_SCALE, GridPoint::new(0, y, x)))
}
