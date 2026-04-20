use alloc::boxed::Box;
use alloc::vec::Vec;
use core::fmt;

use exhaust::Exhaust;
use noise_functions::Noise as _;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use all_is_cubes::arcstr;
use all_is_cubes::block::{self, AIR, Block, Resolution, RotationPlacementRule, Zoom};
use all_is_cubes::content::palette;
use all_is_cubes::linking::{BlockModule, BlockProvider, InGenError};
use all_is_cubes::math::{
    Cube, Face, GridAab, GridCoordinate, GridPoint, GridRotation, GridVector, Rgb, Rgb01, Rgba,
    Vol, ps32, zo32,
};
use all_is_cubes::space::{self, Space, SpacePhysics, SpaceTransaction};
use all_is_cubes::universe::{ReadTicket, UniverseTransaction};
use all_is_cubes::util::YieldProgress;

use crate::Fire;
use crate::alg::scale_color;
use crate::load_block as lb;
use crate::load_image::include_image;

// -------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[exhaust(factory_is_self)]
#[non_exhaustive]
pub(in crate::atrium) enum AtriumBlocks {
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
#[exhaust(factory_is_self)]
pub(in crate::atrium) enum BannerColor {
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

pub(in crate::atrium) async fn install_atrium_blocks(
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

    // This noise is used many times, so compute its blocks once.
    let stone_base_array: Vol<Box<[&Block]>> = {
        let noise_fn = noise_functions::OpenSimplex2
            .seed(0x2e240315)
            .frequency(2.0 * RESOLUTION.recip_f32())
            .mul(0.5);
        Vol::from_fn(GridAab::for_block(RESOLUTION), |cube| {
            let point = cube.center().to_f32().to_array();
            let value = noise_fn.sample3(point);
            &stone_range[(value * 8.0 + 2.5).round().clamp(0.0, 4.0) as usize]
        })
    };

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

    let banner_shape = const {
        lb::Block {
            primitive: lb::PrimitiveOrSuch::Image {
                image: include_image!("banner-shape.png"),
                rotation: GridRotation::RXZY,
                extrusion: &[0..RESOLUTION_G],
                visible: lb::Vox::DEFAULT,
                invisible: lb::Vox::DENOTES_AIR,
            },
            modifiers: &[block::Modifier::SetAttribute(
                block::SetAttribute::DisplayName(arcstr::literal!("Uncolored Banner")),
            )],
        }
    }
    .load(txn)?;

    Ok(BlockProvider::<AtriumBlocks>::new(progress, |key| {
        Ok(match key {
            AtriumBlocks::Sun => const {
                lb::Block {
                    primitive: lb::PrimitiveOrSuch::Atom(block::Atom {
                        color: Rgba::WHITE,
                        emission: Rgb::new(1.0, 1.0, 0.9843).scale(ps32(40.0)),
                        collision: block::BlockCollision::Hard,
                    }),
                    modifiers: &[block::Modifier::SetAttribute(
                        block::SetAttribute::DisplayName(arcstr::literal!("Sun")),
                    )],
                }
            }
            .load(txn)?,

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
                super::BALCONY_RADIUS as GridCoordinate,
                3,
            )?,
            AtriumBlocks::GroundColumn => Block::builder()
                .display_name("Large Atrium Column")
                .rotation_rule(RotationPlacementRule::Attach { by: Face::NY })
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
                .rotation_rule(RotationPlacementRule::Attach { by: Face::NY })
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
                .rotation_rule(RotationPlacementRule::Attach { by: Face::NY })
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

            AtriumBlocks::Banner(color) => banner_shape
                .clone()
                // dye the banner
                .with_modifier(block::Composite::new(
                    Block::from(color.color()),
                    block::CompositeOperator::In,
                ))
                .with_modifier(block::SetAttribute::DisplayName(arcstr::format!(
                    "Atrium Banner {color}"
                ))),
            AtriumBlocks::BannerBottomAccent => const {
                lb::Block {
                    primitive: lb::PrimitiveOrSuch::Image {
                        image: include_image!("banner-trim.png"),
                        rotation: GridRotation::RXyZ,
                        extrusion: &[0..RESOLUTION_G],
                        visible: lb::Vox::DEFAULT,
                        invisible: lb::Vox::DENOTES_AIR,
                    },
                    modifiers: &[block::Modifier::SetAttribute(
                        block::SetAttribute::DisplayName(arcstr::literal!("Banner Accent")),
                    )],
                }
            }
            .load(txn)?,

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
pub(in crate::atrium) enum ArchStyle {
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
pub(in crate::atrium) fn lookup_multiblock_2d(
    blocks: &BlockProvider<AtriumBlocks>,
    ctor: AtriumBlocks,
    [x, y]: [GridCoordinate; 2],
) -> Block {
    blocks[ctor]
        .clone()
        .with_modifier(Zoom::new(MULTIBLOCK_SCALE, GridPoint::new(0, y, x)))
}
