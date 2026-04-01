#![expect(unused_qualifications, reason = "derive macro false positive")]

use core::f64::consts::TAU;

use all_is_cubes::arcstr::literal;
use all_is_cubes::block::{self, AIR, Block, Resolution::*, RotationPlacementRule};
use all_is_cubes::content::load_image::include_image;
use all_is_cubes::content::palette;
use all_is_cubes::euclid::{point3, vec3};
use all_is_cubes::inv;
use all_is_cubes::linking::{BlockModule, BlockProvider, GenError};
use all_is_cubes::math::{
    Cube, Face, FaceMap, GridAab, GridCoordinate, GridRotation, GridVector, Rgb, Rgba, zo32,
};
use all_is_cubes::op;
use all_is_cubes::space::{Space, SpaceTransaction};
use all_is_cubes::universe::{ReadTicket, UniverseTransaction};
use all_is_cubes::util::YieldProgress;

use crate::load_block as lb;

// -------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, exhaust::Exhaust)]
#[exhaust(factory_is_self)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub(crate) enum DungeonBlocks {
    /// A light to attach to corridor ceilings.
    CorridorLight,
    /// A light to put in rooms.
    Brazier,

    /// Normal flooring for the dungeon.
    FloorTile,
    /// Spikes for pit traps, facing upward.
    Spikes,

    /// Gate for blocking passage in the Z axis and to be possibly slid sideways.
    Gate,
    /// Receptacle for a moved `Gate`.
    GatePocket,
    /// Lock to be composited on a `Gate` block.
    GateLock,

    /// Icon of a tool which can unlock `GateLock`s.
    Key,

    /// Block which has a 1-slot inventory, displaying an item the player can take.
    ItemHolder,

    /// Shape into which to cut blocks [using `CompositeOperator::In`] on the sides of a doorway.
    ///
    /// The +X face of this block should be oriented towards the doorway's interior from the left,
    /// while the +Z face should be oriented towards the room it connects to.
    DoorwaySideMask,
}
use DungeonBlocks::*;

impl BlockModule for DungeonBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/dungeon-blocks"
    }
}

/// Add [`DungeonBlocks`] to the universe.
pub(crate) async fn install_dungeon_blocks(
    txn: &mut UniverseTransaction,
    progress: YieldProgress,
) -> Result<(), GenError> {
    let resolution = R16;
    let resolution_g = GridCoordinate::from(resolution);
    let one_diagonal = GridVector::new(1, 1, 1);
    let center_point_doubled = (one_diagonal * resolution_g).to_point();

    let light_voxel = Block::builder()
        .color(Rgba::new(0.7, 0.7, 0.0, 1.0))
        .light_emission(Rgb::new(8.0, 7.0, 0.7) * 0.5)
        .build();
    let spike_metal = block::from_color!(palette::STEEL);

    BlockProvider::<DungeonBlocks>::new(progress, |key| {
        Ok(match key {
            CorridorLight => Block::builder()
                .display_name("Corridor Light")
                .rotation_rule(RotationPlacementRule::Attach { by: Face::PY })
                .voxels_fn(resolution, |cube| {
                    let centered = cube.lower_bounds() * 2 - center_point_doubled;
                    if centered.y > centered.x.abs() && centered.y > centered.z.abs() {
                        &light_voxel
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),

            Brazier => Block::builder()
                .display_name("Brazier")
                .voxels_handle(resolution, {
                    // Use a darker color to dampen the effect of interior light
                    let body_block = Block::from(palette::STEEL * zo32(0.2));
                    let space = Space::for_block(resolution).build_and_mutate(|m| {
                        m.fill(
                            GridAab::from_lower_upper(
                                [0, 0, 0],
                                [resolution_g, resolution_g / 2, resolution_g],
                            )
                            .shrink(FaceMap::symmetric([2, 0, 2]))
                            .unwrap(),
                            |p| {
                                let mid =
                                    (p.lower_bounds() * 2 - center_point_doubled).map(|c| c.abs());
                                if mid.x.max(mid.z) + (mid.y / 2) < resolution_g {
                                    Some(&body_block)
                                } else {
                                    None
                                }
                            },
                        )?;

                        {
                            let fire_inset = 4;
                            let bounds = GridAab::from_lower_upper(
                                // Vertical overlap will be overwritten, making a bowl shape
                                [fire_inset, resolution_g / 2 - 2, fire_inset],
                                [
                                    resolution_g - fire_inset,
                                    resolution_g,
                                    resolution_g - fire_inset,
                                ],
                            );
                            SpaceTransaction::add_behavior(bounds, crate::Fire::new(bounds))
                                .execute_m(m)
                                .unwrap();
                        }
                        Ok(())
                    })?;
                    txn.insert_anonymous(space)
                })
                .build(),

            FloorTile => const {
                lb::Block {
                    primitive: lb::PrimitiveOrSuch::Image {
                        image: include_image!("floor.png"),
                        rotation: GridRotation::RXZY,
                        extrusion: &[0..32], // TODO: "same as image resolution" should be an option
                        visible: lb::Vox::DEFAULT,
                        invisible: lb::Vox::DEFAULT,
                    },
                    modifiers: &[block::Modifier::SetAttribute(
                        block::SetAttribute::DisplayName(literal!("Floor Tile")),
                    )],
                }
            }
            .load(txn)?,

            Spikes => Block::builder()
                .display_name("Spikes")
                .voxels_fn(resolution, |Cube { x, y, z }| {
                    let resolution = f64::from(resolution);
                    let bsin = |coord| (f64::from(coord) * TAU / resolution * 2.0).sin();
                    if f64::from(y) / resolution < bsin(x) * bsin(z) {
                        &spike_metal
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),

            Gate => const {
                lb::Block {
                    primitive: lb::PrimitiveOrSuch::Image {
                        image: include_image!("fence.png"),
                        rotation: GridRotation::RXyZ,
                        extrusion: &[7..9],
                        visible: lb::Vox::DEFAULT,
                        // selectable collidable transparent blocks,
                        // so it is impossible to reach through the gate.
                        invisible: lb::Vox::DEFAULT,
                    },
                    modifiers: &[block::Modifier::SetAttribute(
                        block::SetAttribute::DisplayName(literal!("Gate")),
                    )],
                }
            }
            .load(txn)?,

            GatePocket => const {
                lb::Block {
                    primitive: lb::PrimitiveOrSuch::Image {
                        image: include_image!("fence-pocket.png"),
                        rotation: GridRotation::RXyZ,
                        extrusion: &[6..7, 9..10],
                        visible: lb::Vox::DEFAULT,
                        // selectable collidable transparent blocks,
                        // so it is impossible to reach through the gate.
                        invisible: lb::Vox::DEFAULT,
                    },
                    modifiers: &[block::Modifier::SetAttribute(
                        block::SetAttribute::DisplayName(literal!("Gate Pocket")),
                    )],
                }
            }
            .load(txn)?,

            GateLock => const {
                lb::Block {
                    primitive: lb::PrimitiveOrSuch::Image {
                        image: include_image!("gate-lock.png"),
                        rotation: GridRotation::RXyZ,
                        extrusion: &[5..11],
                        visible: lb::Vox::DEFAULT,
                        invisible: lb::Vox::DENOTES_AIR,
                    },
                    modifiers: &[block::Modifier::SetAttribute(
                        block::SetAttribute::DisplayName(literal!("Keyhole")),
                    )],
                }
            }
            .load(txn)?,

            Key => const {
                lb::Block {
                    primitive: lb::PrimitiveOrSuch::Image {
                        image: include_image!("key.png"),
                        rotation: GridRotation::RXyZ,
                        extrusion: &[7..9],
                        visible: lb::Vox::DEFAULT,
                        invisible: lb::Vox::DENOTES_AIR,
                    },
                    modifiers: &[block::Modifier::SetAttribute(
                        block::SetAttribute::DisplayName(literal!("Key")),
                    )],
                }
            }
            .load(txn)?,

            ItemHolder => {
                // Makes the contained item float with a varying height
                let blocks_by_y_position: [Block; 3] = core::array::from_fn(|y| {
                    Block::builder()
                        .display_name("Item")
                        .color(Rgba::TRANSPARENT) // TODO: have a shape?
                        .collision(block::BlockCollision::None)
                        // TODO: instead of or in addition to having a special action for this,
                        // it should also be expressible as something that happens when the entire
                        // block is taken, as part of Block::unspecialize().
                        .activation_action(op::Operation::TakeInventory {
                            destroy_if_empty: true,
                        })
                        .inventory_config(inv::InvInBlock::new(
                            1,
                            R2,
                            R32, // to allow a shrunk R16 tool icon
                            // Bottom middle.
                            vec![inv::IconRow::new(
                                0..1,
                                point3(8, 1 + y as i32, 8),
                                vec3(0, 0, 0),
                            )],
                        ))
                        .build()
                });

                let animated_def = block::BlockDef::new_animated(
                    txn.read_ticket(),
                    [(0, 0), (20, 1), (30, 2), (50, 1)]
                        .into_iter()
                        .map(|(phase, y)| (phase, blocks_by_y_position[y].clone())),
                );
                Block::from(txn.insert_anonymous(animated_def))
            }

            DoorwaySideMask => Block::builder()
                .display_name("Doorway Side Mask")
                .voxels_fn(R8, |cube| {
                    if cube.x + cube.z / 2 < 8 {
                        block::from_color!(1.0, 1.0, 1.0, 1.0)
                    } else {
                        AIR
                    }
                })
                .unwrap()
                .build_txn(txn),
        })
    })
    .await?
    .install(ReadTicket::stub(), txn)?;

    Ok(())
}
