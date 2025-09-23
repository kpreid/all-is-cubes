//! Block definitions that are specific to the demo/initial content and not fundamental
//! or UI.

use alloc::sync::Arc;
use core::fmt;
use core::num::NonZeroU16;

use exhaust::Exhaust;
use rand::{Rng as _, SeedableRng as _};

use all_is_cubes::block::{
    self, AIR, AnimationHint, Block, BlockCollision, BlockDefTransaction, Primitive, Resolution::*,
    RotationPlacementRule, TickAction,
};
use all_is_cubes::euclid::Vector3D;
use all_is_cubes::linking::{BlockModule, BlockProvider, GenError};
use all_is_cubes::math::{
    Cube, Face6, FreeCoordinate, GridAab, GridCoordinate, GridSizeCoord, GridVector, Rgb, Rgb01,
    Rgba, ps32, rgb_const, rgba_const,
};
use all_is_cubes::op::Operation;
use all_is_cubes::space::{Space, SpacePhysics, SpaceTransaction};
use all_is_cubes::time;
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{ReadTicket, UniverseTransaction};
use all_is_cubes::util::YieldProgress;

use crate::alg::{NoiseFnExt as _, gradient_lookup, scale_color, square_radius};
use crate::landscape::install_landscape_blocks;
use crate::palette;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::IntoStaticStr /* kludge */, Exhaust)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
#[allow(missing_docs)]
pub enum DemoBlocks {
    GlassBlock,
    Lamp(bool),
    LamppostSegment,
    LamppostBase,
    LamppostTop,
    Sconce(bool),
    Arrow,
    Road,
    Curb,
    ExhibitBackground,
    Pedestal,
    Signboard,
    LabelTextVoxel,
    Clock,
    BecomeBlinker(bool),
    Explosion(i8),
    Projectile,
}
impl BlockModule for DemoBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/demo-blocks"
    }
}
impl fmt::Display for DemoBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let variant = <&str>::from(self); // borrow strum::IntoStaticStr to do the base work
        match self {
            DemoBlocks::BecomeBlinker(state) => write!(f, "{variant}/{state}"),
            DemoBlocks::Explosion(i) => write!(f, "{variant}/{i}"),
            DemoBlocks::Lamp(on) => write!(f, "{variant}/{on}"),
            DemoBlocks::Sconce(on) => write!(f, "{variant}/{on}"),
            _ => write!(f, "{variant}"),
        }
    }
}

/// Add to `universe` demo-content blocks: all of [`DemoBlocks`] and [`LandscapeBlocks`].
///
/// [`LandscapeBlocks`]: crate::landscape::LandscapeBlocks
pub async fn install_demo_blocks(
    txn: &mut UniverseTransaction,
    p: YieldProgress,
) -> Result<(), GenError> {
    let resolution = R16;
    let resolution_g = GridCoordinate::from(resolution);

    // In order to have consistent radii from the center point, we need to work with
    // doubled coordinates to allow for center vs. edge of block distinctions, and
    // note when we mean to refer to a cube center.
    // TODO: The whole premise of how to procedurally generate blocks like this ought
    // to be made more convenient, though.
    let one_diagonal = GridVector::new(1, 1, 1);
    let center_point_doubled = (one_diagonal * resolution_g).to_point();

    let [landscape_p, p] = p.split(0.5);
    install_landscape_blocks(txn, resolution, landscape_p).await?;
    p.progress(0.0).await;

    let curb_color: Block = Rgba::new(0.788, 0.765, 0.741, 1.0).into();
    let road_noise_v = noise::Value::new(0x52b19f6a);
    let road_noise = move |cube: Cube| road_noise_v.at_grid(cube.lower_bounds()) * 0.12 + 1.0;

    let curb_fn = |cube: Cube| {
        let width = resolution_g / 3;
        if (cube - Cube::new(width / 2 + 2, 0, 0))
            .component_mul(GridVector::new(1, 2, 0))
            .square_length()
            < width.pow(2)
        {
            scale_color(curb_color.clone(), road_noise(cube), 0.02)
        } else {
            AIR
        }
    };

    let lamp_globe = block::from_color!(1.0, 1.0, 1.0, 0.75);
    let lamp_emitter_off_on: [Block; 2] = core::array::from_fn(|on| {
        Block::builder()
            .color(Rgba::WHITE)
            .light_emission(Rgb::ONE * 180.0 * on as f32)
            .build()
    });
    let lamppost_metal = Block::builder()
        .color(palette::ALMOST_BLACK.with_alpha_one())
        .build();
    let lamppost_edge = Block::from(palette::ALMOST_BLACK.saturating_scale(ps32(1.12)));

    let pedestal_voxel = block::from_color!(palette::STONE);

    use DemoBlocks::*;
    let provider_for_patch = BlockProvider::<DemoBlocks>::new(p, |key| {
        Ok(match key {
            GlassBlock => {
                let glass_densities = [
                    //block::from_color!(1.0, 0.0, 0.0, 1.0),
                    block::from_color!(0.95, 1.0, 0.95, 0.9),
                    block::from_color!(0.95, 0.95, 1.0, 0.65),
                    block::from_color!(0.95, 1.0, 0.95, 0.3),
                    block::from_color!(0.95, 0.95, 1.0, 0.07),
                    block::from_color!(0.95, 1.0, 0.95, 0.05),
                ];

                Block::builder()
                    .display_name("Glass Block")
                    .voxels_fn(resolution, |cube| {
                        let unit_radius_point = cube
                            .center()
                            .map(|c| c / (FreeCoordinate::from(resolution_g) / 2.0) - 1.0);
                        let power = 7.;
                        let r = unit_radius_point
                            .to_vector()
                            .map(|c| c.abs().powf(power))
                            .dot(Vector3D::new(1.0, 1.0, 1.0));
                        gradient_lookup(&glass_densities, (1.0 - r as f32) * 2.0)
                    })?
                    .build_txn(txn)
            }

            Road => {
                let output_resolution = R32;
                // Use the image as a source of color palettes, not an image.
                // TODO: use hash of coord instead of rng

                use all_is_cubes::content::load_image::{PngAdapter, default_srgb};

                let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(3458679152340);

                let palette_image = PngAdapter::adapt(
                    crate::load_image::include_image!("blocks/road-palette.png"),
                    &default_srgb,
                );
                let range = 0..palette_image.size().width;

                Block::builder()
                    .display_name("Road")
                    .voxels_fn(output_resolution, |cube| {
                        let y = (i32::from(output_resolution) - 1 - cube.y) / 2;
                        let x = rng.random_range(range.clone());
                        palette_image.get_brush(x, y).origin_block().unwrap_or(&AIR)
                    })?
                    .build_txn(txn)
            }

            Lamp(on) => {
                let emitter_r2 = (resolution_g * 3 / 8).pow(2);
                let globe_r2 = resolution_g.pow(2);
                Block::builder()
                    .display_name("Lamp")
                    .voxels_fn(resolution, |cube| {
                        let r2 = (cube.lower_bounds() * 2 + one_diagonal - center_point_doubled)
                            .square_length();
                        if r2 <= emitter_r2 {
                            &lamp_emitter_off_on[usize::from(on)]
                        } else if r2 <= globe_r2 {
                            &lamp_globe
                        } else {
                            &AIR
                        }
                    })?
                    .build_txn(txn)
            }

            LamppostSegment => Block::builder()
                .display_name("Lamppost")
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(resolution, |cube| {
                    if (cube.lower_bounds() * 2 + one_diagonal - center_point_doubled)
                        .component_mul(GridVector::new(1, 0, 1))
                        .square_length()
                        <= 4i32.pow(2)
                    {
                        &lamppost_metal
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),

            LamppostBase => Block::builder()
                .display_name("Lamppost Base")
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(resolution, |cube| {
                    let shape: [GridCoordinate; 16] =
                        [8, 8, 7, 7, 6, 6, 6, 5, 5, 5, 6, 6, 5, 4, 4, 3];
                    let [radius, secondary] = square_radius(resolution, cube);
                    if radius < shape.get(cube.y as usize).copied().unwrap_or(0) {
                        if secondary == radius {
                            &lamppost_edge
                        } else {
                            &lamppost_metal
                        }
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),

            LamppostTop => Block::builder()
                .display_name("Lamppost Top")
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(resolution, |cube| {
                    let shape: [GridCoordinate; 16] =
                        [4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 7, 8];
                    let r2 = (cube.lower_bounds() * 2 + one_diagonal - center_point_doubled)
                        .component_mul(GridVector::new(1, 0, 1))
                        .square_length();
                    if r2 < shape.get(cube.y as usize).copied().unwrap_or(0).pow(2) {
                        &lamppost_metal
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),

            Sconce(on) => {
                let emitter_r2 = (resolution_g / 4).pow(2);
                let globe_r2 = (resolution_g - 2).pow(2);
                Block::builder()
                    .display_name("Sconce")
                    .rotation_rule(RotationPlacementRule::Attach { by: Face6::NZ })
                    .voxels_fn(resolution, |cube| {
                        // TODO: fancier/tidier appearance; this was just some tinkering from the original `Lamp` sphere
                        let r2 = (cube.lower_bounds() * 2 + one_diagonal
                            - center_point_doubled
                                .to_vector()
                                .component_mul(GridVector::new(1, 1, 0)))
                        .to_vector()
                        .component_mul(GridVector::new(3, 1, 1))
                        .square_length();
                        if r2 <= emitter_r2 {
                            &lamp_emitter_off_on[usize::from(on)]
                        } else if r2 <= globe_r2 {
                            &lamp_globe
                        } else if r2 <= (resolution_g + 4).pow(2) && cube.z == 0 {
                            &lamppost_metal
                        } else {
                            &AIR
                        }
                    })?
                    .build_txn(txn)
            }

            Arrow => {
                let space = Space::for_block(resolution).build_and_mutate(|m| {
                    // Support legs, identifying the down / -Y direction.
                    let leg = block::from_color!(palette::STEEL);
                    m.fill_uniform(
                        GridAab::from_lower_size(
                            [resolution_g / 2 - 1, 0, resolution_g / 2 - 1],
                            [2, GridSizeCoord::from(resolution) / 2, 2],
                        ),
                        &leg,
                    )?;

                    // Arrow body
                    let base_body_color = rgb_const!(0.5, 0.5, 0.5);
                    m.fill_all(|cube| {
                        // TODO: We really need better procgen tools
                        let p2 = cube.lower_bounds() * 2 + one_diagonal - center_point_doubled;
                        let r = p2
                            .component_mul(Vector3D::new(1, 1, 0))
                            .map(|c| f64::from(c) / 2.0)
                            .length();
                        let body_radius = if cube.z < 6 {
                            f64::from(cube.z) / 1.5 + 1.0
                        } else {
                            2.0
                        };
                        if r < body_radius {
                            // Shade the body with an axis-indicating color.
                            Some(Block::from(Rgb01::new(
                                base_body_color.red().into_inner()
                                    + (p2.x as f32 / resolution_g as f32),
                                base_body_color.green().into_inner()
                                    + (p2.y as f32 / resolution_g as f32),
                                base_body_color.red().into_inner(),
                            )))
                        } else {
                            None
                        }
                    })?;

                    Ok(())
                })?;

                Block::builder()
                    .display_name("Arrow")
                    .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                    .voxels_handle(resolution, txn.insert_anonymous(space))
                    .build()
            }

            Curb => Block::builder()
                .display_name("Curb")
                // TODO: rotation should specify curb line direction
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(resolution, curb_fn)?
                .build_txn(txn),

            ExhibitBackground => {
                let colors = [
                    block::from_color!(0.825, 0.825, 0.825, 1.0),
                    block::from_color!(0.75, 0.75, 0.75, 1.0),
                ];
                Block::builder()
                    .display_name("Exhibit Background")
                    .voxels_fn(R4, |cube| {
                        &colors[(cube.x + cube.y + cube.z).rem_euclid(2) as usize]
                    })?
                    .build_txn(txn)
            }

            Pedestal => Block::builder()
                .display_name("Pedestal")
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(resolution, |cube| {
                    // TODO: fancier shape
                    let shape: [GridCoordinate; 16] =
                        [6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 7, 8, 8];
                    let [radius, secondary] = square_radius(resolution, cube);
                    let size = shape.get(cube.y as usize).copied().unwrap_or(0);
                    if radius <= size && (radius != secondary || radius < size) {
                        &pedestal_voxel
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),

            Signboard => {
                let sign_board = block::from_color!(palette::PLANK);
                let sign_post = block::from_color!(palette::STEEL);

                // This shape has to coordinate with the name-drawing code in city::demo_city.
                // Haven't thought of a good way to abstract/combine it yet.
                let resolution = R16;
                let resolution_g = GridCoordinate::from(resolution);

                let bottom_edge = 3;
                let top_edge = 12;

                let space = Space::builder(GridAab::from_lower_upper(
                    [0, 0, 9],
                    [resolution_g, resolution_g, resolution_g],
                ))
                .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                .build_and_mutate(|m| {
                    // Sign board
                    m.fill_uniform(
                        GridAab::from_lower_upper(
                            [0, bottom_edge, resolution_g - 1],
                            [resolution_g, top_edge, resolution_g],
                        ),
                        &sign_board,
                    )?;

                    // Support posts
                    // TODO: consider drawing this from an image or ascii-art instead,
                    // for more art control with less code.
                    for post_x in [2, resolution_g - 3] {
                        let post_shape_depth = 6;
                        m.fill(
                            GridAab::from_lower_size(
                                [post_x, 0, resolution_g - post_shape_depth - 1],
                                [
                                    1,
                                    top_edge.cast_unsigned() - 1,
                                    post_shape_depth.cast_unsigned(),
                                ],
                            ),
                            |cube| {
                                let vertical_post = cube.z >= resolution_g - 3;
                                let diagonal_brace = (9..11).contains(&(cube.z - cube.y));
                                if vertical_post || diagonal_brace {
                                    Some(&sign_post)
                                } else {
                                    None
                                }
                            },
                        )?;
                    }

                    Ok(())
                })?;

                Block::builder()
                    .display_name("Signboard")
                    .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                    .voxels_handle(resolution, txn.insert_anonymous(space))
                    .build()
            }

            // TODO: not all text that should use this does yet
            LabelTextVoxel => Block::builder()
                .color(palette::ALMOST_BLACK.with_alpha_one())
                .collision(BlockCollision::None)
                .build(),

            Clock => {
                let mut space = Space::builder(GridAab::from_lower_size([0, 0, 0], [16, 16, 1]))
                    .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                    .build();
                SpaceTransaction::add_behavior(space.bounds(), crate::animation::Clock::new())
                    .execute(&mut space, ReadTicket::stub(), &mut transaction::no_outputs)
                    .unwrap();
                Block::builder()
                    .display_name("Clock")
                    .rotation_rule(RotationPlacementRule::Attach { by: Face6::NZ })
                    .animation_hint(AnimationHint::redefinition(
                        block::AnimationChange::ColorSameCategory,
                    ))
                    // Clock behavior assumes R16
                    .voxels_handle(R16, txn.insert_anonymous(space))
                    .build()
            }

            BecomeBlinker(state) => Block::builder()
                .color(if state { Rgba::WHITE } else { Rgba::BLACK })
                .display_name(format!("Blinker {state:?}"))
                .animation_hint(AnimationHint::replacement(
                    block::AnimationChange::ColorSameCategory,
                ))
                .build(),

            Explosion(timer) => {
                let decay = if timer < 0 {
                    1.0
                } else {
                    (f64::from(timer) * -0.1).exp()
                };

                let atom = Block::builder()
                    .display_name(format!("Explosion {timer} particle"))
                    .color(rgba_const!(0.014, 0.01, 0.01, 1.0))
                    .collision(BlockCollision::None)
                    .light_emission(if timer < 0 {
                        if timer.rem_euclid(4) < 2 {
                            Rgb::ZERO
                        } else {
                            rgb_const!(0.8, 0.0, 0.0)
                        }
                    } else {
                        rgb_const!(1.0, 0.77, 0.40) * 10.0 * decay.powf(3.) as f32
                    })
                    .build();

                let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(0);
                Block::builder()
                    .display_name(format!("Explosion {timer}"))
                    .animation_hint(AnimationHint::replacement(if timer == i8::MAX {
                        block::AnimationChange::Shape
                    } else {
                        block::AnimationChange::ColorSameCategory
                    }))
                    .voxels_fn(if timer <= 0 { R1 } else { R8 }, |_| {
                        if rng.random_bool(decay.powf(3.)) {
                            &atom
                        } else {
                            &AIR
                        }
                    })?
                    .build_txn(txn)
            }

            Projectile => Block::builder()
                .display_name("Projectile")
                .voxels_fn(resolution, |cube| {
                    let [r, _] = square_radius(resolution, cube);
                    if r * 4 < resolution_g {
                        &lamppost_metal
                    } else {
                        &AIR
                    }
                })?
                .animation_hint(AnimationHint::replacement(block::AnimationChange::Shape))
                .tick_action(TickAction {
                    operation: Operation::Alt(
                        [
                            Operation::StartMove(block::Move::new(Face6::PY, 32, 32)),
                            // if we can't move, vanish
                            Operation::Become(AIR),
                        ]
                        .into(),
                    ),
                    schedule: time::Schedule::EVERY_TICK,
                })
                .build_txn(txn),
        })
    })
    .await?
    .install(ReadTicket::stub(), txn)?;

    // Join up blinker blocks
    for state in bool::exhaust() {
        modify_def(
            txn,
            &provider_for_patch[BecomeBlinker(state)],
            |read_ticket, block| {
                block.freezing_get_attributes_mut(read_ticket).tick_action = Some(TickAction {
                    operation: Operation::Become(provider_for_patch[BecomeBlinker(!state)].clone()),
                    schedule: time::Schedule::from_period(NonZeroU16::new(60).unwrap()),
                });
            },
        );
    }

    // Join up lamp blocks as toggleable on/off
    for state in bool::exhaust() {
        for ctor in [Lamp, Sconce] {
            modify_def(
                txn,
                &provider_for_patch[ctor(state)],
                |read_ticket, block| {
                    block
                        .freezing_get_attributes_mut(read_ticket)
                        .activation_action =
                        Some(Operation::Become(provider_for_patch[ctor(!state)].clone()));
                },
            );
        }
    }

    // Join up explosion blocks
    for i in i8::exhaust() {
        modify_def(
            txn,
            &provider_for_patch[Explosion(i)],
            |read_ticket, block| {
                let neighbor_ops: Arc<[(Cube, Operation)]> = if i > 22 {
                    // Expire because we're invisible by now
                    [(Cube::ORIGIN, Operation::Become(AIR))].into()
                } else {
                    let next = Operation::DestroyTo(provider_for_patch[Explosion(i + 1)].clone());
                    if i > 0 && i < 10 {
                        // Expand at first out to ~5 blocks
                        if i.rem_euclid(2) == 0 {
                            if i.rem_euclid(4) == 0 {
                                const {
                                    [
                                        Cube::new(0, 0, 0),
                                        Cube::new(1, 0, 0),
                                        Cube::new(-1, 0, 0),
                                        Cube::new(0, 1, 0),
                                        Cube::new(0, -1, 0),
                                        Cube::new(0, 0, 1),
                                        Cube::new(0, 0, -1),
                                    ]
                                }
                                .as_slice()
                            } else {
                                const {
                                    [
                                        Cube::new(0, 0, 0),
                                        Cube::new(1, 1, 0),
                                        Cube::new(-1, 1, 0),
                                        Cube::new(0, 1, 1),
                                        Cube::new(0, -1, 1),
                                        Cube::new(1, 0, 1),
                                        Cube::new(1, 0, -1),
                                        Cube::new(1, -1, 0),
                                        Cube::new(-1, -1, 0),
                                        Cube::new(0, 1, -1),
                                        Cube::new(0, -1, -1),
                                        Cube::new(-1, 0, 1),
                                        Cube::new(-1, 0, -1),
                                    ]
                                }
                                .as_slice()
                            }
                        } else {
                            [Cube::ORIGIN].as_slice()
                        }
                    } else {
                        // Just tick or fade
                        [Cube::ORIGIN].as_slice()
                    }
                    .iter()
                    .map(|&cube| (cube, next.clone()))
                    .collect()
                };
                block.freezing_get_attributes_mut(read_ticket).tick_action = Some(TickAction {
                    operation: Operation::Neighbors(neighbor_ops),
                    schedule: time::Schedule::from_period(NonZeroU16::new(2).unwrap()),
                });
            },
        );
    }

    Ok(())
}

/// Given a block using [`Primitive::Indirect`], apply the function to replace the referenced
/// [`BlockDef`]'s attributes.
#[track_caller]
fn modify_def(
    transaction: &mut UniverseTransaction,
    indirect: &Block,
    f: impl FnOnce(ReadTicket<'_>, &mut Block),
) {
    let Primitive::Indirect(block_def_handle) = indirect.primitive() else {
        panic!("block not indirect, but {indirect:?}");
    };
    let mut block: Block = block_def_handle
        .read(transaction.read_ticket())
        .expect("could not read BlockDef")
        .block()
        .clone();
    f(transaction.read_ticket(), &mut block);
    block_def_handle
        .execute_on_pending(
            transaction.read_ticket(),
            BlockDefTransaction::overwrite(block),
        )
        .expect("BlockDef mutation transaction failed");
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::util::yield_progress_for_testing;

    #[macro_rules_attribute::apply(smol_macros::test)]
    async fn install_demo_blocks_test() {
        install_demo_blocks(
            &mut UniverseTransaction::default(),
            yield_progress_for_testing(),
        )
        .await
        .unwrap();
    }
}
