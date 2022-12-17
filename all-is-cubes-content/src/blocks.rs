//! Block definitions that are specific to the demo/initial content and not fundamental
//! or UI.

use std::fmt;

use exhaust::Exhaust;

use all_is_cubes::block::{
    AnimationHint, Block, BlockCollision, BlockDefTransaction, Primitive, Resolution,
    Resolution::*, RotationPlacementRule, AIR,
};
use all_is_cubes::cgmath::{ElementWise as _, EuclideanSpace as _, InnerSpace, Vector3};
use all_is_cubes::drawing::embedded_graphics::{
    prelude::Point,
    primitives::{Line, PrimitiveStyle, Rectangle, StyledDrawable},
};
use all_is_cubes::drawing::VoxelBrush;
use all_is_cubes::linking::{BlockModule, BlockProvider, GenError, InGenError};
use all_is_cubes::math::{
    cube_to_midpoint, Face6, FreeCoordinate, GridAab, GridCoordinate, GridMatrix, GridPoint,
    GridRotation, GridVector, NotNan, Rgb, Rgba,
};
use all_is_cubes::space::{Space, SpacePhysics, SpaceTransaction};
use all_is_cubes::transaction::Transaction as _;
use all_is_cubes::universe::Universe;
use all_is_cubes::util::YieldProgress;
use all_is_cubes::{rgb_const, rgba_const};

use crate::int_magnitude_squared;
use crate::landscape::install_landscape_blocks;
use crate::noise::NoiseFnExt;
use crate::palette;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::IntoStaticStr /* kludge */, Exhaust)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
#[allow(missing_docs)]
pub enum DemoBlocks {
    GlassBlock,
    Lamp,
    LamppostSegment,
    LamppostBase,
    LamppostTop,
    Sconce,
    Arrow,
    Road,
    Curb,
    ExhibitBackground,
    Pedestal,
    Signboard,
    Clock,
    Explosion(u8),
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
            DemoBlocks::Explosion(i) => write!(f, "{variant}/{i}"),
            _ => write!(f, "{variant}"),
        }
    }
}

/// Add to `universe` demo-content blocks: all of [`DemoBlocks`] and [`LandscapeBlocks`].
///
/// [`LandscapeBlocks`]: crate::landscape::LandscapeBlocks
pub async fn install_demo_blocks(
    universe: &mut Universe,
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
    let center_point_doubled = GridPoint::from_vec(one_diagonal * resolution_g);

    let [landscape_p, p] = p.split(0.5);
    install_landscape_blocks(universe, resolution, landscape_p).await?;
    p.progress(0.0).await;

    let road_color: Block = Rgba::new(0.157, 0.130, 0.154, 1.0).into();
    let curb_color: Block = Rgba::new(0.788, 0.765, 0.741, 1.0).into();
    let road_noise_v = noise::Value::new(0x52b19f6a);
    let road_noise = move |cube| road_noise_v.at_grid(cube) * 0.12 + 1.0;

    let curb_fn = |cube: GridPoint| {
        let width = resolution_g / 3;
        if int_magnitude_squared(
            (cube - GridPoint::new(width / 2 + 2, 0, 0)).mul_element_wise(GridVector::new(1, 2, 0)),
        ) < width.pow(2)
        {
            scale_color(curb_color.clone(), road_noise(cube), 0.02)
        } else {
            AIR
        }
    };

    let lamp_globe = Block::from(Rgba::WHITE);
    let lamppost_metal = Block::from(palette::ALMOST_BLACK);
    let lamppost_edge = Block::from(palette::ALMOST_BLACK * 1.12);

    let pedestal_voxel = Block::from(palette::STONE);

    use DemoBlocks::*;
    BlockProvider::<DemoBlocks>::new(p, |key| {
        Ok(match key {
            GlassBlock => {
                let glass_densities = [
                    //Block::from(rgba_const!(1.0, 0.0, 0.0, 1.0)),
                    Block::from(rgba_const!(0.95, 1.0, 0.95, 0.9)),
                    Block::from(rgba_const!(0.95, 0.95, 1.0, 0.65)),
                    Block::from(rgba_const!(0.95, 1.0, 0.95, 0.3)),
                    Block::from(rgba_const!(0.95, 0.95, 1.0, 0.07)),
                    Block::from(rgba_const!(0.95, 1.0, 0.95, 0.05)),
                ];

                Block::builder()
                    .display_name("Glass Block")
                    .voxels_fn(universe, resolution, |cube| {
                        let unit_radius_point = cube_to_midpoint(cube)
                            .map(|c| c / (FreeCoordinate::from(resolution_g) / 2.0) - 1.0);
                        let power = 7.;
                        let r = unit_radius_point
                            .to_vec()
                            .map(|c| c.abs().powf(power))
                            .dot(Vector3::new(1.0, 1.0, 1.0));
                        gradient_lookup(&glass_densities, (1.0 - r as f32) * 2.0)
                    })?
                    .build()
            }

            Road => Block::builder()
                .display_name("Road")
                .voxels_fn(universe, resolution, |cube| {
                    scale_color(road_color.clone(), road_noise(cube), 0.02)
                })?
                .build(),

            Lamp => Block::builder()
                .display_name("Lamp")
                .light_emission(Rgb::new(20.0, 20.0, 20.0))
                .collision(BlockCollision::Recur)
                .voxels_fn(universe, resolution, |p| {
                    if int_magnitude_squared(p * 2 + one_diagonal - center_point_doubled)
                        <= resolution_g.pow(2)
                    {
                        &lamp_globe
                    } else {
                        &AIR
                    }
                })?
                .build(),

            LamppostSegment => Block::builder()
                .display_name("Lamppost")
                .light_emission(Rgb::new(3.0, 3.0, 3.0))
                .collision(BlockCollision::Recur)
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(universe, resolution, |cube| {
                    if int_magnitude_squared(
                        (cube * 2 + one_diagonal - center_point_doubled)
                            .mul_element_wise(GridVector::new(1, 0, 1)),
                    ) <= 4i32.pow(2)
                    {
                        &lamppost_metal
                    } else {
                        &AIR
                    }
                })?
                .build(),

            LamppostBase => Block::builder()
                .display_name("Lamppost Base")
                .collision(BlockCollision::Recur)
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(universe, resolution, |cube| {
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
                .build(),

            LamppostTop => Block::builder()
                .display_name("Lamppost Top")
                .collision(BlockCollision::Recur)
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(universe, resolution, |cube| {
                    let shape: [GridCoordinate; 16] =
                        [4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 7, 8];
                    let r2 = int_magnitude_squared(
                        (cube * 2 + one_diagonal - center_point_doubled)
                            .mul_element_wise(GridVector::new(1, 0, 1)),
                    );
                    if r2 < shape.get(cube.y as usize).copied().unwrap_or(0).pow(2) {
                        &lamppost_metal
                    } else {
                        &AIR
                    }
                })?
                .build(),

            Sconce => Block::builder()
                .display_name("Sconce")
                .light_emission(Rgb::new(8.0, 7.0, 6.0))
                .collision(BlockCollision::Recur)
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NZ })
                .voxels_fn(universe, resolution, |p| {
                    // TODO: fancier/tidier appearance; this was just some tinkering from the original `Lamp` sphere
                    let r2 = int_magnitude_squared(
                        (p * 2 + one_diagonal
                            - center_point_doubled.mul_element_wise(GridPoint::new(1, 1, 0)))
                        .mul_element_wise(GridVector::new(3, 1, 1)),
                    );
                    if r2 <= (resolution_g - 2).pow(2) {
                        &lamp_globe
                    } else if r2 <= (resolution_g + 4).pow(2) && p.z == 0 {
                        &lamppost_metal
                    } else {
                        &AIR
                    }
                })?
                .build(),

            Arrow => {
                let mut space = Space::for_block(resolution).build();

                // Support legs, identifying the down / -Y direction.
                let leg = Block::from(palette::STEEL);
                space.fill_uniform(
                    GridAab::from_lower_size(
                        [resolution_g / 2 - 1, 0, resolution_g / 2 - 1],
                        [2, resolution_g / 2, 2],
                    ),
                    &leg,
                )?;

                // Arrow body
                let base_body_color = rgb_const!(0.5, 0.5, 0.5);
                space.fill(space.bounds(), |p| {
                    // TODO: We really need better procgen tools
                    let p2 = p * 2 + one_diagonal - center_point_doubled;
                    let r = p2
                        .mul_element_wise(Vector3::new(1, 1, 0))
                        .map(|c| f64::from(c) / 2.0)
                        .magnitude();
                    let body_radius = if p.z < 6 {
                        f64::from(p.z) / 1.5 + 1.0
                    } else {
                        2.0
                    };
                    if r < body_radius {
                        // Shade the body with an axis-indicating color.
                        Some(Block::from(Rgb::new(
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

                Block::builder()
                    .display_name("Arrow")
                    .collision(BlockCollision::Recur)
                    .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                    .voxels_ref(resolution, universe.insert_anonymous(space))
                    .build()
            }

            Curb => Block::builder()
                .display_name("Curb")
                .collision(BlockCollision::Recur)
                // TODO: rotation should specify curb line direction
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(universe, resolution, curb_fn)?
                .build(),

            ExhibitBackground => {
                let colors = [
                    Block::from(Rgba::new(0.825, 0.825, 0.825, 1.0)),
                    Block::from(Rgba::new(0.75, 0.75, 0.75, 1.0)),
                ];
                Block::builder()
                    .display_name("Exhibit Background")
                    .voxels_fn(universe, R4, |cube| {
                        &colors[(cube.x + cube.y + cube.z).rem_euclid(2) as usize]
                    })?
                    .build()
            }

            Pedestal => Block::builder()
                .display_name("Pedestal")
                .collision(BlockCollision::Recur)
                .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                .voxels_fn(universe, resolution, |cube| {
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
                .build(),

            Signboard => {
                let sign_board = Block::from(palette::PLANK);
                let sign_post = Block::from(palette::STEEL);

                // This shape has to coordinate with the name-drawing code in city::demo_city.
                // Haven't thought of a good way to abstract/combine it yet.
                let resolution = R16;
                let resolution_g = GridCoordinate::from(resolution);

                let bottom_edge = 3;
                let top_edge = 12;

                let mut space = Space::builder(GridAab::from_lower_upper(
                    [0, 0, 9],
                    [resolution_g, resolution_g, resolution_g],
                ))
                .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                .build();

                // Sign board
                {
                    let mut plane =
                        space.draw_target(GridMatrix::from_translation([0, 0, resolution_g - 1]));
                    Rectangle::with_corners(
                        Point::new(0, bottom_edge),
                        Point::new(resolution_g - 1, top_edge),
                    )
                    .draw_styled(&PrimitiveStyle::with_fill(&sign_board), &mut plane)?;
                }

                // Support posts
                let mut post = |x| -> Result<(), InGenError> {
                    let mut plane = space.draw_target(
                        GridMatrix::from_translation([x, 0, 0])
                            * GridRotation::RZYX.to_rotation_matrix(),
                    );
                    let style = &PrimitiveStyle::with_stroke(&sign_post, 2);
                    let z = resolution_g - 3;
                    // Vertical post
                    Line::new(Point::new(z, 0), Point::new(z, top_edge - 1))
                        .draw_styled(style, &mut plane)?;
                    // Diagonal brace
                    Line::new(Point::new(z - 4, 0), Point::new(z, 4))
                        .draw_styled(style, &mut plane)?;
                    Ok(())
                };
                post(2)?;
                post(resolution_g - 3)?;

                Block::builder()
                    .display_name("Signboard")
                    .collision(BlockCollision::Recur)
                    .rotation_rule(RotationPlacementRule::Attach { by: Face6::NY })
                    .voxels_ref(resolution, universe.insert_anonymous(space))
                    .build()
            }

            Clock => {
                let resolution = R16;
                let mut space = Space::builder(GridAab::from_lower_size([0, 0, 0], [16, 16, 1]))
                    .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
                    .build();
                SpaceTransaction::add_behavior(space.bounds(), crate::animation::Clock::new())
                    .execute(&mut space)
                    .unwrap();
                Block::builder()
                    .display_name("Clock")
                    .collision(BlockCollision::None)
                    .rotation_rule(RotationPlacementRule::Attach { by: Face6::NZ })
                    .animation_hint(AnimationHint::CONTINUOUS)
                    .voxels_ref(resolution, universe.insert_anonymous(space))
                    .build()
            }

            Explosion(timer) => {
                let decay = (f32::from(timer) * -0.1).exp();
                Block::builder()
                    .display_name(format!("Explosion {timer}"))
                    .collision(BlockCollision::None)
                    .light_emission(Rgb::ONE * decay)
                    .color(Rgb::ONE.with_alpha(NotNan::new(decay).unwrap()))
                    .build()
            }
        })
    })
    .await?
    .install(universe)?;

    // Kludge to patch up tick action cross-references
    // TODO: This should be possible to do as a built-in feature of the "linking" system
    let provider_for_patch = BlockProvider::using(universe)
        .map_err(|e| GenError::failure(e, "TODO: dummy name".into()))?;
    for i in 0..=255 {
        if let Primitive::Indirect(block_def_ref) = provider_for_patch[Explosion(i)].primitive() {
            let mut block: Block = (*block_def_ref.borrow()).clone();

            if let Primitive::Atom(attributes, _) = block.primitive_mut() {
                attributes.tick_action = if i > 30 {
                    // Expire
                    Some(VoxelBrush::single(AIR))
                } else {
                    let next = &provider_for_patch[Explosion(i + 1)];
                    if i < 15 {
                        // Expand for the first 0.25 seconds out to ~5 blocks
                        if i % 3 == 0 {
                            if i % 6 == 0 {
                                Some(VoxelBrush::new([
                                    ([0, 0, 0], next.clone()),
                                    ([1, 0, 0], next.clone()),
                                    ([-1, 0, 0], next.clone()),
                                    ([0, 1, 0], next.clone()),
                                    ([0, -1, 0], next.clone()),
                                    ([0, 0, 1], next.clone()),
                                    ([0, 0, -1], next.clone()),
                                ]))
                            } else {
                                Some(VoxelBrush::new([
                                    ([0, 0, 0], next.clone()),
                                    ([1, 1, 0], next.clone()),
                                    ([-1, 1, 0], next.clone()),
                                    ([0, 1, 1], next.clone()),
                                    ([0, -1, 1], next.clone()),
                                    ([1, 0, 1], next.clone()),
                                    ([1, 0, -1], next.clone()),
                                    ([1, -1, 0], next.clone()),
                                    ([-1, -1, 0], next.clone()),
                                    ([0, 1, -1], next.clone()),
                                    ([0, -1, -1], next.clone()),
                                    ([-1, 0, 1], next.clone()),
                                    ([-1, 0, -1], next.clone()),
                                ]))
                            }
                        } else {
                            Some(VoxelBrush::new([([0, 0, 0], next.clone())]))
                        }
                    } else {
                        // Just fade
                        Some(VoxelBrush::new([([0, 0, 0], next.clone())]))
                    }
                };
            } else {
                panic!("not atom");
            }

            block_def_ref
                .execute(&BlockDefTransaction::overwrite(block))
                .unwrap();
        } else {
            panic!("not indirect");
        }
    }

    Ok(())
}

/// Generate a copy of a [`Primitive::Atom`] block with its color scaled by the given scalar.
///
/// The scalar is rounded to steps of `quantization`, to reduce the number of distinct
/// block types generated.
///
/// If the computation is NaN or the block is not an atom, it is returned unchanged.
pub(crate) fn scale_color(block: Block, scalar: f64, quantization: f64) -> Block {
    let scalar = (scalar / quantization).round() * quantization;
    match (block.primitive(), NotNan::new(scalar as f32)) {
        (Primitive::Atom(attributes, color), Ok(scalar)) => Block::from_primitive(Primitive::Atom(
            attributes.clone(),
            (color.to_rgb() * scalar).with_alpha(color.alpha()),
        )),
        _ => block,
    }
}

/// Subdivide the range 0.0 to 1.0 into `gradient.len()` parts and return the [`Block`]
/// which the value falls into.
///
/// Panics if `gradient.len() == 0`.
pub(crate) fn gradient_lookup(gradient: &[Block], value: f32) -> &Block {
    &gradient[((value * gradient.len() as f32) as usize).clamp(0, gradient.len() - 1)]
}

/// Compute the cube's distance from the midpoint of the Y axis of the block volume.
///
/// The centermost 4 cubes that exist in every resolution above 1 all have a distance of 1.
/// (No cube ever has a distance of 0, so 0 can be used in a comparison for “never”.)
///
/// The first returned number is the "radius" value and the second is the distance
/// on the lesser axis, which may be used for distance from the center or corner along
/// the surface.
fn square_radius(resolution: Resolution, cube: GridPoint) -> [GridCoordinate; 2] {
    let distances_vec = cube.map(|c| (c * 2 + 1 - GridCoordinate::from(resolution)).abs() / 2 + 1);
    if distances_vec.x > distances_vec.z {
        [distances_vec.x, distances_vec.z]
    } else {
        [distances_vec.z, distances_vec.x]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::content::make_some_blocks;

    #[test]
    pub fn install_demo_blocks_test() {
        let mut universe = Universe::new();
        futures_executor::block_on(async {
            install_demo_blocks(&mut universe, YieldProgress::noop())
                .await
                .unwrap()
        });
        // TODO: assert what entries were created, once Universe has iteration
    }

    #[test]
    fn gradient_lookup_cases() {
        let blocks = make_some_blocks::<4>();
        let inputs_and_output_indices = [
            (-f32::INFINITY, 0),
            (-10.0, 0),
            (-0.1, 0),
            (0.0, 0),
            (0.24, 0),
            (0.25, 1),
            (0.26, 1),
            (0.49, 1),
            (0.50, 2),
            (0.51, 2),
            (0.9, 3),
            (1.0, 3),
            (1.1, 3),
            (10.0, 3),
            (f32::INFINITY, 3),
            (f32::NAN, 0),
        ];
        assert_eq!(
            inputs_and_output_indices.map(|(i, _)| gradient_lookup(&blocks, i)),
            inputs_and_output_indices.map(|(_, o)| &blocks[o]),
        );
    }

    #[test]
    fn square_radius_cases() {
        assert_eq!(
            [6, 7, 8, 9].map(|x| square_radius(R16, GridPoint::new(x, 2, 8))[0]),
            [2, 1, 1, 2]
        );
    }
}
