// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Block definitions that are specific to the demo/initial content and not fundamental
//! or UI.

use all_is_cubes::cgmath::{ElementWise as _, EuclideanSpace as _, InnerSpace, Vector3};
use all_is_cubes::drawing::embedded_graphics::{
    prelude::Point,
    primitives::{Line, PrimitiveStyle, Rectangle, StyledDrawable},
};
use all_is_cubes::rgb_const;
use noise::Seedable as _;

use all_is_cubes::block::{Block, BlockCollision, AIR};
use all_is_cubes::linking::{BlockModule, BlockProvider, GenError, InGenError};
use all_is_cubes::math::{
    GridCoordinate, GridMatrix, GridPoint, GridRotation, GridVector, NoiseFnExt as _, NotNan, Rgb,
    Rgba,
};
use all_is_cubes::space::{Grid, Space};
use all_is_cubes::universe::Universe;

use crate::int_magnitude_squared;
use crate::landscape::install_landscape_blocks;
use crate::palette;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub enum DemoBlocks {
    Lamp,
    Lamppost,
    Sconce,
    Arrow,
    Road,
    Curb,
    CurbCorner,
    ExhibitBackground,
    Signboard,
}
impl BlockModule for DemoBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/demo-blocks"
    }
}

/// Add to `universe` demo-content blocks, that might be used by demo worldgen or offered to the player.
pub fn install_demo_blocks(universe: &mut Universe) -> Result<(), GenError> {
    let resolution = 16;
    let resolution_g = GridCoordinate::from(resolution);

    // In order to have consistent radii from the center point, we need to work with
    // doubled coordinates to allow for center vs. edge of block distinctions, and
    // note when we mean to refer to a cube center.
    // TODO: The whole premise of how to procedurally generate blocks like this ought
    // to be made more convenient, though.
    let one_diagonal = GridVector::new(1, 1, 1);
    let center_point_doubled = GridPoint::from_vec(one_diagonal * resolution_g);

    install_landscape_blocks(universe, resolution)?;

    use DemoBlocks::*;
    let road_color: Block = Rgba::new(0.157, 0.130, 0.154, 1.0).into();
    let curb_color: Block = Rgba::new(0.788, 0.765, 0.741, 1.0).into();
    let road_noise_v = noise::Value::new().set_seed(0x52b19f6a);
    let road_noise = noise::ScaleBias::new(&road_noise_v)
        .set_bias(1.0)
        .set_scale(0.12);

    let curb_fn = |cube: GridPoint| {
        let width = resolution_g / 3;
        if int_magnitude_squared(
            (cube - GridPoint::new(width / 2 + 2, 0, 0)).mul_element_wise(GridVector::new(1, 2, 0)),
        ) < width.pow(2)
        {
            scale_color(curb_color.clone(), road_noise.at_grid(cube), 0.02)
        } else {
            AIR
        }
    };

    BlockProvider::<DemoBlocks>::new(|key| {
        Ok(match key {
            Road => Block::builder()
                .display_name("Road")
                .voxels_fn(universe, resolution, |cube| {
                    scale_color(road_color.clone(), road_noise.at_grid(cube), 0.02)
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
                        Rgba::WHITE.into()
                    } else {
                        AIR.clone()
                    }
                })?
                .build(),

            Lamppost => Block::builder()
                .display_name("Lamppost")
                .light_emission(Rgb::new(3.0, 3.0, 3.0))
                .collision(BlockCollision::Recur)
                .voxels_fn(universe, resolution, |p| {
                    if int_magnitude_squared(
                        (p * 2 + one_diagonal - center_point_doubled)
                            .mul_element_wise(GridVector::new(1, 0, 1)),
                    ) <= 4i32.pow(2)
                    {
                        palette::ALMOST_BLACK.into()
                    } else {
                        AIR.clone()
                    }
                })?
                .build(),

            Sconce => Block::builder()
                .display_name("Sconce")
                .light_emission(Rgb::new(8.0, 7.0, 6.0))
                .collision(BlockCollision::Recur)
                .voxels_fn(universe, resolution, |p| {
                    // TODO: fancier/tidier appearance; this was just some tinkering from the original `Lamp` sphere
                    let r2 = int_magnitude_squared(
                        (p * 2 + one_diagonal
                            - center_point_doubled.mul_element_wise(GridPoint::new(1, 1, 0)))
                        .mul_element_wise(GridVector::new(3, 1, 1)),
                    );
                    if r2 <= (resolution_g - 2).pow(2) {
                        Rgba::WHITE.into()
                    } else if r2 <= (resolution_g + 4).pow(2) && p.z == 0 {
                        palette::ALMOST_BLACK.into()
                    } else {
                        AIR.clone()
                    }
                })?
                .build(),

            Arrow => {
                let mut space = Space::for_block(resolution).build_empty();

                // Support legs, identifying the down / -Y direction.
                let leg = Block::from(palette::STEEL);
                space.fill_uniform(
                    Grid::new(
                        [resolution_g / 2 - 1, 0, resolution_g / 2 - 1],
                        [2, resolution_g / 2, 2],
                    ),
                    &leg,
                )?;

                // Arrow body
                let base_body_color = rgb_const!(0.5, 0.5, 0.5);
                space.fill(space.grid(), |p| {
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
                    .voxels_ref(resolution, universe.insert_anonymous(space))
                    .build()
            }

            Curb => Block::builder()
                .display_name("Curb")
                .collision(BlockCollision::Recur)
                .voxels_fn(universe, resolution, curb_fn)?
                .build(),

            CurbCorner => Block::builder()
                .display_name("Curb Corner")
                .collision(BlockCollision::Recur)
                .voxels_fn(universe, resolution, |cube| {
                    // TODO: rework so this isn't redoing the rotation calculations for every single voxel
                    // We should have tools for composing blocks instead...
                    for rot in GridRotation::CLOCKWISE.iterate() {
                        let block = curb_fn(
                            rot.to_positive_octant_matrix(resolution.into())
                                .transform_cube(cube),
                        );
                        if block != AIR {
                            return block;
                        }
                    }
                    AIR
                })?
                .build(),

            ExhibitBackground => {
                let colors = [
                    Block::from(Rgba::new(0.825, 0.825, 0.825, 1.0)),
                    Block::from(Rgba::new(0.75, 0.75, 0.75, 1.0)),
                ];
                Block::builder()
                    .display_name("Exhibit Background")
                    .voxels_fn(universe, 4, |cube| {
                        &colors[(cube.x + cube.y + cube.z).rem_euclid(2) as usize]
                    })?
                    .build()
            }

            Signboard => {
                let sign_board = Block::from(palette::PLANK);
                let sign_post = Block::from(palette::STEEL);

                // This shape has to coordinate with the name-drawing code in city::demo_city.
                // Haven't thought of a good way to abstract/combine it yet.
                let resolution = 16;
                let top_edge = 10;

                let mut space = Space::for_block(resolution).build_empty();

                // Sign board
                {
                    // space.fill_uniform(
                    //     Grid::from_lower_upper([0, 8, 15], [16, 16, 16]),
                    //     &sign_backing,
                    // )?;
                    let mut plane = space.draw_target(GridMatrix::from_translation([
                        0,
                        0,
                        (resolution - 1).into(),
                    ]));
                    Rectangle::with_corners(
                        Point::new(0, (resolution / 4).into()),
                        Point::new((resolution - 1).into(), top_edge),
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
                    let z = (resolution - 3).into();
                    // Vertical post
                    Line::new(Point::new(z, 0), Point::new(z, top_edge - 1))
                        .draw_styled(style, &mut plane)?;
                    // Diagonal brace
                    Line::new(Point::new(z - 4, 0), Point::new(z, 4))
                        .draw_styled(style, &mut plane)?;
                    Ok(())
                };
                post(2)?;
                post((resolution - 3).into())?;

                Block::builder()
                    .display_name("Signboard")
                    .collision(BlockCollision::Recur)
                    .voxels_ref(resolution, universe.insert_anonymous(space))
                    .build()
            }
        })
    })?
    .install(universe)?;

    Ok(())
}

/// Generate a copy of a [`Block::Atom`] with its color scaled by the given scalar.
///
/// The scalar is rounded to steps of `quantization`, to reduce the number of distinct
/// block types generated.
///
/// If the computation is NaN or the block is not an atom, it is returned unchanged.
pub(crate) fn scale_color(block: Block, scalar: f64, quantization: f64) -> Block {
    let scalar = (scalar / quantization).round() * quantization;
    match (block, NotNan::new(scalar as f32)) {
        (Block::Atom(attributes, color), Ok(scalar)) => Block::Atom(
            attributes,
            (color.to_rgb() * scalar).with_alpha(color.alpha()),
        ),
        (block, _) => block,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn install_demo_blocks_test() {
        let mut universe = Universe::new();
        install_demo_blocks(&mut universe).unwrap();
        // TODO: assert what entries were created, once Universe has iteration
    }
}
