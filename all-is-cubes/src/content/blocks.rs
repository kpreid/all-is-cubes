// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Block definitions that are specific to the demo/initial content and not fundamental
//! or UI.

use cgmath::ElementWise as _;
use noise::Seedable as _;

use crate::block::{Block, BlockCollision, AIR};
use crate::content::landscape::install_landscape_blocks;
use crate::linking::{BlockModule, BlockProvider};
use crate::math::{
    int_magnitude_squared, GridCoordinate, GridPoint, GridVector, NoiseFnExt as _, NotNan, Rgb,
    Rgba,
};
use crate::universe::{InsertError, Universe};

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
pub enum DemoBlocks {
    Lamp,
    Lamppost,
    Road,
    Curb,
}
impl BlockModule for DemoBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/demo-blocks"
    }
}

/// Add to `universe` demo-content blocks, that might be used by demo worldgen or offered to the player.
pub fn install_demo_blocks(universe: &mut Universe) -> Result<(), InsertError> {
    let resolution = 16;
    let resolution_g = GridCoordinate::from(resolution);
    let center_point = GridPoint::new(1, 1, 1) * (resolution_g / 2);
    install_landscape_blocks(universe, resolution)?;

    use DemoBlocks::*;
    let road_color: Block = Rgba::new(0.314, 0.306, 0.353, 1.0).into();
    let curb_color: Block = Rgba::new(0.788, 0.765, 0.741, 1.0).into();
    let road_noise_v = noise::Value::new().set_seed(0x51b19f6a);
    let road_noise = noise::ScaleBias::new(&road_noise_v)
        .set_bias(1.0)
        .set_scale(0.04);
    BlockProvider::<DemoBlocks>::new(|key| match key {
        Road => Block::builder()
            .display_name("Road")
            .voxels_fn(universe, resolution, |cube| {
                scale_color(road_color.clone(), road_noise.at_grid(cube), 0.02)
            })
            .unwrap()
            .build(),

        Lamp => Block::builder()
            .display_name("Lamp")
            .light_emission(Rgb::new(20.0, 20.0, 20.0))
            .voxels_fn(universe, resolution, |p| {
                if int_magnitude_squared(p - center_point) <= (resolution_g / 2).pow(2) + 3
                /* fudge */
                {
                    Rgba::WHITE.into()
                } else {
                    AIR.clone()
                }
            })
            .unwrap()
            .build(),

        Lamppost => Block::builder()
            .display_name("Lamppost")
            .light_emission(Rgb::new(3.0, 3.0, 3.0))
            .voxels_fn(universe, resolution, |p| {
                if int_magnitude_squared(
                    (p - center_point).mul_element_wise(GridVector::new(1, 0, 1)),
                ) < 2i32.pow(2)
                {
                    Rgba::BLACK.into()
                } else {
                    AIR.clone()
                }
            })
            .unwrap()
            .build(),

        Curb => Block::builder()
            .display_name("Curb")
            .collision(BlockCollision::None) // TODO: make solid when we have voxel-level collision
            .voxels_fn(universe, resolution, |cube| {
                let width = resolution_g / 3;
                if int_magnitude_squared(
                    (cube - GridPoint::new(width / 2 + 2, 0, 0))
                        .mul_element_wise(GridVector::new(1, 2, 0)),
                ) < width.pow(2)
                {
                    scale_color(curb_color.clone(), road_noise.at_grid(cube), 0.02)
                } else {
                    AIR
                }
            })
            .unwrap()
            .build(),
    })
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
