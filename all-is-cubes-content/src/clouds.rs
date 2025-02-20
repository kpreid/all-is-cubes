//! Cloud generation.

use all_is_cubes::block::{AIR, Block, BlockCollision};
use all_is_cubes::math::{GridAab, GridCoordinate, GridPoint, Rgb, zo32};
use all_is_cubes::space::{SetCubeError, Space};

use crate::alg::NoiseFnExt as _;

/// Fill the [`AIR`] portions of `region` in `space` with clouds made up of various
/// transparent blocks.
///
/// `density` should be in the approximate range 0 to 1, where 0 or less is no clouds.
///
/// TODO: Use named block definitions or not?
pub fn clouds(region: GridAab, space: &mut Space, density: f32) -> Result<(), SetCubeError> {
    let large_noise =
        noise::ScaleBias::new(noise::ScalePoint::new(noise::Perlin::default()).set_scale(0.02))
            .set_scale(10.0);
    let small_noise =
        noise::ScaleBias::new(noise::ScalePoint::new(noise::Perlin::default()).set_scale(0.15))
            .set_scale(4.0);
    let combined_noise = noise::Add::new(large_noise, small_noise);

    fn cloud_block(alpha: f32) -> Block {
        Block::builder()
            .display_name("Cloud")
            .color(Rgb::ONE.with_alpha(zo32(alpha)))
            .collision(if alpha >= 1.0 {
                BlockCollision::Hard
            } else {
                BlockCollision::None
            })
            .build()
    }
    const LEVELS: usize = 4;
    let blocks: [Block; LEVELS] = [
        cloud_block(0.25),
        cloud_block(0.50),
        cloud_block(0.75),
        cloud_block(1.00),
    ];

    for x in region.x_range() {
        for z in region.z_range() {
            let noise_value = combined_noise.at_grid(GridPoint::new(x, 0, z));
            let surface = noise_value as GridCoordinate;
            for y in region.y_range() {
                let cube = GridPoint::new(x, y, z);
                if space[cube] != AIR {
                    continue;
                }
                // TODO: This math isn't really doing what we want -- isn't using the highest levels.
                let above_base = region.lower_bounds().y - y;
                let whatsit = surface - above_base;
                let alpha = (whatsit as f32 * density).clamp(0., 1.);
                if alpha <= 0. {
                    continue;
                }
                let block = &blocks[(alpha / (LEVELS as f32 + 0.99)) as usize];
                space.set(cube, block)?;
            }
        }
    }
    Ok(())
}
