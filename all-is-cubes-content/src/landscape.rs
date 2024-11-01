#![allow(
    clippy::module_name_repetitions,
    reason = "false positive; TODO: remove after Rust 1.84 is released"
)]

use alloc::boxed::Box;
use core::array;
use core::fmt;

use exhaust::Exhaust;
use rand::{Rng as _, SeedableRng as _};

use all_is_cubes::arcstr;
use all_is_cubes::block::{
    Block, BlockAttributes, BlockCollision, Primitive,
    Resolution::{self, R16},
    AIR,
};
use all_is_cubes::linking::{BlockModule, BlockProvider, DefaultProvision, GenError, InGenError};
use all_is_cubes::math::{zo32, Cube, FreeCoordinate, GridAab, GridCoordinate, GridVector, Rgb};
use all_is_cubes::space::Sky;
use all_is_cubes::space::{SetCubeError, Space};
use all_is_cubes::universe::UniverseTransaction;
use all_is_cubes::util::YieldProgress;

use crate::alg::{array_of_noise, scale_color, voronoi_pattern, NoiseFnExt};
use crate::{palette, tree};

/// Names for blocks assigned specific roles in generating outdoor landscapes.
///
/// TODO: This is probably too specific to be useful in the long term; call it a
/// placeholder.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
#[allow(missing_docs)]
pub enum LandscapeBlocks {
    Grass,
    GrassBlades {
        height: GrassHeight,
    },
    Dirt,
    Stone,
    /// Half a tree part; composite it with another one to make a log/branch.
    Log(tree::TreeGrowth),
    Leaves(tree::TreeGrowth),
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust, strum::IntoStaticStr)]
#[non_exhaustive]
#[allow(missing_docs)]
#[repr(u8)]
pub enum GrassHeight {
    H1 = 1,
    H2 = 2,
    H3 = 3,
    H4 = 4,
    H5 = 5,
    H6 = 6,
    H7 = 7,
    H8 = 8,
}
impl GrassHeight {
    fn from_int(i: u8) -> Option<GrassHeight> {
        match i {
            0 => None,
            1 => Some(Self::H1),
            2 => Some(Self::H2),
            3 => Some(Self::H3),
            4 => Some(Self::H4),
            5 => Some(Self::H5),
            6 => Some(Self::H6),
            7 => Some(Self::H7),
            _ => Some(Self::H8), // clamped high
        }
    }
}

impl fmt::Display for LandscapeBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LandscapeBlocks::Grass => write!(f, "grass"),
            &LandscapeBlocks::GrassBlades { height } => {
                write!(f, "grass-blades/{}", height as u8)
            }
            LandscapeBlocks::Dirt => write!(f, "dirt"),
            LandscapeBlocks::Stone => write!(f, "stone"),
            LandscapeBlocks::Log(growth) => write!(f, "log/{growth}"),
            LandscapeBlocks::Leaves(growth) => write!(f, "leaves/{growth}"),
        }
    }
}

impl BlockModule for LandscapeBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/landscape"
    }
}

/// Provides a bland instance of [`LandscapeBlocks`] with single color blocks.
impl DefaultProvision<Block> for LandscapeBlocks {
    fn module_default(self) -> Block {
        fn color_and_name(color: Rgb, name: &'static str) -> Block {
            Block::builder()
                .display_name(name)
                .color(color.with_alpha_one())
                .build()
        }

        fn blades() -> Block {
            Block::builder()
                .display_name("Grass Blades")
                .color(palette::GRASS.with_alpha(zo32(0.1)))
                .collision(BlockCollision::None)
                .build()
        }

        use LandscapeBlocks::*;
        match self {
            Grass => color_and_name(palette::GRASS, "Grass"),
            GrassBlades { height: _ } => blades(),
            Dirt => color_and_name(palette::DIRT, "Dirt"),
            Stone => color_and_name(palette::STONE, "Stone"),
            Log(g) => color_and_name(
                palette::TREE_BARK * (0.8 + (8. - g.radius() as f32) * 0.1),
                "Wood",
            ),

            Leaves(_g) => color_and_name(palette::TREE_LEAVES, "Leaves"),
        }
    }
}

/// Construct blocks for [`LandscapeBlocks`] with some detail and add block definitions to the universe.
///
/// This is an async function for the sake of cancellation and optional cooperative
/// multitasking. It may be blocked on from a synchronous context.
pub async fn install_landscape_blocks(
    txn: &mut UniverseTransaction,
    resolution: Resolution,
    progress: YieldProgress,
) -> Result<(), GenError> {
    use LandscapeBlocks::*;
    let colors = BlockProvider::<LandscapeBlocks>::default();
    let rng = &mut rand_xoshiro::Xoshiro256Plus::seed_from_u64(123890483921741);

    let mut grass_blade_atom = colors[GrassBlades {
        height: GrassHeight::H4,
    }]
    .clone();
    // TODO: easier way to do this?
    if let Primitive::Atom(atom) = grass_blade_atom.primitive_mut() {
        atom.color = atom.color.to_rgb().with_alpha_one();
        atom.collision = BlockCollision::None;
    }

    let blade_color_noise = {
        let blade_color_noise_v = noise::Value::new(0x2e240365);
        move |cube: Cube| blade_color_noise_v.at_grid(cube.lower_bounds()) * 0.12 + 1.0
    };
    let overhang_noise = array_of_noise(resolution, &noise::Value::new(0), |value| {
        value * 2.5 + f64::from(resolution) * 0.75
    });
    let blade_noise = array_of_noise(
        // TODO: not very well sized for the job — we just want a different chunk for each height
        resolution.double().unwrap(),
        &noise::ScalePoint::new(noise::OpenSimplex::new(0x7af8c181)).set_y_scale(0.1),
        |value| value * (f64::from(resolution) * 1.7) + (f64::from(resolution) * -0.4),
    );

    // boxed to avoid the async fn future being huge
    let stone_points: Box<[_; 240]> = Box::new(array::from_fn(|_| {
        (
            Cube::ORIGIN.aab().random_point(rng),
            scale_color(colors[Stone].clone(), rng.gen_range(0.9..1.1), 0.02),
        )
    }));
    let stone_pattern = voronoi_pattern(resolution, true, &*stone_points);

    // TODO: give dirt a palette of varying hue and saturation
    let dirt_points: Box<[_; 1024]> = Box::new(array::from_fn(|_| {
        (
            Cube::ORIGIN.aab().random_point(rng),
            scale_color(colors[Dirt].clone(), rng.gen_range(0.9..1.1), 0.02),
        )
    }));
    let dirt_pattern = voronoi_pattern(resolution, true, &*dirt_points);

    // TODO: needs a tiling and abruptly-changing pattern -- perhaps a coordinate-streched voronoi noise instead
    let bark_noise = {
        let noise = noise::ScalePoint::new(noise::Value::new(0x28711937)).set_y_scale(1. / 4.);
        move |cube: Cube| noise.at_grid(cube.lower_bounds()) * 0.4 + 0.7
    };

    let attributes_from = |block: &Block| -> Result<BlockAttributes, InGenError> {
        Ok(block
            .evaluate()
            .map_err(InGenError::other)?
            .attributes()
            .clone())
    };

    BlockProvider::<LandscapeBlocks>::new(progress, |key| {
        let grass_blades = |txn, height: GrassHeight| -> Result<Block, InGenError> {
            let height_index = height as GridCoordinate - 1;
            // give each grass variant a different portion of the noise array
            let noise_section = GridVector::new(
                height_index.rem_euclid(2),
                height_index.div_euclid(2).rem_euclid(2),
                height_index.div_euclid(4).rem_euclid(2),
            ) * GridCoordinate::from(resolution);

            // Increase the brightness of the blade color to compensate for the way the
            // voxel blade shape is darkened by its own opacity.
            // This is essentially “baked ambient occlusion” but backwards:
            // faking the highlights that the algorithm does not comprehend.
            // TODO: Ideally this would be handled by some kind of lighting hints instead.
            let ao_fudge = 1.0 + f64::from(height as u8) * 0.15;

            Ok(Block::builder()
                .attributes(attributes_from(&grass_blade_atom)?)
                .display_name(arcstr::format!("Grass Blades {}", height as u8))
                .voxels_fn(resolution, |cube| {
                    let mut cube_for_lookup = cube;
                    cube_for_lookup.y = 0;
                    cube_for_lookup += noise_section;
                    if f64::from(cube.y - height_index) < blade_noise[cube_for_lookup] {
                        scale_color(
                            grass_blade_atom.clone(),
                            blade_color_noise(cube) * ao_fudge,
                            0.02,
                        )
                    } else {
                        AIR
                    }
                })?
                .build_txn(txn))
        };

        Ok(match key {
            Stone => Block::builder()
                .attributes(attributes_from(&colors[Stone])?)
                .voxels_fn(resolution, &stone_pattern)?
                .build_txn(txn),

            Grass => Block::builder()
                .attributes(attributes_from(&colors[Grass])?)
                .voxels_fn(resolution, |cube| {
                    if f64::from(cube.y) >= overhang_noise[cube] {
                        scale_color(colors[Grass].clone(), blade_color_noise(cube), 0.02)
                    } else {
                        dirt_pattern(cube).clone()
                    }
                })?
                .build_txn(txn),

            GrassBlades { height } => grass_blades(txn, height)?,

            Dirt => Block::builder()
                .attributes(attributes_from(&colors[Dirt])?)
                .voxels_fn(resolution, &dirt_pattern)?
                .build_txn(txn),

            key @ Log(growth) => {
                let resolution = R16;
                let mid = GridCoordinate::from(resolution) / 2;
                let radius = growth as GridCoordinate;
                let trunk_box = GridAab::from_lower_upper(
                    [mid - radius, 0, mid - radius],
                    [mid + radius, mid + radius, mid + radius],
                );
                let color_block = &colors[key];
                Block::builder()
                    .attributes(attributes_from(color_block)?)
                    .voxels_fn(resolution, |cube| {
                        if trunk_box.contains_cube(cube) {
                            // TODO: separate bark from inner wood
                            scale_color(color_block.clone(), bark_noise(cube), 0.05)
                        } else {
                            AIR
                        }
                    })?
                    .build_txn(txn)
            }

            key @ Leaves(growth) => Block::builder()
                .attributes(attributes_from(&colors[key])?)
                .voxels_fn(resolution, |cube| {
                    // Distance this cube is from the center.
                    // TODO: This is the same computation as done by square_radius() but
                    // not with the same output. Can we share some logic there?
                    // Or add a helpful method on GridAab?
                    let radius_vec =
                        cube.map(|c| (c * 2 + 1 - GridCoordinate::from(resolution)).abs() / 2 + 1);
                    let radius = radius_vec
                        .x
                        .abs()
                        .max(radius_vec.y.abs())
                        .max(radius_vec.z.abs());

                    let signed_distance_from_edge = radius - growth.radius();
                    let unit_scale_distance =
                        f64::from(signed_distance_from_edge) / f64::from(growth.radius());

                    if unit_scale_distance <= 1.0
                        && !rng.gen_bool(
                            ((unit_scale_distance * 4.0).powi(2) / 2.0 + 0.5).clamp(0., 1.),
                        )
                    {
                        &colors[key]
                    } else {
                        &AIR
                    }
                })?
                .build_txn(txn),
        })
    })
    .await?
    .install(txn)?;
    Ok(())
}

/// Generate a landscape of grass-on-top-of-rock with some bumps to it.
/// Replaces all blocks in the specified region except for those intended to be “air”.
///
/// ```
/// use all_is_cubes::space::Space;
/// use all_is_cubes::linking::BlockProvider;
/// use all_is_cubes_content::{LandscapeBlocks, wavy_landscape};
///
/// let mut space = Space::empty_positive(10, 10, 10);
/// wavy_landscape(
///     space.bounds(),
///     &mut space,
///     &BlockProvider::<LandscapeBlocks>::default(),
///     1.0,
/// ).unwrap();
/// # // TODO: It didn't panic, but how about some assertions?
/// ```
pub fn wavy_landscape(
    region: GridAab,
    space: &mut Space,
    blocks: &BlockProvider<LandscapeBlocks>,
    max_slope: FreeCoordinate,
) -> Result<(), SetCubeError> {
    // TODO: justify this constant (came from cubes v1 code).
    let slope_scaled = max_slope / 0.904087;
    let middle_y = (region.lower_bounds().y + region.upper_bounds().y) / 2;

    let grass_at = grass_placement_function(0x21b5cc6b);

    for x in region.x_range() {
        for z in region.z_range() {
            let fx = FreeCoordinate::from(x);
            let fz = FreeCoordinate::from(z);
            let terrain_variation = slope_scaled
                * (((fx / 8.0).sin() + (fz / 8.0).sin()) * 1.0
                    + ((fx / 14.0).sin() + (fz / 14.0).sin()) * 3.0
                    + ((fx / 2.0).sin() + (fz / 2.0).sin()) * 0.6);
            let surface_y = middle_y + (terrain_variation as GridCoordinate);
            for y in region.y_range() {
                let altitude = y - surface_y;
                use LandscapeBlocks::*;
                let cube = Cube::new(x, y, z);
                let block: &Block = if altitude > 1 {
                    continue;
                } else if altitude == 1 {
                    if let Some(height) = grass_at(cube) {
                        // TODO: add randomized rotation like the city grass has
                        &blocks[GrassBlades { height }]
                    } else {
                        &AIR
                    }
                } else if altitude == 0 {
                    &blocks[Grass]
                } else if altitude == -1 {
                    &blocks[Dirt]
                } else {
                    &blocks[Stone]
                };
                space.set(cube, block)?;
                // TODO: Add various decorations on the ground. And trees.
            }
        }
    }
    Ok(())
}

pub(crate) fn grass_placement_function(seed: u32) -> impl Fn(Cube) -> Option<GrassHeight> {
    let grass_noise = noise::ScalePoint::new(
        noise::ScaleBias::new(noise::OpenSimplex::new(seed))
            .set_bias(1.0) // grass rather than nongrass
            .set_scale(15.0), // height variation
    )
    .set_scale(0.25);

    move |cube| GrassHeight::from_int(grass_noise.at_cube(cube) as u8)
}

/// Sky whose lower half pretends to be a grassy plane.
pub(crate) fn sky_with_grass(sky_color: Rgb) -> Sky {
    let ground = palette::GRASS.with_alpha_one().reflect(sky_color);
    Sky::Octants([
        ground, ground, sky_color, sky_color, //
        ground, ground, sky_color, sky_color, //
    ])
}
