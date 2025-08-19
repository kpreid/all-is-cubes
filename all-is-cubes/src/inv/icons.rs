use core::fmt;

use euclid::vec3;
use exhaust::Exhaust;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::block::{self, AIR, Block, Resolution::*};
use crate::content::load_image::{block_from_image, default_srgb, include_image};
use crate::drawing::VoxelBrush;
use crate::linking::{BlockModule, BlockProvider};
use crate::math::{FreeCoordinate, GridCoordinate, GridRotation, Rgba, rgb_const, rgba_const};
use crate::universe::{ReadTicket, UniverseTransaction};

#[cfg(doc)]
use crate::inv::Tool;
use crate::util::YieldProgress;

/// Blocks that are icons for [`Tool`]s.
///
/// TODO: Should this be considered strictly part of the UI/content and not fundamentals,
/// since it is making lots of aesthetic decisions?
/// If so, then [`Tool::icon()`] needs to go away, and the UI will need to either contain
/// these icons or accept them as configuration.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[non_exhaustive]
pub enum Icons {
    /// Icon for an empty toolbar slot.
    EmptySlot,
    /// Icon for [`Tool::Activate`],
    Activate,
    /// Icon for [`Tool::RemoveBlock`].
    Delete,
    /// Icon for [`Tool::CopyFromSpace`].
    CopyFromSpace,
    /// Icon for [`Tool::EditBlock`].
    EditBlock,
    /// Icon for [`Tool::PushPull`].
    PushPull,
    /// Icon for [`Tool::Jetpack`].
    Jetpack {
        /// Actually flying?
        active: bool,
    },
}

impl BlockModule for Icons {
    fn namespace() -> &'static str {
        "all-is-cubes/vui/icons"
    }
}

impl fmt::Display for Icons {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Icons::EmptySlot => write!(f, "empty-slot"),
            Icons::Activate => write!(f, "activate"),
            Icons::Delete => write!(f, "delete"),
            Icons::CopyFromSpace => write!(f, "copy-from-space"),
            Icons::EditBlock => write!(f, "edit-block"),
            Icons::PushPull => write!(f, "push"),
            Icons::Jetpack { active } => write!(f, "jetpack/{active}"),
        }
    }
}

impl Icons {
    /// Construct the standard icons, inserting block definitions into the given
    /// [`UniverseTransaction`].
    pub async fn new(txn: &mut UniverseTransaction, p: YieldProgress) -> BlockProvider<Icons> {
        let resolution = R16;

        BlockProvider::new(p, |key| {
            Ok(match key {
                Icons::EmptySlot => Block::builder()
                    .attributes(block::AIR_EVALUATED.attributes().clone())
                    .display_name("")
                    .color(Rgba::TRANSPARENT)
                    .build(),

                Icons::Activate => block_from_image(
                    ReadTicket::stub(),
                    include_image!("icons/hand.png"),
                    GridRotation::RXyZ,
                    &default_srgb,
                )?
                .display_name("Activate")
                .build_txn(txn),

                Icons::Delete => block_from_image(
                    ReadTicket::stub(),
                    include_image!("icons/placeholder-hammer.png"),
                    GridRotation::RXyZ,
                    &default_srgb,
                )?
                .display_name("Delete Block")
                .build_txn(txn),

                Icons::CopyFromSpace => Block::builder()
                    .display_name("Copy Block from Cursor")
                    // TODO: design actual icon
                    .color(Rgba::new(0., 1., 0., 1.))
                    .build(),

                Icons::EditBlock => Block::builder()
                    .display_name("Edit Block")
                    // TODO: design actual icon
                    .color(Rgba::new(0., 1., 0., 1.))
                    .build(),

                Icons::PushPull => {
                    let dots = [block::from_color!(Rgba::BLACK), AIR];
                    let dots = move |y: GridCoordinate| dots[y.rem_euclid(2) as usize].clone();
                    block_from_image(
                        ReadTicket::stub(),
                        include_image!("icons/push.png"),
                        GridRotation::RXZY,
                        &|color| {
                            // TODO: Figure out abstractions to not need so much fiddly custom code
                            let bcolor = Block::from(Rgba::from_srgb8(color));
                            match color {
                                [0, 0, 0, 255] => VoxelBrush::new(vec![([0, 15, 0], dots(0))]),
                                [0x85, 0x85, 0x85, 255] => {
                                    VoxelBrush::new(vec![([0, 0, 0], dots(0))])
                                }
                                [0, 127, 0, 255] => {
                                    VoxelBrush::new((0..16).map(|y| ([0, y, 0], dots(y))))
                                }
                                [0, 255, 0, 255] => {
                                    VoxelBrush::new((0..16).map(|y| ([0, y, 0], dots(y + 1))))
                                }
                                [255, 0, 0, 255] => {
                                    VoxelBrush::new((0..16).map(|y| ([0, y, 0], bcolor.clone())))
                                }
                                _ => VoxelBrush::new([([0, 0, 0], bcolor)]),
                            }
                            .translate([0, 8, 0])
                        },
                    )?
                    .display_name("Push/Pull")
                    .build_txn(txn)
                }

                Icons::Jetpack { active } => {
                    let shell_block = block::from_color!(0.5, 0.5, 0.5);
                    let stripe_block = block::from_color!(0.9, 0.1, 0.1);
                    let exhaust = if active {
                        Block::builder()
                            .color(rgba_const!(1.0, 1.0, 1.0, 0.1))
                            .light_emission(rgb_const!(1.0, 0.8, 0.8) * 16.0)
                            .build()
                    } else {
                        AIR
                    };
                    let active_color = if active {
                        block::from_color!(1.0, 1.0, 0.5, 1.)
                    } else {
                        block::from_color!(0.4, 0.4, 0.4, 1.)
                    };
                    let shape: [(FreeCoordinate, &Block); 16] = [
                        (4., &shell_block),
                        (6., &shell_block),
                        (6.5, &shell_block),
                        (7., &shell_block),
                        (7.25, &shell_block),
                        (5., &active_color),
                        (7.25, &shell_block),
                        (5., &active_color),
                        (7.25, &shell_block),
                        (6.5, &shell_block),
                        (6.0, &shell_block),
                        (5.5, &shell_block),
                        (5.0, &shell_block),
                        (4.5, &shell_block),
                        (4.5, &exhaust),
                        (4.5, &exhaust),
                    ];
                    Block::builder()
                        .display_name(if active {
                            "Jetpack (on)"
                        } else {
                            "Jetpack (off)"
                        })
                        .voxels_fn(resolution, |cube| {
                            let (shape_radius, block) =
                                shape[((GridCoordinate::from(resolution) - 1) - cube.y) as usize];
                            let centered_p = cube.center().map(|c| c - f64::from(resolution) / 2.0);
                            let r4 = centered_p
                                .to_vector()
                                .component_mul(vec3(1., 0., 1.))
                                .square_length()
                                .powi(2);
                            if r4 <= shape_radius.powi(4) {
                                if block == &shell_block
                                    && (centered_p.x.abs() <= 1.0 || centered_p.z.abs() <= 1.0)
                                {
                                    &stripe_block
                                } else {
                                    block
                                }
                            } else {
                                &AIR
                            }
                        })?
                        .build_txn(txn)
                }
            })
        })
        .await
        .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::yield_progress_for_testing;

    #[macro_rules_attribute::apply(smol_macros::test)]
    async fn icons_smoke_test() {
        Icons::new(
            &mut UniverseTransaction::default(),
            yield_progress_for_testing(),
        )
        .await;
    }
}
