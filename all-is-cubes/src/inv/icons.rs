use core::fmt;

use embedded_graphics::geometry::Point;
use embedded_graphics::prelude::{Drawable, Primitive};
use embedded_graphics::primitives::{Circle, Line, PrimitiveStyleBuilder};
use euclid::vec3;
use exhaust::Exhaust;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::block::{self, AIR, Block, Resolution::*};
use crate::content::load_image::{default_srgb, include_image, space_from_image};
use crate::drawing::VoxelBrush;
use crate::linking::{BlockModule, BlockProvider};
use crate::math::{
    Face6, FreeCoordinate, GridCoordinate, GridRotation, GridVector, Gridgid, Rgba, rgb_const,
    rgba_const,
};
use crate::space::Space;
use crate::universe::UniverseTransaction;

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

                Icons::Activate => Block::builder()
                    .display_name("Activate")
                    .voxels_handle(
                        R16, // TODO: get resolution from image file
                        txn.insert_anonymous(space_from_image(
                            include_image!("icons/hand.png"),
                            GridRotation::RXyZ,
                            &default_srgb,
                        )?),
                    )
                    .build(),

                Icons::Delete => {
                    let x_radius = i32::from(resolution) * 3 / 16;
                    let background_block_1: Block = Rgba::new(1.0, 0.05, 0.0, 1.0).into(); // TODO: Use palette colors
                    let background_block_2: Block = Rgba::new(0.8, 0.05, 0.0, 1.0).into(); // TODO: Use palette colors
                    let background_brush = VoxelBrush::new([
                        ([0, 0, 1], &background_block_1),
                        ([1, 0, 0], &background_block_2),
                        ([-1, 0, 0], &background_block_2),
                        ([0, 1, 0], &background_block_2),
                        ([0, -1, 0], &background_block_2),
                    ]);
                    let line_brush = VoxelBrush::single(block::from_color!(Rgba::BLACK))
                        .translate(GridVector::new(0, 0, 2));
                    let line_style = PrimitiveStyleBuilder::new()
                        .stroke_color(&line_brush)
                        .stroke_width(1)
                        .build();

                    let mut space = Space::for_block(resolution).build();
                    let display = &mut space.draw_target(Gridgid {
                        translation: GridVector::new(1, 1, 1)
                            * (GridCoordinate::from(resolution) / 2),
                        rotation: GridRotation::from_basis([Face6::PX, Face6::NY, Face6::PZ]),
                    });

                    // Draw X on circle
                    Circle::with_center(Point::new(0, 0), u32::from(resolution) - 4)
                        .into_styled(
                            PrimitiveStyleBuilder::new()
                                .fill_color(&background_brush)
                                .build(),
                        )
                        .draw(display)?;
                    Line::new(
                        Point::new(-x_radius, -x_radius),
                        Point::new(x_radius, x_radius),
                    )
                    .into_styled(line_style)
                    .draw(display)?;
                    Line::new(
                        Point::new(x_radius, -x_radius),
                        Point::new(-x_radius, x_radius),
                    )
                    .into_styled(line_style)
                    .draw(display)?;

                    Block::builder()
                        .display_name("Delete Block")
                        .voxels_handle(resolution, txn.insert_anonymous(space))
                        .build()
                }

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
                    Block::builder()
                        .display_name("Push/Pull")
                        .voxels_handle(
                            R32, // TODO: get resolution from image file,
                            txn.insert_anonymous(space_from_image(
                                include_image!("icons/push.png"),
                                GridRotation::RXZY,
                                &|color| {
                                    // TODO: Figure out abstractions to not need so much fiddly custom code
                                    let bcolor = Block::from(Rgba::from_srgb8(color));
                                    match color {
                                        [0, 0, 0, 255] => {
                                            VoxelBrush::new(vec![([0, 15, 0], dots(0))])
                                        }
                                        [0x85, 0x85, 0x85, 255] => {
                                            VoxelBrush::new(vec![([0, 0, 0], dots(0))])
                                        }
                                        [0, 127, 0, 255] => {
                                            VoxelBrush::new((0..16).map(|y| ([0, y, 0], dots(y))))
                                        }
                                        [0, 255, 0, 255] => VoxelBrush::new(
                                            (0..16).map(|y| ([0, y, 0], dots(y + 1))),
                                        ),
                                        [255, 0, 0, 255] => VoxelBrush::new(
                                            (0..16).map(|y| ([0, y, 0], bcolor.clone())),
                                        ),
                                        _ => VoxelBrush::new([([0, 0, 0], bcolor)]),
                                    }
                                    .translate([8, 8, 0])
                                },
                            )?),
                        )
                        .build()
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
                            let centered_p =
                                cube.midpoint().map(|c| c - f64::from(resolution) / 2.0);
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

    #[tokio::test]
    async fn icons_smoke_test() {
        Icons::new(
            &mut UniverseTransaction::default(),
            yield_progress_for_testing(),
        )
        .await;
    }
}
