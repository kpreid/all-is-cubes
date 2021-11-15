// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use cgmath::{ElementWise, EuclideanSpace as _, InnerSpace, Vector3};
use embedded_graphics::geometry::Point;
use embedded_graphics::prelude::{Drawable, Primitive};
use embedded_graphics::primitives::{Circle, Line, PrimitiveStyleBuilder};

use crate::block::{Block, BlockCollision, AIR, AIR_EVALUATED};
use crate::drawing::VoxelBrush;
use crate::linking::{BlockModule, BlockProvider};
use crate::math::{
    Face, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridVector, Rgb, Rgba,
};
use crate::space::{Space, SpacePhysics};
use crate::universe::Universe;

#[cfg(doc)]
use crate::inv::Tool;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub enum Icons {
    /// HUD crosshair indicating cursor position.
    ///
    /// TODO: This should be kept elsewhere than the icons enum.
    Crosshair,
    /// Icon for an empty toolbar slot.
    EmptySlot,
    /// Icon for [`Tool::Activate`],
    Activate,
    /// Icon for [`Tool::RemoveBlock`].
    Delete,
    /// Icon for [`Tool::CopyFromSpace`].
    CopyFromSpace,
    /// Icon for [`Tool::Jetpack`], active.
    JetpackOn,
    /// Icon for [`Tool::Jetpack`], inactive.
    JetpackOff,
}

impl BlockModule for Icons {
    fn namespace() -> &'static str {
        "all-is-cubes/vui/icons"
    }
}

impl Icons {
    pub fn new(universe: &mut Universe) -> BlockProvider<Icons> {
        let resolution = 16;
        let crosshair_resolution = 29; // Odd resolution allows centering

        BlockProvider::new(|key| {
            Ok(match key {
                Icons::Crosshair => {
                    let mut space = Space::empty_positive(
                        crosshair_resolution.into(),
                        crosshair_resolution.into(),
                        1,
                    );
                    space.set_physics(SpacePhysics::DEFAULT_FOR_BLOCK);
                    let center_x2 =
                        GridPoint::new(1, 1, 0) * (GridCoordinate::from(crosshair_resolution) - 1);

                    let line_block_1 = Block::from(rgba_const!(0.1, 0.1, 0.1, 1.0));
                    let line_block_2 = line_block_1.clone(); // TODO: experiment with patterning
                    for i in 5..8 {
                        for &direction in &[Face::PX, Face::PY, Face::NX, Face::NY] {
                            let position_x2 = center_x2 + i * direction.normal_vector() * 2;
                            space.set(
                                position_x2 / 2,
                                [&line_block_1, &line_block_2][i as usize % 2],
                            )?;
                        }
                    }
                    Block::builder()
                        .display_name("Crosshair")
                        .voxels_ref(crosshair_resolution, universe.insert_anonymous(space))
                        .build()
                }

                Icons::EmptySlot => Block::builder()
                    .attributes(AIR_EVALUATED.attributes)
                    .display_name("")
                    .color(Rgba::TRANSPARENT)
                    .build(),

                Icons::Activate => {
                    // TODO: This doesn't appear in the UI yet. When it does, give it an
                    // actual icon; perhaps the traditional "white gloved 👆 pointing finger" cursor.
                    Block::from(rgb_const!(0.0, 1.0, 0.0))
                }

                Icons::Delete => {
                    let x_radius = i32::from(resolution) * 3 / 16;
                    let background_block_1: Block = Rgba::new(1.0, 0.05, 0.0, 1.0).into(); // TODO: Use palette colors
                    let background_block_2: Block = Rgba::new(0.8, 0.05, 0.0, 1.0).into(); // TODO: Use palette colors
                    let background_brush = VoxelBrush::new(vec![
                        ((0, 0, 1), &background_block_1),
                        ((1, 0, 0), &background_block_2),
                        ((-1, 0, 0), &background_block_2),
                        ((0, 1, 0), &background_block_2),
                        ((0, -1, 0), &background_block_2),
                    ]);
                    let line_brush = VoxelBrush::single(Block::from(Rgba::BLACK))
                        .translate(GridVector::new(0, 0, 2));
                    let line_style = PrimitiveStyleBuilder::new()
                        .stroke_color(&line_brush)
                        .stroke_width(1)
                        .build();

                    let mut space = Space::for_block(resolution).build_empty();
                    let display = &mut space.draw_target(GridMatrix::from_origin(
                        GridPoint::new(1, 1, 1) * GridCoordinate::from(resolution / 2),
                        Face::PX,
                        Face::NY,
                        Face::PZ,
                    ));

                    // Draw X on circle
                    Circle::with_center(Point::new(0, 0), (resolution - 4).into())
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
                        .voxels_ref(resolution, universe.insert_anonymous(space))
                        .build()
                }

                Icons::CopyFromSpace => Block::builder()
                    .display_name("Copy Block from Cursor")
                    // TODO: design actual icon
                    .color(Rgba::new(0., 1., 0., 1.))
                    .build(),

                j @ (Icons::JetpackOff | Icons::JetpackOn) => {
                    let active = j == Icons::JetpackOn;
                    let shell_block = Block::from(rgb_const!(0.5, 0.5, 0.5));
                    let stripe_block = Block::from(rgb_const!(0.9, 0.1, 0.1));
                    let exhaust = if active {
                        Block::from(rgba_const!(1.0, 1.0, 1.0, 0.1))
                    } else {
                        AIR
                    };
                    let active_color = if active {
                        Block::from(Rgba::new(1.0, 1.0, 0.5, 1.))
                    } else {
                        Block::from(Rgba::new(0.4, 0.4, 0.4, 1.))
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
                        .display_name("Jetpack")
                        .collision(BlockCollision::Recur)
                        .light_emission(if active {
                            rgb_const!(1.0, 0.8, 0.8) * 0.5
                        } else {
                            Rgb::ZERO
                        })
                        .voxels_fn(universe, resolution, |p| {
                            let (shape_radius, block) =
                                shape[((GridCoordinate::from(resolution) - 1) - p.y) as usize];
                            let centered_p =
                                p.map(|c| f64::from(c) + 0.5 - f64::from(resolution) / 2.0);
                            let r4 = centered_p
                                .to_vec()
                                .mul_element_wise(Vector3::new(1., 0., 1.))
                                .magnitude2()
                                .powi(2);
                            if r4 <= shape_radius.powi(4) {
                                if block == &shell_block
                                    && (centered_p.x.abs() <= 1.0 || centered_p.z.abs() <= 1.0)
                                {
                                    &stripe_block
                                } else {
                                    &block
                                }
                            } else {
                                &AIR
                            }
                        })?
                        .build()
                }
            })
        })
        .unwrap()
    }
}
