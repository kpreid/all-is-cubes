// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::borrow::Cow;

use cgmath::{ElementWise, EuclideanSpace as _, InnerSpace, Vector3};
use embedded_graphics::geometry::Point;
use embedded_graphics::prelude::{Drawable, PixelColor, Primitive, Size};
use embedded_graphics::primitives::{
    Circle, Line, PrimitiveStyle, PrimitiveStyleBuilder, Rectangle, RoundedRectangle,
    StrokeAlignment, StyledDrawable,
};

use crate::block::{Block, BlockCollision, Resolution, AIR, AIR_EVALUATED};
use crate::content::palette;
use crate::drawing::{DrawingPlane, VoxelBrush};
use crate::linking::{BlockModule, BlockProvider, InGenError};
use crate::math::{
    cube_to_midpoint, Face, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridVector, Rgb,
    Rgba,
};
use crate::space::{GridArray, Space, SpacePhysics};
use crate::universe::Universe;

#[cfg(doc)]
use crate::inv::Tool;

/// Blocks that are icons for tools or UI components.
///
/// TODO: Consider splitting the tools part from the UI part, especially as tool icons may
/// end up being game-specific?
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub enum Icons {
    /// HUD crosshair indicating cursor position.
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
    PauseButtonOn,
    PauseButtonOff,
    MouselookButtonOn,
    MouselookButtonOff,
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
                    // TODO: Replace this 2D scribble with a 3D hand shape
                    #[rustfmt::skip]
                    let image = GridArray::from_y_flipped_array([[
                        *b"                ",
                        *b"      #         ",
                        *b"     #.#        ",
                        *b"     #.#        ",
                        *b"     #.## # #   ",
                        *b"   # #.#.#.#.#  ",
                        *b"  #.##.#.#.#.#  ",
                        *b"  #.##.......#  ",
                        *b"  #..#......#   ",
                        *b"   #........#   ",
                        *b"    #......#    ",
                        *b"    #......#    ",
                        *b"    #......#    ",
                        *b"                ",
                    ]]);
                    Block::builder()
                        .display_name("Activate")
                        .voxels_fn(universe, 16, |p| {
                            Block::from(
                                match image
                                    .get(p - GridVector::new(0, 0, 8))
                                    .copied()
                                    .unwrap_or(b' ')
                                {
                                    b' ' => Rgba::TRANSPARENT,
                                    b'.' => Rgba::WHITE,
                                    b'#' => Rgba::BLACK,
                                    byte => panic!("unrecognized {:?}", byte as char),
                                },
                            )
                        })?
                        .build()
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
                        .display_name(if active {
                            "Jetpack (on)"
                        } else {
                            "Jetpack (off)"
                        })
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
                                cube_to_midpoint(p).map(|c| c - f64::from(resolution) / 2.0);
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

                b @ (Icons::PauseButtonOff | Icons::PauseButtonOn) => {
                    let active = b == Icons::PauseButtonOn;
                    let mut button_builder = ButtonBuilder::new(active)?;

                    // Draw pause symbol
                    for x in [-3, 2] {
                        Line::new(Point::new(x, -4), Point::new(x, 3)).draw_styled(
                            &PrimitiveStyle::with_stroke(button_builder.label_color, 3),
                            &mut button_builder.label_draw_target(),
                        )?;
                    }

                    button_builder.into_block(universe, "Pause")
                }

                b @ (Icons::MouselookButtonOff | Icons::MouselookButtonOn) => {
                    let active = b == Icons::MouselookButtonOn;
                    let mut button_builder = ButtonBuilder::new(active)?;

                    // Draw crosshair
                    // TODO: Suspicious inconsistency between x and y coordinates
                    let style = PrimitiveStyle::with_stroke(button_builder.label_color, 2);
                    Line::new(Point::new(-1, -4), Point::new(-1, 3))
                        .draw_styled(&style, &mut button_builder.label_draw_target())?;
                    Line::new(Point::new(-4, 0), Point::new(3, 0))
                        .draw_styled(&style, &mut button_builder.label_draw_target())?;

                    button_builder.into_block(universe, "Mouselook")
                }
            })
        })
        .unwrap()
    }
}

struct ButtonBuilder {
    space: Space,
    active: bool,
    label_z: GridCoordinate,
    label_color: Rgba,
}

impl ButtonBuilder {
    // TODO: We probably want a higher resolution, but it has to wait for texture alloc improvements
    pub const RESOLUTION: Resolution = 16;
    pub const RESOLUTION_G: GridCoordinate = Self::RESOLUTION as GridCoordinate;

    pub fn new(active: bool) -> Result<Self, InGenError> {
        let back_block = Block::from(if active {
            palette::BUTTON_ACTIVATED_BACK
        } else {
            palette::BUTTON_BACK
        });
        let cap_rim_block = Block::from(palette::BUTTON_BACK.map_rgb(|rgb| rgb * 1.1));

        let frame_brush = VoxelBrush::single(Block::from(palette::BUTTON_FRAME));
        let back_brush = VoxelBrush::new(vec![
            ([0, 0, 0], &back_block),
            ([0, 0, 1], &back_block),
            ([0, 0, 2], &back_block),
            ([0, 0, 3], &back_block),
            ([0, 0, 4], &back_block),
            ([0, 0, 5], &back_block),
            ([0, 0, 6], &back_block),
            ([0, 0, 7], &back_block),
            // up to but not including BUTTON_LABEL_Z
        ]);
        let cap_rim_brush = VoxelBrush::new(vec![([0, 0, 7], &cap_rim_block)]);

        let outer_rectangle = Rectangle::with_corners(
            Point::new(1, 1),
            Point::new(Self::RESOLUTION_G - 2, Self::RESOLUTION_G - 2),
        );
        let rr = |inset: i32| {
            RoundedRectangle::with_equal_corners(
                outer_rectangle.offset(-inset),
                Size::new(3 - inset as u32, 3 - inset as u32),
            )
        };

        let mut space = Space::for_block(Self::RESOLUTION).build_empty();
        let draw_target = &mut space.draw_target(
            GridMatrix::from_translation([0, Self::RESOLUTION_G - 1, 0]) * GridMatrix::FLIP_Y,
        );

        // unwrap()s because if this drawing fails, tests will catch that — no parameters
        rr(0)
            .into_styled(
                PrimitiveStyleBuilder::new()
                    .fill_color(&back_brush)
                    .stroke_color(&frame_brush)
                    .stroke_width(1)
                    .stroke_alignment(StrokeAlignment::Inside)
                    .build(),
            )
            .draw(draw_target)?;
        rr(1)
            .into_styled(
                PrimitiveStyleBuilder::new()
                    .stroke_color(&cap_rim_brush)
                    .stroke_width(1)
                    .stroke_alignment(StrokeAlignment::Inside)
                    .build(),
            )
            .draw(draw_target)?;

        Ok(ButtonBuilder {
            space,
            active,
            label_z: 8,
            label_color: if active {
                palette::BUTTON_ACTIVATED_LABEL
            } else {
                palette::BUTTON_LABEL
            },
        })
    }

    pub fn into_block(self, universe: &mut Universe, label: impl Into<Cow<'static, str>>) -> Block {
        Block::builder()
            .display_name(label)
            .light_emission(if self.active {
                palette::BUTTON_ACTIVATED_GLOW
            } else {
                Rgb::ZERO
            })
            .voxels_ref(Self::RESOLUTION, universe.insert_anonymous(self.space))
            .build()
    }

    /// Returns an [`embedded_graphics::DrawTarget`] for drawing the button label, with a
    /// Y-down coordinate system whose origin is centered on the button (or more precisely,
    /// (0, 0) is the lower-right pixel closest to the center, since e-g uses a convention
    /// where coordinates identify pixels, not their edges).
    pub fn label_draw_target<C: PixelColor>(&mut self) -> DrawingPlane<'_, C> {
        self.space.draw_target(
            GridMatrix::from_translation([
                Self::RESOLUTION_G / 2,
                Self::RESOLUTION_G / 2 - 1,
                self.label_z,
            ]) * GridMatrix::FLIP_Y,
        )
    }
}

#[test]
fn icons_smoke_test() {
    Icons::new(&mut Universe::new());
}
