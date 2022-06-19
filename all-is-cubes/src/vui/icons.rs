// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::borrow::Cow;
use std::fmt;

use cgmath::{ElementWise, EuclideanSpace as _, InnerSpace, Vector3};
use embedded_graphics::geometry::Point;
use embedded_graphics::mono_font::iso_8859_1::{FONT_5X8, FONT_6X12};
use embedded_graphics::mono_font::{MonoFont, MonoTextStyle};
use embedded_graphics::prelude::{Drawable, PixelColor, Primitive, Size};
use embedded_graphics::primitives::{
    Circle, Line, PrimitiveStyle, PrimitiveStyleBuilder, Rectangle, RoundedRectangle,
    StrokeAlignment, StyledDrawable,
};
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use exhaust::Exhaust;

use crate::block::{Block, BlockCollision, Resolution, AIR, AIR_EVALUATED};
use crate::content::load_image::{default_srgb, include_image, space_from_image};
use crate::content::palette;
use crate::drawing::{DrawingPlane, VoxelBrush};
use crate::linking::{BlockModule, BlockProvider, InGenError};
use crate::math::{
    cube_to_midpoint, Face7, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridRotation,
    GridVector, Rgb, Rgba,
};
use crate::space::{Space, SpacePhysics};
use crate::universe::Universe;

#[cfg(doc)]
use crate::inv::Tool;
use crate::util::YieldProgress;
use crate::vui::widgets::ToggleButtonVisualState;

/// Blocks that are icons for tools or UI components.
///
/// TODO: Consider splitting the tools part from the UI part, especially as tool icons may
/// end up being game-specific?
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
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
    /// Icon for [`Tool::EditBlock`].
    EditBlock,
    /// Icon for [`Tool::PushPull`].
    PushPull,
    /// Icon for [`Tool::Jetpack`].
    Jetpack {
        active: bool,
    },
    // TODO: Should we do a `Button(ButtonLabel, ToggleButtonVisualState)` variant instead?
    // Probably this whole business of Icons being responsible for this job should go away.
    PauseButton(ToggleButtonVisualState),
    MouselookButton(ToggleButtonVisualState),
    DebugInfoTextButton(ToggleButtonVisualState),
    DebugChunkBoxesButton(ToggleButtonVisualState),
    DebugCollisionBoxesButton(ToggleButtonVisualState),
    DebugLightRaysButton(ToggleButtonVisualState),
}

impl BlockModule for Icons {
    fn namespace() -> &'static str {
        "all-is-cubes/vui/icons"
    }
}

impl fmt::Display for Icons {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Icons::Crosshair => write!(f, "crosshair"),
            Icons::EmptySlot => write!(f, "empty-slot"),
            Icons::Activate => write!(f, "activate"),
            Icons::Delete => write!(f, "delete"),
            Icons::CopyFromSpace => write!(f, "copy-from-space"),
            Icons::EditBlock => write!(f, "edit-block"),
            Icons::PushPull => write!(f, "push"),
            Icons::Jetpack { active } => write!(f, "jetpack/{}", active),
            Icons::PauseButton(state) => write!(f, "pause-button/{}", state),
            Icons::MouselookButton(state) => write!(f, "mouselook-button/{}", state),
            Icons::DebugInfoTextButton(state) => write!(f, "debug-info-text-button/{}", state),
            Icons::DebugChunkBoxesButton(state) => write!(f, "debug-chunk-boxes-button/{}", state),
            Icons::DebugCollisionBoxesButton(state) => {
                write!(f, "debug-collision-boxes-button/{}", state)
            }
            Icons::DebugLightRaysButton(state) => write!(f, "debug-light-rays-button/{}", state),
        }
    }
}

impl Icons {
    pub async fn new(universe: &mut Universe, p: YieldProgress) -> BlockProvider<Icons> {
        let resolution = 16;
        let crosshair_resolution = 29; // Odd resolution allows centering

        BlockProvider::new(p, |key| {
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
                        for &direction in &[Face7::PX, Face7::PY, Face7::NX, Face7::NY] {
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

                Icons::Activate => Block::builder()
                    .display_name("Activate")
                    .voxels_ref(
                        16, // TODO: get resolution from image file
                        universe.insert_anonymous(space_from_image(
                            include_image!("icons/hand.png"),
                            GridRotation::RXyZ,
                            default_srgb,
                        )?),
                    )
                    .build(),

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
                        Face7::PX,
                        Face7::NY,
                        Face7::PZ,
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

                Icons::EditBlock => Block::builder()
                    .display_name("Edit Block")
                    // TODO: design actual icon
                    .color(Rgba::new(0., 1., 0., 1.))
                    .build(),

                Icons::PushPull => {
                    let dots = [Block::from(Rgba::BLACK), AIR];
                    let dots = move |y: GridCoordinate| dots[(y % 2) as usize].clone();
                    Block::builder()
                        .display_name("Push/Pull")
                        .voxels_ref(
                            32, // TODO: get resolution from image file,
                            universe.insert_anonymous(space_from_image(
                                include_image!("icons/push.png"),
                                GridRotation::RXZY,
                                |color| {
                                    // TODO: Figure out abstractions to not need so much fiddly custom code
                                    let bcolor = Block::from(Rgba::from_srgb8(color.0));
                                    match color.0 {
                                        [0, 0, 0, 255] => {
                                            VoxelBrush::new(vec![([0, 15, 0], dots(0))])
                                        }
                                        [0x85, 0x85, 0x85, 255] => {
                                            VoxelBrush::new(vec![([0, 0, 0], dots(0))])
                                        }
                                        [0, 127, 0, 255] => VoxelBrush::new(
                                            (0..16)
                                                .into_iter()
                                                .map(|y| ([0, y, 0], dots(y)))
                                                .collect(),
                                        ),
                                        [0, 255, 0, 255] => VoxelBrush::new(
                                            (0..16)
                                                .into_iter()
                                                .map(|y| ([0, y, 0], dots(y + 1)))
                                                .collect(),
                                        ),
                                        [255, 0, 0, 255] => VoxelBrush::new(
                                            (0..16)
                                                .into_iter()
                                                .map(|y| ([0, y, 0], bcolor.clone()))
                                                .collect(),
                                        ),
                                        _ => VoxelBrush::new(vec![([0, 0, 0], bcolor)]),
                                    }
                                    .translate([8, 8, 0])
                                },
                            )?),
                        )
                        .build()
                }

                Icons::Jetpack { active } => {
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
                                    block
                                }
                            } else {
                                &AIR
                            }
                        })?
                        .build()
                }

                Icons::PauseButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;

                    // Draw pause symbol
                    for x in [-3, 2] {
                        Line::new(Point::new(x, -4), Point::new(x, 3)).draw_styled(
                            &PrimitiveStyle::with_stroke(button_builder.label_color, 3),
                            &mut button_builder.label_draw_target(),
                        )?;
                    }

                    button_builder.into_block(universe, "Pause")
                }

                Icons::MouselookButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;

                    // Draw crosshair
                    // TODO: Suspicious inconsistency between x and y coordinates
                    let style = PrimitiveStyle::with_stroke(button_builder.label_color, 2);
                    Line::new(Point::new(-1, -4), Point::new(-1, 3))
                        .draw_styled(&style, &mut button_builder.label_draw_target())?;
                    Line::new(Point::new(-4, 0), Point::new(3, 0))
                        .draw_styled(&style, &mut button_builder.label_draw_target())?;

                    button_builder.into_block(universe, "Mouselook")
                }

                Icons::DebugInfoTextButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder.draw_text(&FONT_6X12, "i")?;
                    button_builder.into_block(universe, "Debug: Info Text")
                }

                Icons::DebugChunkBoxesButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder.draw_text(&FONT_5X8, "Ch")?;
                    button_builder.into_block(universe, "Debug: Chunk Boxes")
                }

                Icons::DebugCollisionBoxesButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder.draw_text(&FONT_5X8, "Co")?;
                    button_builder.into_block(universe, "Debug: Collision Boxes")
                }

                Icons::DebugLightRaysButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder.draw_text(&FONT_5X8, "Li")?;
                    button_builder.into_block(universe, "Debug: Light Rays at Cursor")
                }
            })
        })
        .await
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

    pub fn new(state: ToggleButtonVisualState) -> Result<Self, InGenError> {
        let active = state.value;
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

    // Draw a text label (only one or two characters will fit).
    pub fn draw_text(&mut self, font: &MonoFont<'_>, text: &str) -> Result<(), InGenError> {
        Text::with_text_style(
            text,
            Point::new(-1, -1),
            MonoTextStyle::new(font, self.label_color),
            TextStyleBuilder::new()
                .baseline(Baseline::Middle)
                .alignment(Alignment::Center)
                .build(),
        )
        .draw(&mut self.label_draw_target())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_executor::block_on;
    #[test]
    fn icons_smoke_test() {
        block_on(Icons::new(&mut Universe::new(), YieldProgress::noop()));
    }
}
