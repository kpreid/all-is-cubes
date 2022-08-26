use std::borrow::Cow;
use std::fmt;

use embedded_graphics::geometry::Point;
use embedded_graphics::image::Image as EgImage;
use embedded_graphics::mono_font::{MonoFont, MonoTextStyle};
use embedded_graphics::prelude::{Dimensions, Drawable, PixelColor, Primitive, Size};
use embedded_graphics::primitives::{
    PrimitiveStyleBuilder, Rectangle, RoundedRectangle, StrokeAlignment,
};
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use exhaust::Exhaust;

use crate::block::{Block, Resolution, Resolution::*};
use crate::content::load_image::{default_srgb, include_image, space_from_image, ImageAdapter};
use crate::content::palette;
use crate::drawing::{DrawingPlane, VoxelBrush};
use crate::linking::{BlockModule, BlockProvider, InGenError};
use crate::math::{GridAab, GridCoordinate, GridMatrix, GridRotation, Rgb, Rgba};
use crate::space::{Space, SpacePhysics};
use crate::universe::Universe;

#[cfg(doc)]
use crate::inv::Tool;
use crate::util::YieldProgress;
use crate::vui::widgets::ToggleButtonVisualState;

/// Blocks that are used within the VUI, only.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Exhaust)]
#[doc(hidden)] // public for testing only
#[non_exhaustive]
pub enum UiBlocks {
    /// HUD crosshair indicating cursor position.
    Crosshair,

    // TODO: Should we do a `Button(ButtonLabel, ToggleButtonVisualState)` variant instead?
    PauseButton(ToggleButtonVisualState),
    MouselookButton(ToggleButtonVisualState),
    DebugInfoTextButton(ToggleButtonVisualState),
    DebugChunkBoxesButton(ToggleButtonVisualState),
    DebugCollisionBoxesButton(ToggleButtonVisualState),
    DebugLightRaysButton(ToggleButtonVisualState),
}

impl BlockModule for UiBlocks {
    fn namespace() -> &'static str {
        "all-is-cubes/vui/blocks"
    }
}

impl fmt::Display for UiBlocks {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UiBlocks::Crosshair => write!(f, "crosshair"),
            UiBlocks::PauseButton(state) => write!(f, "pause-button/{}", state),
            UiBlocks::MouselookButton(state) => write!(f, "mouselook-button/{}", state),
            UiBlocks::DebugInfoTextButton(state) => write!(f, "debug-info-text-button/{}", state),
            UiBlocks::DebugChunkBoxesButton(state) => {
                write!(f, "debug-chunk-boxes-button/{}", state)
            }
            UiBlocks::DebugCollisionBoxesButton(state) => {
                write!(f, "debug-collision-boxes-button/{}", state)
            }
            UiBlocks::DebugLightRaysButton(state) => write!(f, "debug-light-rays-button/{}", state),
        }
    }
}

impl UiBlocks {
    pub async fn new(universe: &mut Universe, p: YieldProgress) -> BlockProvider<UiBlocks> {
        BlockProvider::new(p, |key| {
            Ok(match key {
                UiBlocks::Crosshair => Block::builder()
                    .display_name("Crosshair")
                    .voxels_ref(
                        R64, // TODO: get resolution from image file
                        universe.insert_anonymous(space_from_image(
                            include_image!("icons/crosshair.png"),
                            GridRotation::RXyZ,
                            default_srgb,
                        )?),
                    )
                    .build(),

                UiBlocks::PauseButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder.draw_icon(include_image!("icons/button-pause.png"))?;
                    button_builder.into_block(universe, "Pause")
                }

                UiBlocks::MouselookButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder.draw_icon(include_image!("icons/button-mouselook.png"))?;
                    button_builder.into_block(universe, "Mouselook")
                }

                UiBlocks::DebugInfoTextButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder.draw_icon(include_image!("icons/button-debug-info-text.png"))?;
                    button_builder.into_block(universe, "Debug: Info Text")
                }

                UiBlocks::DebugChunkBoxesButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder
                        .draw_icon(include_image!("icons/button-debug-chunk-boxes.png"))?;
                    button_builder.into_block(universe, "Debug: Chunk Boxes")
                }

                UiBlocks::DebugCollisionBoxesButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder
                        .draw_icon(include_image!("icons/button-debug-collision-boxes.png"))?;
                    button_builder.into_block(universe, "Debug: Collision Boxes")
                }

                UiBlocks::DebugLightRaysButton(state) => {
                    let mut button_builder = ButtonBuilder::new(state)?;
                    button_builder
                        .draw_icon(include_image!("icons/button-debug-light-rays.png"))?;
                    button_builder.into_block(universe, "Debug: Light Rays at Cursor")
                }
            })
        })
        .await
        .unwrap()
    }
}

/// TODO: should this live in [`crate::vui::widgets`]?
struct ButtonBuilder {
    space: Space,
    active: bool,
    label_z: GridCoordinate,
    label_color: Rgba,
}

impl ButtonBuilder {
    // TODO: We probably want a higher resolution so that button icons can have more detail, but that will require updating all the uses
    pub const RESOLUTION: Resolution = R32;
    pub const RESOLUTION_G: GridCoordinate = Self::RESOLUTION.to_grid();

    pub fn new(state: ToggleButtonVisualState) -> Result<Self, InGenError> {
        let label_z = 12;
        let active = state.value;
        let back_block = Block::from(if active {
            palette::BUTTON_ACTIVATED_BACK
        } else {
            palette::BUTTON_BACK
        });
        let cap_rim_block = Block::from(palette::BUTTON_BACK.map_rgb(|rgb| rgb * 1.1));

        let frame_brush = VoxelBrush::single(Block::from(palette::BUTTON_FRAME));
        let back_brush = VoxelBrush::new((0..label_z).map(|z| ([0, 0, z], &back_block)));
        let cap_rim_brush = VoxelBrush::new([([0, 0, label_z - 1], &cap_rim_block)]);

        let outer_inset = 2;
        let outer_rectangle = Rectangle::with_corners(
            Point::new(outer_inset, outer_inset),
            Point::new(
                // - 1 because e-g rectangles are specified in terms of their outermost pixels
                Self::RESOLUTION_G - outer_inset - 1,
                Self::RESOLUTION_G - outer_inset - 1,
            ),
        );
        let rr = |inset: i32| {
            RoundedRectangle::with_equal_corners(
                outer_rectangle.offset(-inset),
                Size::new(5 - inset as u32, 5 - inset as u32),
            )
        };

        let mut space = Space::builder(GridAab::from_lower_size(
            [0, 0, 0],
            // this will need to be changed if we want to support thick labels
            [Self::RESOLUTION_G, Self::RESOLUTION_G, label_z + 1],
        ))
        .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
        .build();
        let draw_target = &mut space.draw_target(
            GridMatrix::from_translation([0, Self::RESOLUTION_G - 1, 0]) * GridMatrix::FLIP_Y,
        );

        // unwrap()s because if this drawing fails, tests will catch that — no parameters
        rr(0)
            .into_styled(
                PrimitiveStyleBuilder::new()
                    .fill_color(&back_brush)
                    .stroke_color(&frame_brush)
                    .stroke_width(2)
                    .stroke_alignment(StrokeAlignment::Inside)
                    .build(),
            )
            .draw(draw_target)?;
        rr(2)
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
            label_z,
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
    pub fn label_draw_target<C: PixelColor>(&mut self) -> DrawingPlane<'_, Space, C> {
        self.space.draw_target(
            GridMatrix::from_translation([
                Self::RESOLUTION_G / 2,
                Self::RESOLUTION_G / 2 - 1,
                self.label_z,
            ]) * GridMatrix::FLIP_Y,
        )
    }

    pub fn draw_icon(&mut self, icon: &image::DynamicImage) -> Result<(), InGenError> {
        let id = &ImageAdapter::adapt(icon, default_srgb);
        EgImage::new(&id, -id.bounding_box().center() - Point::new(1, 1))
            .draw(&mut self.label_draw_target())?;
        Ok(())
    }

    /// Draw a text label (only a few characters will fit).
    #[allow(unused)] // TODO: delete this if we continue to have only icon buttons
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
    fn blocks_smoke_test() {
        block_on(UiBlocks::new(&mut Universe::new(), YieldProgress::noop()));
    }
}
