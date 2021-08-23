// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::convert::TryInto;

use cgmath::Vector2;
use embedded_graphics::geometry::Point;
use embedded_graphics::prelude::{Primitive as _, Transform as _};
use embedded_graphics::primitives::{Circle, PrimitiveStyleBuilder, Triangle};
use embedded_graphics::Drawable as _;
use embedded_graphics::Pixel;

use crate::block::{space_to_blocks, Block, BlockAttributes, Resolution, AIR};
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::linking::BlockProvider;
use crate::math::{Face, GridCoordinate, GridMatrix, GridPoint, GridRotation, Rgba};
use crate::space::{Grid, Space, SpacePhysics};

use crate::universe::{URef, Universe};
use crate::vui::Icons;

pub(crate) use embedded_graphics::mono_font::iso_8859_1::FONT_8X13_BOLD as HudFont;

/// Knows where and how to place graphics within the HUD space, but does not store
/// the space or any related state itself; depends only on the screen size and other
/// parameters not primarily dependent on user interaction. This split is intended to
/// simplify the problem of adapting to size changes (though right now there is no
/// actual such handling).
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct HudLayout {
    size: Vector2<GridCoordinate>,
    pub(crate) toolbar_positions: usize,
}

// TODO: This will probably not make sense once we have aspect ratio adaptations
impl Default for HudLayout {
    fn default() -> Self {
        Self {
            // Odd width benefits the toolbar and crosshair.
            size: Vector2::new(25, 17),
            toolbar_positions: 10,
        }
    }
}

const TOOLBAR_STEP: GridCoordinate = 2;
impl HudLayout {
    pub(crate) fn grid(&self) -> Grid {
        Grid::from_lower_upper((0, 0, -5), (self.size.x, self.size.y, 5))
    }

    // TODO: taking the entire Universe doesn't seem like the best interface
    // but we want room to set up new blocks. Figure out a route for that.
    // TODO: validate this doesn't crash on wonky sizes.
    pub(crate) fn new_space(&self, universe: &mut Universe, hud_blocks: &HudBlocks) -> URef<Space> {
        let Vector2 { x: w, y: h } = self.size;
        let grid = self.grid();
        let mut space = Space::builder(grid)
            .physics(SpacePhysics {
                sky_color: palette::HUD_SKY,
                ..SpacePhysics::default()
            })
            .build_empty();

        if false {
            // Visualization of the bounds of the space we're drawing.
            let mut add_frame = |z, color| {
                let frame_block = Block::from(color);
                space
                    .fill_uniform(Grid::new((0, 0, z), (w, h, 1)), frame_block)
                    .unwrap();
                space
                    .fill_uniform(Grid::new((1, 1, z), (w - 2, h - 2, 1)), &AIR)
                    .unwrap();
            };
            add_frame(grid.lower_bounds().z, Rgba::new(0.5, 0., 0., 1.));
            add_frame(-1, Rgba::new(0.5, 0.5, 0.5, 1.));
            add_frame(grid.upper_bounds().z - 1, Rgba::new(0., 1., 1., 1.));
        }

        // Draw background for toolbar.
        let toolbar_disp = &mut space.draw_target(GridMatrix::from_origin(
            self.tool_icon_position(0),
            Face::PX,
            Face::NY,
            Face::PZ,
        ));
        Pixel(Point::new(-1, 0), &hud_blocks.toolbar_left_cap)
            .draw(toolbar_disp)
            .unwrap();
        Pixel(
            Point::new((self.toolbar_positions as i32 - 1) * TOOLBAR_STEP + 1, 0),
            &hud_blocks.toolbar_right_cap,
        )
        .draw(toolbar_disp)
        .unwrap();
        for index in 0..self.toolbar_positions {
            let x = index as i32 * TOOLBAR_STEP;
            Pixel(Point::new(x, 0), &hud_blocks.toolbar_middle)
                .draw(toolbar_disp)
                .unwrap();
            if index > 0 {
                Pixel(Point::new(x - 1, 0), &hud_blocks.toolbar_divider)
                    .draw(toolbar_disp)
                    .unwrap();
            }
        }

        space.fast_evaluate_light();

        universe.insert_anonymous(space)
    }

    // TODO: Make a block-setting function instead
    pub(crate) fn crosshair_position(&self) -> GridPoint {
        GridPoint::new(self.size.x / 2, self.size.y / 2, 0)
    }

    pub(crate) fn tool_icon_position(&self, index: usize) -> GridPoint {
        let x_start =
            (self.size.x - (self.toolbar_positions as GridCoordinate) * TOOLBAR_STEP + 1) / 2;
        // TODO: set depth sensibly
        GridPoint::new(x_start + (index as GridCoordinate) * TOOLBAR_STEP, 1, 1)
    }

    pub(super) fn toolbar_text_frame(&self) -> Grid {
        Grid::new((0, 3, 0), (self.size.x, 1, 1))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct HudBlocks {
    pub(crate) icons: BlockProvider<Icons>,
    pub(crate) text: VoxelBrush<'static>,
    toolbar_left_cap: VoxelBrush<'static>,
    toolbar_right_cap: VoxelBrush<'static>,
    toolbar_divider: VoxelBrush<'static>,
    toolbar_middle: VoxelBrush<'static>,
    /// Index is a bitmask of "selected_slots[i] == this slot"
    pub(crate) toolbar_pointer: [VoxelBrush<'static>; 4],
}

impl HudBlocks {
    pub(crate) fn new(universe: &mut Universe, resolution: Resolution) -> Self {
        let resolution_g = GridCoordinate::from(resolution);

        let icons = Icons::new(universe).install(universe).unwrap();

        let text_brush = VoxelBrush::new::<_, Block>(vec![
            ([0, 0, 1], palette::HUD_TEXT_FILL.into()),
            ([1, 0, 0], palette::HUD_TEXT_STROKE.into()),
            ([-1, 0, 0], palette::HUD_TEXT_STROKE.into()),
            ([0, 1, 0], palette::HUD_TEXT_STROKE.into()),
            ([0, -1, 0], palette::HUD_TEXT_STROKE.into()),
        ]);

        // TODO: This toolbar graphic is a "get the bugs in the drawing tools worked out"
        // placeholder for better art...
        // The frame is drawn multiple times, with different copies having different status
        // indicators present, so that we can have the graphics not rigidly aligned to blocks.
        let frame_count = 4;
        let frame_spacing_blocks = 2;

        let toolbar_frame_block_grid =
            Grid::new([-1, -1, -1], [1 + frame_count * frame_spacing_blocks, 3, 3]);
        let toolbar_frame_voxel_grid = toolbar_frame_block_grid.multiply(resolution_g);
        let mut toolbar_drawing_space = Space::builder(toolbar_frame_voxel_grid)
            .physics(SpacePhysics::DEFAULT_FOR_BLOCK)
            .build_empty();

        // Draw background for icons to “rest on”
        let horizontal_drawing = &mut toolbar_drawing_space.draw_target(
            GridMatrix::from_translation([0, -2, 0]) * GridRotation::RXZY.to_rotation_matrix(),
        );
        let padding = 3;
        let stroke_width = 1;
        let background_fill = VoxelBrush::single(palette::HUD_TOOLBAR_BACK);
        let background_stroke = VoxelBrush::single(palette::HUD_TOOLBAR_FRAME);
        let icon_background_rectangle = Circle::new(
            // TODO: confirm these offsets are exactly right
            Point::new(-padding, -padding),
            (resolution_g + padding * 2) as u32,
        )
        .into_styled(
            PrimitiveStyleBuilder::new()
                .fill_color(&background_fill)
                .stroke_color(&background_stroke)
                .stroke_width(stroke_width as u32)
                .build(),
        );
        for i in 0..frame_count {
            icon_background_rectangle
                .translate(Point::new(resolution_g * frame_spacing_blocks * i, 0))
                .draw(horizontal_drawing)
                .unwrap();
        }

        // Draw pointers. The pointers are placed above the frames in a (none, 0, 1, 0&1) bitmask pattern.
        // TODO: Remove Y flip
        let vertical_drawing = &mut toolbar_drawing_space.draw_target(GridMatrix::FLIP_Y);
        let pointer_offset = Point::new(resolution_g / 2, -resolution_g);
        let pointer_z = resolution_g / 2;
        // TODO: use different related colors
        let pointer_fill =
            VoxelBrush::single(palette::HUD_TOOLBAR_BACK).translate((0, 0, pointer_z));
        let pointer_stroke =
            VoxelBrush::single(palette::HUD_TOOLBAR_FRAME).translate((0, 0, pointer_z));
        let pointer_style = PrimitiveStyleBuilder::new()
            .fill_color(&pointer_fill)
            .stroke_color(&pointer_stroke)
            .stroke_width(stroke_width as u32)
            .build();
        for i in 0..frame_count {
            let translation =
                pointer_offset + Point::new(resolution_g * frame_spacing_blocks * i, 0);
            // TODO: Replace these triangles with maybe mouse button icons?
            if i & 1 != 0 {
                // Selection 0/left-click icon.
                Triangle::new(Point::new(-5, -5), Point::new(0, 0), Point::new(0, -5))
                    .into_styled(pointer_style)
                    .translate(translation)
                    .draw(vertical_drawing)
                    .unwrap();
            }
            if i & 2 != 0 {
                // Selection 1/right-click icon.
                Triangle::new(Point::new(0, -5), Point::new(0, 0), Point::new(5, -5))
                    .into_styled(pointer_style)
                    .translate(translation)
                    .draw(vertical_drawing)
                    .unwrap();
            }
        }

        // TODO: use a name for the space
        let toolbar_blocks_space = space_to_blocks(
            resolution,
            BlockAttributes::default(),
            universe.insert_anonymous(toolbar_drawing_space),
        )
        .unwrap();

        // TODO: Make this a feature of VoxelBrush?
        let slice_drawing = |points: Grid| {
            VoxelBrush::new(
                points
                    .interior_iter()
                    .map(|p| (p, toolbar_blocks_space[p].clone()))
                    .collect(),
            )
        };

        Self {
            icons,
            text: text_brush,
            toolbar_middle: slice_drawing(Grid::from_lower_upper((0, -1, -1), (1, 2, 2))),
            toolbar_divider: slice_drawing(Grid::from_lower_upper((1, -1, -1), (2, 2, 2)))
                .translate((-1, 0, 0)),
            toolbar_left_cap: slice_drawing(Grid::from_lower_upper((-1, -1, -1), (0, 2, 2)))
                .translate((1, 0, 0)),
            // Right cap comes from the right end of the frames
            toolbar_right_cap: slice_drawing(
                Grid::from_lower_upper((1, -1, -1), (2, 2, 2)).translate([
                    frame_spacing_blocks * (frame_count - 1),
                    0,
                    0,
                ]),
            )
            .translate([-(frame_spacing_blocks * (frame_count - 1) + 1), 0, 0]),
            toolbar_pointer: {
                (0..frame_count)
                    .map(|i| {
                        slice_drawing(Grid::new([i * frame_spacing_blocks, 1, -1], [1, 1, 3]))
                            .translate([-(i * frame_spacing_blocks), 0, 0])
                    })
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap()
            },
        }
    }
}
