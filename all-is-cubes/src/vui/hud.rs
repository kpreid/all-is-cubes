// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use cgmath::{EuclideanSpace as _, Vector2};
use embedded_graphics::geometry::Point;
use embedded_graphics::mono_font::MonoTextStyle;
use embedded_graphics::prelude::{Primitive as _, Transform as _};
use embedded_graphics::primitives::{PrimitiveStyleBuilder, Rectangle, Triangle};
use embedded_graphics::text::{Alignment, Baseline, Text, TextStyleBuilder};
use embedded_graphics::Drawable as _;
use embedded_graphics::Pixel;

use crate::block::{space_to_blocks, Block, BlockAttributes, Resolution, AIR};
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::linking::BlockProvider;
use crate::math::{Face, GridCoordinate, GridMatrix, GridPoint, GridVector, Rgba};
use crate::space::{Grid, SetCubeError, Space, SpacePhysics};
use crate::tools::Tool;
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
    toolbar_positions: usize,
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

    fn text_resolution(&self) -> Resolution {
        16
    }

    // TODO: taking the entire Universe doesn't seem like the best interface
    // but we want room to set up new blocks. Figure out a route for that.
    // TODO: validate this doesn't crash on wonky sizes.
    pub(crate) fn new_space(&self, universe: &mut Universe, hud_blocks: &HudBlocks) -> URef<Space> {
        let Vector2 { x: w, y: h } = self.size;
        let grid = self.grid();
        let mut space = Space::empty(grid);

        space.set_physics(SpacePhysics {
            sky_color: palette::HUD_SKY,
            ..SpacePhysics::default()
        });

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

        // Set up toolbar info text space
        let frame = self.toolbar_text_frame();
        let mut toolbar_text_space = Space::empty(Grid::new(
            GridPoint::origin(),
            GridVector::new(
                frame.size().x * GridCoordinate::from(self.text_resolution()),
                frame.size().y * GridCoordinate::from(self.text_resolution()),
                2,
            ),
        ));
        toolbar_text_space.set_physics(SpacePhysics::DEFAULT_FOR_BLOCK);
        let toolbar_text_blocks = space_to_blocks(
            self.text_resolution(),
            BlockAttributes::default(),
            universe.insert_anonymous(toolbar_text_space),
        )
        .unwrap();
        debug_assert_eq!(toolbar_text_blocks.grid().size(), frame.size());
        space
            .fill(frame, |p| {
                Some(&toolbar_text_blocks[p - frame.lower_bounds().to_vec()])
            })
            .unwrap();

        space.fast_evaluate_light();

        universe.insert_anonymous(space)
    }

    // TODO: Make a block-setting function instead
    pub(crate) fn crosshair_position(&self) -> GridPoint {
        GridPoint::new(self.size.x / 2, self.size.y / 2, 0)
    }

    fn tool_icon_position(&self, index: usize) -> GridPoint {
        let x_start =
            (self.size.x - (self.toolbar_positions as GridCoordinate) * TOOLBAR_STEP + 1) / 2;
        // TODO: set depth sensibly
        GridPoint::new(x_start + (index as GridCoordinate) * TOOLBAR_STEP, 1, 1)
    }

    fn toolbar_text_frame(&self) -> Grid {
        Grid::new((0, 3, 0), (self.size.x, 1, 1))
    }

    /// Repaint the toolbar with a new set of tools and selected tools.
    ///
    /// Returns an error if using the tools' icons produced an error — or possibly if
    /// there was a drawing layout problem.
    // TODO: Error return should probably be something other than SetCubeError
    pub(crate) fn set_toolbar(
        &self,
        space: &mut Space,
        hud_blocks: &HudBlocks,
        tools: &[Tool],
        selections: &[usize],
    ) -> Result<(), SetCubeError> {
        for (index, tool) in tools.iter().enumerate() {
            if index >= self.toolbar_positions {
                break;
            }

            let position = self.tool_icon_position(index);
            // Draw icon
            space.set(position, &*tool.icon(&hud_blocks.icons))?;
            // Draw pointers.
            // TODO: refactor to not use FLIP_Y now that it isn't a hardcoded feature
            let toolbar_disp = &mut space
                .draw_target(GridMatrix::from_translation(position.to_vec()) * GridMatrix::FLIP_Y);
            for sel in 0..2 {
                let slot = selections.get(sel).copied().unwrap_or(usize::MAX);
                let brush: &VoxelBrush<'_> =
                    &hud_blocks.toolbar_pointer[sel][usize::from(slot == index)];
                Pixel(Point::new(0, 0), brush).draw(toolbar_disp)?;
            }
        }

        Ok(())
    }

    pub(crate) fn set_tooltip_text(
        &self,
        space: &mut Space,
        hud_blocks: &HudBlocks,
        text: &str,
    ) -> Result<(), SetCubeError> {
        let mut toolbar_text_space =
            if let Block::Recur { space, .. } = &space[self.toolbar_text_frame().lower_bounds()] {
                space.borrow_mut()
            } else {
                panic!("failed to retrieve toolbar space")
            };
        let grid = toolbar_text_space.grid();
        toolbar_text_space.fill_uniform(grid, &AIR).unwrap();

        // Note on dimensions: HudFont is currently 13 pixels tall, and we're using
        // the standard 16-voxel space resolution, and hud_blocks.text has a 1-pixel stroke,
        // so we have 16 - (13 + 2) = 1 voxel of free alignment, which I've chosen to put on
        // the top edge.
        let text_obj = Text::with_text_style(
            text,
            Point::new(grid.size().x / 2, -1),
            MonoTextStyle::new(&HudFont, &hud_blocks.text),
            TextStyleBuilder::new()
                .baseline(Baseline::Bottom)
                .alignment(Alignment::Center)
                .build(),
        );
        text_obj.draw(&mut toolbar_text_space.draw_target(GridMatrix::FLIP_Y))?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct HudBlocks {
    pub(crate) icons: BlockProvider<Icons>,
    text: VoxelBrush<'static>,
    toolbar_left_cap: VoxelBrush<'static>,
    toolbar_right_cap: VoxelBrush<'static>,
    toolbar_divider: VoxelBrush<'static>,
    toolbar_middle: VoxelBrush<'static>,
    /// Outer index is "which pointer", inner index is "shown or hidden".
    toolbar_pointer: [[VoxelBrush<'static>; 2]; 2],
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

        // Draw toolbar icon frame, twice so we can have start-middle-end shapes.
        let toolbar_frame_block_grid = Grid::new((-1, -1, -1), (5, 3, 3));
        let toolbar_frame_voxel_grid = toolbar_frame_block_grid.multiply(resolution_g);
        let mut toolbar_drawing_space = Space::empty(toolbar_frame_voxel_grid);
        toolbar_drawing_space.set_physics(SpacePhysics::DEFAULT_FOR_BLOCK);
        // TODO: remove y flip
        let display = &mut toolbar_drawing_space.draw_target(GridMatrix::FLIP_Y);

        let padding = 3;
        let stroke_width = 1;
        let background_fill = VoxelBrush::single(palette::HUD_TOOLBAR_BACK).translate((0, 0, -1));
        let background_stroke = VoxelBrush::single(palette::HUD_TOOLBAR_FRAME);
        let icon_background_rectangle = Rectangle::with_corners(
            // TODO: confirm these offsets are exactly right
            Point::new(-padding - stroke_width, -padding - resolution_g),
            Point::new(resolution_g + padding, padding + stroke_width),
        )
        .into_styled(
            PrimitiveStyleBuilder::new()
                .fill_color(&background_fill)
                .stroke_color(&background_stroke)
                .stroke_width(stroke_width as u32)
                .build(),
        );
        icon_background_rectangle.draw(display).unwrap();
        icon_background_rectangle
            .translate(Point::new(resolution_g * 2, 0))
            .draw(display)
            .unwrap();

        // Draw pointers. The pointers are placed above and below the second
        // icon frame.
        let pointer_offset = Point::new(resolution_g * 5 / 2, 0);
        // TODO: use different related colors
        let pointer_fill =
            VoxelBrush::single(palette::HUD_TOOLBAR_BACK).translate((0, 0, resolution_g - 1));
        let pointer_stroke =
            VoxelBrush::single(palette::HUD_TOOLBAR_FRAME).translate((0, 0, resolution_g - 1));
        let pointer_style = PrimitiveStyleBuilder::new()
            .fill_color(&pointer_fill)
            .stroke_color(&pointer_stroke)
            .stroke_width(stroke_width as u32)
            .build();
        Triangle::new(Point::new(-5, 5), Point::new(5, 5), Point::new(0, 0))
            .into_styled(pointer_style)
            .translate(pointer_offset)
            .draw(display)
            .unwrap();
        Triangle::new(Point::new(-5, -5), Point::new(0, 0), Point::new(5, -5))
            .into_styled(pointer_style)
            .translate(Point::new(0, -resolution_g)) // position point at top of block
            .translate(pointer_offset)
            .draw(display)
            .unwrap();

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
            toolbar_right_cap: slice_drawing(Grid::from_lower_upper((3, -1, -1), (4, 2, 2)))
                .translate((-3, 0, 0)),
            toolbar_pointer: [
                [
                    slice_drawing(Grid::from_lower_upper((0, 1, -1), (1, 2, 2))),
                    slice_drawing(Grid::from_lower_upper((2, 1, -1), (3, 2, 2)))
                        .translate((-2, 0, 0)),
                ],
                [
                    slice_drawing(Grid::from_lower_upper((0, -1, -1), (1, 0, 2))),
                    slice_drawing(Grid::from_lower_upper((2, -1, -1), (3, 0, 2)))
                        .translate((-2, 0, 0)),
                ],
            ],
        }
    }
}
