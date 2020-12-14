// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Voxel User Interface.
//!
//! We've got all this rendering and interaction code, so let's reuse it for the
//! GUI as well as the game.

use cgmath::{EuclideanSpace as _, Vector2};
use embedded_graphics::geometry::Point;
use embedded_graphics::prelude::{Drawable, Pixel, Primitive, Transform as _};
use embedded_graphics::primitives::{Rectangle, Triangle};
use embedded_graphics::style::PrimitiveStyleBuilder;
use std::time::Duration;

use crate::block::{space_to_blocks, Block, BlockAttributes, AIR};
use crate::blockgen::BlockGen;
use crate::content::palette;
use crate::drawing::{VoxelBrush, VoxelDisplayAdapter};
use crate::math::{FreeCoordinate, GridCoordinate, GridPoint, RGBA};
use crate::space::{Grid, SetCubeError, Space};
use crate::tools::Tool;
use crate::universe::{URef, Universe, UniverseStepInfo};

/// `Vui` builds user interfaces out of voxels. It owns a `Universe` dedicated to the
/// purpose and draws into spaces to form the HUD and menus.
#[derive(Debug)] // TODO: probably not very informative Debug as derived
pub(crate) struct Vui {
    universe: Universe,
    current_space: URef<Space>,
    hud_blocks: HudBlocks,
    hud_space: URef<Space>,
    aspect_ratio: FreeCoordinate,
}

impl Vui {
    pub fn new() -> Self {
        let mut universe = Universe::new();
        let hud_blocks = HudBlocks::new(&mut BlockGen::new(&mut universe, 16));
        let hud_space = HudLayout::default().new_space(&mut universe, &hud_blocks);

        Self {
            universe,
            current_space: hud_space.clone(),
            hud_blocks,
            hud_space,
            aspect_ratio: 4. / 3., // arbitrary placeholder assumption
        }
    }

    // TODO: It'd be more encapsulating if we could provide a _read-only_ reference...
    pub fn current_space(&self) -> &URef<Space> {
        &self.current_space
    }

    pub fn step(&mut self, timestep: Duration) -> UniverseStepInfo {
        self.universe.step(timestep)
    }

    // TODO: return type leaks implementation details, ish
    // (but we do want to return/log an error rather than eithe panicking or doing nothing)
    pub fn set_toolbar(
        &mut self,
        tools: &[Tool],
        selections: [usize; 2],
    ) -> Result<(), SetCubeError> {
        HudLayout::default().set_toolbar(
            &mut *self.hud_space.borrow_mut(),
            &self.hud_blocks,
            tools,
            selections,
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
struct HudLayout {
    size: Vector2<GridCoordinate>,
    toolbar_positions: usize,
}

// TODO: This will probably not make sense once we have aspect ratio adaptations
impl Default for HudLayout {
    fn default() -> Self {
        Self {
            size: Vector2::new(24, 18),
            toolbar_positions: 10,
        }
    }
}

const TOOLBAR_STEP: GridCoordinate = 2;
impl HudLayout {
    fn grid(&self) -> Grid {
        Grid::new((-1, -1, 0), (self.size.x + 2, self.size.y + 2, 10))
    }

    // TODO: taking the entire Universe doesn't seem like the best interface
    // but we want room to set up new blocks. Figure out a route for that.
    // TODO: validate this doesn't crash on wonky sizes.
    fn new_space(&self, universe: &mut Universe, hud_blocks: &HudBlocks) -> URef<Space> {
        let Vector2 { x: w, y: h } = self.size;
        let grid = self.grid();
        let mut space = Space::empty(grid);

        space.set_sky_color(palette::HUD_SKY);

        if false {
            // Visualization of the bounds of the space we're drawing.
            let frame_block = Block::from(RGBA::new(0.0, 1.0, 1.0, 1.0));
            let mut add_frame = |z| {
                space
                    .fill(Grid::new((-1, -1, z), (w + 2, h + 2, 1)), |_| {
                        Some(&frame_block)
                    })
                    .unwrap();
                space
                    .fill(Grid::new((0, 0, z), (w, h, 1)), |_| Some(&AIR))
                    .unwrap();
            };
            add_frame(0);
            add_frame(grid.upper_bounds().z - 1);
        }

        // Draw background for toolbar.
        // TODO: give this more shape and decoration (custom outline blocks).
        // And a selected-highlight.
        let toolbar_disp = &mut VoxelDisplayAdapter::new(&mut space, self.tool_icon_position(0));
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

        universe.insert_anonymous(space)
    }

    fn tool_icon_position(&self, index: usize) -> GridPoint {
        let x_start =
            (self.size.x - (self.toolbar_positions as GridCoordinate - 1) * TOOLBAR_STEP + 1) / 2;
        // TODO: set depth sensibly
        GridPoint::new(x_start + (index as GridCoordinate) * TOOLBAR_STEP, 0, 9)
    }

    /// Repaint the toolbar with a new set of tools and selected tools.
    ///
    /// Returns an error if using the tools' icons produced an error — or possibly if
    /// there was a drawing layout problem.
    // TODO: Error return should probably be something other than SetCubeError
    pub fn set_toolbar(
        &self,
        space: &mut Space,
        hud_blocks: &HudBlocks,
        tools: &[Tool],
        selections: [usize; 2],
    ) -> Result<(), SetCubeError> {
        for (index, tool) in tools.iter().enumerate() {
            if index >= self.toolbar_positions {
                break;
            }

            let position = self.tool_icon_position(index);
            // Draw icon
            space.set(position, &*tool.icon())?;
            // Draw pointers. TODO: avoid erase-then-draw strategy
            let toolbar_disp = &mut VoxelDisplayAdapter::new(space, position);
            Pixel(Point::new(0, 0), &hud_blocks.toolbar_pointer_erase).draw(toolbar_disp)?;
            if selections[0] == index {
                Pixel(Point::new(0, 0), &hud_blocks.toolbar_pointer_0).draw(toolbar_disp)?;
            }
            if selections[1] == index {
                Pixel(Point::new(0, 0), &hud_blocks.toolbar_pointer_1).draw(toolbar_disp)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct HudBlocks {
    toolbar_left_cap: VoxelBrush<'static>,
    toolbar_right_cap: VoxelBrush<'static>,
    toolbar_divider: VoxelBrush<'static>,
    toolbar_middle: VoxelBrush<'static>,
    toolbar_pointer_erase: VoxelBrush<'static>,
    toolbar_pointer_0: VoxelBrush<'static>,
    toolbar_pointer_1: VoxelBrush<'static>,
}

impl HudBlocks {
    fn new(blockgen: &mut BlockGen) -> Self {
        let resolution = GridCoordinate::from(blockgen.resolution);
        // TODO: This toolbar graphic is a "get the bugs in the drawing tools worked out"
        // placeholder for better art...

        // Draw toolbar icon frame, twice so we can have start-middle-end shapes.
        let toolbar_frame_block_grid = Grid::new((-1, -1, -1), (5, 3, 3));
        let toolbar_frame_voxel_grid = toolbar_frame_block_grid.multiply(resolution);
        let mut toolbar_drawing_space = Space::empty(toolbar_frame_voxel_grid);
        let display =
            &mut VoxelDisplayAdapter::new(&mut toolbar_drawing_space, GridPoint::origin());

        let padding = 4;
        let stroke_width = 1;
        let icon_background_rectangle = Rectangle::new(
            // TODO: confirm these offsets are exactly right
            Point::new(-padding - stroke_width, -padding - resolution),
            Point::new(resolution + padding, padding + stroke_width),
        )
        .into_styled(
            PrimitiveStyleBuilder::new()
                .fill_color(palette::HUD_TOOLBAR_BACK)
                .stroke_color(palette::HUD_TOOLBAR_FRAME)
                .stroke_width(stroke_width as u32)
                .build(),
        );
        icon_background_rectangle.draw(display).unwrap();
        icon_background_rectangle
            .translate(Point::new(resolution * 2, 0))
            .draw(display)
            .unwrap();

        // Draw pointers. The pointers are placed above and below the second
        // icon frame.
        let pointer_offset = Point::new(resolution * 5 / 2, 0);
        // TODO: use different related colors
        let pointer_fill =
            VoxelBrush::single(palette::HUD_TOOLBAR_BACK).translate((0, 0, resolution - 1));
        let pointer_stroke =
            VoxelBrush::single(palette::HUD_TOOLBAR_FRAME).translate((0, 0, resolution - 1));
        let pointer_style = PrimitiveStyleBuilder::new()
            .fill_color(&pointer_fill)
            .stroke_color(&pointer_stroke)
            .stroke_width(stroke_width as u32)
            .build();
        Triangle::new(Point::new(-5, 10), Point::new(5, 10), Point::new(0, 0))
            .into_styled(pointer_style)
            .translate(pointer_offset)
            .draw(display)
            .unwrap();
        Triangle::new(Point::new(-5, -10), Point::new(0, 0), Point::new(5, -10))
            .into_styled(pointer_style)
            .translate(Point::new(0, -resolution)) // position point at top of block
            .translate(pointer_offset)
            .draw(display)
            .unwrap();

        // TODO: use a name for the space
        let toolbar_blocks_space = space_to_blocks(
            blockgen.resolution,
            BlockAttributes::default(),
            blockgen.universe.insert_anonymous(toolbar_drawing_space),
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

        // TODO: reconcile: the hud space has the icons "flush with the front" but these slice coordinates assume a 1-block fringe around the icons in all directions.
        Self {
            toolbar_middle: slice_drawing(Grid::from_lower_upper((0, -1, -1), (1, 2, 2))),
            toolbar_divider: slice_drawing(Grid::from_lower_upper((1, -1, -1), (2, 2, 2)))
                .translate((-1, 0, 0)),
            toolbar_left_cap: slice_drawing(Grid::from_lower_upper((-1, -1, -1), (0, 2, 2)))
                .translate((1, 0, 0)),
            toolbar_right_cap: slice_drawing(Grid::from_lower_upper((3, -1, -1), (4, 2, 2)))
                .translate((-3, 0, 0)),
            toolbar_pointer_erase: slice_drawing(Grid::from_lower_upper((0, 1, -1), (1, 2, 2))),
            toolbar_pointer_0: slice_drawing(Grid::from_lower_upper((2, 1, -1), (3, 2, 2)))
                .translate((-2, 0, 0)),
            toolbar_pointer_1: slice_drawing(Grid::from_lower_upper((2, -1, -1), (3, 0, 2)))
                .translate((-2, 0, 0)),
        }
    }
}

#[allow(unused)] // TODO: not yet used for real
pub(crate) fn draw_background(space: &mut Space) {
    let grid = space.grid();
    let background_rect = Rectangle::new(
        Point::new(grid.lower_bounds().x, -grid.upper_bounds().y + 1),
        Point::new(grid.upper_bounds().x - 1, -grid.lower_bounds().y),
    );

    let display = &mut VoxelDisplayAdapter::new(space, GridPoint::new(0, 0, grid.lower_bounds().z));

    let background =
        VoxelBrush::single(Block::Atom(BlockAttributes::default(), palette::MENU_BACK));
    let frame = VoxelBrush::single(Block::Atom(BlockAttributes::default(), palette::MENU_FRAME))
        .translate((0, 0, 1));

    background_rect
        .into_styled(
            PrimitiveStyleBuilder::new()
                .stroke_width(1)
                .stroke_color(&frame)
                .fill_color(&background)
                .build(),
        )
        .draw(display)
        .unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vui_smoke_test() {
        let _ = Vui::new();
    }

    #[test]
    fn background_smoke_test() {
        let mut space = Space::empty_positive(100, 100, 10);
        draw_background(&mut space);
    }
}
