// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Voxel User Interface.
//!
//! We've got all this rendering and interaction code, so let's reuse it for the
//! GUI as well as the game.

use cgmath::{Angle as _, Deg, EuclideanSpace as _, Matrix4, Vector2, Vector3};
use embedded_graphics::fonts::{Font8x16, Text};
use embedded_graphics::geometry::Point;
use embedded_graphics::prelude::{Drawable, Font, Pixel, Primitive, Transform as _};
use embedded_graphics::primitives::{Circle, Line, Rectangle, Triangle};
use embedded_graphics::style::{PrimitiveStyleBuilder, TextStyleBuilder};
use std::borrow::Cow;
use std::time::Duration;

use crate::block::{space_to_blocks, Block, BlockAttributes, Resolution, AIR, AIR_EVALUATED};
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::linking::{BlockModule, BlockProvider};
use crate::math::{Face, FreeCoordinate, GridCoordinate, GridMatrix, GridPoint, GridVector, Rgba};
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

    /// None if the tooltip is blanked
    tooltip_age: Option<Duration>,
}

impl Vui {
    pub fn new() -> Self {
        let mut universe = Universe::new();
        let hud_blocks = HudBlocks::new(&mut universe, 16);
        let hud_space = HudLayout::default().new_space(&mut universe, &hud_blocks);

        Self {
            universe,
            current_space: hud_space.clone(),
            hud_blocks,
            hud_space,
            aspect_ratio: 4. / 3., // arbitrary placeholder assumption

            tooltip_age: None,
        }
    }

    // TODO: It'd be more encapsulating if we could provide a _read-only_ reference...
    pub fn current_space(&self) -> &URef<Space> {
        &self.current_space
    }

    /// Computes an OpenGL style view matrix that should be used to display the
    /// [`Vui::current_space`].
    ///
    /// It does not need to be rechecked other than on aspect ratio changes.
    ///
    /// TODO: This is not a method because the code structure makes it inconvenient for
    /// renderers to get access to `Vui` itself. Add some other communication path.
    pub fn view_matrix(space: &Space, fov_y: Deg<FreeCoordinate>) -> Matrix4<FreeCoordinate> {
        let grid = space.grid();
        let mut ui_center = grid.center();

        // Arrange a view distance which will place the Z=0 plane sized to fill the viewport
        // (at least vertically, as we don't have aspect ratio support yet).
        ui_center.z = 0.0;

        let view_distance = FreeCoordinate::from(grid.size().y) * (fov_y / 2.).cot() / 2.;
        Matrix4::look_at_rh(
            ui_center + Vector3::new(0., 0., view_distance),
            ui_center,
            Vector3::new(0., 1., 0.),
        )
    }

    pub const SUGGESTED_FOV_Y: Deg<FreeCoordinate> = Deg(30.);

    pub fn step(&mut self, timestep: Duration) -> UniverseStepInfo {
        if let Some(ref mut age) = self.tooltip_age {
            *age += timestep;
            if *age > Duration::from_secs(1) {
                // TODO: log errors
                let _ = self.set_tooltip_text("");
                self.tooltip_age = None;
            }
        }

        self.universe.step(timestep)
    }

    // TODO: return type leaks implementation details, ish
    // (but we do want to return/log an error rather than eithe panicking or doing nothing)
    pub fn set_toolbar(
        &mut self,
        tools: &[Tool],
        selections: &[usize],
    ) -> Result<(), SetCubeError> {
        HudLayout::default().set_toolbar(
            &mut *self.hud_space.borrow_mut(),
            &self.hud_blocks,
            tools,
            selections,
        )?;

        // TODO: We should do this only if the actual selected item changed (but we don't yet
        // have enough state information to track that).
        // TODO: It's inefficient to perform a non-cached block evaluation just for the sake of
        // getting the text — should we have a partial evaluation? Should tools keep evaluated
        // icons on offer?
        let text = selections
            .get(1)
            .and_then(|&i| tools.get(i))
            .and_then(|tool| tool.icon(&self.hud_blocks.icons).evaluate().ok())
            .map(|ev_block| ev_block.attributes.display_name)
            .unwrap_or(Cow::Borrowed(""));
        self.set_tooltip_text(&text)?;

        Ok(())
    }

    // TODO: handle errors in a local/transient way instead of propagating
    pub fn set_tooltip_text(&mut self, text: &str) -> Result<(), SetCubeError> {
        self.tooltip_age = Some(Duration::from_secs(0));
        HudLayout::default().set_tooltip_text(
            &mut *self.hud_space.borrow_mut(),
            &self.hud_blocks,
            text,
        )
    }
}

/// Knows where and how to place graphics within the HUD space, but does not store
/// the space or any related state itself; depends only on the screen size and other
/// parameters not primarily dependent on user interaction. This split is intended to
/// simplify the problem of adapting to size changes (though right now there is no
/// actual such handling).
#[derive(Clone, Debug, PartialEq)]
struct HudLayout {
    size: Vector2<GridCoordinate>,
    toolbar_positions: usize,
}

// TODO: This will probably not make sense once we have aspect ratio adaptations
impl Default for HudLayout {
    fn default() -> Self {
        Self {
            // Odd width benefits the toolbar.
            size: Vector2::new(25, 18),
            toolbar_positions: 10,
        }
    }
}

const TOOLBAR_STEP: GridCoordinate = 2;
impl HudLayout {
    fn grid(&self) -> Grid {
        Grid::from_lower_upper((0, 0, -5), (self.size.x, self.size.y, 5))
    }

    fn text_resolution(&self) -> Resolution {
        16
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
        toolbar_text_space
            .fill_uniform(toolbar_text_space.grid(), Block::from(Rgba::WHITE))
            .unwrap();
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

        universe.insert_anonymous(space)
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
    pub fn set_toolbar(
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
                let brush: &VoxelBrush =
                    &hud_blocks.toolbar_pointer[sel][usize::from(slot == index)];
                Pixel(Point::new(0, 0), brush).draw(toolbar_disp)?;
            }
        }

        Ok(())
    }

    fn set_tooltip_text(
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

        let text_width =
            text.len() as GridCoordinate * Font8x16::CHARACTER_SIZE.width as GridCoordinate;
        let text_start_x = (grid.size().x - text_width) / 2;

        Text::new(&text, Point::new(text_start_x, -16))
            .into_styled(
                TextStyleBuilder::new(Font8x16)
                    .text_color(&hud_blocks.text)
                    .build(),
            )
            .draw(&mut toolbar_text_space.draw_target(GridMatrix::FLIP_Y))?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
struct HudBlocks {
    icons: BlockProvider<Icons>,
    text: VoxelBrush<'static>,
    toolbar_left_cap: VoxelBrush<'static>,
    toolbar_right_cap: VoxelBrush<'static>,
    toolbar_divider: VoxelBrush<'static>,
    toolbar_middle: VoxelBrush<'static>,
    /// Outer index is "which pointer", inner index is "shown or hidden".
    toolbar_pointer: [[VoxelBrush<'static>; 2]; 2],
}

impl HudBlocks {
    fn new(universe: &mut Universe, resolution: Resolution) -> Self {
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
        // TODO: remove y flip
        let display = &mut toolbar_drawing_space.draw_target(GridMatrix::FLIP_Y);

        let padding = 3;
        let stroke_width = 1;
        let background_fill = VoxelBrush::single(palette::HUD_TOOLBAR_BACK).translate((0, 0, -1));
        let background_stroke = VoxelBrush::single(palette::HUD_TOOLBAR_FRAME);
        let icon_background_rectangle = Rectangle::new(
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

#[allow(unused)] // TODO: not yet used for real
pub(crate) fn draw_background(space: &mut Space) {
    let grid = space.grid();
    let background_rect = Rectangle::new(
        Point::new(grid.lower_bounds().x, grid.lower_bounds().y),
        Point::new(grid.upper_bounds().x - 1, grid.upper_bounds().y - 1),
    );

    let display =
        &mut space.draw_target(GridMatrix::from_translation([0, 0, grid.lower_bounds().z]));

    let background = VoxelBrush::single(Block::from(palette::MENU_BACK));
    let frame = VoxelBrush::single(Block::from(palette::MENU_FRAME)).translate((0, 0, 1));

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

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, strum::Display, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
pub enum Icons {
    EmptySlot,
    Delete,
    CopyFromSpace,
}
impl BlockModule for Icons {
    fn namespace() -> &'static str {
        "all-is-cubes/vui/icons"
    }
}
impl Icons {
    fn new(universe: &mut Universe) -> BlockProvider<Icons> {
        let resolution = 16;
        BlockProvider::new(|key| {
            match key {
                Icons::EmptySlot => Block::builder()
                    .attributes(AIR_EVALUATED.attributes)
                    .display_name("")
                    .color(Rgba::TRANSPARENT)
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

                    let mut space = Space::empty(Grid::for_block(resolution));
                    let display = &mut space.draw_target(GridMatrix::from_origin(
                        GridPoint::new(1, 1, 1) * GridCoordinate::from(resolution / 2),
                        Face::PX,
                        Face::NY,
                        Face::PZ,
                    ));

                    // Draw X on circle
                    Circle::new(Point::new(0, 0), (resolution / 2 - 2).into())
                        .into_styled(
                            PrimitiveStyleBuilder::new()
                                .fill_color(&background_brush)
                                .build(),
                        )
                        .draw(display)
                        .unwrap();
                    Line::new(
                        Point::new(-x_radius, -x_radius),
                        Point::new(x_radius, x_radius),
                    )
                    .into_styled(line_style)
                    .draw(display)
                    .unwrap();
                    Line::new(
                        Point::new(x_radius, -x_radius),
                        Point::new(-x_radius, x_radius),
                    )
                    .into_styled(line_style)
                    .draw(display)
                    .unwrap();

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
            }
        })
    }
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

    #[test]
    fn tooltip_timeout() {
        let mut vui = Vui::new();
        assert_eq!(vui.tooltip_age, None);
        vui.set_tooltip_text("Hello world").unwrap();
        assert_eq!(vui.tooltip_age, Some(Duration::from_secs(0)));
        vui.step(Duration::from_millis(500));
        assert_eq!(vui.tooltip_age, Some(Duration::from_millis(500)));
        vui.step(Duration::from_millis(501));
        assert_eq!(vui.tooltip_age, None);
    }
}
