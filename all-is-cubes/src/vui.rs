// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Voxel User Interface.
//!
//! We've got all this rendering and interaction code, so let's reuse it for the
//! GUI as well as the game.

use cgmath::{Angle as _, Deg, Matrix4, Vector3};
use embedded_graphics::geometry::Point;
use embedded_graphics::prelude::{Drawable, Primitive};
use embedded_graphics::primitives::Rectangle;
use embedded_graphics::style::PrimitiveStyleBuilder;
use ordered_float::NotNan;
use std::borrow::Cow;
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use std::time::Duration;

use crate::apps::{InputProcessor, Tick};
use crate::block::{Block, AIR};
use crate::camera::{FogOption, GraphicsOptions};
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::listen::{ListenableSource, Listener};
use crate::math::{FreeCoordinate, GridMatrix};
use crate::space::{SetCubeError, Space};
use crate::tools::Tool;
use crate::universe::{URef, Universe, UniverseStepInfo};

mod hud;
use hud::*;
mod icons;
pub use icons::*;

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

    todo: Rc<RefCell<VuiTodo>>,

    // Things we're listening to...
    mouselook_mode: ListenableSource<bool>,
}

impl Vui {
    /// `input_processor` is the `InputProcessor` whose state may be reflected on the HUD.
    /// TODO: Reduce coupling, perhaps by passing in a separate struct with just the listenable
    /// elements.
    pub fn new(input_processor: &InputProcessor) -> Self {
        let mut universe = Universe::new();
        let hud_blocks = HudBlocks::new(&mut universe, 16);
        let hud_space = HudLayout::default().new_space(&mut universe, &hud_blocks);

        let todo = Rc::new(RefCell::new(VuiTodo::default()));
        input_processor.mouselook_mode().listen(TodoListener {
            target: Rc::downgrade(&todo),
            handler: |todo| todo.crosshair = true,
        });

        Self {
            universe,
            current_space: hud_space.clone(),
            hud_blocks,
            hud_space,
            aspect_ratio: 4. / 3., // arbitrary placeholder assumption

            tooltip_age: None,

            todo,

            mouselook_mode: input_processor.mouselook_mode(),
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

    /// Compute graphics options to render the VUI space given the user's regular options.
    pub fn graphics_options(mut options: GraphicsOptions) -> GraphicsOptions {
        // Set FOV to give a predictable, not-too-wide-angle perspective.
        options.fov_y = NotNan::new(30.).unwrap();

        // Disable fog for maximum clarity and because we shouldn't have any far clipping to hide.
        options.fog = FogOption::None;

        // Fixed view distance for our layout.
        // TODO: Derive this from HudLayout and also FOV (since FOV determines eye-to-space distance).
        options.view_distance = NotNan::new(100.0).unwrap();

        // clutter
        options.debug_chunk_boxes = false;

        options
    }

    pub fn step(&mut self, tick: Tick) -> UniverseStepInfo {
        // Note: clone is necessary to be compatible with borrowing self later
        // TODO: figure out a better strategy; maybe eventually we will have no self borrows
        let todo_rc = self.todo.clone();
        let mut todo = RefCell::borrow_mut(&todo_rc);

        if todo.crosshair {
            todo.crosshair = false;

            // Update crosshair block
            self.hud_space
                .borrow_mut()
                .set(
                    HudLayout::default().crosshair_position(),
                    if *self.mouselook_mode.get() {
                        &self.hud_blocks.icons[Icons::Crosshair]
                    } else {
                        &AIR
                    },
                )
                .unwrap(); // TODO: Handle internal errors better than panicking
        }

        if let Some(ref mut age) = self.tooltip_age {
            *age += tick.delta_t;
            if *age > Duration::from_secs(1) {
                // TODO: log errors
                let _ = self.set_tooltip_text("");
                self.tooltip_age = None;
            }
        }

        self.universe.step(tick)
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

/// [`Vui`]'s set of things that need updating.
#[derive(Debug, Default)]
struct VuiTodo {
    crosshair: bool,
}

/// [`Listener`] adapter for [`VuiTodo`].
struct TodoListener {
    target: Weak<RefCell<VuiTodo>>,
    handler: fn(&mut VuiTodo),
}

impl Listener<()> for TodoListener {
    fn receive(&self, _message: ()) {
        if let Some(cell) = self.target.upgrade() {
            let mut todo = RefCell::borrow_mut(&cell);
            (self.handler)(&mut todo);
        }
    }

    fn alive(&self) -> bool {
        self.target.strong_count() > 0
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

#[cfg(test)]
mod tests {
    use super::*;

    fn new_vui_for_test() -> Vui {
        Vui::new(&InputProcessor::new())
    }

    #[test]
    fn vui_smoke_test() {
        let _ = new_vui_for_test();
    }

    #[test]
    fn background_smoke_test() {
        let mut space = Space::empty_positive(100, 100, 10);
        draw_background(&mut space);
    }

    #[test]
    fn tooltip_timeout() {
        let mut vui = new_vui_for_test();
        assert_eq!(vui.tooltip_age, None);
        vui.set_tooltip_text("Hello world").unwrap();
        assert_eq!(vui.tooltip_age, Some(Duration::from_secs(0)));
        vui.step(Tick::from_seconds(0.5));
        assert_eq!(vui.tooltip_age, Some(Duration::from_millis(500)));
        vui.step(Tick::from_seconds(0.501));
        assert_eq!(vui.tooltip_age, None);
    }
}
