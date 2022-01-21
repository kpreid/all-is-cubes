// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Voxel User Interface.
//!
//! We've got all this rendering and interaction code, so let's reuse it for the
//! GUI as well as the game.

use std::sync::{mpsc, Arc, Mutex};

use cgmath::{Angle as _, Decomposed, Deg, Transform, Vector3};
use embedded_graphics::geometry::Point;
use embedded_graphics::prelude::{Drawable, Primitive};
use embedded_graphics::primitives::{PrimitiveStyleBuilder, Rectangle};
use ordered_float::NotNan;

use crate::apps::{ControlMessage, InputProcessor};
use crate::block::Block;
use crate::camera::{FogOption, GraphicsOptions, ViewTransform};
use crate::character::Character;
use crate::content::palette;
use crate::drawing::VoxelBrush;
use crate::inv::ToolError;
use crate::listen::{DirtyFlag, ListenableSource};
use crate::math::{FreeCoordinate, GridMatrix};
use crate::space::Space;
use crate::time::Tick;
use crate::universe::{URef, Universe, UniverseStepInfo};
use crate::util::YieldProgress;
use crate::vui::widgets::TooltipState;

mod hud;
use hud::*;
mod icons;
pub use icons::*;
mod layout;
#[doc(hidden)] // going to be used by all-is-cubes-content, but not yet stable
pub use layout::*;
mod widget_trait;
#[doc(hidden)]
pub use widget_trait::*;
pub mod widgets;

/// `Vui` builds user interfaces out of voxels. It owns a `Universe` dedicated to the
/// purpose and draws into spaces to form the HUD and menus.
#[derive(Debug)] // TODO: probably not very informative Debug as derived
pub(crate) struct Vui {
    universe: Universe,
    current_space: URef<Space>,

    #[allow(dead_code)] // TODO: not used but probably will be when we have more dynamic UI
    hud_blocks: Arc<HudBlocks>,
    #[allow(dead_code)] // TODO: not used but probably will be when we have more dynamic UI
    hud_space: URef<Space>,

    character_source: ListenableSource<Option<URef<Character>>>,
    changed_character: DirtyFlag,
    tooltip_state: Arc<Mutex<TooltipState>>,
}

impl Vui {
    /// `input_processor` is the `InputProcessor` whose state may be reflected on the HUD.
    /// `character_source` reports the `Character` whose inventory should be displayed.
    /// TODO: Reduce coupling, perhaps by passing in a separate struct with just the listenable
    /// elements.
    ///
    /// This is an async function for the sake of cancellation and optional cooperative
    /// multitasking. It may be blocked on from a synchronous context.
    pub async fn new(
        input_processor: &InputProcessor,
        character_source: ListenableSource<Option<URef<Character>>>,
        paused: ListenableSource<bool>,
        graphics_options: ListenableSource<GraphicsOptions>,
        control_channel: mpsc::SyncSender<ControlMessage>,
    ) -> Self {
        let mut universe = Universe::new();
        // TODO: take YieldProgress as a parameter
        let hud_blocks = Arc::new(HudBlocks::new(&mut universe, YieldProgress::noop(), 16).await);

        let changed_character = DirtyFlag::new(false);
        character_source.listen(changed_character.listener());

        let tooltip_state = Arc::<Mutex<TooltipState>>::default();

        // TODO: terrible mess of tightly coupled parameters
        let hud_space = new_hud_space(
            &mut universe,
            tooltip_state.clone(),
            input_processor,
            character_source.clone(),
            paused,
            &HudInputs {
                hud_blocks: hud_blocks.clone(),
                control_channel,
                graphics_options,
            },
        );

        Self {
            universe,
            current_space: hud_space.clone(),
            hud_blocks,
            hud_space,

            character_source,
            changed_character,
            tooltip_state,
        }
    }

    // TODO: It'd be more encapsulating if we could provide a _read-only_ reference...
    pub fn current_space(&self) -> &URef<Space> {
        &self.current_space
    }

    /// Computes a [`ViewTransform`] that should be used to view the [`Vui::current_space`].
    ///
    /// It does not need to be rechecked other than on aspect ratio changes.
    ///
    /// TODO: This is not a method because the code structure makes it inconvenient for
    /// renderers to get access to `Vui` itself. Add some other communication path.
    pub fn view_transform(space: &Space, fov_y: Deg<FreeCoordinate>) -> ViewTransform {
        let grid = space.grid();
        let mut ui_center = grid.center();

        // Arrange a view distance which will place the Z=0 plane sized to fill the viewport
        // (at least vertically, as we don't have aspect ratio support yet).
        ui_center.z = 0.0;

        let view_distance = FreeCoordinate::from(grid.size().y) * (fov_y / 2.).cot() / 2.;
        Decomposed::look_at_rh(
            ui_center + Vector3::new(0., 0., view_distance),
            ui_center,
            Vector3::new(0., 1., 0.),
        )
        .inverse_transform() // Our view transform standard is camera-to-world not world-to-camera
        .unwrap()
    }

    /// Compute graphics options to render the VUI space given the user's regular options.
    pub fn graphics_options(mut options: GraphicsOptions) -> GraphicsOptions {
        // Set FOV to give a predictable, not-too-wide-angle perspective.
        options.fov_y = NotNan::from(30);

        // Disable fog for maximum clarity and because we shouldn't have any far clipping to hide.
        options.fog = FogOption::None;

        // Fixed view distance for our layout.
        // TODO: Derive this from HudLayout and also FOV (since FOV determines eye-to-space distance).
        options.view_distance = NotNan::from(100);

        // clutter
        options.debug_chunk_boxes = false;

        options
    }

    pub fn step(&mut self, tick: Tick) -> UniverseStepInfo {
        // TODO: This should possibly be the responsibility of the TooltipState itself?
        if self.changed_character.get_and_clear() {
            if let Some(character_ref) = &*self.character_source.get() {
                TooltipState::bind_to_character(&self.tooltip_state, character_ref.clone());
            }
        }

        self.universe.step(tick)
    }

    pub fn show_tool_error(&mut self, error: ToolError) {
        // TODO: review text formatting
        if let Ok(mut state) = self.tooltip_state.lock() {
            state.set_message(error.to_string().into());
        }
    }
}

#[allow(unused)] // TODO: not yet used for real
pub(crate) fn draw_background(space: &mut Space) {
    let grid = space.grid();
    let background_rect = Rectangle::with_corners(
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
    use futures_executor::block_on;

    fn new_vui_for_test() -> Vui {
        block_on(Vui::new(
            &InputProcessor::new(),
            ListenableSource::constant(None),
            ListenableSource::constant(false),
            ListenableSource::constant(GraphicsOptions::default()),
            mpsc::sync_channel(1).0,
        ))
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
}
