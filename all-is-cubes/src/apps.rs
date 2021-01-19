// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Components for "apps", or game clients: user interface and top-level state.

use crate::camera::{Camera, CameraChange, InputProcessor};
use crate::content::UniverseTemplate;
use crate::listen::{DirtyFlag, ListenerHelper as _};
use crate::space::Space;
use crate::universe::{FrameClock, URef, Universe, UniverseStepInfo};
use crate::vui::Vui;

/// Everything that a game application needs regardless of platform.
///
/// Once we have multiplayer / client-server support, this will become the client-side
/// structure.
#[derive(Debug)]
pub struct AllIsCubesAppState {
    /// Determines the timing of simulation and drawing. The caller must arrange
    /// to advance time in the clock.
    pub frame_clock: FrameClock,

    /// Handles (some) user input. The caller must provide input events/state;
    /// `AllIsCubesAppState` will handle calling [`InputProcessor::apply_input`].
    pub input_processor: InputProcessor,

    game_universe: Universe,
    game_camera: URef<Camera>,

    ui: Vui,
    ui_dirty: DirtyFlag,
}

impl AllIsCubesAppState {
    /// Construct a new `AllIsCubesAppState` with a new [`Universe`] from the given
    /// template.
    pub fn new(template: UniverseTemplate) -> Self {
        let game_universe = template.build();

        let mut new_self = Self {
            frame_clock: FrameClock::new(),
            input_processor: InputProcessor::new(),
            game_camera: game_universe.get_default_camera(),
            game_universe,
            ui: Vui::new(),
            ui_dirty: DirtyFlag::new(true),
        };

        // TODO: once it's possible to switch cameras we will need to clear and reinstall this
        new_self
            .game_camera
            .borrow()
            .listen(new_self.ui_dirty.listener().filter(|msg| match msg {
                CameraChange::Inventory | CameraChange::Selections => Some(()),
            }));
        new_self.maybe_sync_ui();

        new_self
    }

    /// Returns a reference to the [`Camera`] that should be shown to the user.
    pub fn camera(&self) -> &URef<Camera> {
        &self.game_camera
    }

    /// Returns a mutable reference to the [`Universe`].
    pub fn universe_mut(&mut self) -> &mut Universe {
        &mut self.game_universe
    }

    pub fn ui_space(&self) -> &URef<Space> {
        &self.ui.current_space()
    }

    /// Steps the universe if the `FrameClock` says it's time to do so.
    pub fn maybe_step_universe(&mut self) -> Option<UniverseStepInfo> {
        if self.frame_clock.should_step() {
            let step_length = self.frame_clock.step_length();
            self.frame_clock.did_step();

            self.input_processor
                .apply_input(&mut *self.camera().borrow_mut(), step_length);
            self.input_processor.step(step_length);

            let mut info = self.game_universe.step(step_length);

            self.maybe_sync_ui();
            info += self.ui.step(step_length);
            Some(info)
        } else {
            None
        }
    }

    fn maybe_sync_ui(&mut self) {
        if self.ui_dirty.get_and_clear() {
            // TODO: Exact interaction between Camera and Vui probably shouldn't be AllIsCubesAppState's responsibility.
            let camera = self.game_camera.borrow();
            self.ui
                .set_toolbar(&camera.inventory().slots, &camera.selected_slots())
                .unwrap();
        }
    }
}
