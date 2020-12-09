// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Components for "apps", or game clients: user interface and top-level state.

use crate::camera::{Camera, InputProcessor};
use crate::content::demo::new_universe_with_stuff;
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
}

impl AllIsCubesAppState {
    /// Construct a new `AllIsCubesAppState` using the result of
    /// [`new_universe_with_stuff()`] as initial content.
    pub fn new() -> Self {
        let game_universe = new_universe_with_stuff();
        let game_camera = game_universe.get_default_camera();
        let mut ui = Vui::new();

        // TODO: this will need to be done on a notification/invalidation basis
        ui.set_tools(&game_camera.borrow().tools);

        Self {
            frame_clock: FrameClock::new(),
            input_processor: InputProcessor::new(),
            game_camera,
            game_universe,
            ui,
        }
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
            info += self.ui.step(step_length);
            Some(info)
        } else {
            None
        }
    }
}

impl Default for AllIsCubesAppState {
    fn default() -> Self {
        Self::new()
    }
}
