// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Components for "apps", or game clients: user interface and top-level state.

use crate::camera::{Camera, InputProcessor};
use crate::demo_content::new_universe_with_stuff;
use crate::universe::{FrameClock, URef, Universe, UniverseStepInfo};

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

    universe: Universe,
    camera: URef<Camera>,
}

impl AllIsCubesAppState {
    /// Construct a new `AllIsCubesAppState` using the result of
    /// [`new_universe_with_stuff()`] as initial content.
    pub fn new() -> Self {
        let universe = new_universe_with_stuff();
        Self {
            frame_clock: FrameClock::new(),
            input_processor: InputProcessor::new(),
            camera: universe.get_default_camera(),
            universe,
        }
    }

    /// Returns a reference to the [`Camera`] that should be shown to the user.
    pub fn camera(&self) -> &URef<Camera> {
        &self.camera
    }

    /// Returns a mutable reference to the [`Universe`].
    pub fn universe_mut(&mut self) -> &mut Universe {
        &mut self.universe
    }

    // TODO: Universe should have a proper info struct return
    /// Steps the universe if the `FrameClock` says it's time to do so.
    pub fn maybe_step_universe(&mut self) -> Option<UniverseStepInfo> {
        if self.frame_clock.should_step() {
            let step_length = self.frame_clock.step_length();
            self.frame_clock.did_step();
            self.input_processor
                .apply_input(&mut *self.camera().borrow_mut(), step_length);
            self.input_processor.step(step_length);
            Some(self.universe.step(step_length))
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
