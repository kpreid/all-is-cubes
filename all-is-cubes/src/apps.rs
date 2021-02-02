// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Components for "apps", or game clients: user interface and top-level state.

use cgmath::{Vector2, Vector3, Zero as _};
use std::collections::{HashMap, HashSet};
use std::time::Duration;

use crate::camera::{Camera, CameraChange};
use crate::content::UniverseTemplate;
use crate::listen::{DirtyFlag, ListenerHelper as _};
use crate::math::FreeCoordinate;
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

/// Parse input events, particularly key-down/up pairs, into camera control and such.
///
/// This is designed to be a leaf of the dependency graph: it does not own or send
/// messages to any other elements of the application. Instead, the following steps
/// must occur in the given order.
///
/// 1. The platform-specific code should call [`InputProcessor::key_down`] and such to
///    to provide input information.
/// 2. The game loop should call [`InputProcessor::apply_input`] to apply the effects
///    of input on the relevant [`Camera`].
/// 3. The game loop should call [`InputProcessor::step`] to apply the effects of time
///    on the input processor.
#[derive(Clone, Debug)]
pub struct InputProcessor {
    keys_held: HashSet<Key>,
    momentary_timeout: HashMap<Key, Duration>,
    mouselook_buffer: Vector2<FreeCoordinate>,
}

impl InputProcessor {
    pub fn new() -> Self {
        Self {
            keys_held: HashSet::new(),
            momentary_timeout: HashMap::new(),
            mouselook_buffer: Vector2::zero(),
        }
    }

    fn is_bound(key: Key) -> bool {
        // Eventually we'll have actual configurable keybindings...
        match key {
            // Used in `InputProcessor::movement()`.
            Key::Character('w') => true,
            Key::Character('a') => true,
            Key::Character('s') => true,
            Key::Character('d') => true,
            Key::Character('e') => true,
            Key::Character('c') => true,
            // Used in `InputProcessor::apply_input()`.
            Key::Left => true,
            Key::Right => true,
            Key::Up => true,
            Key::Down => true,
            Key::Character(' ') => true,
            Key::Character(d) if d.is_ascii_digit() => true,
            _ => false,
        }
    }

    /// Handles incoming key-down events. Returns whether the key was unbound.
    pub fn key_down(&mut self, key: Key) -> bool {
        let bound = Self::is_bound(key);
        if bound {
            self.keys_held.insert(key);
        }
        bound
    }

    /// Handles incoming key-up events.
    pub fn key_up(&mut self, key: Key) {
        self.keys_held.remove(&key);
    }

    /// Handles incoming key events in the case where key-up events are not available,
    /// such that an assumption about equivalent press duration must be made.
    pub fn key_momentary(&mut self, key: Key) -> bool {
        self.momentary_timeout
            .insert(key, Duration::from_millis(200));
        self.key_up(key);
        self.key_down(key)
    }

    /// Handles the keyboard focus being gained or lost. If the platform does not have
    /// a concept of focus, you need not call this method, but may call it with `true`.
    ///
    /// `InputProcessor` will assume that if focus is lost, key-up events may be lost and
    /// so currently held keys should stop taking effect.
    pub fn key_focus(&mut self, has_focus: bool) {
        if has_focus {
            // Nothing to do.
        } else {
            self.keys_held.clear();
            self.momentary_timeout.clear();
        }
    }

    /// Provide relative movement information for mouselook.
    ///
    /// This value is an accumulated displacement, not an angular velocity, so it is not
    /// suitable for joystick-type input.
    pub fn mouselook_delta(&mut self, delta: Vector2<FreeCoordinate>) {
        // TODO: sensitivity option
        self.mouselook_buffer += delta * 0.2;
    }

    /// Returns the character movement velocity that input is currently requesting.
    pub fn movement(&self) -> Vector3<FreeCoordinate> {
        Vector3::new(
            self.net_movement(Key::Character('a'), Key::Character('d')),
            self.net_movement(Key::Character('c'), Key::Character('e')),
            self.net_movement(Key::Character('w'), Key::Character('s')),
        )
    }

    /// Advance time insofar as input interpretation is affected by time.
    ///
    /// This method should be called *after* [`apply_input`](Self::apply_input), when
    /// applicable.
    pub fn step(&mut self, timestep: Duration) {
        let mut to_drop = Vec::new();
        for (key, duration) in self.momentary_timeout.iter_mut() {
            if let Some(reduced) = duration.checked_sub(timestep) {
                *duration = reduced;
            } else {
                to_drop.push(*key);
            }
        }
        for key in to_drop.drain(..) {
            self.momentary_timeout.remove(&key);
            self.key_up(key);
        }

        self.mouselook_buffer = Vector2::zero();
    }

    /// Applies the current input to the given `Camera`.
    pub fn apply_input(&mut self, camera: &mut Camera, timestep: Duration) {
        let movement = self.movement();
        if movement != Vector3::zero() {
            camera.auto_rotate = false;
        }
        camera.set_velocity_input(movement);

        let dt = timestep.as_secs_f64();
        let key_turning_step = 80.0 * dt;
        camera.body.yaw = (camera.body.yaw
            + key_turning_step * self.net_movement(Key::Left, Key::Right)
            + self.mouselook_buffer.x)
            .rem_euclid(360.0);
        camera.body.pitch = (camera.body.pitch
            + key_turning_step * self.net_movement(Key::Up, Key::Down)
            + self.mouselook_buffer.y)
            .min(90.0)
            .max(-90.0);

        if self.keys_held.contains(&Key::Character(' ')) {
            camera.jump_if_able();
        }

        // TODO: would be nice to express this in a more straightforward fashion
        // (though it's probably fast enough that the O(n) doesn't matter)
        for slot in 0..=9 {
            let digit = if slot == 9 {
                '0'
            } else {
                std::char::from_digit(slot as u32 + 1, 10).unwrap()
            };
            if self.keys_held.contains(&Key::Character(digit)) {
                camera.set_selected_slot(1, slot);
            }
        }
    }

    /// Computes the net effect of a pair of opposed inputs (e.g. "forward" and "back").
    fn net_movement(&self, negative: Key, positive: Key) -> FreeCoordinate {
        match (
            self.keys_held.contains(&negative),
            self.keys_held.contains(&positive),
        ) {
            (true, false) => -1.0,
            (false, true) => 1.0,
            _ => 0.0,
        }
    }
}

/// A platform-neutral representation of keyboard keys for [`InputProcessor`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Key {
    /// Letters should be lowercase.
    Character(char),
    /// Left arrow key.
    Left,
    /// Right arrow key.
    Right,
    /// Up arrow key.
    Up,
    /// Down arrow key.
    Down,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_movement() {
        let mut input = InputProcessor::new();
        assert_eq!(input.movement(), Vector3::new(0.0, 0.0, 0.0));
        input.key_down(Key::Character('d'));
        assert_eq!(input.movement(), Vector3::new(1.0, 0.0, 0.0));
        input.key_down(Key::Character('a'));
        assert_eq!(input.movement(), Vector3::new(0.0, 0.0, 0.0));
        input.key_up(Key::Character('d'));
        assert_eq!(input.movement(), Vector3::new(-1.0, 0.0, 0.0));
    }

    #[test]
    fn input_focus_lost_cancels_keys() {
        let mut input = InputProcessor::new();
        assert_eq!(input.movement(), Vector3::zero());
        input.key_down(Key::Character('d'));
        assert_eq!(input.movement(), Vector3::unit_x());
        input.key_focus(false);
        assert_eq!(input.movement(), Vector3::zero()); // Lost focus, no movement.

        // Confirm that keys work again afterward.
        input.key_focus(true);
        assert_eq!(input.movement(), Vector3::zero());
        input.key_down(Key::Character('d'));
        assert_eq!(input.movement(), Vector3::unit_x());
        // TODO: test (and handle) key events arriving while focus is lost, just in case.
    }

    // TODO: test jump and flying logic
}
