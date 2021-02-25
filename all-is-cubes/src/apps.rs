// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Components for "apps", or game clients: user interface and top-level state.

use cgmath::{Vector2, Vector3, Zero as _};
use std::collections::{HashMap, HashSet};
use std::time::Duration;

use crate::camera::GraphicsOptions;
use crate::character::{Character, CharacterChange};
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
    game_character: URef<Character>,

    ui: Vui,
    ui_dirty: DirtyFlag,
}

impl AllIsCubesAppState {
    /// Construct a new `AllIsCubesAppState` with a new [`Universe`] from the given
    /// template.
    pub fn new(template: UniverseTemplate) -> Self {
        let game_universe = template
            .build()
            // TODO: better error handling
            .expect("Failure while constructing template");

        let mut new_self = Self {
            frame_clock: FrameClock::new(),
            input_processor: InputProcessor::new(),
            game_character: game_universe.get_default_character(),
            game_universe,
            ui: Vui::new(),
            ui_dirty: DirtyFlag::new(true),
        };

        // TODO: once it's possible to switch characters we will need to clear and reinstall this
        new_self
            .game_character
            .borrow()
            .listen(new_self.ui_dirty.listener().filter(|msg| match msg {
                CharacterChange::Inventory | CharacterChange::Selections => Some(()),
            }));
        new_self.maybe_sync_ui();

        new_self
    }

    /// Returns a reference to the [`Character`] that should be shown to the user.
    pub fn character(&self) -> &URef<Character> {
        &self.game_character
    }

    /// Returns a mutable reference to the [`Universe`].
    pub fn universe_mut(&mut self) -> &mut Universe {
        &mut self.game_universe
    }

    pub fn ui_space(&self) -> &URef<Space> {
        &self.ui.current_space()
    }

    pub fn graphics_options(&self) -> GraphicsOptions {
        // TODO: this should be a constructor parameter (and mutable, once we have
        // a change propagation strategy).
        GraphicsOptions::default()
    }

    /// Steps the universe if the `FrameClock` says it's time to do so.
    pub fn maybe_step_universe(&mut self) -> Option<UniverseStepInfo> {
        if self.frame_clock.should_step() {
            let step_length = self.frame_clock.step_length();
            self.frame_clock.did_step();

            self.input_processor
                .apply_input(&mut *self.character().borrow_mut(), step_length);
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
            // TODO: Exact interaction between Character and Vui probably shouldn't be AllIsCubesAppState's responsibility.
            let character = self.game_character.borrow();
            self.ui
                .set_toolbar(&character.inventory().slots, &character.selected_slots())
                .unwrap();
        }
    }
}

/// Parse input events, particularly key-down/up pairs, into character control and such.
///
/// This is designed to be a leaf of the dependency graph: it does not own or send
/// messages to any other elements of the application. Instead, the following steps
/// must occur in the given order.
///
/// 1. The platform-specific code should call [`InputProcessor::key_down`] and such to
///    to provide input information.
/// 2. The game loop should call [`InputProcessor::apply_input`] to apply the effects
///    of input on the relevant [`Character`].
/// 3. The game loop should call [`InputProcessor::step`] to apply the effects of time
///    on the input processor.
#[derive(Clone, Debug)]
pub struct InputProcessor {
    /// All [`Key`]s currently pressed.
    keys_held: HashSet<Key>,
    /// As a special feature for supporting input without key-up events, stores all
    /// keypresses arriving through [`Self::key_momentary`] and virtually holds them
    /// for a short time. The value is the remaining time.
    momentary_timeout: HashMap<Key, Duration>,
    /// Net mouse movement since the last [`Self::apply_input`].
    mouselook_buffer: Vector2<FreeCoordinate>,
    /// [`Key`]s with one-shot effects when pressed which need to be applied
    /// once per press rather than while held.
    command_buffer: Vec<Key>,
}

impl InputProcessor {
    #[allow(clippy::new_without_default)] // I expect it'll grow some parameters
    pub fn new() -> Self {
        Self {
            keys_held: HashSet::new(),
            momentary_timeout: HashMap::new(),
            mouselook_buffer: Vector2::zero(),
            command_buffer: Vec::new(),
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

    /// Returns true if the key should go in `command_buffer`.
    fn is_command(key: Key) -> bool {
        #[allow(clippy::match_like_matches_macro)]
        match key {
            Key::Character(d) if d.is_ascii_digit() => true,
            _ => false,
        }
    }

    /// Handles incoming key-down events. Returns whether the key was unbound.
    pub fn key_down(&mut self, key: Key) -> bool {
        let bound = Self::is_bound(key);
        if bound {
            self.keys_held.insert(key);
            if Self::is_command(key) {
                self.command_buffer.push(key);
            }
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

    /// Applies the current input to the given [`Character`].
    pub fn apply_input(&mut self, character: &mut Character, timestep: Duration) {
        let dt = timestep.as_secs_f64();
        let key_turning_step = 80.0 * dt;

        let movement = self.movement();
        character.set_velocity_input(movement);

        let turning = Vector2::new(
            key_turning_step * self.net_movement(Key::Left, Key::Right) + self.mouselook_buffer.x,
            key_turning_step * self.net_movement(Key::Up, Key::Down) + self.mouselook_buffer.y,
        );
        character.body.yaw = (character.body.yaw + turning.x).rem_euclid(360.0);
        character.body.pitch = (character.body.pitch + turning.y).min(90.0).max(-90.0);

        if movement != Vector3::zero() || turning != Vector2::zero() {
            character.auto_rotate = false;
        }

        if self.keys_held.contains(&Key::Character(' ')) {
            character.jump_if_able();
        }

        for key in self.command_buffer.drain(..) {
            match key {
                Key::Character(numeral) if numeral.is_digit(10) => {
                    let digit = numeral.to_digit(10).unwrap() as usize;
                    let slot = (digit + 9).rem_euclid(10); // wrap 0 to 9
                    character.set_selected_slot(1, slot);
                }
                _ => {}
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
    use cgmath::{EuclideanSpace as _, Point3};

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

    #[test]
    fn input_slot_selection() {
        // TODO: Awful lot of setup boilerplate...
        let mut u = Universe::new();
        let space = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let character = u.insert_anonymous(Character::new(space.clone(), Point3::origin()));
        let mut input = InputProcessor::new();

        input.key_down(Key::Character('5'));
        input.key_up(Key::Character('5'));
        input.apply_input(&mut *character.borrow_mut(), Duration::from_secs(1));
        assert_eq!(character.borrow_mut().selected_slots()[1], 4);

        // Tenth slot
        input.key_down(Key::Character('0'));
        input.key_up(Key::Character('0'));
        input.apply_input(&mut *character.borrow_mut(), Duration::from_secs(1));
        assert_eq!(character.borrow_mut().selected_slots()[1], 9);
    }

    // TODO: test jump and flying logic
}
