use alloc::vec::Vec;
use core::time::Duration;
use std::collections::{HashMap, HashSet};

use all_is_cubes::character::{self, Character};
use all_is_cubes::euclid::{Point2D, Vector2D};
use all_is_cubes::inv;
use all_is_cubes::listen;
use all_is_cubes::math::{FreeCoordinate, FreeVector, zo32};
use all_is_cubes::physics;
use all_is_cubes::time::Tick;
use all_is_cubes::universe::{self, Handle, Universe};
use all_is_cubes_render::camera::{
    FogOption, LightingOption, NdcPoint2, NominalPixel, RenderMethod, TransparencyOption, Viewport,
};

use crate::{apps::ControlMessage, settings::Settings};

type MousePoint = Point2D<f64, NominalPixel>;

/// Parse input events, particularly key-down/up pairs, into character control and such.
///
/// This is designed to be a leaf of the dependency graph: it does not own or send
/// messages to any other elements of the application. Instead, the following steps
/// must occur in the given order.
///
/// 1. The platform-specific code should call [`InputProcessor::key_down`] and such to
///    to provide input information.
/// 2. The game loop should call `InputProcessor::apply_input` to apply the effects
///    of input on the relevant [`Character`]. (This is currently only possible via
///    [`Session`].)
/// 3. The game loop should call `InputProcessor::step` to apply the effects of time
///    on the input processor. (This is currently only possible via [`Session`].)
///
/// TODO: Refactor APIs till this can be explained more cleanly without reference to
/// private items.
///
/// [`Session`]: super::Session
#[derive(Debug)]
pub struct InputProcessor {
    /// All [`Key`]s currently pressed.
    keys_held: HashSet<Key>,
    /// As a special feature for supporting input without key-up events, stores all
    /// keypresses arriving through [`Self::key_momentary`] and virtually holds them
    /// for a short time. The value is the remaining time.
    momentary_timeout: HashMap<Key, Duration>,
    /// [`Key`]s with one-shot effects when pressed which need to be applied
    /// once per press rather than while held.
    command_buffer: Vec<Key>,

    /// Do we *want* pointer lock for mouselook?
    ///
    /// This is listenable so that the UI can react to this state.
    mouselook_mode: listen::Cell<bool>,
    /// Do we *have* pointer lock for mouselook? Reported by calling input implementation.
    has_pointer_lock: bool,

    /// Net mouse movement since the last [`Self::apply_input`].
    mouselook_buffer: Vector2D<FreeCoordinate, NominalPixel>,

    /// Mouse position in NDC. None if out of bounds/lost focus.
    mouse_ndc_position: Option<NdcPoint2>,

    /// Mouse position used for generating mouselook deltas.
    /// [`None`] if games.
    mouse_previous_pixel_position: Option<MousePoint>,
}

impl InputProcessor {
    /// Constructs a new [`InputProcessor`].
    ///
    /// Consider using [`Session`](crate::apps::Session) instead of directly calling this.
    #[expect(
        clippy::new_without_default,
        reason = "I expect it'll grow some parameters"
    )]
    pub fn new() -> Self {
        Self {
            keys_held: HashSet::new(),
            momentary_timeout: HashMap::new(),
            command_buffer: Vec::new(),
            mouselook_mode: listen::Cell::new(false), // TODO: might want a parameter
            has_pointer_lock: false,
            mouselook_buffer: Vector2D::zero(),
            mouse_ndc_position: Some(NdcPoint2::origin()),
            mouse_previous_pixel_position: None,
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
            Key::Escape => true,
            Key::Left => true,
            Key::Right => true,
            Key::Up => true,
            Key::Down => true,
            Key::Character(' ') => true,
            Key::Character(d) if d.is_ascii_digit() => true,
            Key::Character('i') => true,
            Key::Character('l') => true,
            Key::Character('o') => true,
            Key::Character('p') => true,
            Key::Character('u') => true,
            Key::Character('y') => true,
            Key::Character('`' | '~') => true,
            _ => false,
        }
    }

    /// Returns true if the key should go in `command_buffer`.
    fn is_command(key: Key) -> bool {
        match key {
            Key::Escape => true,
            Key::Character(d) if d.is_ascii_digit() => true,
            Key::Character('i') => true,
            Key::Character('l') => true,
            Key::Character('o') => true,
            Key::Character('p') => true,
            Key::Character('u') => true,
            Key::Character('y') => true,
            Key::Character('`' | '~') => true,
            // TODO: move slot selection commands here
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
        self.momentary_timeout.insert(key, Duration::from_millis(200));
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

            self.mouselook_mode.set(false);
        }
    }

    /// True when the UI is in a state which _should_ have mouse pointer
    /// lock/capture/disable. This is not the same as actually having it since the window
    /// may lack focus, the application may lack permission, etc.; use
    /// [`InputProcessor::has_pointer_lock`] to report that state.
    pub fn wants_pointer_lock(&self) -> bool {
        self.mouselook_mode.get()
    }

    /// Use this method to report whether mouse mouse pointer lock/capture/disable is
    /// known to be successfully enabled, after [`InputProcessor::wants_pointer_lock`]
    /// requests it or it is disabled for any reason.
    pub fn has_pointer_lock(&mut self, value: bool) {
        self.has_pointer_lock = value;
    }

    /// Provide relative movement information for mouselook.
    ///
    /// This value is an accumulated displacement, not an angular velocity, so it is not
    /// suitable for joystick-type input.
    ///
    /// Note that absolute cursor positions must be provided separately.
    pub fn mouselook_delta(&mut self, delta: Vector2D<FreeCoordinate, NominalPixel>) {
        // TODO: sensitivity option
        if self.has_pointer_lock {
            self.mouselook_buffer += delta * 0.2;
        }
    }

    /// Provide position of mouse pointer or other input device in normalized device
    /// coordinates (range -1 to 1 upward and rightward).
    /// [`None`] denotes the cursor being outside the viewport, and out-of-range
    /// coordinates will be treated the same.
    ///
    /// Pixel coordinates may be converted to NDC using [`Viewport::normalize_nominal_point`]
    /// or by using [`InputProcessor::mouse_pixel_position`].
    ///
    /// If this is never called, the default value is (0, 0) which corresponds to the
    /// center of the screen.
    pub fn mouse_ndc_position(&mut self, position: Option<NdcPoint2>) {
        self.mouse_ndc_position = position.filter(|p| p.x.abs() <= 1. && p.y.abs() <= 1.);
    }

    /// Provide position of mouse pointer or other input device in pixel coordinates
    /// framed by the given [`Viewport`]'s `nominal_size`.
    /// [`None`] denotes the cursor being outside the viewport, and out-of-range
    /// coordinates will be treated the same.
    ///
    /// This is equivalent to converting the coordinates and calling
    /// [`InputProcessor::mouse_ndc_position`].
    ///
    /// If this is never called, the default `mouse_ndc_position` value is (0, 0), which
    /// corresponds to the center of the screen.
    ///
    /// TODO: this should take float input, probably
    pub fn mouse_pixel_position(
        &mut self,
        viewport: Viewport,
        position: Option<Point2D<f64, NominalPixel>>,
        derive_movement: bool,
    ) {
        self.mouse_ndc_position(position.map(|p| viewport.normalize_nominal_point(p)));

        if derive_movement {
            if let (Some(p1), Some(p2)) = (self.mouse_previous_pixel_position, position) {
                self.mouselook_delta((p2 - p1).map(FreeCoordinate::from));
            }
            self.mouse_previous_pixel_position = position;
        } else {
            self.mouse_previous_pixel_position = None;
        }
    }

    /// Returns the character movement direction that input is currently requesting.
    ///
    /// This is always a vector of length at most 1.
    pub fn movement(&self) -> FreeVector {
        let mut vector = FreeVector::new(
            self.net_movement(Key::Character('a'), Key::Character('d')),
            self.net_movement(Key::Character('c'), Key::Character('e')),
            self.net_movement(Key::Character('w'), Key::Character('s')),
        );
        if vector != FreeVector::zero() {
            vector = vector.normalize();
        }
        vector
    }

    /// Advance time insofar as input interpretation is affected by time.
    ///
    /// This method should be called *after* [`apply_input`](Self::apply_input), when
    /// applicable.
    pub(crate) fn step(&mut self, tick: Tick) {
        let mut to_drop = Vec::new();
        for (key, duration) in self.momentary_timeout.iter_mut() {
            if let Some(reduced) = duration.checked_sub(tick.delta_t_duration()) {
                *duration = reduced;
            } else {
                to_drop.push(*key);
            }
        }
        for key in to_drop.drain(..) {
            self.momentary_timeout.remove(&key);
            self.key_up(key);
        }

        self.mouselook_buffer = Vector2D::zero();
    }

    /// Applies the accumulated input from previous events.
    /// `targets` specifies the objects it should be applied to.
    ///
    /// Returns an error if `targets` contains inaccessible handles.
    pub(crate) fn apply_input(
        &mut self,
        targets: InputTargets<'_>,
        tick: Tick,
    ) -> Result<(), universe::HandleError> {
        let InputTargets {
            mut universe,
            character: character_opt,
            paused: paused_opt,
            settings,
            control_channel,
            ui,
        } = targets;

        // TODO: universe input is not yet used but it will be, as we start having inputs that trigger transactions
        let _ = universe;

        let dt = tick.delta_t_f64();
        let key_turning_step = 80.0 * dt;

        // Effects of UI on input processing.
        // TODO: this should be done *after* a session step in addition to before, for lower
        // latency / immediate effects -- but currently this is the only place we have
        // `InputTargets` available.
        if ui.is_some_and(|ui| ui.should_focus_on_ui()) && self.mouselook_mode.get() {
            self.mouselook_mode.set(false)
        }

        // Direct character controls
        if let (Some(universe), Some(character_handle)) = (&mut universe, character_opt) {
            let movement = self.movement();

            let turning = Vector2D::<_, ()>::new(
                key_turning_step.mul_add(
                    self.net_movement(Key::Left, Key::Right),
                    self.mouselook_buffer.x,
                ),
                key_turning_step.mul_add(
                    self.net_movement(Key::Up, Key::Down),
                    self.mouselook_buffer.y,
                ),
            );

            universe.mutate_component(character_handle, |body: &mut physics::Body| {
                body.yaw = (body.yaw + turning.x).rem_euclid(360.0);
                body.pitch = (body.pitch + turning.y).clamp(-90.0, 90.0);
            })?;

            universe.mutate_component(character_handle, |input: &mut character::Input| {
                input.velocity_input = movement;
                if self.keys_held.contains(&Key::Character(' ')) {
                    input.jump = true;
                }
            })?;
        }

        for key in self.command_buffer.drain(..) {
            match key {
                Key::Escape => {
                    if let Some(ch) = control_channel {
                        let _ = ch.try_send(ControlMessage::Back);
                    }
                }
                Key::Character('i') => {
                    if let Some(settings) = settings {
                        settings.mutate_graphics_options(|options| {
                            options.lighting_display = match options.lighting_display {
                                LightingOption::None => LightingOption::Flat,
                                LightingOption::Flat => LightingOption::Smooth,
                                LightingOption::Smooth => {
                                    // TODO: the question we actually want to ask is,
                                    // what does the *current renderer* support?
                                    if options.render_method == RenderMethod::Reference {
                                        LightingOption::Bounce
                                    } else {
                                        LightingOption::None
                                    }
                                }
                                LightingOption::Bounce => LightingOption::None,
                                _ => LightingOption::None, // TODO: either stop doing cycle-commands or put it on the enum so it can be exhaustive
                            };
                        });
                    }
                }
                Key::Character('l') => {
                    // TODO: duplicated with fn toggle_mouselook_mode() because of borrow conflicts
                    let new_state = !self.mouselook_mode.get();
                    self.mouselook_mode.set(new_state);
                    if new_state {
                        // Clear delta tracking just in case
                        self.mouse_previous_pixel_position = None;
                    }
                }
                Key::Character('o') => {
                    if let Some(settings) = settings {
                        settings.mutate_graphics_options(|options| {
                            options.transparency = match options.transparency {
                                TransparencyOption::Surface => TransparencyOption::Volumetric,
                                TransparencyOption::Volumetric => {
                                    TransparencyOption::Threshold(zo32(0.5))
                                }
                                TransparencyOption::Threshold(_) => TransparencyOption::Surface,
                                _ => TransparencyOption::Surface, // TODO: either stop doing cycle-commands or put it on the enum so it can be exhaustive
                            };
                        });
                    }
                }
                Key::Character('p') => {
                    // TODO: eliminate this weird binding once escape-based pausing is working well
                    if let Some(paused) = paused_opt {
                        paused.update_mut(|p| *p = !*p);
                    }
                }
                Key::Character('u') => {
                    if let Some(settings) = settings {
                        settings.mutate_graphics_options(|options| {
                            options.fog = match options.fog {
                                FogOption::None => FogOption::Abrupt,
                                FogOption::Abrupt => FogOption::Compromise,
                                FogOption::Compromise => FogOption::Physical,
                                FogOption::Physical => FogOption::None,
                                _ => FogOption::None, // TODO: either stop doing cycle-commands or put it on the enum so it can be exhaustive
                            };
                        });
                    }
                }
                Key::Character('y') => {
                    if let Some(settings) = settings {
                        settings.mutate_graphics_options(|options| {
                            options.render_method = match options.render_method {
                                RenderMethod::Mesh => RenderMethod::Reference,
                                RenderMethod::Reference => RenderMethod::Mesh,
                                _ => RenderMethod::Reference,
                            };
                        });
                    }
                }
                Key::Character('`' | '~') => {
                    if let Some(ch) = control_channel {
                        let _ = ch.try_send(ControlMessage::EnterDebug);
                    }
                }
                Key::Character(numeral) if numeral.is_ascii_digit() => {
                    let digit = numeral.to_digit(10).unwrap() as inv::Ix;
                    let slot = (digit + 9).rem_euclid(10); // wrap 0 to 9
                    if let (Some(universe), Some(character_handle)) = (&mut universe, character_opt)
                    {
                        universe.mutate_component(
                            character_handle,
                            |c: &mut character::Input| {
                                c.set_selected_slots[1] = Some(slot);
                            },
                        )?;
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// Returns a source which reports whether the mouselook mode (mouse movement is
    /// interpreted as view rotation) is currently active.
    ///
    /// This value may be toggled by in-game UI.
    pub fn mouselook_mode(&self) -> listen::DynSource<bool> {
        self.mouselook_mode.as_source()
    }

    pub(crate) fn toggle_mouselook_mode(&mut self) {
        self.set_mouselook_mode(!self.mouselook_mode.get());
    }

    /// Activates or deactivates mouselook mode, identically to user input doing so.
    pub fn set_mouselook_mode(&mut self, active: bool) {
        // Note: duplicated with the keybinding impl because of borrow conflicts
        let was_active = self.mouselook_mode.get();
        self.mouselook_mode.set(active);
        if active && !was_active {
            // Clear delta tracking just in case
            self.mouse_previous_pixel_position = None;
        }
    }

    /// Returns the position which should be used for click/cursor raycasting.
    /// This is not necessarily equal to the tracked mouse position.
    ///
    /// Returns [`None`] if the mouse position is out of bounds, the window has lost
    /// focus, or similar conditions under which no cursor should be shown.
    pub fn cursor_ndc_position(&self) -> Option<NdcPoint2> {
        if self.mouselook_mode.get() {
            Some(NdcPoint2::origin())
        } else {
            self.mouse_ndc_position
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

/// Things needed to apply input.
///
/// Missing inputs will cause input to be ignored.
/// TODO: Specify a warning reporting scheme.
#[derive(Debug, Default)]
#[non_exhaustive]
pub(crate) struct InputTargets<'a> {
    pub universe: Option<&'a mut Universe>,
    pub character: Option<&'a Handle<Character>>,
    pub paused: Option<&'a listen::Cell<bool>>,
    pub settings: Option<&'a Settings>,
    // TODO: replace cells with control channel?
    // TODO: make the control channel a type alias?
    pub control_channel: Option<&'a flume::Sender<ControlMessage>>,
    pub ui: Option<&'a crate::ui_content::Vui>,
}

/// A platform-neutral representation of keyboard keys for [`InputProcessor`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Key {
    /// Letters should be lowercase.
    Character(char),
    /// Escape key (or controller ‘start’ or mobile ‘back’).
    Escape,
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
    use all_is_cubes::euclid::vec3;
    use all_is_cubes::space::Space;
    use all_is_cubes::time;
    use all_is_cubes::universe::{Handle, ReadTicket};

    fn apply_input_helper(
        input: &mut InputProcessor,
        universe: &mut Universe,
        character: &Handle<Character>,
    ) {
        input
            .apply_input(
                InputTargets {
                    universe: Some(universe),
                    character: Some(character),
                    paused: None,
                    settings: None,
                    control_channel: None,
                    ui: None,
                },
                Tick::arbitrary(),
            )
            .unwrap();
    }

    #[test]
    fn movement() {
        let mut input = InputProcessor::new();
        assert_eq!(input.movement(), vec3(0.0, 0.0, 0.0));
        input.key_down(Key::Character('d'));
        assert_eq!(input.movement(), vec3(1.0, 0.0, 0.0));
        input.key_down(Key::Character('a'));
        assert_eq!(input.movement(), vec3(0.0, 0.0, 0.0));
        input.key_up(Key::Character('d'));
        assert_eq!(input.movement(), vec3(-1.0, 0.0, 0.0));
    }

    #[test]
    fn focus_lost_cancels_keys() {
        let mut input = InputProcessor::new();
        assert_eq!(input.movement(), FreeVector::zero());
        input.key_down(Key::Character('d'));
        assert_eq!(input.movement(), vec3(1., 0., 0.));
        input.key_focus(false);
        assert_eq!(input.movement(), FreeVector::zero()); // Lost focus, no movement.

        // Confirm that keys work again afterward.
        input.key_focus(true);
        assert_eq!(input.movement(), FreeVector::zero());
        input.key_down(Key::Character('d'));
        assert_eq!(input.movement(), vec3(1., 0., 0.));
        // TODO: test (and handle) key events arriving while focus is lost, just in case.
    }

    /// Test when the pause menu (or any menu, really) is displayed,
    /// mouselook is ended so the mouse  can interact with the menus.
    #[macro_rules_attribute::apply(smol_macros::test)]
    async fn pause_menu_cancels_mouselook() {
        let paused = listen::Cell::new(false);
        // TODO: This test is both verbose and expensive.
        // We need simpler way to create a cheap Vui for a test, or some abstraction here.
        let (cctx, _) = flume::bounded(1);
        let mut ui = crate::ui_content::Vui::new(crate::ui_content::UiTargets {
            mouselook_mode: listen::constant(false),
            character_source: listen::constant(None),
            paused: paused.as_source(),
            graphics_options: listen::constant(Default::default()),
            app_control_channel: cctx,
            viewport_source: listen::constant(Viewport::ARBITRARY),
            fullscreen_mode: listen::constant(None),
            set_fullscreen: None,
            quit: None,
            custom_commands: listen::constant(Default::default()),
        })
        .await;

        // Create input processor and set it to have mouselook mode
        let mut input = InputProcessor::new();
        input.toggle_mouselook_mode();
        assert!(input.mouselook_mode().get());

        // No effect when unpaused...
        input
            .apply_input(
                InputTargets {
                    ui: Some(&ui),
                    ..InputTargets::default()
                },
                Tick::arbitrary(),
            )
            .unwrap();
        assert!(input.mouselook_mode().get());

        // But when paused, the UI enters the pause menu and, as a consequence, cancels mouselook.
        paused.set(true);
        ui.step(Tick::arbitrary(), time::Deadline::Asap, ReadTicket::stub());
        input
            .apply_input(
                InputTargets {
                    ui: Some(&ui),
                    ..InputTargets::default()
                },
                Tick::arbitrary(),
            )
            .unwrap();
        assert!(!input.mouselook_mode().get());
    }

    #[test]
    fn slot_selection() {
        let u = &mut Universe::new();
        let space = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let character = u
            .insert(
                "c".into(),
                Character::spawn_default(u.read_ticket(), space).unwrap(),
            )
            .unwrap();
        let mut input = InputProcessor::new();

        input.key_down(Key::Character('5'));
        input.key_up(Key::Character('5'));
        apply_input_helper(&mut input, u, &character);
        u.step(false, time::Deadline::Whenever);
        assert_eq!(
            character.read(u.read_ticket()).unwrap().selected_slots()[1],
            4
        );

        // Tenth slot
        input.key_down(Key::Character('0'));
        input.key_up(Key::Character('0'));
        apply_input_helper(&mut input, u, &character);
        u.step(false, time::Deadline::Whenever);
        assert_eq!(
            character.read(u.read_ticket()).unwrap().selected_slots()[1],
            9
        );
    }

    // TODO: test jump and flying logic
}
