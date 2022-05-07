// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Components for "apps", or game clients: user interface and top-level state.

use std::fmt::{self, Display};
use std::future::Future;
use std::mem;
use std::sync::mpsc::{self, TryRecvError};
use std::sync::Arc;
use std::task::{Context, Poll};

use cgmath::{One, Point2};
use futures_core::future::BoxFuture;
use futures_task::noop_waker_ref;

use crate::camera::{Camera, GraphicsOptions, Viewport};
use crate::character::{cursor_raycast, Character, Cursor};
use crate::inv::{Tool, ToolError, ToolInput};
use crate::listen::{DirtyFlag, ListenableCell, ListenableCellWithLocal, ListenableSource};
use crate::math::FreeCoordinate;
use crate::space::Space;
use crate::transaction::Transaction;
use crate::universe::{URef, Universe, UniverseStepInfo};
use crate::util::{CustomFormat, StatusText};
use crate::vui::Vui;

mod input;
pub use input::*;

mod time;
pub use time::*;

const LOG_FIRST_FRAMES: bool = false;

/// A game session; a bundle of a [`Universe`] and supporting elements such as
/// a [`FrameClock`] and UI state.
///
/// Once we have multiplayer / client-server support, this will become the client-side
/// structure.
pub struct Session {
    /// Determines the timing of simulation and drawing. The caller must arrange
    /// to advance time in the clock.
    pub frame_clock: FrameClock,

    /// Handles (some) user input. The caller must provide input events/state;
    /// [`Session`] will handle calling [`InputProcessor::apply_input`].
    pub input_processor: InputProcessor,

    graphics_options: ListenableCell<GraphicsOptions>,

    game_universe: Universe,
    game_character: ListenableCellWithLocal<Option<URef<Character>>>,

    /// If present, a future that should be polled to produce a new [`Universe`]
    /// to replace `self.game_universe`. See [`Self::set_universe_async`].
    game_universe_in_progress: Option<BoxFuture<'static, Result<Universe, ()>>>,

    paused: ListenableCell<bool>,

    ui: Vui,

    /// Messages for controlling the state that aren't via [`InputProcessor`].
    ///
    /// TODO: This is originally a quick kludge to make onscreen UI buttons work.
    /// Not sure whether it is a good strategy overall.
    control_channel: mpsc::Receiver<ControlMessage>,

    /// Last cursor raycast result.
    /// TODO: This needs to handle clicking on the HUD and thus explicitly point into
    /// one of two different spaces.
    cursor_result: Option<Cursor>,

    last_step_info: UniverseStepInfo,

    tick_counter_for_logging: u8,
    // When adding fields, remember to update the `Debug` impl.
}

impl fmt::Debug for Session {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Session")
            .field("frame_clock", &self.frame_clock)
            .field("input_processor", &self.input_processor)
            .field("graphics_options", &self.graphics_options)
            .field("game_universe", &self.game_universe)
            .field("game_character", &self.game_character)
            .field(
                "game_universe_in_progress",
                &self.game_universe_in_progress.as_ref().map(|_| "..."),
            )
            .field("paused", &self.paused)
            .field("ui", &self.ui)
            .field("cursor_result", &self.cursor_result)
            .field("last_step_info", &self.last_step_info)
            .field("tick_counter_for_logging", &self.tick_counter_for_logging)
            .finish_non_exhaustive()
    }
}

impl Session {
    /// Construct a new [`Session`] with an empty [`Universe`].
    ///
    /// This is an async function for the sake of cancellation and optional cooperative
    /// multitasking, while constructing the initial state. It may safely be blocked on
    /// from a synchronous context.
    #[allow(clippy::new_without_default)]
    pub async fn new() -> Self {
        let game_universe = Universe::new();
        let game_character = ListenableCellWithLocal::new(None);
        let input_processor = InputProcessor::new();
        let graphics_options = ListenableCell::new(GraphicsOptions::default());
        let paused = ListenableCell::new(false);
        let (control_send, control_recv) = mpsc::sync_channel(100);

        Self {
            ui: Vui::new(
                &input_processor,
                game_character.as_source(),
                paused.as_source(),
                graphics_options.as_source(),
                control_send,
            )
            .await,

            frame_clock: FrameClock::new(),
            input_processor,
            graphics_options,
            game_character,
            game_universe,
            game_universe_in_progress: None,
            paused,
            control_channel: control_recv,
            cursor_result: None,
            last_step_info: UniverseStepInfo::default(),
            tick_counter_for_logging: 0,
        }
    }

    /// Returns a source for the [`Character`] that should be shown to the user.
    pub fn character(&self) -> ListenableSource<Option<URef<Character>>> {
        self.game_character.as_source()
    }

    /// Replace the game universe, such as on initial startup or because the player
    /// chose to load a new one.
    pub fn set_universe(&mut self, u: Universe) {
        // Clear any previous set_universe_async.
        self.game_universe_in_progress = None;

        self.game_universe = u;
        self.game_character
            .set(self.game_universe.get_default_character());
    }

    /// Perform [`Self::set_universe`] on the result of the provided future when it
    /// completes.
    ///
    /// This is intended to be used for simultaneously initializing the UI and universe.
    /// Later upgrades might might add a loading screen.
    ///
    /// The future will be cancelled if [`Self::set_universe_async`] or
    /// [`Self::set_universe`] is called before it completes.
    /// Currently, the future is polled once per frame unconditionally.
    ///
    /// If the future returns `Err`, then the current universe is not replaced. There is
    /// not any mechanism to display an error message; that must be done separately.
    pub fn set_universe_async<F>(&mut self, future: F)
    where
        F: Future<Output = Result<Universe, ()>> + Send + 'static,
    {
        self.game_universe_in_progress = Some(Box::pin(future));
    }

    /// Returns a mutable reference to the [`Universe`].
    ///
    /// Note: Replacing the universe will not update the UI and character state.
    /// Use [`Self::set_universe`] instead.
    pub fn universe_mut(&mut self) -> &mut Universe {
        &mut self.game_universe
    }

    pub fn ui_space(&self) -> ListenableSource<Option<URef<Space>>> {
        self.ui.current_space()
    }

    pub fn graphics_options(&self) -> ListenableSource<GraphicsOptions> {
        self.graphics_options.as_source()
    }

    pub fn graphics_options_mut(&self) -> &ListenableCell<GraphicsOptions> {
        &self.graphics_options
    }

    /// Steps the universe if the `FrameClock` says it's time to do so.
    /// Always returns info for the last step even if multiple steps were taken.
    ///
    /// Also applies input from the control channel. TODO: Should that be separate?
    pub fn maybe_step_universe(&mut self) -> Option<UniverseStepInfo> {
        loop {
            match self.control_channel.try_recv() {
                Ok(msg) => match msg {
                    ControlMessage::TogglePause => {
                        self.paused.set(!*self.paused.get());
                    }
                    ControlMessage::ToggleMouselook => {
                        self.input_processor.toggle_mouselook_mode();
                    }
                    ControlMessage::ModifyGraphicsOptions(f) => {
                        self.graphics_options.set(f(self.graphics_options.get()));
                    }
                },
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    // Lack of whatever control sources is non-fatal.
                }
            }
        }

        if let Some(future) = self.game_universe_in_progress.as_mut() {
            match future
                .as_mut()
                .poll(&mut Context::from_waker(noop_waker_ref()))
            {
                Poll::Pending => {}
                Poll::Ready(result) => {
                    self.game_universe_in_progress = None;
                    match result {
                        Ok(universe) => {
                            self.set_universe(universe);
                        }
                        Err(()) => {
                            // No error reporting, for now; let it be the caller's resposibility
                            // (which we indicate by making the error type be ()).
                            // There should be something, but it's not clear what; perhaps
                            // it will become clearer as the UI gets fleshed out.
                        }
                    }
                }
            }
        }

        let mut result = None;
        // TODO: Catch-up implementation should probably live in FrameClock.
        for _ in 0..FrameClock::CATCH_UP_STEPS {
            if self.frame_clock.should_step() {
                let base_tick = self.frame_clock.tick();
                let game_tick = if *self.paused.get() {
                    base_tick.pause()
                } else {
                    base_tick
                };
                self.frame_clock.did_step();

                if let Some(character_ref) = self.game_character.borrow() {
                    self.input_processor.apply_input(
                        InputTargets {
                            universe: Some(&mut self.game_universe),
                            character: Some(character_ref),
                            paused: Some(&self.paused),
                            graphics_options: Some(&self.graphics_options),
                        },
                        game_tick,
                    );
                }
                self.input_processor.step(game_tick);

                let mut info = self.game_universe.step(game_tick);

                info += self.ui.step(base_tick);

                if LOG_FIRST_FRAMES && self.tick_counter_for_logging <= 10 {
                    self.tick_counter_for_logging = self.tick_counter_for_logging.saturating_add(1);
                    log::debug!(
                        "tick={} step {}",
                        self.tick_counter_for_logging,
                        info.computation_time.custom_format(StatusText)
                    );
                }
                self.last_step_info = info.clone();
                result = Some(info);
            }
        }
        result
    }

    /// Call this once per frame to update the cursor raycast.
    ///
    /// TODO: bad API; revisit general cursor handling logic.
    /// We'd like to not have too much dependencies on the rendering, but also
    /// not obligate each platform/renderer layer to have too much boilerplate.
    pub fn update_cursor(&mut self, cameras: &StandardCameras) {
        self.cursor_result = self
            .input_processor
            .cursor_ndc_position()
            .and_then(|ndc_pos| cameras.project_cursor(ndc_pos));
    }

    pub fn cursor_result(&self) -> &Option<Cursor> {
        &self.cursor_result
    }

    /// Handle a mouse-click event, at the position specified by the last
    /// [`Self::update_cursor()`].
    ///
    /// TODO: Clicks should be passed through `InputProcessor` instead of being an entirely separate path.
    pub fn click(&mut self, button: usize) {
        match self.click_impl(button) {
            Ok(()) => {}
            Err(e) => self.ui.show_tool_error(e),
        }
    }

    /// Implementation of click interpretation logic, called by [`Self::click`].
    /// TODO: This function needs tests.
    fn click_impl(&mut self, button: usize) -> Result<(), ToolError> {
        let cursor_space = self.cursor_result.as_ref().map(|c| &c.space);
        if cursor_space == Option::as_ref(&self.ui_space().get()) {
            // Clicks on UI use `Tool::Activate`.
            // TODO: We'll probably want to distinguish buttons eventually.
            // TODO: It should be easier to use a tool
            let transaction = Tool::Activate.use_immutable_tool(&ToolInput {
                cursor: self.cursor_result.clone(),
                character: None,
            })?;
            transaction
                .execute(self.universe_mut()) // TODO: wrong universe
                .map_err(|e| ToolError::Internal(e.to_string()))?;
            Ok(())
        } else {
            // Otherwise, it's a click inside the game world (even if the cursor hit nothing at all).
            // TODO: if the cursor space is not the game space this should be an error
            if let Some(character_ref) = self.game_character.borrow() {
                let transaction =
                    Character::click(character_ref.clone(), self.cursor_result.as_ref(), button)?;
                transaction
                    .execute(self.universe_mut())
                    .map_err(|e| ToolError::Internal(e.to_string()))?;

                // Spend a little time doing light updates, to ensure that changes right in front of
                // the player are clean (and not flashes of blackness).
                if let Some(space_ref) = self.cursor_result.as_ref().map(|c| &c.space) {
                    // TODO: Instead of ignoring error, log it
                    let _ = space_ref.try_modify(|space| {
                        space.update_lighting_from_queue();
                    });
                }

                Ok(())
            } else {
                Err(ToolError::NoTool)
            }
        }
    }

    /// Returns textual information intended to be overlaid as a HUD on top of the rendered scene
    /// containing diagnostic information about rendering and stepping.
    pub fn info_text<T: CustomFormat<StatusText>>(&self, render: T) -> InfoText<'_, T> {
        if LOG_FIRST_FRAMES && self.tick_counter_for_logging <= 10 {
            log::debug!(
                "tick={} draw {}",
                self.tick_counter_for_logging,
                render.custom_format(StatusText)
            )
        }

        InfoText {
            session: self,
            render,
        }
    }

    #[doc(hidden)] // TODO: Decide whether we want FpsCounter in our public API
    pub fn draw_fps_counter(&self) -> &FpsCounter {
        self.frame_clock.draw_fps_counter()
    }
}

/// A collection of values associated with each of the layers of graphics that
/// is normally drawn (HUD on top of world, currently).
// Exhaustive: Changing this will probably be breaking anyway, until we make it a
// more thorough abstraction.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct Layers<T> {
    pub world: T,
    pub ui: T,
}

impl<T> Layers<T> {
    #[doc(hidden)] // used in related crates, but it's ugly and experimental
    pub fn try_map_ref<U, E>(&self, mut f: impl FnMut(&T) -> Result<U, E>) -> Result<Layers<U>, E> {
        Ok(Layers {
            world: f(&self.world)?,
            ui: f(&self.ui)?,
        })
    }
}

#[derive(Debug)]
pub struct StandardCameras {
    /// Cameras are synced with this
    graphics_options: ListenableSource<GraphicsOptions>,
    graphics_options_dirty: DirtyFlag,

    character_source: ListenableSource<Option<URef<Character>>>,
    /// Tracks whether the character was replaced (not whether its view changed).
    character_dirty: DirtyFlag,
    character: Option<URef<Character>>,
    /// Cached and listenable version of character's space.
    /// TODO: This should be in a Layers along with ui_space.
    world_space: ListenableCell<Option<URef<Space>>>,

    ui_space_source: ListenableSource<Option<URef<Space>>>,
    ui_space_dirty: DirtyFlag,
    ui_space: Option<URef<Space>>,

    viewport_dirty: bool,

    cameras: Layers<Camera>,
}

impl StandardCameras {
    /// Most general constructor; private because the details needed might vary.
    fn new(
        graphics_options: ListenableSource<GraphicsOptions>,
        viewport: Viewport,
        character_source: ListenableSource<Option<URef<Character>>>,
        ui_space_source: ListenableSource<Option<URef<Space>>>,
    ) -> Result<Self, std::convert::Infallible> {
        // TODO: Add a unit test that each of these listeners works as intended.
        // TODO: This is also an awful lot of repetitive code; we should design a pattern
        // to not have it (some kind of "following cell")?
        let graphics_options_dirty = DirtyFlag::listening(false, |l| graphics_options.listen(l));
        let initial_options = &*graphics_options.get();

        let mut this = Self {
            graphics_options,
            graphics_options_dirty,

            character_dirty: DirtyFlag::listening(true, |l| character_source.listen(l)),
            character_source,
            character: None, // update() will fix these up
            world_space: ListenableCell::new(None),

            ui_space: ui_space_source.snapshot(),
            ui_space_dirty: DirtyFlag::listening(true, |l| ui_space_source.listen(l)),
            ui_space_source,

            viewport_dirty: true,

            cameras: Layers {
                ui: Camera::new(Vui::graphics_options(initial_options.clone()), viewport),
                world: Camera::new(initial_options.clone(), viewport),
            },
        };

        this.update();
        Ok(this)
    }

    /// Constructs a [`StandardCameras`] that will display, and track, the current state of the [`Session`]
    pub fn from_session(
        session: &Session,
        viewport: Viewport,
    ) -> Result<Self, std::convert::Infallible> {
        Self::new(
            session.graphics_options(),
            viewport,
            session.character(),
            session.ui_space(),
        )
    }

    /// Updates camera state from data sources.
    ///
    /// This should be called at the beginning of each frame or as needed when the
    /// cameras are to be used.
    pub fn update(&mut self) {
        let options_dirty = self.graphics_options_dirty.get_and_clear();
        if options_dirty {
            let current_options = self.graphics_options.snapshot();
            self.cameras.world.set_options(current_options.clone());
            self.cameras
                .ui
                .set_options(Vui::graphics_options(current_options));
        }

        let ui_space_dirty = self.ui_space_dirty.get_and_clear();
        if ui_space_dirty {
            self.ui_space = self.ui_space_source.snapshot();
            if self.ui_space.is_none() {
                // Reset transform so it isn't a *stale* transform.
                // TODO: set an error flag saying that nothing should be drawn
                self.cameras.ui.set_view_transform(One::one());
            }
        }

        // Update UI view if the FOV changed or the viewport did
        let viewport_dirty = mem::take(&mut self.viewport_dirty);
        if options_dirty || viewport_dirty || ui_space_dirty {
            if let Some(space_ref) = &self.ui_space {
                // TODO: try_borrow()
                // TODO: ...or just skip the whole idea
                self.cameras.ui.set_view_transform(Vui::view_transform(
                    &*space_ref.borrow(),
                    self.cameras.ui.fov_y(),
                ));
            }
        }

        if self.character_dirty.get_and_clear() {
            self.character = self.character_source.snapshot();
            if self.character.is_none() {
                // Reset transform so it isn't a *stale* transform.
                // TODO: set an error flag saying that nothing should be drawn
                self.cameras.world.set_view_transform(One::one());
            }
        }

        if let Some(character_ref) = &self.character {
            match character_ref.try_borrow() {
                Ok(character) => {
                    // TODO: Shouldn't we also grab the character's Space while we
                    // have the access? Renderers could use that.
                    self.cameras.world.set_view_transform(character.view());

                    // TODO: ListenableCell should make this easier and cheaper
                    if Option::as_ref(&*self.world_space.get()) != Some(&character.space) {
                        self.world_space.set(Some(character.space.clone()));
                    }
                }
                Err(_) => {
                    // TODO: set an error flag indicating failure to update
                }
            }
        } else {
            if *self.world_space.get() != None {
                self.world_space.set(None);
            }
        }
    }

    pub fn graphics_options(&self) -> &GraphicsOptions {
        self.cameras.world.options()
    }

    /// Returns [`Camera`]s appropriate for drawing each graphical layer.
    pub fn cameras(&self) -> &Layers<Camera> {
        &self.cameras
    }

    /// Returns the character's viewpoint to draw in the world layer.
    /// May be [`None`] if there is no current character.
    pub fn character(&self) -> Option<&URef<Character>> {
        self.character.as_ref()
    }

    /// Returns the space that should be drawn as the game world, using `self.cameras().world`.
    ///
    /// This is a [`ListenableSource`] to make it simple to cache the Space rendering data and
    /// follow space transitions.
    /// It updates when [`Self::update()`] is called.
    pub fn world_space(&self) -> ListenableSource<Option<URef<Space>>> {
        self.world_space.as_source()
    }

    /// Returns the UI space, that should be drawn on top of the world using `self.cameras().ui`.
    ///
    /// TODO: Make this also a ListenableSource
    pub fn ui_space(&self) -> Option<&URef<Space>> {
        self.ui_space.as_ref()
    }

    pub fn viewport(&self) -> Viewport {
        self.cameras.world.viewport()
    }

    pub fn set_viewport(&mut self, viewport: Viewport) {
        // TODO: this should be an iter_mut() or something
        self.cameras.world.set_viewport(viewport);
        self.cameras.ui.set_viewport(viewport);
    }

    /// Perform a raycast through these cameras to find what the cursor hits.
    ///
    /// Make sure to call [`StandardCameras::update`] first so that the cameras are
    /// up to date with game state.
    pub fn project_cursor(&self, ndc_pos: Point2<FreeCoordinate>) -> Option<Cursor> {
        if let Some(ui_space_ref) = self.ui_space.as_ref() {
            let ray = self.cameras.ui.project_ndc_into_world(ndc_pos);
            if let Some(cursor) = cursor_raycast(ray, ui_space_ref, FreeCoordinate::INFINITY) {
                return Some(cursor);
            }
        }

        if let Some(character_ref) = self.character.as_ref() {
            let ray = self.cameras.world.project_ndc_into_world(ndc_pos);
            // TODO: maximum distance should be determined by character/universe parameters instead of hardcoded
            if let Some(cursor) = cursor_raycast(ray, &character_ref.borrow().space, 6.0) {
                return Some(cursor);
            }
        }

        None
    }
}

/// A message sent to the [`Session`], such as from a user interface element.
// TODO: make public if this proves to be a good approach
#[non_exhaustive]
pub(crate) enum ControlMessage {
    TogglePause,
    ToggleMouselook,
    /// TODO: this should be "modify user preferences", from which graphics options are derived.
    ModifyGraphicsOptions(Box<dyn FnOnce(Arc<GraphicsOptions>) -> Arc<GraphicsOptions> + Send>),
}

#[derive(Copy, Clone, Debug)]
pub struct InfoText<'a, T> {
    session: &'a Session,
    render: T,
}

impl<T: CustomFormat<StatusText>> Display for InfoText<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(character_ref) = self.session.game_character.borrow() {
            write!(f, "{}", character_ref.borrow().custom_format(StatusText)).unwrap();
        }
        write!(
            f,
            "\n\n{:#?}\n\nFPS: {:2.1}\n{:#?}\n\n",
            self.session.last_step_info.custom_format(StatusText),
            self.session
                .frame_clock
                .draw_fps_counter()
                .frames_per_second(),
            self.render.custom_format(StatusText),
        )?;
        match self.session.cursor_result() {
            Some(cursor) => write!(f, "{}", cursor),
            None => write!(f, "No block"),
        }?;
        Ok(())
    }
}

impl Clone for StandardCameras {
    /// Returns a [`StandardCameras`] which tracks the same data sources (graphics
    /// options, scene sources) as `self`, but whose local state (such as viewport size
    /// and last updated camera state) is independent.
    fn clone(&self) -> Self {
        Self::new(
            self.graphics_options.clone(),
            self.viewport(),
            self.character_source.clone(),
            self.ui_space_source.clone(),
        )
        .unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::space::Space;
    use crate::universe::{Name, Universe, UniverseIndex};
    use futures_channel::oneshot;
    use futures_executor::block_on;

    #[test]
    fn set_universe_async() {
        let old_marker = Name::from("old");
        let new_marker = Name::from("new");
        let mut session = block_on(Session::new());
        session
            .universe_mut()
            .insert(old_marker.clone(), Space::empty_positive(1, 1, 1))
            .unwrap();

        // Set up async loading but don't deliver anything yet
        let (send, recv) = oneshot::channel();
        session.set_universe_async(async { recv.await.unwrap() });

        // Existing universe should still be present.
        session.maybe_step_universe();
        assert!(UniverseIndex::<Space>::get(session.universe_mut(), &old_marker).is_some());

        // Deliver new universe.
        let mut new_universe = Universe::new();
        new_universe
            .insert(new_marker.clone(), Space::empty_positive(1, 1, 1))
            .unwrap();
        send.send(Ok(new_universe)).unwrap();

        // Receive it.
        session.maybe_step_universe();
        assert!(UniverseIndex::<Space>::get(session.universe_mut(), &new_marker).is_some());
        assert!(UniverseIndex::<Space>::get(session.universe_mut(), &old_marker).is_none());

        // Verify cleanup (that the next step can succeed).
        session.maybe_step_universe();
    }

    #[test]
    fn cameras_follow_character_and_world() {
        let mut session = block_on(Session::new());
        let mut cameras = StandardCameras::from_session(&session, Viewport::ARBITRARY).unwrap();

        let world_source = cameras.world_space();
        let flag = DirtyFlag::listening(false, |l| world_source.listen(l));
        assert_eq!(world_source.snapshot().as_ref(), None);

        // No redundant notification when world is absent
        cameras.update();
        assert!(!flag.get_and_clear());

        // Create a universe with space and character
        // TODO: This has to be a new one because there currently isn't an Session::set_character()!
        let mut universe = Universe::new();
        let space_ref = universe.insert_anonymous(Space::empty_positive(1, 1, 1));
        // TODO: "character" is a special default name used for finding the character the
        // player actually uses, and we should replace that or handle it more formally.
        universe
            .insert(
                "character".into(),
                Character::spawn_default(space_ref.clone()),
            )
            .unwrap();
        session.set_universe(universe);

        // Now the world_source should be reporting the new space
        assert!(!flag.get_and_clear());
        cameras.update();
        assert!(flag.get_and_clear());
        assert_eq!(world_source.snapshot().as_ref(), Some(&space_ref));

        // No redundant notification when world is present
        cameras.update();
        assert!(!flag.get_and_clear());

        // TODO: test further changes
    }

    #[test]
    fn cameras_clone() {
        let session = block_on(Session::new());
        let mut cameras = StandardCameras::from_session(&session, Viewport::ARBITRARY).unwrap();
        let mut cameras2 = cameras.clone();

        let default_o = GraphicsOptions::default();
        let mut different_o = default_o.clone();
        different_o.debug_chunk_boxes = true;
        session.graphics_options_mut().set(different_o.clone());

        // Each `StandardCameras` has independent updating from the same data sources.
        assert_eq!(cameras.cameras().world.options(), &default_o);
        assert_eq!(cameras2.cameras().world.options(), &default_o);
        cameras.update();
        assert_eq!(cameras.cameras().world.options(), &different_o);
        assert_eq!(cameras2.cameras().world.options(), &default_o);
        cameras2.update();
        assert_eq!(cameras.cameras().world.options(), &different_o);
        assert_eq!(cameras2.cameras().world.options(), &different_o);
    }
}
