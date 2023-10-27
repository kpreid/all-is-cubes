use alloc::sync::Arc;
use core::fmt;
use core::future::Future;
use core::marker::PhantomData;
use core::task::{Context, Poll};
use std::sync::mpsc::{self, TryRecvError};

use futures_core::future::BoxFuture;
use futures_task::noop_waker_ref;

use all_is_cubes::arcstr::{self, ArcStr};
use all_is_cubes::camera::{GraphicsOptions, StandardCameras, UiViewState, Viewport};
use all_is_cubes::character::{Character, Cursor};
use all_is_cubes::fluff::Fluff;
use all_is_cubes::inv::ToolError;
use all_is_cubes::listen::{
    self, Listen as _, ListenableCell, ListenableCellWithLocal, ListenableSource, Listener,
    Notifier,
};
use all_is_cubes::space::{self, Space};
use all_is_cubes::time::{self, Duration};
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{self, URef, Universe, UniverseStepInfo};
use all_is_cubes::util::{Fmt, Refmt as _, StatusText, YieldProgressBuilder};

use crate::apps::{FpsCounter, FrameClock, InputProcessor, InputTargets};
use crate::ui_content::Vui;

const LOG_FIRST_FRAMES: bool = false;

/// A game session; a bundle of a [`Universe`] and supporting elements such as
/// a [`FrameClock`] and UI state.
///
/// Once we have multiplayer / client-server support, this will become the client-side
/// structure.
///
/// `I` controls the time source used for frame pacing and performance logging.
/// TODO: This will probably become some general “scheduler” trait.
pub struct Session<I> {
    /// Determines the timing of simulation and drawing. The caller must arrange
    /// to advance time in the clock.
    pub frame_clock: FrameClock<I>,

    /// Handles (some) user input. The caller must provide input events/state to this.
    /// [`Session`] will handle applying it to the game state.
    pub input_processor: InputProcessor,

    graphics_options: ListenableCell<GraphicsOptions>,

    game_universe: Universe,
    game_character: ListenableCellWithLocal<Option<URef<Character>>>,
    space_watch_state: SpaceWatchState,

    /// If present, a future that should be polled to produce a new [`Universe`]
    /// to replace `self.game_universe`. See [`Self::set_universe_async`].
    game_universe_in_progress: Option<BoxFuture<'static, Result<Universe, ()>>>,

    /// Outputs [`Fluff`] from the game character's viewpoint and also the session UI.
    //---
    // TODO: should include spatial information and source information
    fluff_notifier: Arc<Notifier<Fluff>>,

    paused: ListenableCell<bool>,

    ui: Option<Vui>,

    /// Messages for controlling the state that aren't via [`InputProcessor`].
    ///
    /// TODO: This is originally a quick kludge to make onscreen UI buttons work.
    /// Not sure whether it is a good strategy overall.
    control_channel: mpsc::Receiver<ControlMessage>,
    control_channel_sender: mpsc::SyncSender<ControlMessage>,

    /// Last cursor raycast result.
    /// TODO: This needs to handle clicking on the HUD and thus explicitly point into
    /// one of two different spaces.
    cursor_result: Option<Cursor>,

    last_step_info: UniverseStepInfo,

    tick_counter_for_logging: u8,
}

impl<I: fmt::Debug> fmt::Debug for Session<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            frame_clock,
            input_processor,
            graphics_options,
            game_universe,
            game_character,
            space_watch_state,
            game_universe_in_progress,
            fluff_notifier,
            paused,
            ui,
            control_channel: _,
            control_channel_sender: _,
            cursor_result,
            last_step_info,
            tick_counter_for_logging,
        } = self;

        f.debug_struct("Session")
            .field("frame_clock", frame_clock)
            .field("input_processor", input_processor)
            .field("graphics_options", graphics_options)
            .field("game_universe", game_universe)
            .field("game_character", game_character)
            .field("space_watch_state", space_watch_state)
            .field(
                "game_universe_in_progress",
                &game_universe_in_progress.as_ref().map(|_| "..."),
            )
            .field("fluff_notifier", fluff_notifier)
            .field("paused", &paused)
            .field("ui", &ui)
            .field("cursor_result", &cursor_result)
            .field("last_step_info", &last_step_info)
            .field("tick_counter_for_logging", &tick_counter_for_logging)
            .finish_non_exhaustive()
    }
}

impl<I: time::Instant> Session<I> {
    /// Returns a [`SessionBuilder`] with which to construct a new [`Session`].
    pub fn builder() -> SessionBuilder<I> {
        SessionBuilder::default()
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

        self.sync_character_space();
    }

    /// Set the character which this session is “looking through the eyes of”.
    /// It must be from the universe previously set with `set_universe()`.
    pub fn set_character(&mut self, character: Option<URef<Character>>) {
        if let Some(character) = &character {
            assert!(character.universe_id() == Some(self.game_universe.universe_id()));
        }

        self.game_character.set(character);
        self.sync_character_space();
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

    /// Returns the current game universe owned by this session.
    pub fn universe(&self) -> &Universe {
        &self.game_universe
    }

    /// Returns a mutable reference to the [`Universe`].
    ///
    /// Note: Replacing the universe will not update the UI and character state.
    /// Use [`Self::set_universe`] instead.
    pub fn universe_mut(&mut self) -> &mut Universe {
        &mut self.game_universe
    }

    /// What the renderer should be displaying on screen for the UI.
    pub fn ui_view(&self) -> ListenableSource<UiViewState> {
        match &self.ui {
            Some(ui) => ui.view(),
            None => ListenableSource::constant(UiViewState::default()), // TODO: cache this to allocate less
        }
    }

    /// Allows reading, and observing changes to, the current graphics options.
    pub fn graphics_options(&self) -> ListenableSource<GraphicsOptions> {
        self.graphics_options.as_source()
    }

    /// Allows setting the current graphics options.
    pub fn graphics_options_mut(&self) -> &ListenableCell<GraphicsOptions> {
        &self.graphics_options
    }

    /// Create [`StandardCameras`] which may be used in rendering a view of this session.
    pub fn create_cameras(&self, viewport_source: ListenableSource<Viewport>) -> StandardCameras {
        StandardCameras::new(
            self.graphics_options(),
            viewport_source,
            self.character(),
            self.ui_view(),
        )
    }

    /// Listen for [`Fluff`] events from this session. Fluff constitutes short-duration
    /// sound or particle effects.
    pub fn listen_fluff(&self, listener: impl Listener<Fluff> + Send + Sync + 'static) {
        self.fluff_notifier.listen(listener)
    }

    /// Steps the universe if the `FrameClock` says it's time to do so.
    /// Always returns info for the last step even if multiple steps were taken.
    ///
    /// Also applies input from the control channel. TODO: Should that be separate?
    pub fn maybe_step_universe(&mut self) -> Option<UniverseStepInfo> {
        self.sync_character_space();

        'handle_message: loop {
            match self.control_channel.try_recv() {
                Ok(msg) => match msg {
                    ControlMessage::Back => {
                        // TODO: error reporting … ? hm.
                        if let Some(ui) = &mut self.ui {
                            ui.back();
                        }
                    }
                    ControlMessage::Save => {
                        // TODO: Make this asynchronous. We will need to suspend normal
                        // stepping during that period.
                        let u = &self.game_universe;
                        let fut = u.whence.save(
                            u,
                            YieldProgressBuilder::new()
                                .yield_using(|_| async {}) // noop yield
                                .build(),
                        );
                        match futures_util::FutureExt::now_or_never(fut) {
                            Some(Ok(())) => {
                                // TODO: show a momentary "Saved!" message
                            }
                            Some(Err(e)) => {
                                self.show_modal_message(arcstr::format!(
                                    "{}",
                                    all_is_cubes::util::ErrorChain(&*e)
                                ));
                                continue 'handle_message;
                            }
                            None => {
                                self.show_modal_message(
                                    "unsupported: saving did not complete synchronously".into(),
                                );
                                continue 'handle_message;
                            }
                        }
                    }
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
                    break;
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
        for _ in 0..FrameClock::<I>::CATCH_UP_STEPS {
            if self.frame_clock.should_step() {
                let u_clock = self.game_universe.clock();
                let paused = *self.paused.get();
                let ui_tick = u_clock.next_tick(false);
                let game_tick = u_clock.next_tick(paused);

                self.frame_clock.did_step(u_clock.schedule());

                if let Some(character_ref) = self.game_character.borrow() {
                    self.input_processor.apply_input(
                        InputTargets {
                            universe: Some(&mut self.game_universe),
                            character: Some(character_ref),
                            paused: Some(&self.paused),
                            graphics_options: Some(&self.graphics_options),
                            control_channel: Some(&self.control_channel_sender),
                        },
                        game_tick,
                    );
                }
                // TODO: switch from FrameClock tick to asking the universe for its tick
                self.input_processor.step(game_tick);

                // TODO(time-budget): better timing policy that explicitly trades off with time spent
                // on rendering, event handling, etc.
                // (That policy should probably live in `frame_clock`.)
                let deadline = time::Deadline::At(I::now() + game_tick.delta_t() / 4);

                // TODO(time-budget): give UI a minimum fraction of budget
                let mut info = self.game_universe.step(paused, deadline);
                if let Some(ui) = &mut self.ui {
                    info += ui.step(ui_tick, deadline);
                }

                if LOG_FIRST_FRAMES && self.tick_counter_for_logging <= 10 {
                    self.tick_counter_for_logging = self.tick_counter_for_logging.saturating_add(1);
                    log::debug!(
                        "tick={} step {}",
                        self.tick_counter_for_logging,
                        info.computation_time.refmt(&StatusText)
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

    /// Returns the [`Cursor`] computed by the last call to [`Session::update_cursor()`].
    pub fn cursor_result(&self) -> Option<&Cursor> {
        self.cursor_result.as_ref()
    }

    /// Returns the suggested mouse-pointer/cursor appearance for the current [`Cursor`]
    /// as computed by the last call to [`Session::update_cursor()`].
    ///
    /// Note that this does not report any information about whether the pointer should be
    /// *hidden*. (TODO: Should we change that?)
    pub fn cursor_icon(&self) -> &CursorIcon {
        match self.cursor_result {
            // TODO: add more distinctions.
            // * Non-clickable UI should get normal arrow cursor.
            // * Maybe a lack-of-world should be indicated with a disabled cursor.
            None => &CursorIcon::Crosshair,
            Some(_) => &CursorIcon::PointingHand,
        }
    }

    /// Display a dialog box with a message. The user can exit the dialog box to return
    /// to the previous UI page.
    ///
    /// The message may contain newlines and will be word-wrapped.
    ///
    /// If this session was constructed without UI then the message will be logged instead.
    ///
    /// Caution: calling this repeatedly will currently result in stacking up arbitrary
    /// numbers of dialogs. Avoid using it for situations not in response to user action.
    pub fn show_modal_message(&mut self, message: ArcStr) {
        if let Some(ui) = &mut self.ui {
            ui.show_modal_message(message);
        } else {
            log::info!("UI message not shown: {message}");
        }
    }

    /// Handle a mouse-click event, at the position specified by the last
    /// [`Self::update_cursor()`].
    ///
    /// TODO: Clicks should be passed through `InputProcessor` instead of being an entirely separate path.
    pub fn click(&mut self, button: usize) {
        // TODO: This function has no tests.

        let result = self.click_impl(button);

        // Now, do all the _reporting_ of the tool's success or failure.
        // (The architectural reason this isn't inside of the use_tool() itself is so that
        // it is possible to use a tool more silently. That may or may not be a good idea.)

        if let Err(error @ ToolError::Internal(_)) = &result {
            // Log the message because the UI text field currently doesn't
            // fit long errors at all.
            log::error!(
                "Error applying tool: {error}",
                error = all_is_cubes::util::ErrorChain(&error)
            );
        }

        if let Err(error) = &result {
            for fluff in error.fluff() {
                self.fluff_notifier.notify(fluff);
            }
        } else {
            // success effects should come from the tool's transaction
        }

        if let Some(ui) = &self.ui {
            ui.show_click_result(button, result);
        }
    }

    /// Implementation of click interpretation logic, called by [`Self::click`].
    /// TODO: This function needs tests.
    fn click_impl(&mut self, button: usize) -> Result<(), ToolError> {
        let cursor_space = self.cursor_result.as_ref().map(|c| c.space());
        // TODO: A better condition for this would be "is one of the spaces in the UI universe"
        if cursor_space == Option::as_ref(&self.ui_view().get().space) {
            // TODO: refactor away unwrap
            self.ui
                .as_mut()
                .unwrap()
                .click(button, self.cursor_result.clone())
        } else {
            // Otherwise, it's a click inside the game world (even if the cursor hit nothing at all).
            // Character::click will validate against being a click in the wrong space.
            if let Some(character_ref) = self.game_character.borrow() {
                let transaction =
                    Character::click(character_ref.clone(), self.cursor_result.as_ref(), button)?;
                transaction
                    .execute(self.universe_mut(), &mut transaction::no_outputs)
                    .map_err(|e| ToolError::Internal(e.to_string()))?;

                // Spend a little time doing light updates, to ensure that changes right in front of
                // the player are clean (and not flashes of blackness).
                if let Some(space_ref) = self.cursor_result.as_ref().map(Cursor::space) {
                    // TODO: make this a kind of SpaceTransaction, eliminating this try_modify.
                    let _ = space_ref.try_modify(|space| {
                        space.update_lighting_from_queue::<I>(Some(Duration::from_millis(1)));
                    });
                }

                Ok(())
            } else {
                Err(ToolError::NoTool)
            }
        }
    }

    /// Check if the current game character's current space differs from the current
    /// `SpaceWatchState`, and update the latter if so.
    fn sync_character_space(&mut self) {
        let character_read: Option<universe::UBorrow<Character>> = self
            .game_character
            .borrow()
            .as_ref()
            .map(|cref| cref.read().expect("TODO: decide how to handle error"));
        let space: Option<&URef<Space>> = character_read.as_ref().map(|ch| &ch.space);

        if space != self.space_watch_state.space.as_ref() {
            self.space_watch_state = SpaceWatchState::new(space.cloned(), &self.fluff_notifier)
                .expect("TODO: decide how to handle error");
        }
    }

    /// Returns textual information intended to be overlaid as a HUD on top of the rendered scene
    /// containing diagnostic information about rendering and stepping.
    pub fn info_text<T: Fmt<StatusText>>(&self, render: T) -> InfoText<'_, I, T> {
        if LOG_FIRST_FRAMES && self.tick_counter_for_logging <= 10 {
            log::debug!(
                "tick={} draw {}",
                self.tick_counter_for_logging,
                render.refmt(&StatusText)
            )
        }

        InfoText {
            session: self,
            render,
        }
    }

    #[doc(hidden)] // TODO: Decide whether we want FpsCounter in our public API
    pub fn draw_fps_counter(&self) -> &FpsCounter<I> {
        self.frame_clock.draw_fps_counter()
    }
}

/// Builder for providing the configuration of a new [`Session`].
#[derive(Clone)]
#[must_use]
#[allow(missing_debug_implementations)]
pub struct SessionBuilder<I> {
    viewport_for_ui: Option<ListenableSource<Viewport>>,

    fullscreen_state: ListenableSource<FullscreenState>,
    set_fullscreen: FullscreenSetter,

    quit: Option<QuitFn>,

    _instant: PhantomData<I>,
}

impl<I> Default for SessionBuilder<I> {
    fn default() -> Self {
        Self {
            viewport_for_ui: None,
            fullscreen_state: ListenableSource::constant(None),
            set_fullscreen: None,
            quit: None,
            _instant: PhantomData,
        }
    }
}

impl<I: time::Instant> SessionBuilder<I> {
    /// Create the [`Session`] with configuration from this builder.
    ///
    /// This is an async function for the sake of cancellation and optional cooperative
    /// multitasking, while constructing the initial state. It may safely be blocked on
    /// from a synchronous context.
    pub async fn build(self) -> Session<I> {
        let Self {
            viewport_for_ui,
            fullscreen_state,
            set_fullscreen,
            quit,
            _instant: _,
        } = self;
        let game_universe = Universe::new();
        let game_character = ListenableCellWithLocal::new(None);
        let input_processor = InputProcessor::new();
        let graphics_options = ListenableCell::new(GraphicsOptions::default());
        let paused = ListenableCell::new(false);
        let (control_send, control_recv) = mpsc::sync_channel(100);

        let space_watch_state = SpaceWatchState::empty();

        Session {
            ui: match viewport_for_ui {
                Some(viewport) => Some(
                    Vui::new(
                        &input_processor,
                        game_character.as_source(),
                        paused.as_source(),
                        graphics_options.as_source(),
                        control_send.clone(),
                        viewport,
                        fullscreen_state,
                        set_fullscreen,
                        quit,
                    )
                    .await,
                ),
                None => None,
            },
            frame_clock: FrameClock::new(game_universe.clock().schedule()),
            input_processor,
            graphics_options,
            game_character,
            game_universe,
            space_watch_state,
            game_universe_in_progress: None,
            fluff_notifier: Arc::new(Notifier::new()),
            paused,
            control_channel: control_recv,
            control_channel_sender: control_send,
            cursor_result: None,
            last_step_info: UniverseStepInfo::default(),
            tick_counter_for_logging: 0,
        }
    }

    /// Enable graphical user interface.
    ///
    /// Requires knowing the expected viewport so that UI can be laid out to fit the aspect
    /// ratio.
    ///
    /// If this is not called, then the session will simulate a world but not present any
    /// controls for it other than those provided directly by the [`InputProcessor`].
    pub fn ui(mut self, viewport: ListenableSource<Viewport>) -> Self {
        self.viewport_for_ui = Some(viewport);
        self
    }

    /// Enable awareness of whether the session is being displayed full-screen.
    ///
    /// * `state` should report the current state (`true` = is full screen).
    ///   A `None` value means the state is unknown.
    /// * `setter` is a function which attempts to change the fullscreen state.
    pub fn fullscreen(
        mut self,
        state: ListenableSource<Option<bool>>,
        setter: Option<Arc<dyn Fn(bool) + Send + Sync>>,
    ) -> Self {
        self.fullscreen_state = state;
        self.set_fullscreen = setter;
        self
    }

    /// Enable a “quit”/“exit” command in the session's user interface.
    ///
    /// This does not cause the session to self-destruct; rather, the provided callback
    /// function should cause the session’s owner to stop presenting it to the user.
    pub fn quit(mut self, quit_fn: QuitFn) -> Self {
        self.quit = Some(quit_fn);
        self
    }
}

// TODO: these should be in one struct or something.
pub(crate) type FullscreenState = Option<bool>;
pub(crate) type FullscreenSetter = Option<Arc<dyn Fn(bool) + Send + Sync>>;

/// A message sent to the [`Session`], such as from a user interface element.
#[non_exhaustive]
pub(crate) enum ControlMessage {
    /// Perform the conventional escape/back/pause function:
    /// * navigate towards the root of a menu tree
    /// * if at the root, return to game
    /// * if in-game, pause and open menu
    Back,

    /// Save the game universe back to its [`WhenceUniverse`].
    Save,

    TogglePause,

    ToggleMouselook,

    /// TODO: this should be "modify user preferences", from which graphics options are derived.
    ModifyGraphicsOptions(Box<dyn FnOnce(Arc<GraphicsOptions>) -> Arc<GraphicsOptions> + Send>),
}

impl fmt::Debug for ControlMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Manual implementation required due to contained function.
        match self {
            Self::Back => write!(f, "Back"),
            Self::Save => write!(f, "Save"),
            Self::TogglePause => write!(f, "TogglePause"),
            Self::ToggleMouselook => write!(f, "ToggleMouselook"),
            Self::ModifyGraphicsOptions(_f) => f
                .debug_struct("ModifyGraphicsOptions")
                .finish_non_exhaustive(),
        }
    }
}

/// Bundle of things relating to the [`Session`] watching a particular [`Space`] that
/// its [`Character`] is in.
#[derive(Debug)]
struct SpaceWatchState {
    /// Which space this relates to watching.
    space: Option<URef<Space>>,

    /// Gates the message forwarding from the `space` to `Session::fluff_notifier`.
    #[allow(dead_code)] // acts upon being dropped
    fluff_gate: listen::Gate,
    // /// Camera state copied from the character, for use by fluff forwarder.
    // camera: Camera,
}

impl SpaceWatchState {
    fn new(
        space: Option<URef<Space>>,
        fluff_notifier: &Arc<listen::Notifier<Fluff>>,
    ) -> Result<Self, universe::RefError> {
        if let Some(space) = space {
            let space_read = space.read()?;
            let (fluff_gate, fluff_forwarder) =
                listen::Notifier::forwarder(Arc::downgrade(fluff_notifier))
                    .filter(|sf: space::SpaceFluff| {
                        // TODO: do not discard spatial information; and add source information
                        Some(sf.fluff)
                    })
                    .gate();
            space_read.fluff().listen(fluff_forwarder);
            Ok(Self {
                space: Some(space),
                fluff_gate,
            })
        } else {
            Ok(Self::empty())
        }
    }

    fn empty() -> SpaceWatchState {
        Self {
            space: None,
            fluff_gate: listen::Gate::default(),
        }
    }
}

/// Displayable data returned by [`Session::info_text()`].
#[derive(Copy, Clone, Debug)]
pub struct InfoText<'a, I, T> {
    session: &'a Session<I>,
    render: T,
}

impl<I: time::Instant, T: Fmt<StatusText>> fmt::Display for InfoText<'_, I, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(character_ref) = self.session.game_character.borrow() {
            write!(f, "{}", character_ref.read().unwrap().refmt(&StatusText)).unwrap();
        }
        write!(
            f,
            "\n\n{:#?}\n\nFPS: {:2.1}\n{:#?}\n\n",
            self.session.last_step_info.refmt(&StatusText),
            self.session
                .frame_clock
                .draw_fps_counter()
                .frames_per_second(),
            self.render.refmt(&StatusText),
        )?;
        match self.session.cursor_result() {
            Some(cursor) => write!(f, "{cursor}"),
            None => write!(f, "No block"),
        }?;
        Ok(())
    }
}

/// Suggested mouse pointer appearance for a given [`Cursor`] state.
///
/// Obtain this from [`Session::cursor_icon()`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum CursorIcon {
    /// The platform's default appearance; often an arrowhead.
    Normal,
    /// A crosshair “┼”, suggesting aiming/positioning/selecting.
    Crosshair,
    /// A hand with finger extended as if to press a button.
    PointingHand,
}

/// TODO: this should be an async fn
pub(crate) type QuitFn = Arc<dyn Fn() -> Result<QuitSucceeded, QuitCancelled> + Send + Sync>;

/// Return type of a [`SessionBuilder::quit()`] callback on successful quit.
/// This is uninhabited (cannot happen) since the callback should never be observed to
/// finish if it successfully quits.
pub type QuitSucceeded = std::convert::Infallible;

/// Return type of a [`SessionBuilder::quit()`] callback if other considerations cancelled
/// the quit operation. In this case, the session will return to normal operation.
#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct QuitCancelled;

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::character::CharacterTransaction;
    use all_is_cubes::math::Cube;
    use all_is_cubes::space::{Space, SpaceTransaction};
    use all_is_cubes::transaction::no_outputs;
    use all_is_cubes::universe::{Name, Universe};
    use futures_channel::oneshot;

    #[tokio::test]
    async fn fluff_forwarding_following() {
        // Create universe members
        let mut u = Universe::new();
        let space1 = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let space2 = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let character = u.insert_anonymous(Character::spawn_default(space1.clone()));
        let st = SpaceTransaction::fluff(Cube::ORIGIN, Fluff::Happened);

        // Create session
        let mut session = Session::<std::time::Instant>::builder().build().await;
        session.set_universe(u);
        session.set_character(Some(character.clone()));
        let sink = listen::Sink::<Fluff>::new();
        session.listen_fluff(sink.listener());

        // Try some fluff with the initial state (we haven't even stepped the session)
        space1.execute(&st, &mut no_outputs).unwrap();
        assert_eq!(sink.drain(), vec![Fluff::Happened]);

        // Change spaces
        character
            .execute(
                &CharacterTransaction::move_to_space(space2.clone()),
                &mut no_outputs,
            )
            .unwrap();
        session.maybe_step_universe();

        // Check we're now listening to the new space only
        space1.execute(&st, &mut no_outputs).unwrap();
        assert_eq!(sink.drain(), vec![]);
        space2.execute(&st, &mut no_outputs).unwrap();
        assert_eq!(sink.drain(), vec![Fluff::Happened]);
    }

    #[tokio::test]
    async fn set_universe_async() {
        let old_marker = Name::from("old");
        let new_marker = Name::from("new");
        let mut session = Session::<std::time::Instant>::builder().build().await;
        session
            .universe_mut()
            .insert(old_marker.clone(), Space::empty_positive(1, 1, 1))
            .unwrap();

        // Set up async loading but don't deliver anything yet
        let (send, recv) = oneshot::channel();
        session.set_universe_async(async { recv.await.unwrap() });

        // Existing universe should still be present.
        session.maybe_step_universe();
        assert!(session.universe_mut().get::<Space>(&old_marker).is_some());

        // Deliver new universe.
        let mut new_universe = Universe::new();
        new_universe
            .insert(new_marker.clone(), Space::empty_positive(1, 1, 1))
            .unwrap();
        send.send(Ok(new_universe)).unwrap();

        // Receive it.
        session.maybe_step_universe();
        assert!(session.universe_mut().get::<Space>(&new_marker).is_some());
        assert!(session.universe_mut().get::<Space>(&old_marker).is_none());

        // Verify cleanup (that the next step can succeed).
        session.maybe_step_universe();
    }
}
