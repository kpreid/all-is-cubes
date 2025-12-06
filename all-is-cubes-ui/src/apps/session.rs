use alloc::boxed::Box;
use alloc::string::ToString as _;
use alloc::sync::{Arc, Weak};
use core::fmt;
use core::mem;
use core::pin::Pin;
use core::task::{Context, Poll, Waker};
use std::sync::RwLock;

use flume::TryRecvError;
use futures_core::future::BoxFuture;
use sync_wrapper::SyncWrapper;

use all_is_cubes::arcstr::{self, ArcStr, literal};
use all_is_cubes::character::{self, Character, Cursor};
use all_is_cubes::fluff::Fluff;
use all_is_cubes::inv::ToolError;
use all_is_cubes::listen::{self, Listen as _, Listener as _, Source as _};
use all_is_cubes::math::FreePoint;
use all_is_cubes::save::WhenceUniverse;
use all_is_cubes::sound;
use all_is_cubes::space::{self, Space};
use all_is_cubes::time::{self, Duration};
use all_is_cubes::transaction::{self, Transaction as _};
use all_is_cubes::universe::{
    self, Handle, ReadTicket, StrongHandle, Universe, UniverseId, UniverseStepInfo,
    UniverseTransaction,
};
use all_is_cubes::util::{
    ConciseDebug, Fmt, Refmt as _, ShowStatus, StatusText, YieldProgress, YieldProgressBuilder,
};
use all_is_cubes_render::camera::{
    GraphicsOptions, Layers, StandardCameras, UiViewState, Viewport,
};

use crate::apps::{FpsCounter, FrameClock, InputProcessor, InputTargets};
use crate::settings::Settings;
use crate::ui_content::Vui;
use crate::ui_content::notification::{self, Notification};
use crate::vui::widgets::ProgressBarState;

const LOG_FIRST_FRAMES: bool = false;

const SHUTTLE_PANIC_MSG: &str = "Shuttle not returned to Session; \
    this indicates something went wrong with main task execution";

/// A game session; a bundle of a [`Universe`] and supporting elements such as
/// a [`FrameClock`] and UI state.
///
/// Once we have multiplayer / client-server support, this will become the client-side
/// structure.
pub struct Session {
    /// Determines the timing of simulation and drawing. The caller must arrange
    /// to advance time in the clock.
    pub frame_clock: FrameClock,

    /// Handles (some) user input. The caller must provide input events/state to this.
    /// [`Session`] will handle applying it to the game state.
    pub input_processor: InputProcessor,

    /// The game universe and other parts of the session that can be mutated by the
    /// main task. See [`Shuttle`]'s documentation.
    ///
    /// Boxed to make the move a cheap pointer move, since `Shuttle` is a large struct.
    shuttle: Option<Box<Shuttle>>,

    /// If present, a future that is polled at the beginning of stepping,
    /// which may read or write parts of the session state via the context it was given.
    ///
    /// The `SyncWrapper` ensures that `Session: Sync` even though this future need not be
    /// (which is sound because the future is only polled with `&mut Session`).
    ///
    main_task: Option<SyncWrapper<BoxFuture<'static, ExitMainTask>>>,

    /// Jointly owned by the main task.
    /// The `Option` is filled only when the main task is executing.
    /// The `RwLock` is never blocked on.
    task_context_inner: Arc<RwLock<Option<Box<Shuttle>>>>,

    paused: listen::Cell<bool>,

    ambient_sound_source: listen::DynSource<sound::SpatialAmbient>,

    /// Messages for controlling the state that aren't via [`InputProcessor`].
    ///
    /// TODO: This is originally a quick kludge to make onscreen UI buttons work.
    /// Not sure whether it is a good strategy overall.
    ///
    /// Design note: Using `flume` not because we want MPMC, but because its receiver is
    /// `Send + Sync`, unlike the `std` one.
    /// Our choice of `flume` in particular is just because our other crates use it.
    control_channel: flume::Receiver<ControlMessage>,
    control_channel_sender: flume::Sender<ControlMessage>,

    last_step_info: UniverseStepInfo,

    tick_counter_for_logging: u8,
}

/// Data abstractly belonging to [`Session`] whose ownership is temporarily moved as needed.
///
/// Currently, this is between the `Session` and its [`MainTaskContext`], but in the future it
/// might also be moved to a background task to allow the session stepping to occur independent
/// of the event loop or other owner of the `Session`.
struct Shuttle {
    settings: Settings,

    /// Pre-computed clonable source for graphics options from `settings`.
    graphics_options_source: listen::DynSource<Arc<GraphicsOptions>>,

    game_universe: Universe,

    /// Subset of information from `game_universe` that is largely immutable and can be
    /// meaningfully listened to.
    game_universe_info: listen::Cell<SessionUniverseInfo>,

    /// Character we're designating as “the player character”.
    /// Always a member of `game_universe`.
    game_character: listen::CellWithLocal<Option<StrongHandle<Character>>>,

    ui: Option<Vui>,

    space_watch_state: Layers<SpaceWatchState>,

    control_channel_sender: flume::Sender<ControlMessage>,

    /// Last cursor raycast result.
    cursor_result: Option<Cursor>,

    /// Outputs [`Fluff`] from the game character's viewpoint and also the session UI.
    fluff_notifier: Arc<listen::Notifier<SessionFluff>>,

    ambient_sound_state: listen::CellWithLocal<sound::SpatialAmbient>,

    /// Notifies when the session transitions to particular states.
    session_event_notifier: Arc<listen::Notifier<Event>>,

    quit_fn: Option<QuitFn>,

    custom_commands: listen::CellWithLocal<Arc<[crate::ui_content::Command]>>,
}

impl fmt::Debug for Session {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            frame_clock,
            input_processor,
            main_task,
            task_context_inner: _,
            paused,
            ambient_sound_source: _,
            control_channel: _,
            control_channel_sender: _,
            last_step_info,
            tick_counter_for_logging,

            shuttle,
        } = self;

        let Some(shuttle) = shuttle else {
            write!(f, "Session(is in the middle of an operation)")?;
            return Ok(());
        };
        let Shuttle {
            settings,
            graphics_options_source: _,
            game_universe,
            game_universe_info,
            game_character,
            space_watch_state,
            ui,
            control_channel_sender: _,
            cursor_result,
            quit_fn,
            fluff_notifier,
            ambient_sound_state,
            session_event_notifier,
            custom_commands,
        } = &**shuttle;

        f.debug_struct("Session")
            .field("frame_clock", frame_clock)
            .field("input_processor", input_processor)
            .field("settings", settings)
            .field("game_universe", game_universe)
            .field("game_universe_info", game_universe_info)
            .field("game_character", game_character)
            .field("space_watch_state", space_watch_state)
            .field("main_task", &main_task.as_ref().map(|_| "..."))
            .field("session_event_notifier", session_event_notifier)
            .field("fluff_notifier", fluff_notifier)
            .field("ambient_sound_state", ambient_sound_state)
            .field("paused", &paused)
            .field("ui", &ui)
            .field(
                "quit_fn",
                &quit_fn.as_ref().map(|_cant_print_a_function| ()),
            )
            .field("custom_commands", &custom_commands)
            .field("cursor_result", &cursor_result)
            .field("last_step_info", &last_step_info)
            .field("tick_counter_for_logging", &tick_counter_for_logging)
            .finish_non_exhaustive()
    }
}

impl Session {
    /// Returns a [`SessionBuilder`] with which to construct a new [`Session`].
    pub fn builder() -> SessionBuilder {
        SessionBuilder::default()
    }

    #[track_caller]
    fn shuttle(&self) -> &Shuttle {
        self.shuttle.as_ref().expect(SHUTTLE_PANIC_MSG)
    }
    #[track_caller]
    fn shuttle_mut(&mut self) -> &mut Shuttle {
        self.shuttle.as_mut().expect(SHUTTLE_PANIC_MSG)
    }

    /// Returns a source for the [`Character`] that should be shown to the user.
    pub fn character(&self) -> listen::DynSource<Option<StrongHandle<Character>>> {
        self.shuttle().game_character.as_source()
    }

    /// Replaces the game universe, such as for initial setup or because the player
    /// chose to load a new one.
    /// This also resets the character to be the new universe's default character.
    pub fn set_universe(&mut self, universe: Box<Universe>) {
        self.shuttle_mut().set_universe(universe);
    }

    /// Set the character which this session is “looking through the eyes of”.
    /// It must be from the universe previously set with `set_universe()`.
    pub fn set_character(&mut self, character: Option<StrongHandle<Character>>) {
        let shuttle = self.shuttle_mut();
        if let Some(character) = &character {
            assert!(character.universe_id() == Some(shuttle.game_universe.universe_id()));
        }

        shuttle.game_character.set(character);
        shuttle.sync_universe_and_character_derived();
    }

    /// Install a main task in the session, replacing any existing one.
    ///
    /// The main task is an `async` task (that is, a [`Future`] that will be polled without further
    /// intervention) which executes interleaved with this session's routine operations done by
    /// [`Session::maybe_step_universe()`], and therefore has the opportunity to intervene in them.
    ///
    ///
    /// The main task is given a [`MainTaskContext`] by which it can manipulate the session.
    /// The context will panic if it is used at times when the main task is not running.
    pub fn set_main_task<F>(&mut self, task_ctor: F)
    where
        F: async_fn_traits::AsyncFnOnce1<
                MainTaskContext,
                Output = ExitMainTask,
                OutputFuture: Send + 'static,
            >,
    {
        let context = MainTaskContext {
            shuttle: self.task_context_inner.clone(),
            session_event_notifier: Arc::downgrade(&self.shuttle().session_event_notifier),
        };
        self.main_task = Some(SyncWrapper::new(Box::pin(task_ctor(context))));
    }

    /// Returns the current game universe owned by this session.
    pub fn universe(&self) -> &Universe {
        &self.shuttle().game_universe
    }

    /// Returns a mutable reference to the [`Universe`].
    ///
    /// Note: Replacing the universe will not update the UI and character state.
    /// Use [`Self::set_universe()`] instead.
    pub fn universe_mut(&mut self) -> &mut Universe {
        &mut self.shuttle_mut().game_universe
    }

    /// Allows observing replacement of the current universe in this session, or updates to its
    /// [`WhenceUniverse`].
    pub fn universe_info(&self) -> listen::DynSource<SessionUniverseInfo> {
        self.shuttle().game_universe_info.as_source()
    }

    /// What the renderer should be displaying on screen for the UI.
    pub fn ui_view(&self) -> listen::DynSource<Arc<UiViewState>> {
        self.shuttle().ui_view()
    }

    /// Returns [`ReadTicket`]s for the current game universe and UI universe, suitable for passing to
    /// a renderer.
    pub fn read_tickets(&self) -> Layers<ReadTicket<'_>> {
        self.shuttle().read_tickets()
    }

    /// Allows reading, and observing changes to, the current graphics options.
    pub fn graphics_options(&self) -> listen::DynSource<Arc<GraphicsOptions>> {
        Arc::new(self.shuttle().settings.as_source().map(|data| data.to_graphics_options()))
    }

    /// Allows changing the settings associated with this session.
    ///
    /// Note that these settings may be shared with other sessions.
    pub fn settings(&self) -> &Settings {
        &self.shuttle().settings
    }

    /// Returns a [`StandardCameras`] which may be used in rendering a view of this session,
    /// including following changes to the current character or universe.
    ///
    /// They will be freshly [updated][StandardCameras::update].
    pub fn create_cameras(&self, viewport_source: listen::DynSource<Viewport>) -> StandardCameras {
        let mut c = StandardCameras::new(
            self.graphics_options(),
            viewport_source,
            self.character(),
            self.ui_view(),
        );
        c.update(self.read_tickets());
        c
    }

    /// Listen for [`Fluff`] events from this session. Fluff constitutes short-duration
    /// sound or particle effects.
    pub fn listen_fluff(
        &self,
        listener: impl listen::Listener<SessionFluff> + Send + Sync + 'static,
    ) {
        self.shuttle().fluff_notifier.listen(listener)
    }

    /// Source of the current ambient sound environment from the game world.
    pub fn ambient_sound(&self) -> listen::DynSource<sound::SpatialAmbient> {
        self.ambient_sound_source.clone()
    }

    /// Steps the universe if the [`FrameClock`] says it's time to do so.
    /// Also may update other session state from any incoming events.
    ///
    /// Always returns info for the last step even if multiple steps were taken.
    pub fn maybe_step_universe(&mut self) -> Option<UniverseStepInfo> {
        self.process_control_messages_and_stuff();

        // Let main task do things before the step due to external inputs.
        self.poll_main_task();

        // Process any messages generated by the main task
        self.process_control_messages_and_stuff();

        let mut step_result = None;
        // TODO: Catch-up implementation should probably live in FrameClock.
        for _ in 0..FrameClock::CATCH_UP_STEPS {
            if self.frame_clock.should_step() {
                let shuttle = self.shuttle.as_mut().expect(SHUTTLE_PANIC_MSG);
                let u_clock = shuttle.game_universe.clock();
                let paused = self.paused.get();
                let ui_tick = u_clock.next_tick(false);
                let game_tick = u_clock.next_tick(paused);

                self.frame_clock.did_step(u_clock.schedule());

                self.input_processor
                    .apply_input(
                        InputTargets {
                            universe: Some(&mut shuttle.game_universe),
                            character: shuttle
                                .game_character
                                .get()
                                .as_ref()
                                .map(StrongHandle::as_ref),
                            paused: Some(&self.paused),
                            settings: Some(&shuttle.settings),
                            control_channel: Some(&self.control_channel_sender),
                            ui: shuttle.ui.as_ref(),
                        },
                        game_tick,
                    )
                    .expect("applying input failed");
                // TODO: switch from FrameClock tick to asking the universe for its tick
                self.input_processor.step(game_tick);

                // TODO(time-budget): better timing policy that explicitly trades off with time spent
                // on rendering, event handling, etc.
                // (That policy should probably live in `frame_clock`.)
                // TODO(time-budget): give UI a time that reflects how much time it needs, rather
                // than arbitrarily partitioning the delta_t
                let step_start_time = time::Instant::now();
                let dt = game_tick.delta_t_duration();
                let deadlines = Layers {
                    world: time::Deadline::At(step_start_time + dt / 2),
                    ui: time::Deadline::At(step_start_time + dt / 2 + dt / 4),
                };

                let mut info = shuttle.game_universe.step(paused, deadlines.world);
                if let Some(ui) = &mut shuttle.ui {
                    info += ui.step(ui_tick, deadlines.ui, shuttle.game_universe.read_ticket());
                }

                if LOG_FIRST_FRAMES && self.tick_counter_for_logging <= 10 {
                    self.tick_counter_for_logging = self.tick_counter_for_logging.saturating_add(1);
                    log::debug!(
                        "tick={} step {}",
                        self.tick_counter_for_logging,
                        info.computation_time.refmt(&ConciseDebug)
                    );
                }
                self.last_step_info = info.clone();
                step_result = Some(info);

                // --- Post-step activities ---

                shuttle.session_event_notifier.notify(&Event::Stepped);

                // Let main task do things triggered by the step.
                // Note that we do this once per step even with catch-up.
                self.poll_main_task();

                // Process any messages generated by stepping (from a behavior or main task).
                self.process_control_messages_and_stuff();
            }
        }
        step_result
    }

    /// Call this each time something happens the main task might care about.
    fn poll_main_task(&mut self) {
        // TODO: for efficiency, use a waker
        if let Some(sync_wrapped_future) = self.main_task.as_mut() {
            {
                let mut shuttle_guard = match self.task_context_inner.write() {
                    Ok(g) => g,
                    Err(poisoned) => poisoned.into_inner(),
                };
                *shuttle_guard = Some(
                    self.shuttle
                        .take()
                        .expect("Session lost its shuttle before polling the main task"),
                );
            }
            // Reset on drop even if the future panics
            let _reset_on_drop = scopeguard::guard((), |()| {
                let mut shuttle_guard = match self.task_context_inner.write() {
                    Ok(g) => g,
                    Err(poisoned) => poisoned.into_inner(),
                };
                if let Some(shuttle) = shuttle_guard.take() {
                    self.shuttle = Some(shuttle);
                } else {
                    log::error!(
                        "Session lost its shuttle while polling the main task and will be unusable"
                    );
                }
                *shuttle_guard = None;
            });

            let future: Pin<&mut dyn Future<Output = ExitMainTask>> =
                sync_wrapped_future.get_mut().as_mut();
            match future.poll(&mut Context::from_waker(Waker::noop())) {
                Poll::Pending => {}
                Poll::Ready(ExitMainTask) => {
                    self.main_task = None;
                }
            }
        }
    }

    fn process_control_messages_and_stuff(&mut self) {
        loop {
            match self.control_channel.try_recv() {
                Ok(msg) => match msg {
                    ControlMessage::Back => {
                        // TODO: error reporting … ? hm.
                        if let Some(ui) = &mut self.shuttle_mut().ui {
                            ui.back();
                        }
                    }
                    ControlMessage::ShowModal(message) => {
                        self.show_modal_message(message);
                    }
                    ControlMessage::EnterDebug => {
                        let shuttle = self.shuttle_mut();
                        if let Some(ui) = &mut shuttle.ui {
                            if let Some(cursor) = &shuttle.cursor_result {
                                ui.enter_debug(shuttle.game_universe.read_ticket(), cursor);
                            } else {
                                // TODO: not actually a click
                                ui.show_click_result(usize::MAX, Err(ToolError::NothingSelected));
                            }
                        }
                    }
                    ControlMessage::Save => {
                        // TODO: Make this asynchronous. We will need to suspend normal
                        // stepping during that period.
                        let u = &self.shuttle().game_universe;
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
                            }
                            None => {
                                self.show_modal_message(
                                    "unsupported: saving did not complete synchronously".into(),
                                );
                            }
                        }
                    }
                    ControlMessage::TogglePause => {
                        self.paused.set(!self.paused.get());
                    }
                    ControlMessage::ToggleMouselook => {
                        self.input_processor.toggle_mouselook_mode();
                    }
                },
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    // Lack of whatever control sources is non-fatal.
                    break;
                }
            }
        }

        self.shuttle_mut().sync_universe_and_character_derived();
    }

    /// Call this once per frame to update the cursor raycast.
    ///
    /// TODO: bad API; revisit general cursor handling logic.
    /// We'd like to not have too much dependencies on the rendering, but also
    /// not obligate each platform/renderer layer to have too much boilerplate.
    pub fn update_cursor(&mut self, cameras: &StandardCameras) {
        let cursor_result = self.input_processor.cursor_ndc_position().and_then(|ndc_pos| {
            cameras
                .project_cursor(self.read_tickets(), ndc_pos)
                .expect("shouldn't happen: read error in update_cursor()")
        });
        self.shuttle_mut().cursor_result = cursor_result;
    }

    /// Returns the [`Cursor`] computed by the last call to [`Session::update_cursor()`].
    pub fn cursor_result(&self) -> Option<&Cursor> {
        self.shuttle().cursor_result.as_ref()
    }

    /// Returns the suggested mouse-pointer/cursor appearance for the current [`Cursor`]
    /// as computed by the last call to [`Session::update_cursor()`].
    ///
    /// Note that this does not report any information about whether the pointer should be
    /// *hidden*. (TODO: Should we change that?)
    pub fn cursor_icon(&self) -> &CursorIcon {
        match self.shuttle().cursor_result {
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
        if let Some(ui) = &mut self.shuttle_mut().ui {
            ui.show_modal_message(message);
        } else {
            log::info!("UI message not shown: {message}");
        }
    }

    /// Display a notification to the user. Notifications persist until dismissed or the returned
    /// [`Notification`] handle is dropped, and their content may be updated through that handle.
    ///
    /// Returns an error if there is no UI to display notifications or if there are too many.
    pub fn show_notification(
        &mut self,
        content: impl Into<notification::NotificationContent>,
    ) -> Result<Notification, notification::Error> {
        // TODO: stop requiring mut by using a dedicated channel...?
        match &mut self.shuttle_mut().ui {
            Some(ui) => Ok(ui.show_notification(content)),
            None => Err(notification::Error::NoUi),
        }
    }

    /// Handle a mouse-click event, at the position specified by the last
    /// [`Self::update_cursor()`].
    ///
    /// TODO: Clicks should be passed through `InputProcessor` instead of being an entirely separate path.
    pub fn click(&mut self, button: usize) {
        // TODO: This function has no tests.

        let result = self.shuttle_mut().click_impl(button);

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
            let shuttle = self.shuttle();
            for fluff in error.fluff() {
                shuttle.fluff_notifier.notify(&SessionFluff {
                    fluff,
                    source: SessionFluffSource::NonSpatial,
                });
            }
        } else {
            // success effects should come from the tool's transaction
        }

        if let Some(ui) = &self.shuttle_mut().ui {
            ui.show_click_result(button, result);
        }
    }

    /// Invoke the [`SessionBuilder::quit()`] callback as if the user clicked a quit button inside
    /// our UI.
    ///
    /// This may be used in response to a window's close button, for example.
    ///
    /// The session state *may* decline to actually call the callback, such as if there are
    /// user-visible unsaved changes.
    ///
    /// The returned future will produce a [`QuitCancelled`] value if quitting was unsuccessful for
    /// any reason. If it is successful, the future never resolves. It is not necessary to poll
    /// the future if the result value is not wanted.
    pub fn quit(&self) -> impl Future<Output = QuitResult> + Send + 'static + use<> {
        self.shuttle().quit()
    }

    /// Returns textual information intended to be overlaid as a HUD on top of the rendered scene
    /// containing diagnostic information about rendering and stepping.
    pub fn info_text<T: Fmt<StatusText>>(&self, render: T) -> InfoText<'_, T> {
        let fopt = StatusText {
            show: self.shuttle().graphics_options_source.get().debug_info_text_contents,
        };

        if LOG_FIRST_FRAMES && self.tick_counter_for_logging <= 10 {
            log::debug!(
                "tick={} draw {}",
                self.tick_counter_for_logging,
                render.refmt(&fopt)
            )
        }

        InfoText {
            session: self,
            render,
            fopt,
        }
    }

    #[doc(hidden)] // TODO: Decide whether we want FpsCounter in our public API
    pub fn draw_fps_counter(&self) -> &FpsCounter {
        self.frame_clock.draw_fps_counter()
    }
}

/// Methods on `Shuttle` are those operations that can be called from both [`Session`] and
/// [ `MainTaskContext`].
impl Shuttle {
    fn set_universe(&mut self, universe: Box<Universe>) {
        self.game_universe = *universe;
        self.game_character
            .set(self.game_universe.get_default_character().map(StrongHandle::new));

        self.sync_universe_and_character_derived();
        // TODO: Need to sync FrameClock's schedule with the universe in case it is different
    }

    fn quit(&self) -> impl Future<Output = QuitResult> + Send + 'static + use<> {
        let fut: BoxFuture<'static, QuitResult> = match (&self.ui, &self.quit_fn) {
            (Some(ui), _) => Box::pin(ui.quit()),
            (None, Some(quit_fn)) => Box::pin(std::future::ready(quit_fn())),
            (None, None) => Box::pin(std::future::ready(Err(QuitCancelled::Unsupported))),
        };
        fut
    }

    /// What the renderer should be displaying on screen for the UI.
    fn ui_view(&self) -> listen::DynSource<Arc<UiViewState>> {
        match &self.ui {
            Some(ui) => ui.view(),
            None => listen::constant(Arc::new(UiViewState::default())), // TODO: cache this to allocate less
        }
    }

    /// Implementation of click interpretation logic, called by [`Self::click`].
    /// TODO: This function needs tests.
    fn click_impl(&mut self, button: usize) -> Result<(), ToolError> {
        let cursor_space = self.cursor_result.as_ref().map(|c| c.space());
        // TODO: A better condition for this would be "is one of the spaces in the UI universe"
        if let Some(ui) = self
            .ui
            .as_mut()
            .filter(|ui| cursor_space.is_some() && cursor_space == ui.view().get().space.as_ref())
        {
            ui.click(button, self.cursor_result.clone())
        } else {
            // Otherwise, it's a click inside the game world (even if the cursor hit nothing at all).
            // Character::click will validate against being a click in the wrong space.
            if let Some(character_handle) = self.game_character.get() {
                let transaction = Character::click(
                    self.game_universe.read_ticket(),
                    character_handle.to_weak(),
                    self.cursor_result.as_ref(),
                    button,
                )?;
                transaction
                    .execute(&mut self.game_universe, (), &mut transaction::no_outputs)
                    .map_err(|e| ToolError::Internal(e.to_string()))?;

                // Spend a little time doing light updates, to ensure that changes right in front of
                // the player are clean (and not flashes of blackness).
                if let Some(space_handle) = self.cursor_result.as_ref().map(Cursor::space) {
                    let _ = self.game_universe.mutate_space(space_handle, |m| {
                        m.evaluate_light_for_time(Duration::from_millis(1));
                    });
                }

                Ok(())
            } else {
                Err(ToolError::NoTool)
            }
        }
    }

    /// Update derived information that might have changed.
    ///
    /// * Check if the current game character's current space differs from the current
    ///   `SpaceWatchState`, and update the latter if so.
    /// * Check if the universe or the universe's `WhenceUniverse` have changed.
    /// * Sync sound state.
    fn sync_universe_and_character_derived(&mut self) {
        // Sync game_universe_info. The WhenceUniverse might in principle be overwritten any time.
        self.game_universe_info.set_if_unequal(SessionUniverseInfo {
            id: self.game_universe.universe_id(),
            whence: Arc::clone(&self.game_universe.whence),
        });

        // Sync space_watch_state.world and ambient_sound_state.
        {
            let character: Option<character::Read<'_>> =
                self.game_character.get().as_ref().map(|cref| {
                    cref.read(self.game_universe.read_ticket())
                        .expect("TODO: decide how to handle error")
                });
            let space: Option<&Handle<Space>> = character.map(|ch| ch.space());

            if space != self.space_watch_state.world.space.as_ref() {
                self.space_watch_state.world = SpaceWatchState::new(
                    self.game_universe.read_ticket(),
                    space.cloned(),
                    &self.fluff_notifier,
                )
                .expect("TODO: decide how to handle error");
            }

            let new_sound_state: &sound::SpatialAmbient = if let Some(character) = &character {
                character.ambient_sound()
            } else {
                &sound::SpatialAmbient::SILENT
            };
            if *new_sound_state != self.ambient_sound_state.get() {
                self.ambient_sound_state.set(new_sound_state.clone());
            }
        }

        // Sync space_watch_state.ui.
        {
            // TODO: don't get() and clone every time, add a dirty flag
            let (read_ticket, space) = match self.ui.as_ref() {
                Some(ui) => (ui.read_ticket(), ui.view().get().space.clone()),
                None => (ReadTicket::stub(), None),
            };
            if space != self.space_watch_state.ui.space {
                self.space_watch_state.ui =
                    SpaceWatchState::new(read_ticket, space, &self.fluff_notifier)
                        .expect("TODO: decide how to handle error");
            }
        }
    }

    fn read_tickets(&self) -> Layers<ReadTicket<'_>> {
        Layers {
            world: self.game_universe.read_ticket(),
            ui: match &self.ui {
                Some(ui) => ui.read_ticket(),
                None => ReadTicket::stub(),
            },
        }
    }
}

/// Builder for providing the configuration of a new [`Session`].
#[derive(Clone)]
#[must_use]
#[expect(missing_debug_implementations)]
pub struct SessionBuilder {
    viewport_for_ui: Option<listen::DynSource<Viewport>>,

    fullscreen_state: listen::DynSource<FullscreenState>,
    set_fullscreen: FullscreenSetter,

    settings: Option<Settings>,

    quit: Option<QuitFn>,
}

impl Default for SessionBuilder {
    fn default() -> Self {
        Self {
            viewport_for_ui: None,
            fullscreen_state: listen::constant(None),
            set_fullscreen: None,
            settings: None,
            quit: None,
        }
    }
}

impl SessionBuilder {
    /// Create the [`Session`] with configuration from this builder.
    ///
    /// This is an async function for the sake of cancellation and optional cooperative
    /// multitasking, while constructing the initial state. It may safely be blocked on
    /// from a synchronous context.
    pub async fn build(self) -> Session {
        let Self {
            viewport_for_ui,
            fullscreen_state,
            set_fullscreen,
            settings,
            quit: quit_fn,
        } = self;

        let settings = settings.unwrap_or_else(|| Settings::new(Default::default()));
        let graphics_options_source =
            Arc::new(settings.as_source().map(|data| data.to_graphics_options()));
        let game_character = listen::CellWithLocal::new(None);
        let input_processor = InputProcessor::new();
        let paused = listen::Cell::new(false);
        let ambient_sound_state = listen::CellWithLocal::new(sound::SpatialAmbient::SILENT);
        let (control_send, control_recv) = flume::bounded(100);
        let custom_commands = listen::CellWithLocal::new([].into());

        let space_watch_state = Layers {
            world: SpaceWatchState::empty(),
            ui: SpaceWatchState::empty(),
        };

        let ui = match viewport_for_ui {
            Some(viewport) => Some(
                Vui::new(crate::ui_content::UiTargets {
                    mouselook_mode: input_processor.mouselook_mode(),
                    character_source: game_character.as_source(),
                    paused: paused.as_source(),
                    settings: settings.clone(),
                    app_control_channel: control_send.clone(),
                    viewport_source: viewport,
                    fullscreen_mode: fullscreen_state,
                    set_fullscreen,
                    quit: quit_fn.clone(),
                    custom_commands: custom_commands.as_source(),
                })
                .await,
            ),
            None => None,
        };

        // Note: Putting this after the last .await avoids the future needing to be big enough to
        // own the `Universe` (which is quite a large struct).
        let game_universe = Universe::new();

        Session {
            frame_clock: FrameClock::new(game_universe.clock().schedule()),
            ambient_sound_source: Arc::new(ambient_sound_state.as_source().map({
                // TODO: need to properly register `paused` as a dependency
                let paused = paused.as_source();
                move |sound| {
                    if paused.get() {
                        sound::SpatialAmbient::SILENT
                    } else {
                        sound
                    }
                }
            })),

            shuttle: Some(Box::new(Shuttle {
                ui,
                settings,
                graphics_options_source,
                game_character,
                game_universe_info: listen::Cell::new(SessionUniverseInfo {
                    id: game_universe.universe_id(),
                    whence: game_universe.whence.clone(),
                }),
                game_universe: *game_universe,
                space_watch_state,
                cursor_result: None,
                session_event_notifier: Arc::new(listen::Notifier::new()),
                fluff_notifier: Arc::new(listen::Notifier::new()),
                ambient_sound_state,
                control_channel_sender: control_send.clone(),
                quit_fn,
                custom_commands,
            })),
            input_processor,
            main_task: None,
            task_context_inner: Arc::new(RwLock::new(None)),
            paused,
            control_channel: control_recv,
            control_channel_sender: control_send,
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
    pub fn ui(mut self, viewport: listen::DynSource<Viewport>) -> Self {
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
        state: listen::DynSource<Option<bool>>,
        setter: Option<Arc<dyn Fn(bool) + Send + Sync>>,
    ) -> Self {
        self.fullscreen_state = state;
        self.set_fullscreen = setter;
        self
    }

    /// Enable reading and writing user settings.
    ///
    /// The session will get a new [`Settings`] object which inherits the given settings.
    ///
    /// If this is not called, then the session will have all default settings,
    /// and they will not be persisted.
    pub fn settings_from(mut self, settings: Settings) -> Self {
        self.settings = Some(Settings::inherit(settings));
        self
    }

    /// Enable a “quit”/“exit” command in the session's user interface.
    ///
    /// This does not cause the session to self-destruct; rather, the provided callback
    /// function should cause the session’s owner to stop presenting it to the user (and
    /// be dropped). It may also report that the quit was cancelled for whatever reason.
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

    ShowModal(ArcStr),

    EnterDebug,

    TogglePause,

    ToggleMouselook,
}

impl fmt::Debug for ControlMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Manual implementation required due to contained function.
        match self {
            Self::Back => write!(f, "Back"),
            Self::Save => write!(f, "Save"),
            Self::ShowModal(_msg) => f.debug_struct("ShowModal").finish_non_exhaustive(),
            Self::EnterDebug => write!(f, "EnterDebug"),
            Self::TogglePause => write!(f, "TogglePause"),
            Self::ToggleMouselook => write!(f, "ToggleMouselook"),
        }
    }
}

/// Bundle of things relating to the [`Session`] watching a particular [`Space`] that
/// its [`Character`] is in.
#[derive(Debug)]
struct SpaceWatchState {
    /// Which space this relates to watching.
    space: Option<Handle<Space>>,

    /// Gates the message forwarding from the `space` to `Session::fluff_notifier`.
    #[expect(dead_code, reason = "acts upon being dropped")]
    fluff_gate: listen::Gate,
}

impl SpaceWatchState {
    fn new(
        read_ticket: ReadTicket<'_>,
        space: Option<Handle<Space>>,
        fluff_notifier: &Arc<listen::Notifier<SessionFluff>>,
    ) -> Result<Self, universe::HandleError> {
        if let Some(space) = space {
            let space_read = space.read(read_ticket)?;
            let (fluff_gate, fluff_forwarder) =
                listen::Notifier::forwarder(Arc::downgrade(fluff_notifier))
                    .filter(move |sf: &space::SpaceFluff| {
                        Some(SessionFluff {
                            fluff: sf.fluff.clone(),
                            source: SessionFluffSource::World(sf.position.center()),
                        })
                    })
                    .with_stack_buffer::<10>() // TODO: non-arbitrary number
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

// -------------------------------------------------------------------------------------------------

/// [`Fluff`] with additional information about its origin, produced by [`Session`].
#[derive(Debug, Clone)]
#[expect(clippy::exhaustive_structs)]
pub struct SessionFluff {
    #[allow(missing_docs)]
    pub fluff: Fluff,
    /// Origin of the fluff, to be used for spatial audio, independent volume control, etc.
    pub source: SessionFluffSource,
}

/// Identifies the source of a [`SessionFluff`].
///
/// May be used to modify its rendering/playback according to spatial location or user settings.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum SessionFluffSource {
    /// Fluff is from the game world (current character’s current [`Space`]).
    World(FreePoint),
    /// Fluff is from a non-spatial source.
    /// TODO: Eventually this should be more specific, like "UI"
    NonSpatial,
}

// -------------------------------------------------------------------------------------------------

/// Information about the [`Universe`] currently owned by a [`Session`].
///
/// TODO: This suspiciously resembles a struct that should be part of the universe itself...
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct SessionUniverseInfo {
    /// The [`Universe::universe_id()`] of the universe.
    pub id: UniverseId,
    /// The [`Universe::whence`] of the universe.
    pub whence: Arc<dyn WhenceUniverse>,
}

impl PartialEq for SessionUniverseInfo {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && Arc::ptr_eq(&self.whence, &other.whence)
    }
}

/// Displayable data returned by [`Session::info_text()`].
#[derive(Copy, Clone, Debug)]
pub struct InfoText<'a, T> {
    session: &'a Session,
    render: T,
    fopt: StatusText,
}

impl<T: Fmt<StatusText>> fmt::Display for InfoText<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fopt = self.fopt;
        let mut empty = true;
        if fopt.show.contains(ShowStatus::CHARACTER)
            && let Some(shuttle) = self.session.shuttle.as_ref()
            && let Some(character_handle) = shuttle.game_character.get().as_ref()
        {
            empty = false;
            match character_handle.read(shuttle.game_universe.read_ticket()) {
                Ok(character_read) => {
                    write!(f, "{}", character_read.refmt(&fopt))?;
                }
                Err(_) => write!(f, "<error reading character>")?,
            }
        }
        if fopt.show.contains(ShowStatus::STEP) {
            if !mem::take(&mut empty) {
                write!(f, "\n\n")?;
            }
            write!(
                f,
                "{:#?}\n\nFPS: {:2.1}\n{:#?}",
                self.session.last_step_info.refmt(&fopt),
                self.session.frame_clock.draw_fps_counter().frames_per_second(),
                self.render.refmt(&fopt),
            )?;
        }
        if fopt.show.contains(ShowStatus::CURSOR) {
            if !mem::take(&mut empty) {
                write!(f, "\n\n")?;
            }
            match self.session.cursor_result() {
                Some(cursor) => write!(f, "{cursor}"),
                None => write!(f, "No block"),
            }?;
        }
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
pub(crate) type QuitFn = Arc<dyn Fn() -> QuitResult + Send + Sync>;
pub(crate) type QuitResult = Result<QuitSucceeded, QuitCancelled>;

/// Return type of a [`SessionBuilder::quit()`] callback on successful quit.
/// This is uninhabited (cannot happen) since the callback should never be observed to
/// finish if it successfully quits.
pub type QuitSucceeded = std::convert::Infallible;

/// Return type of a [`SessionBuilder::quit()`] callback if other considerations cancelled
/// the quit operation. In this case, the session will return to normal operation.
#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub enum QuitCancelled {
    /// Quitting is not a supported operation.
    ///
    /// [`SessionBuilder::quit()`] callback functions should not usually return this; this value is
    /// primarily for when there is no callback to call.
    Unsupported,

    /// Some user interaction occurred before quitting, and the user indicated that the quit
    /// should be cancelled (for example, due to unsaved changes).
    UserCancelled,
}

/// Indicates that a [`Session`]'s main task wishes to exit, leaving the session
/// only controlled externally.
///
/// This type carries no information and merely exists to distinguish an intentional exit
/// from accidentally returning early.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[expect(clippy::exhaustive_structs)]
pub struct ExitMainTask;

#[derive(Debug)]
enum Event {
    /// The session has just completed a step.
    Stepped,
}

/// Given to the task of a [`Session::set_main_task()`] to allow manipulating the session.
pub struct MainTaskContext {
    shuttle: Arc<RwLock<Option<Box<Shuttle>>>>,
    session_event_notifier: Weak<listen::Notifier<Event>>,
}

impl fmt::Debug for MainTaskContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MainTaskContext").finish_non_exhaustive()
    }
}

impl MainTaskContext {
    /// Returns a [`StandardCameras`] which may be used in rendering a view of this session,
    /// including following changes to the current character or universe.
    ///
    /// They will be freshly [updated][StandardCameras::update].
    pub fn create_cameras(&self, viewport_source: listen::DynSource<Viewport>) -> StandardCameras {
        self.with_ref(|shuttle| {
            let mut c = StandardCameras::new(
                shuttle.graphics_options_source.clone(),
                viewport_source,
                shuttle.game_character.as_source(),
                shuttle.ui_view(),
            );
            c.update(shuttle.read_tickets());
            c
        })
    }

    /// Provides a reference to the current game universe of this session.
    pub fn with_universe<R>(&self, f: impl FnOnce(&Universe) -> R) -> R {
        self.with_ref(|shuttle| f(&shuttle.game_universe))
    }

    /// Provides `ReadTicket`s for access to the game and UI universes.
    pub fn with_read_tickets<R>(&self, f: impl FnOnce(Layers<ReadTicket<'_>>) -> R) -> R {
        self.with_ref(|shuttle| f(shuttle.read_tickets()))
    }

    /// Executes a transaction on the game universe of this session..
    pub fn execute(
        &mut self,
        transaction: UniverseTransaction,
    ) -> Result<(), transaction::ExecuteError> {
        self.with_mut(|shuttle| {
            transaction.execute(&mut shuttle.game_universe, (), &mut transaction::no_outputs)
        })
    }

    /// Replaces the game universe, such as for initial setup or because the player
    /// chose to load a new one.
    /// This also resets the character to be the new universe's default character.
    ///
    /// Panics if called while the main task is suspended.
    pub fn set_universe(&mut self, universe: Box<Universe>) {
        self.with_mut(|shuttle| {
            shuttle.set_universe(universe);
        })
    }

    /// Run the given async task which will obtain a new [`Universe`] and replace the current one.
    /// Display a progress notification during this period.
    ///
    /// If possible, the produced future should do its work on a background thread
    /// to minimize impact on the session execution.
    ///
    /// Panics if called while the main task is suspended.
    //---
    // TODO: Additional UI features:
    // * Optional confirmation with preview after loading and before actually replacing
    // * Optional fade to black transition
    // Also add a test for this
    pub async fn set_universe_async<F>(&mut self, f: F)
    where
        F: async_fn_traits::AsyncFnOnce1<
                YieldProgress,
                // TODO: we need to choose some standard "rich error to report to the user" format
                Output = Result<Box<Universe>, ArcStr>,
                OutputFuture: Send + 'static,
            > + Send
            + 'static,
    {
        let notification = self
            .show_notification(notification::NotificationContent::Progress {
                title: literal!("Loading..."),
                progress: ProgressBarState::new(0.0),
                part: literal!(""),
            })
            .ok();

        let progress = YieldProgressBuilder::new()
            .progress_using(move |info| {
                if let Some(notification) = &notification {
                    notification.set_content(notification::NotificationContent::Progress {
                        title: literal!("Loading..."),
                        progress: ProgressBarState::new(info.fraction().into()),
                        part: info.label_str().into(),
                    });
                }
            })
            .build();

        match f(progress).await {
            Ok(universe) => {
                self.set_universe(universe);
            }
            Err(error_message) => {
                self.show_modal_message(error_message);
            }
        }

        // TODO: Force the notification to go away even if somebody cloned our YieldProgress
    }

    /// Allows reading or changing the settings of this session.
    ///
    /// Note that these settings may be shared with other sessions.
    pub fn settings(&self) -> Settings {
        self.with_ref(|shuttle| shuttle.settings.clone())
    }

    /// Invoke the [`SessionBuilder::quit()`] callback as if the user clicked a quit button inside
    /// our UI.
    ///
    /// This may be used in response to a window's close button, for example.
    ///
    /// The session state *may* decline to actually call the callback, such as if there are
    /// user-visible unsaved changes.
    ///
    /// The returned future will produce a [`QuitCancelled`] value if quitting was unsuccessful for
    /// any reason. If it is successful, the future never resolves. It is not necessary to poll
    /// the future if the result value is not wanted.
    pub fn quit(&self) -> impl Future<Output = QuitResult> + Send + 'static + use<> {
        self.with_ref(|shuttle| shuttle.quit())
    }

    /// Add a custom command button, which will be displayed in the pause menu.
    ///
    /// Future versions may allow such commands to be bound to keys or displayed elsewhere.
    ///
    /// Panics if called while the main task is suspended.
    //---
    // TODO: make it so that this returns a handle, or rather a "command was invoked" channel
    // in lieu of an arbitrary function, that deletes the command if dropped.
    pub fn add_custom_command(&mut self, command: crate::ui_content::Command) {
        self.with_mut(|shuttle| {
            shuttle
                .custom_commands
                .set(shuttle.custom_commands.get().iter().cloned().chain([command]).collect())
        });
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
    //---
    // TODO: Replace this entirely with `show_notification`.
    pub fn show_modal_message(&self, message: ArcStr) {
        self.with_ref(|shuttle| {
            if let Err(e) = shuttle.control_channel_sender.send(ControlMessage::ShowModal(message))
            {
                log::warn!("could not send modal message for display: {e:?}");
            }
        })
    }

    /// Display a notification to the user. Notifications persist until dismissed or the returned
    /// [`Notification`] handle is dropped, and their content may be updated through that handle.
    ///
    /// Returns an error if there is no UI to display notifications or if there are too many.
    pub fn show_notification(
        &mut self,
        content: impl Into<notification::NotificationContent>,
    ) -> Result<Notification, notification::Error> {
        // TODO: stop requiring mut by using a dedicated channel...?
        self.with_mut(|shuttle| match &mut shuttle.ui {
            Some(ui) => Ok(ui.show_notification(content)),
            None => Err(notification::Error::NoUi),
        })
    }

    /// Waits until exactly one step has completed that had not already happened when it was
    /// called, then immediately completes. The universe members will be available for reading
    /// at that point.
    ///
    /// Calling this in a loop is thus a means of observing the outcome of every step, such as
    /// for a renderer/recorder.
    ///
    /// Panics if called while the main task is suspended.
    pub async fn yield_to_step(&self) {
        self.with_ref(|_| {});
        let notifier = self
            .session_event_notifier
            .upgrade()
            .expect("can't happen: session_event_notifier dead");
        let (mut wake_flag, listener) = listen::WakeFlag::new(false);
        notifier.listen(listener.filter(|event| match event {
            Event::Stepped => Some(()),
        }));
        let _alive = wake_flag.wait().await;
    }

    #[track_caller]
    fn with_ref<R>(&self, f: impl FnOnce(&Shuttle) -> R) -> R {
        f(self
            .shuttle
            .try_read()
            .expect("MainTaskContext lock misused somehow (could not read)")
            .as_ref()
            .expect(
                "MainTaskContext operations may not be called while the main task is not executing",
            ))
    }

    /// Note: By accepting `&mut self` even though it is unnecessary, we prevent run-time
    /// read/write or write/write conflicts with the lock.
    #[track_caller]
    fn with_mut<R>(&mut self, f: impl FnOnce(&mut Shuttle) -> R) -> R {
        f(self
            .shuttle
            .try_write()
            .expect("MainTaskContext lock misused somehow (could not write)")
            .as_mut()
            .expect(
                "MainTaskContext operations may not be called while the main task is not executing",
            ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::apps::Key;
    use all_is_cubes::character::CharacterTransaction;
    use all_is_cubes::math::Cube;
    use all_is_cubes::universe::Name;
    use all_is_cubes::util::assert_send_sync;
    use core::sync::atomic::{AtomicUsize, Ordering};
    use futures_channel::oneshot;

    fn advance_time(session: &mut Session) {
        session.frame_clock.advance_by(session.universe().clock().schedule().delta_t());
        let step = session.maybe_step_universe();
        assert_ne!(step, None);
    }

    #[test]
    fn is_send_sync() {
        assert_send_sync::<Session>();
    }

    #[macro_rules_attribute::apply(smol_macros::test)]
    async fn fluff_forwarding_following() {
        // Create universe members
        let mut u = Universe::new();
        let space1 = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let space2 = u.insert_anonymous(Space::empty_positive(1, 1, 1));
        let character = StrongHandle::from(
            u.insert_anonymous(Character::spawn_default(u.read_ticket(), space1.clone()).unwrap()),
        );
        let st = space::CubeTransaction::fluff(Fluff::Happened).at(Cube::ORIGIN);

        // Create session
        let mut session = Session::builder().build().await;
        session.set_universe(u);
        session.set_character(Some(character.clone()));
        let log = listen::Log::<Fluff>::new();
        session.listen_fluff(log.listener().filter(|sf: &SessionFluff| Some(sf.fluff.clone())));

        // Try some fluff with the initial state (we haven't even stepped the session)
        session.universe_mut().execute_1(&space1, st.clone()).unwrap();
        assert_eq!(log.drain(), vec![Fluff::Happened]);

        // Change spaces
        session
            .universe_mut()
            .execute_1(
                &character,
                CharacterTransaction::move_to_space(space2.clone()),
            )
            .unwrap();
        session.maybe_step_universe();

        // Check we're now listening to the new space only
        session.universe_mut().execute_1(&space1, st.clone()).unwrap();
        assert_eq!(log.drain(), vec![]);
        session.universe_mut().execute_1(&space2, st).unwrap();
        assert_eq!(log.drain(), vec![Fluff::Happened]);
    }

    #[macro_rules_attribute::apply(smol_macros::test)]
    async fn main_task() {
        let old_marker = Name::from("old");
        let new_marker = Name::from("new");
        let noticed_step = Arc::new(AtomicUsize::new(0));
        let mut session = Session::builder().build().await;
        session
            .universe_mut()
            .insert(old_marker.clone(), Space::empty_positive(1, 1, 1))
            .unwrap();

        // Set up task (that won't do anything until it's polled as part of stepping)
        let (send, recv) = oneshot::channel::<Box<Universe>>();
        let mut cameras = session.create_cameras(listen::constant(Viewport::ARBITRARY));
        session.set_main_task({
            let noticed_step = noticed_step.clone();
            async move |mut ctx: MainTaskContext| {
                eprintln!("main task: waiting for new universe");
                let new_universe = recv.await.unwrap();
                ctx.set_universe(new_universe);
                eprintln!("main task: have set new universe");

                ctx.with_read_tickets(|read_tickets| cameras.update(read_tickets));
                assert!(cameras.character().is_some(), "has character");

                // Now try noticing steps
                for _ in 0..2 {
                    ctx.yield_to_step().await;
                    eprintln!("main task: got yield_to_step()");
                    noticed_step.fetch_add(1, Ordering::Relaxed);
                }

                eprintln!("main task: exiting");
                ExitMainTask
            }
        });

        // Existing universe should still be present.
        session.maybe_step_universe();
        assert!(session.universe_mut().get::<Space>(&old_marker).is_some());

        // Deliver new universe.
        let mut new_universe = Universe::new();
        let new_space =
            new_universe.insert(new_marker.clone(), Space::empty_positive(1, 1, 1)).unwrap();
        new_universe
            .insert(
                Name::from("character"),
                Character::spawn_default(new_universe.read_ticket(), new_space).unwrap(),
            )
            .unwrap();
        send.send(new_universe).unwrap();

        // Receive it.
        assert_eq!(noticed_step.load(Ordering::Relaxed), 0);
        session.maybe_step_universe();
        assert!(session.universe_mut().get::<Space>(&new_marker).is_some());
        assert!(session.universe_mut().get::<Space>(&old_marker).is_none());
        assert_eq!(noticed_step.load(Ordering::Relaxed), 0);

        // Try stepping.
        advance_time(&mut session);
        assert_eq!(noticed_step.load(Ordering::Relaxed), 1);
        advance_time(&mut session);
        assert_eq!(noticed_step.load(Ordering::Relaxed), 2);

        // Verify cleanup (that the next step can succeed even though the task exited).
        advance_time(&mut session);
        assert_eq!(noticed_step.load(Ordering::Relaxed), 2);
    }

    #[macro_rules_attribute::apply(smol_macros::test)]
    async fn input_is_processed_even_without_character() {
        let mut session =
            Session::builder().ui(listen::constant(Viewport::ARBITRARY)).build().await;
        assert!(!session.paused.get());

        session.input_processor.key_momentary(Key::Escape); // need to not just use control_channel
        advance_time(&mut session);

        assert!(session.paused.get());
    }
}
