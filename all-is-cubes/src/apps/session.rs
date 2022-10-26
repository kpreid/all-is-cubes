use std::fmt;
use std::future::Future;
use std::sync::mpsc::{self, TryRecvError};
use std::sync::Arc;
use std::task::{Context, Poll};

use futures_core::future::BoxFuture;
use futures_task::noop_waker_ref;

use crate::apps::{FpsCounter, FrameClock, InputProcessor, InputTargets, StandardCameras};
use crate::camera::{GraphicsOptions, Viewport};
use crate::character::{Character, Cursor};
use crate::inv::ToolError;
use crate::listen::{ListenableCell, ListenableCellWithLocal, ListenableSource};
use crate::space::Space;
use crate::transaction::Transaction;
use crate::universe::{URef, Universe, UniverseStepInfo};
use crate::util::{CustomFormat, StatusText};
use crate::vui::Vui;

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

    /// Handles (some) user input. The caller must provide input events/state to this.
    /// [`Session`] will handle applying it to the game state.
    pub input_processor: InputProcessor,

    graphics_options: ListenableCell<GraphicsOptions>,

    game_universe: Universe,
    game_character: ListenableCellWithLocal<Option<URef<Character>>>,

    /// If present, a future that should be polled to produce a new [`Universe`]
    /// to replace `self.game_universe`. See [`Self::set_universe_async`].
    game_universe_in_progress: Option<BoxFuture<'static, Result<Universe, ()>>>,

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
    /// Returns a [`SessionBuilder`] with which to construct a new [`Session`].
    pub fn builder() -> SessionBuilder {
        SessionBuilder {
            viewport_for_ui: None,
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
        match &self.ui {
            Some(ui) => ui.current_space(),
            None => ListenableSource::constant(None), // TODO: cache this to allocate less
        }
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
                    ControlMessage::Back => {
                        // TODO: error reporting … ? hm.
                        if let Some(ui) = &mut self.ui {
                            ui.back();
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
                            control_channel: Some(&self.control_channel_sender),
                        },
                        game_tick,
                    );
                }
                self.input_processor.step(game_tick);

                let mut info = self.game_universe.step(game_tick);

                if let Some(ui) = &mut self.ui {
                    info += ui.step(base_tick);
                }

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

    /// Handle a mouse-click event, at the position specified by the last
    /// [`Self::update_cursor()`].
    ///
    /// TODO: Clicks should be passed through `InputProcessor` instead of being an entirely separate path.
    pub fn click(&mut self, button: usize) {
        let result = self.click_impl(button);

        if let Err(error @ ToolError::Internal(_)) = &result {
            // Log the message because the UI text field currently doesn't
            // fit long errors at all.
            // TODO: include source() chain
            log::error!("Error applying tool: {error}")
        }

        if let Some(ui) = &self.ui {
            ui.show_click_result(button, result);
        }
    }

    /// Implementation of click interpretation logic, called by [`Self::click`].
    /// TODO: This function needs tests.
    fn click_impl(&mut self, button: usize) -> Result<(), ToolError> {
        let cursor_space = self.cursor_result.as_ref().map(|c| &c.space);
        // TODO: A better condition for this would be "is one of the spaces in the UI universe"
        if cursor_space == Option::as_ref(&self.ui_space().get()) {
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

/// Builder for providing the configuration of a new [`Session`].
#[derive(Debug, Clone)]
#[must_use]
pub struct SessionBuilder {
    viewport_for_ui: Option<ListenableSource<Viewport>>,
}

impl SessionBuilder {
    /// Create the [`Session`] with configuration from this builder.
    ///
    /// This is an async function for the sake of cancellation and optional cooperative
    /// multitasking, while constructing the initial state. It may safely be blocked on
    /// from a synchronous context.
    pub async fn build(self) -> Session {
        let game_universe = Universe::new();
        let game_character = ListenableCellWithLocal::new(None);
        let input_processor = InputProcessor::new();
        let graphics_options = ListenableCell::new(GraphicsOptions::default());
        let paused = ListenableCell::new(false);
        let (control_send, control_recv) = mpsc::sync_channel(100);

        Session {
            ui: match self.viewport_for_ui {
                Some(viewport) => Some(
                    Vui::new(
                        &input_processor,
                        game_character.as_source(),
                        paused.as_source(),
                        graphics_options.as_source(),
                        control_send.clone(),
                        viewport,
                    )
                    .await,
                ),
                None => None,
            },
            frame_clock: FrameClock::new(),
            input_processor,
            graphics_options,
            game_character,
            game_universe,
            game_universe_in_progress: None,
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
}

/// A message sent to the [`Session`], such as from a user interface element.
#[non_exhaustive]
pub(crate) enum ControlMessage {
    /// Perform the conventional escape/back/pause function:
    /// * navigate towards the root of a menu tree
    /// * if at the root, return to game
    /// * if in-game, pause and open menu
    Back,
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
            Self::TogglePause => write!(f, "TogglePause"),
            Self::ToggleMouselook => write!(f, "ToggleMouselook"),
            Self::ModifyGraphicsOptions(_f) => f
                .debug_struct("ModifyGraphicsOptions")
                .finish_non_exhaustive(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct InfoText<'a, T> {
    session: &'a Session,
    render: T,
}

impl<T: CustomFormat<StatusText>> fmt::Display for InfoText<'_, T> {
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
        let mut session = block_on(Session::builder().build());
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
}
