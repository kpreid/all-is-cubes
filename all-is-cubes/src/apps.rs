// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Components for "apps", or game clients: user interface and top-level state.

use std::fmt::{self, Display};
use std::future::Future;
use std::task::{Context, Poll};

use futures_core::future::BoxFuture;
use futures_task::noop_waker_ref;

use crate::camera::{Camera, GraphicsOptions};
use crate::character::{cursor_raycast, Character, Cursor};
use crate::inv::{Tool, ToolError, ToolInput};
use crate::listen::{ListenableCell, ListenableCellWithLocal, ListenableSource};
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

/// Everything that a game application needs regardless of platform.
///
/// Once we have multiplayer / client-server support, this will become the client-side
/// structure.
pub struct AllIsCubesAppState {
    /// Determines the timing of simulation and drawing. The caller must arrange
    /// to advance time in the clock.
    pub frame_clock: FrameClock,

    /// Handles (some) user input. The caller must provide input events/state;
    /// `AllIsCubesAppState` will handle calling [`InputProcessor::apply_input`].
    pub input_processor: InputProcessor,

    graphics_options: ListenableCell<GraphicsOptions>,

    game_universe: Universe,
    game_character: ListenableCellWithLocal<Option<URef<Character>>>,

    /// If present, a future that should be polled to produce a new [`Universe`]
    /// to replace `self.game_universe`. See [`Self::set_universe_async`].
    game_universe_in_progress: Option<BoxFuture<'static, Result<Universe, ()>>>,

    paused: ListenableCell<bool>,

    ui: Vui,

    /// Last cursor raycast result.
    /// TODO: This needs to handle clicking on the HUD and thus explicitly point into
    /// one of two different spaces.
    cursor_result: Option<Cursor>,

    last_step_info: UniverseStepInfo,
    // When adding fields, remember to update the `Debug` impl.
}

impl fmt::Debug for AllIsCubesAppState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AllIsCubesAppState")
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
            .finish_non_exhaustive()
    }
}

impl AllIsCubesAppState {
    /// Construct a new `AllIsCubesAppState` with a new [`Universe`] from the given
    /// template.
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let game_universe = Universe::new();
        let input_processor = InputProcessor::new();
        let paused = ListenableCell::new(false);

        Self {
            ui: Vui::new(&input_processor, paused.as_source()),

            frame_clock: FrameClock::new(),
            input_processor,
            graphics_options: ListenableCell::new(GraphicsOptions::default()),
            game_character: ListenableCellWithLocal::new(None),
            game_universe,
            game_universe_in_progress: None,
            paused,
            cursor_result: None,
            last_step_info: UniverseStepInfo::default(),
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
        let c = self.game_universe.get_default_character();
        self.game_character.set(c.clone());
        self.ui.set_character(c);
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

    pub fn ui_space(&self) -> &URef<Space> {
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
    pub fn maybe_step_universe(&mut self) -> Option<UniverseStepInfo> {
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
                let mut tick = self.frame_clock.tick();
                if *self.paused.get() {
                    tick = tick.pause();
                }
                self.frame_clock.did_step();

                if let Some(character_ref) = self.game_character.borrow() {
                    self.input_processor.apply_input(
                        InputTargets {
                            universe: Some(&mut self.game_universe),
                            character: Some(character_ref),
                            paused: Some(&self.paused),
                        },
                        tick,
                    );
                }
                self.input_processor.step(tick);

                let mut info = self.game_universe.step(tick);

                info += self.ui.step(tick);

                self.last_step_info = info.clone();
                result = Some(info)
            }
        }
        result
    }

    /// Call this once per frame to update the cursor raycast.
    ///
    /// TODO: bad API; revisit general cursor handling logic.
    pub fn update_cursor(&mut self, ui_camera: &Camera, game_camera: &Camera) {
        let ndc_pos = self.input_processor.cursor_ndc_position();

        self.cursor_result = ndc_pos
            .map(|p| ui_camera.project_ndc_into_world(p))
            .and_then(|ray| cursor_raycast(ray, self.ui.current_space(), FreeCoordinate::INFINITY));

        if self.cursor_result.is_none() {
            if let Some(character_ref) = self.game_character.borrow() {
                // TODO: maximum distance should be determined by character/universe parameters instead of hardcoded
                self.cursor_result = ndc_pos
                    .map(|p| game_camera.project_ndc_into_world(p))
                    .and_then(|ray| cursor_raycast(ray, &character_ref.borrow().space, 6.0));
            }
        }
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
        if cursor_space == Some(self.ui_space()) {
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
                Ok(())
            } else {
                Err(ToolError::NoTool)
            }
        }
    }

    /// Returns textual information intended to be overlaid as a HUD on top of the rendered scene
    /// containing diagnostic information about rendering and stepping.
    pub fn info_text<T>(&self, render: T) -> InfoText<'_, T> {
        InfoText { app: self, render }
    }

    #[doc(hidden)] // TODO: Decide whether we want FpsCounter in our public API
    pub fn draw_fps_counter(&self) -> &FpsCounter {
        self.frame_clock.draw_fps_counter()
    }
}

#[derive(Copy, Clone, Debug)]
pub struct InfoText<'a, T> {
    app: &'a AllIsCubesAppState,
    render: T,
}

impl<T: CustomFormat<StatusText>> Display for InfoText<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(character_ref) = self.app.game_character.borrow() {
            write!(f, "{}", character_ref.borrow().custom_format(StatusText)).unwrap();
        }
        write!(
            f,
            "\n\n{:#?}\n\nFPS: {:2.1}\n{:#?}\n\n",
            self.app.last_step_info.custom_format(StatusText),
            self.app.frame_clock.draw_fps_counter().frames_per_second(),
            self.render.custom_format(StatusText),
        )?;
        match self.app.cursor_result() {
            Some(cursor) => write!(f, "{}", cursor),
            None => write!(f, "No block"),
        }?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use futures_channel::oneshot;

    use crate::apps::AllIsCubesAppState;
    use crate::space::Space;
    use crate::universe::{Name, Universe, UniverseIndex};

    #[test]
    fn set_universe_async() {
        let old_marker = Name::from("old");
        let new_marker = Name::from("new");
        let mut app = AllIsCubesAppState::new();
        app.universe_mut()
            .insert(old_marker.clone(), Space::empty_positive(1, 1, 1))
            .unwrap();

        // Set up async loading but don't deliver anything yet
        let (send, recv) = oneshot::channel();
        app.set_universe_async(async { recv.await.unwrap() });

        // Existing universe should still be present.
        app.maybe_step_universe();
        assert!(UniverseIndex::<Space>::get(app.universe_mut(), &old_marker).is_some());

        // Deliver new universe.
        let mut new_universe = Universe::new();
        new_universe
            .insert(new_marker.clone(), Space::empty_positive(1, 1, 1))
            .unwrap();
        send.send(Ok(new_universe)).unwrap();

        // Receive it.
        app.maybe_step_universe();
        assert!(UniverseIndex::<Space>::get(app.universe_mut(), &new_marker).is_some());
        assert!(UniverseIndex::<Space>::get(app.universe_mut(), &old_marker).is_none());

        // Verify cleanup (that the next step can succeed).
        app.maybe_step_universe();
    }
}
