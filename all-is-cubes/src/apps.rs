// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Components for "apps", or game clients: user interface and top-level state.

use std::fmt::Display;

use crate::camera::{Camera, GraphicsOptions};
use crate::character::{cursor_raycast, Character, CharacterChange, Cursor};
use crate::content::UniverseTemplate;
use crate::listen::{DirtyFlag, ListenableCell, ListenableSource, ListenerHelper as _};
use crate::space::Space;
use crate::tools::ToolError;
use crate::transactions::Transaction;
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
#[derive(Debug)]
pub struct AllIsCubesAppState {
    /// Determines the timing of simulation and drawing. The caller must arrange
    /// to advance time in the clock.
    pub frame_clock: FrameClock,

    /// Handles (some) user input. The caller must provide input events/state;
    /// `AllIsCubesAppState` will handle calling [`InputProcessor::apply_input`].
    pub input_processor: InputProcessor,

    graphics_options: ListenableCell<GraphicsOptions>,

    game_universe: Universe,
    game_character: Option<URef<Character>>,

    paused: ListenableCell<bool>,

    ui: Vui,
    ui_dirty: DirtyFlag,

    /// Last cursor raycast result.
    /// TODO: This needs to handle clicking on the HUD and thus explicitly point into
    /// one of two different spaces.
    cursor_result: Option<Cursor>,

    last_step_info: UniverseStepInfo,
}

impl AllIsCubesAppState {
    /// Construct a new `AllIsCubesAppState` with a new [`Universe`] from the given
    /// template.
    pub fn new(template: UniverseTemplate) -> Self {
        let game_universe = template
            .build()
            // TODO: better error handling
            .expect("Failure while constructing template");

        let input_processor = InputProcessor::new();
        let paused = ListenableCell::new(false);

        let mut new_self = Self {
            ui: Vui::new(&input_processor, paused.as_source()),

            frame_clock: FrameClock::new(),
            input_processor,
            graphics_options: ListenableCell::new(GraphicsOptions::default()),
            game_character: game_universe.get_default_character(),
            game_universe,
            paused,
            ui_dirty: DirtyFlag::new(true),
            cursor_result: None,
            last_step_info: UniverseStepInfo::default(),
        };

        // TODO: once it's possible to switch characters we will need to clear and reinstall this
        if let Some(character_ref) = &new_self.game_character {
            character_ref
                .borrow()
                .listen(new_self.ui_dirty.listener().filter(|msg| match msg {
                    CharacterChange::Inventory(_) | CharacterChange::Selections => Some(()),
                }));
        }
        new_self.maybe_sync_ui();

        new_self
    }

    /// Returns a reference to the [`Character`] that should be shown to the user.
    pub fn character(&self) -> Option<&URef<Character>> {
        self.game_character.as_ref()
    }

    /// Returns a mutable reference to the [`Universe`].
    pub fn universe_mut(&mut self) -> &mut Universe {
        &mut self.game_universe
    }

    pub fn ui_space(&self) -> &URef<Space> {
        &self.ui.current_space()
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
        let mut result = None;
        // TODO: Catch-up implementation should probably live in FrameClock.
        for _ in 0..FrameClock::CATCH_UP_STEPS {
            if self.frame_clock.should_step() {
                let mut tick = self.frame_clock.tick();
                if *self.paused.get() {
                    tick = tick.pause();
                }
                self.frame_clock.did_step();

                if let Some(character_ref) = &self.game_character {
                    self.input_processor.apply_input(
                        &mut character_ref.borrow_mut(),
                        &self.paused,
                        tick,
                    );
                }
                self.input_processor.step(tick);

                let mut info = self.game_universe.step(tick);

                self.maybe_sync_ui();
                info += self.ui.step(tick);

                self.last_step_info = info.clone();
                result = Some(info)
            }
        }
        result
    }

    fn maybe_sync_ui(&mut self) {
        if self.ui_dirty.get_and_clear() {
            // TODO: Exact interaction between Character and Vui probably shouldn't be AllIsCubesAppState's responsibility.
            if let Some(character_ref) = &self.game_character {
                let character = character_ref.borrow();
                self.ui
                    .set_toolbar(&character.inventory().slots, &character.selected_slots())
                    .unwrap();
            }
        }
    }

    /// Call this once per frame to update the cursor raycast.
    ///
    /// TODO: bad API; revisit general cursor handling logic.
    pub fn update_cursor(&mut self, ui_camera: &Camera, game_camera: &Camera) {
        let ndc_pos = self.input_processor.cursor_ndc_position();

        self.cursor_result = ndc_pos
            .map(|p| ui_camera.project_ndc_into_world(p))
            .and_then(|ray| cursor_raycast(ray, &self.ui.current_space()));

        if self.cursor_result.is_none() {
            if let Some(character_ref) = &self.game_character {
                self.cursor_result = ndc_pos
                    .map(|p| game_camera.project_ndc_into_world(p))
                    .and_then(|ray| cursor_raycast(ray, &character_ref.borrow().space));
            }
        }
    }

    pub fn cursor_result(&self) -> &Option<Cursor> {
        &self.cursor_result
    }

    /// TODO: Should have click feedback in VUI, not via return value.
    pub fn click(&mut self, button: usize) -> Result<(), ToolError> {
        if let (Some(cursor), Some(character_ref)) = (&self.cursor_result, &self.game_character) {
            let transaction = Character::click(character_ref.clone(), cursor, button)?;
            transaction
                .execute(self.universe_mut())
                .map_err(|e| ToolError::Internal(e.to_string()))?;
            Ok(())
        } else {
            Err(ToolError::NothingSelected) // TODO: slightly wrong
        }
    }

    /// Returns textual information intended to be overlaid as a HUD on top of the rendered scene
    /// containing diagnostic information about rendering and stepping.
    pub fn info_text<T>(&self, render: T) -> InfoText<'_, T> {
        InfoText { app: self, render }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct InfoText<'a, T> {
    app: &'a AllIsCubesAppState,
    render: T,
}

impl<T: CustomFormat<StatusText>> Display for InfoText<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(character_ref) = self.app.character() {
            write!(f, "{}", character_ref.borrow().custom_format(StatusText)).unwrap();
        }
        write!(
            f,
            "\n\n{:#?}\n\n{:#?}\n\n",
            self.app.last_step_info.custom_format(StatusText),
            self.render.custom_format(StatusText),
        )?;
        match self.app.cursor_result() {
            Some(cursor) => write!(f, "{}", cursor),
            None => write!(f, "No block"),
        }?;
        Ok(())
    }
}
