//! Voxel User Interface.
//!
//! We've got all this rendering and interaction code, so let's reuse it for the
//! GUI as well as the game.

use std::sync::mpsc::TryRecvError;
use std::sync::{mpsc, Arc, Mutex};

use all_is_cubes::camera::{FogOption, GraphicsOptions, UiViewState, ViewTransform, Viewport};
use all_is_cubes::cgmath::{Angle as _, Decomposed, Deg, One, Transform, Vector3};
use all_is_cubes::character::{Character, Cursor};
use all_is_cubes::inv::{Tool, ToolError, ToolInput};
use all_is_cubes::listen::{DirtyFlag, ListenableCell, ListenableSource, Notifier};
use all_is_cubes::math::FreeCoordinate;
use all_is_cubes::math::NotNan;
use all_is_cubes::space::Space;
use all_is_cubes::time::Tick;
use all_is_cubes::transaction::Transaction;
use all_is_cubes::universe::{URef, Universe, UniverseStepInfo};
use all_is_cubes::util::YieldProgress;

use crate::apps::{ControlMessage, FullscreenSetter, FullscreenState, InputProcessor};
use crate::vui::pages::{PageInst, UiSize};
use crate::vui::widgets::TooltipState;

#[doc(hidden)] // public for use by test-renderers only
pub mod blocks;
pub(crate) use blocks::UiBlocks;
mod hud;
use hud::*;
mod layout;
pub use layout::*;
mod options;
mod pages;
mod widget_trait;
pub use widget_trait::*;
pub mod widgets;

/// `Vui` builds user interfaces out of voxels. It owns a `Universe` dedicated to the
/// purpose and draws into spaces to form the HUD and menus.
#[derive(Debug)] // TODO: probably not very informative Debug as derived
pub(crate) struct Vui {
    /// Universe used for storing VUI elements.
    universe: Universe,

    /// The space that should be displayed to the user, drawn on top of the world.
    /// The value of this cell is derived from `self.state`.
    current_view: ListenableCell<UiViewState>,
    /// Identifies which “page” the UI should be showing — what
    /// should be in `current_space`, taken from one of the [`PageInst`]s.
    state: ListenableCell<VuiPageState>,

    changed_viewport: DirtyFlag,
    viewport_source: ListenableSource<Viewport>,
    /// Size computed from `viewport_source` and compared with `PageInst`.
    last_ui_size: UiSize,
    #[allow(dead_code)] // TODO: probably going to need this for more dynamic UIs
    hud_inputs: HudInputs,

    hud_page: PageInst,
    paused_page: PageInst,
    about_page: PageInst,

    /// Receiving internal messages from widgets for controlling the UI itself
    /// (changing `state`, etc).
    control_channel: mpsc::Receiver<VuiMessage>,
    character_source: ListenableSource<Option<URef<Character>>>,
    changed_character: DirtyFlag,
    tooltip_state: Arc<Mutex<TooltipState>>,
    /// Messages from session to UI that don't fit as [`ListenableSource`] changes.
    cue_channel: CueNotifier,
}

impl Vui {
    /// `input_processor` is the `InputProcessor` whose state may be reflected on the HUD.
    /// `character_source` reports the `Character` whose inventory should be displayed.
    ///
    /// TODO: Reduce coupling, perhaps by passing in a separate struct with just the listenable
    /// elements.
    ///
    /// This is an async function for the sake of cancellation and optional cooperative
    /// multitasking. It may safely be blocked on from a synchronous context.
    #[allow(clippy::too_many_arguments)]
    pub(crate) async fn new(
        input_processor: &InputProcessor,
        character_source: ListenableSource<Option<URef<Character>>>,
        paused: ListenableSource<bool>,
        graphics_options: ListenableSource<GraphicsOptions>,
        app_control_channel: mpsc::SyncSender<ControlMessage>,
        viewport_source: ListenableSource<Viewport>,
        fullscreen_source: ListenableSource<FullscreenState>,
        set_fullscreen: FullscreenSetter,
    ) -> Self {
        let mut universe = Universe::new();
        // TODO: take YieldProgress as a parameter
        let hud_blocks = Arc::new(HudBlocks::new(&mut universe, YieldProgress::noop()).await);

        let (control_send, control_recv) = mpsc::sync_channel(100);
        let state = ListenableCell::new(VuiPageState::Hud);

        let tooltip_state = Arc::<Mutex<TooltipState>>::default();
        let cue_channel: CueNotifier = Arc::new(Notifier::new());

        // TODO: terrible mess of tightly coupled parameters
        let changed_viewport = DirtyFlag::listening(false, |l| viewport_source.listen(l));
        let ui_size = UiSize::new(viewport_source.snapshot());
        let hud_inputs = HudInputs {
            hud_blocks,
            cue_channel: cue_channel.clone(),
            vui_control_channel: control_send,
            app_control_channel,
            graphics_options,
            paused,
            page_state: state.as_source(),
            mouselook_mode: input_processor.mouselook_mode(),
            fullscreen_mode: fullscreen_source,
            set_fullscreen,
        };
        let hud_widget_tree = new_hud_widget_tree(
            character_source.clone(),
            &hud_inputs,
            &mut universe,
            tooltip_state.clone(),
        );

        let paused_widget_tree = pages::new_paused_widget_tree(&hud_inputs);
        let about_widget_tree = pages::new_about_widget_tree(&mut universe, &hud_inputs).unwrap();

        let mut new_self = Self {
            universe,
            current_view: ListenableCell::new(UiViewState::default()),
            state: ListenableCell::new(VuiPageState::Hud),

            changed_viewport,
            viewport_source,
            last_ui_size: ui_size,
            hud_inputs,

            hud_page: PageInst::new(hud_widget_tree),
            paused_page: PageInst::new(paused_widget_tree),
            about_page: PageInst::new(about_widget_tree),

            control_channel: control_recv,
            changed_character: DirtyFlag::listening(false, |l| character_source.listen(l)),
            character_source,
            tooltip_state,
            cue_channel,
        };
        new_self.set_space_from_state();
        new_self
    }

    /// The space that should be displayed to the user, drawn on top of the world.
    // TODO: It'd be more encapsulating if we could provide a _read-only_ URef...
    pub fn view(&self) -> ListenableSource<UiViewState> {
        self.current_view.as_source()
    }

    pub(crate) fn set_state(&mut self, state: VuiPageState) {
        self.state.set(state);
        self.set_space_from_state();
    }

    /// Update `self.current_space` from `self.state` and the source of the selected space.
    fn set_space_from_state(&mut self) {
        let size = self.last_ui_size;
        let universe = &mut self.universe;

        let next_space: Option<URef<Space>> = match &*self.state.get() {
            VuiPageState::Hud => Some(self.hud_page.get_or_create_space(size, universe)),
            VuiPageState::Paused => Some(self.paused_page.get_or_create_space(size, universe)),
            VuiPageState::AboutText => Some(self.about_page.get_or_create_space(size, universe)),
        };

        if next_space.as_ref() != Option::as_ref(&self.current_view.get().space) {
            self.current_view
                .set(Self::view_state_for(next_space, &self.hud_inputs));
            log::trace!(
                "UI switched to {:?} ({:?})",
                self.current_view.get().space,
                self.state.get()
            );
        }
    }

    fn view_state_for(space: Option<URef<Space>>, inputs: &HudInputs) -> UiViewState {
        // TODO: compute the derived graphics options only once
        let graphics_options = Self::graphics_options(inputs.graphics_options.snapshot());

        UiViewState {
            view_transform: match space.as_ref() {
                Some(space) => Self::view_transform(
                    &space.read().unwrap(), // TODO: eliminate this unwrap
                    all_is_cubes::cgmath::Deg(graphics_options.fov_y.into_inner()),
                ),
                None => ViewTransform::one(),
            },
            space,
            graphics_options,
        }
    }

    /// Computes a [`ViewTransform`] that should be used to view the [`Vui::current_space`].
    ///
    /// It does not need to be rechecked other than on aspect ratio changes.
    ///
    /// TODO: used to be public before [`UiViewState`]; refactor in light of
    fn view_transform(space: &Space, fov_y: Deg<FreeCoordinate>) -> ViewTransform {
        let bounds = space.bounds();
        let mut ui_center = bounds.center();

        // Arrange a view distance which will place the Z=0 plane sized to fill the viewport
        // (at least vertically, as we don't have aspect ratio support yet).
        ui_center.z = 0.0;

        let view_distance = FreeCoordinate::from(bounds.size().y) * (fov_y / 2.).cot() / 2.;
        Decomposed::look_at_rh(
            ui_center + Vector3::new(0., 0., view_distance),
            ui_center,
            Vector3::new(0., 1., 0.),
        )
        .inverse_transform() // Our view transform standard is camera-to-world not world-to-camera
        .unwrap()
    }

    /// Compute graphics options to render the VUI space given the user's regular options.
    fn graphics_options(mut options: GraphicsOptions) -> GraphicsOptions {
        // Set FOV to give a predictable, not-too-wide-angle perspective.
        options.fov_y = NotNan::from(30);

        // Disable fog for maximum clarity and because we shouldn't have any far clipping to hide.
        options.fog = FogOption::None;

        // Fixed view distance for our layout.
        // TODO: Derive this from HudLayout and also FOV (since FOV determines eye-to-space distance).
        options.view_distance = NotNan::from(100);

        // clutter
        options.debug_chunk_boxes = false;

        options
    }

    pub fn step(&mut self, tick: Tick) -> UniverseStepInfo {
        // TODO: This should possibly be the responsibility of the TooltipState itself?
        if self.changed_character.get_and_clear() {
            if let Some(character_ref) = &*self.character_source.get() {
                TooltipState::bind_to_character(&self.tooltip_state, character_ref.clone());
            }
        }

        // Drain the control channel.
        loop {
            match self.control_channel.try_recv() {
                Ok(msg) => match msg {
                    VuiMessage::Back => {
                        self.back();
                    }
                    VuiMessage::About => {
                        // TODO: States should be stackable somehow, and this should not totally overwrite the previous state.
                        self.set_state(VuiPageState::AboutText);
                    }
                },
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    // Lack of whatever control sources is non-fatal.
                }
            }
        }

        if self.changed_viewport.get_and_clear() {
            let new_viewport = self.viewport_source.snapshot();
            let new_size = UiSize::new(new_viewport);
            if new_size != self.last_ui_size {
                self.last_ui_size = new_size;
                self.current_view.set(UiViewState::default()); // force reconstruction
                self.set_space_from_state();
            }
        }

        // Decide what state we should be in.
        {
            let current_state: &VuiPageState = &self.state.get();
            let paused = *self.hud_inputs.paused.get();
            if paused && matches!(current_state, VuiPageState::Hud) {
                // TODO: also do this for lost focus
                self.set_state(VuiPageState::Paused);
            } else if !paused && matches!(current_state, VuiPageState::Paused) {
                self.set_state(VuiPageState::Hud);
            }
        }

        self.universe.step(tick)
    }

    /// Present the UI visual response to a click (that has already been handled),
    /// either a small indication that a button was pressed or an error message.
    pub fn show_click_result(&self, button: usize, result: Result<(), ToolError>) {
        self.cue_channel.notify(CueMessage::Clicked(button));
        match result {
            Ok(()) => {}
            Err(error) => self.show_tool_error(error),
        }
    }

    fn show_tool_error(&self, error: ToolError) {
        // TODO: review text formatting
        if let Ok(mut state) = self.tooltip_state.lock() {
            state.set_message(error.to_string().into());
        }
    }

    /// Handle clicks that hit the UI itself
    pub fn click(&mut self, _button: usize, cursor: Option<Cursor>) -> Result<(), ToolError> {
        if cursor.as_ref().map(Cursor::space) != Option::as_ref(&self.current_view.get().space) {
            return Err(ToolError::Internal(String::from(
                "Vui::click: space didn't match",
            )));
        }
        // TODO: We'll probably want to distinguish buttons eventually.
        // TODO: It should be easier to use a tool
        let transaction = Tool::Activate.use_immutable_tool(&ToolInput {
            cursor,
            character: None,
        })?;
        transaction
            .execute(&mut self.universe)
            .map_err(|e| ToolError::Internal(e.to_string()))?;
        Ok(())
    }

    /// Perform the back/escape key action.
    ///
    /// This may cancel out of a menu/dialog, or pause or unpause the game.
    pub fn back(&mut self) {
        match *self.state.get() {
            VuiPageState::Hud => {
                if !*self.hud_inputs.paused.get() {
                    // Pause
                    // TODO: instead of unwrapping, log and visually report the error
                    // (there should be some simple way to do that).
                    // TODO: We should have a "set paused state" message instead of toggle.
                    self.hud_inputs
                        .app_control_channel
                        .send(ControlMessage::TogglePause)
                        .unwrap();
                }
            }
            VuiPageState::Paused => {
                if *self.hud_inputs.paused.get() {
                    // Unpause
                    self.hud_inputs
                        .app_control_channel
                        .send(ControlMessage::TogglePause)
                        .unwrap();
                }
            }
            VuiPageState::AboutText => {
                // The next step will decide whether we should be paused or unpaused.
                // TODO: Instead check right now, but in a reusable fashion.
                self.set_state(VuiPageState::Hud);
            }
        }
    }
}

/// Identifies which “page” the UI should be showing — what should be in
/// [`Vui::current_space()`].
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum VuiPageState {
    /// Normal gameplay, with UI elements around the perimeter.
    Hud,
    /// Report the paused (or lost-focus) state and offer a button to unpause
    /// and reactivate mouselook.
    Paused,
    AboutText,
}

/// Message indicating a UI action that affects the UI itself
#[derive(Clone, Debug)]
pub(crate) enum VuiMessage {
    /// Perform the VUI-internal “back” action. This is not necessarily the same as an
    /// external button press.
    ///
    /// TODO: This is a kludge being used for 'close the current page state' but ideally
    /// it would specify *what* it's closing in case of message race conditions etc.
    Back,
    /// Open [`VuiPageState::AboutText`].
    About,
}

/// Channel for broadcasting, from session to widgets, various user interface responses
/// to events (that don't fit into the [`ListenableSource`] model).
///
/// TODO: This `Arc` is a kludge; probably Notifier should have some kind of clonable
/// add-a-listener handle to itself, and that would help out other situations too.
pub(crate) type CueNotifier = Arc<Notifier<CueMessage>>;

/// Message from session to widget.
#[derive(Clone, Copy, Debug)]
pub(crate) enum CueMessage {
    /// User clicked the specified button and it was handled as a tool usage.
    ///
    /// TODO: This needs to communicate "which space" or be explicitly restricted to the
    /// world space.
    Clicked(usize),
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_executor::block_on;

    fn new_vui_for_test(paused: bool) -> (Vui, mpsc::Receiver<ControlMessage>) {
        let (cctx, ccrx) = mpsc::sync_channel(1);
        let vui = block_on(Vui::new(
            &InputProcessor::new(),
            ListenableSource::constant(None),
            ListenableSource::constant(paused),
            ListenableSource::constant(GraphicsOptions::default()),
            cctx,
            ListenableSource::constant(Viewport::ARBITRARY),
            ListenableSource::constant(None),
            None,
        ));
        (vui, ccrx)
    }

    #[test]
    fn back_pause() {
        let (mut vui, control_channel) = new_vui_for_test(false);
        vui.back();
        let msg = control_channel.try_recv().unwrap();
        assert!(matches!(msg, ControlMessage::TogglePause), "{msg:?}");
        assert!(control_channel.try_recv().is_err());
    }

    #[test]
    fn back_unpause() {
        let (mut vui, control_channel) = new_vui_for_test(true);
        vui.set_state(VuiPageState::Paused);
        vui.back();
        let msg = control_channel.try_recv().unwrap();
        assert!(matches!(msg, ControlMessage::TogglePause), "{msg:?}");
        assert!(control_channel.try_recv().is_err());
    }
}
