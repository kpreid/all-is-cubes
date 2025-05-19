use alloc::string::{String, ToString as _};
use alloc::sync::Arc;
use std::sync::Mutex;

use flume::TryRecvError;

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes::character::{Character, Cursor};
use all_is_cubes::inv::{EphemeralOpaque, Tool, ToolError, ToolInput};
use all_is_cubes::listen::{self, Notifier};
use all_is_cubes::space::Space;
use all_is_cubes::time;
use all_is_cubes::transaction::{self, Transaction};
use all_is_cubes::universe::{Handle, ReadTicket, Universe, UniverseStepInfo, UniverseTransaction};
use all_is_cubes_render::camera::{FogOption, GraphicsOptions, UiViewState, Viewport};

use crate::apps::{
    ControlMessage, FullscreenSetter, FullscreenState, QuitCancelled, QuitFn, QuitResult,
};
use crate::ui_content::hud::{HudBlocks, HudInputs};
use crate::ui_content::{notification, pages};
use crate::vui::widgets::TooltipState;
use crate::vui::{self, PageInst, UiSize};

// -------------------------------------------------------------------------------------------------

/// All the things exposed by [`crate::apps::Session`] to [`Vui`],
/// and that are required to create a [`Vui`].
///
/// TODO: Better name, better data flow...?
#[derive(Clone)]
pub(crate) struct UiTargets {
    pub(crate) mouselook_mode: listen::DynSource<bool>,

    /// Reports the `Character` whose inventory should be displayed.
    pub(crate) character_source: listen::DynSource<Option<Handle<Character>>>,

    pub(crate) paused: listen::DynSource<bool>,

    pub(crate) graphics_options: listen::DynSource<Arc<GraphicsOptions>>,

    pub(crate) app_control_channel: flume::Sender<ControlMessage>,

    pub(crate) viewport_source: listen::DynSource<Viewport>,

    pub(crate) fullscreen_mode: listen::DynSource<FullscreenState>,
    pub(crate) set_fullscreen: FullscreenSetter,

    pub(crate) quit: Option<QuitFn>,
}

// -------------------------------------------------------------------------------------------------

/// `Vui` builds user interfaces out of voxels. It owns a `Universe` dedicated to the
/// purpose and draws into spaces to form the HUD and menus.
///
/// TODO: This needs a renaming given the revised focus of the `vui` module as being about
/// "widget framework" and not about application UI
#[derive(Debug)] // TODO: probably not very informative Debug as derived
pub(crate) struct Vui {
    /// Universe used for storing VUI elements.
    universe: Universe,

    /// The space that should be displayed to the user, drawn on top of the world.
    /// The value of this cell is derived from `self.state`.
    current_view: listen::Cell<Arc<UiViewState>>,

    /// The `focus_on_ui` value from the current [`vui::Page`].
    ///
    /// TODO: this is a kludge which should be part of a more general mechanism
    /// analogous to `UiViewState.`
    current_focus_on_ui: bool,

    /// Identifies which [`Page`] the UI should be showing — what
    /// should be in `current_space`, taken from one of the [`PageInst`]s.
    state: listen::Cell<Arc<VuiPageState>>,

    /// Listens to the provided user graphics options.
    changed_graphics_options: listen::Flag,
    /// Modified version of the user graphics options for rendering UI.
    ui_graphics_options: listen::Cell<Arc<GraphicsOptions>>,

    changed_viewport: listen::Flag,
    /// Size computed from `viewport_source` and compared with `PageInst`.
    last_ui_size: UiSize,
    hud_inputs: HudInputs,

    hud_page: PageInst,
    paused_page: PageInst,
    about_page: PageInst,
    progress_page: PageInst,
    options_page: PageInst,
    /// Whatever [`VuiPageState::Dump`] contained.
    dump_page: PageInst,

    /// Receiving internal messages from widgets for controlling the UI itself
    /// (changing `state`, etc).
    ///
    /// Design note: Using `flume` not because we want MPMC, but because its receiver is
    /// `Send + Sync`, unlike the `std` one.
    /// Our choice of `flume` in particular is just because our other crates use it.
    control_channel: flume::Receiver<VuiMessage>,
    changed_character: listen::Flag,
    tooltip_state: Arc<Mutex<TooltipState>>,
    /// Messages from session to UI that don't fit as [`listen::DynSource`] changes.
    cue_channel: CueNotifier,

    notif_hub: notification::Hub,
}

impl Vui {
    /// TODO: Reduce coupling, perhaps by passing in a separate struct with just the listenable
    /// elements.
    ///
    /// This is an async function for the sake of cancellation and optional cooperative
    /// multitasking. It may safely be blocked on from a synchronous context.
    pub(crate) async fn new(params: UiTargets) -> Self {
        let UiTargets {
            viewport_source, ..
        } = &params;

        let mut universe = Universe::new();

        let mut content_txn = UniverseTransaction::default();
        // TODO: take YieldProgress as a parameter
        let hud_blocks = Arc::new(
            HudBlocks::new(
                universe.read_ticket(),
                &mut content_txn,
                all_is_cubes::util::YieldProgressBuilder::new().build(),
            )
            .await,
        );
        content_txn
            .execute(&mut universe, (), &mut transaction::no_outputs)
            .unwrap();

        let (control_send, control_recv) = flume::bounded(100);
        let state = listen::Cell::new(Arc::new(VuiPageState::Hud));

        let tooltip_state = Arc::<Mutex<TooltipState>>::default();
        let cue_channel: CueNotifier = Arc::new(Notifier::new());
        let notif_hub = notification::Hub::new();

        let changed_graphics_options = listen::Flag::listening(false, &params.graphics_options);
        let ui_graphics_options = listen::Cell::new(Arc::new(Self::graphics_options(
            (*params.graphics_options.get()).clone(),
        )));

        // TODO: HudInputs should own `UiTargets` or something so that we don’t have one
        // line per thing.
        let changed_viewport = listen::Flag::listening(false, &viewport_source);
        let ui_size = UiSize::new(viewport_source.get());
        let hud_inputs = HudInputs {
            base: params.clone(),
            hud_blocks,
            cue_channel: cue_channel.clone(),
            vui_control_channel: control_send,
            page_state: state.as_source(),
        };
        let hud_page = super::hud::new_hud_page(&hud_inputs, tooltip_state.clone());

        let paused_page = pages::new_paused_page(&mut universe, &hud_inputs).unwrap();
        let options_page = pages::new_options_widget_tree(&hud_inputs);
        let about_page = pages::new_about_page(&mut universe, &hud_inputs).unwrap();
        let progress_page =
            pages::new_progress_page(&hud_inputs.hud_blocks.widget_theme, &notif_hub);

        let mut new_self = Self {
            universe,

            current_view: listen::Cell::new(Arc::new(UiViewState::default())),
            current_focus_on_ui: false,
            state: listen::Cell::new(Arc::new(VuiPageState::Hud)),

            changed_graphics_options,
            ui_graphics_options,

            changed_viewport,
            last_ui_size: ui_size,
            hud_inputs,

            hud_page: PageInst::new(hud_page),
            paused_page: PageInst::new(paused_page),
            options_page: PageInst::new(options_page),
            about_page: PageInst::new(about_page),
            dump_page: PageInst::new(vui::Page::empty()),
            progress_page: PageInst::new(progress_page),

            control_channel: control_recv,
            changed_character: listen::Flag::listening(false, &params.character_source),
            tooltip_state,
            cue_channel,
            notif_hub,
        };
        new_self.compute_view_state();
        new_self
    }

    /// The space that should be displayed to the user, drawn on top of the world.
    // TODO: It'd be more encapsulating if we could provide a _read-only_ Handle...
    pub fn view(&self) -> listen::DynSource<Arc<UiViewState>> {
        self.current_view.as_source()
    }

    /// Returns a [`ReadTicket`] for use with the space handle in [`Self::view()`].
    pub fn read_ticket(&self) -> ReadTicket<'_> {
        self.universe.read_ticket()
    }

    /// Returns whether the input/output mechanisms for this UI should direct input to the UI
    /// rather than gameplay controls. That is: disable mouselook and direct typing to text input.
    //---
    // TODO: This should be public and be a listen::Cell, but we don't really know what the name
    // and data type should be
    pub(crate) fn should_focus_on_ui(&self) -> bool {
        self.current_focus_on_ui
    }

    pub(crate) fn set_state(&mut self, state: impl Into<Arc<VuiPageState>>) {
        self.state.set(state.into());

        // Special case: the dump state has to replace the widget tree, and
        // unconditionally because we can't just check if it is equal (WidgetTree: !Eq)
        if let VuiPageState::Dump {
            previous: _,
            content,
        } = &*self.state.get()
        {
            let content: vui::Page = match content.try_ref() {
                Some(page) => (*page).clone(),
                None => vui::Page::empty(),
            };
            self.dump_page = PageInst::new(content);
        }

        self.compute_view_state();
    }

    /// Update `self.current_space` from `self.state` and the source of the selected space.
    fn compute_view_state(&mut self) {
        let size = self.last_ui_size;
        let universe = &mut self.universe;

        let next_page: &mut PageInst = match &*self.state.get() {
            VuiPageState::Hud => &mut self.hud_page,
            VuiPageState::Paused => &mut self.paused_page,
            VuiPageState::Options => &mut self.options_page,
            VuiPageState::AboutText => &mut self.about_page,
            VuiPageState::Progress => &mut self.progress_page,

            // Note: checking the `content` is handled in `set_state()`.
            VuiPageState::Dump {
                previous: _,
                content: _,
            } => &mut self.dump_page,
        };
        let next_space: Handle<Space> = next_page.get_or_create_space(size, universe);
        let page_layout = next_page.page().layout;
        let graphics_options = self.ui_graphics_options.get();

        let new_view_state = UiViewState {
            view_transform: page_layout.view_transform(
                &next_space.read(universe.read_ticket()).unwrap(), // TODO: eliminate this unwrap
                graphics_options.fov_y.into_inner(),
            ),
            space: Some(next_space),
            graphics_options,
        };

        if new_view_state != *self.current_view.get() {
            self.current_view.set(Arc::new(new_view_state));
            self.current_focus_on_ui = next_page.page().focus_on_ui;
            log::trace!(
                "UI switched to {:?} ({:?})",
                self.current_view.get().space,
                self.state.get()
            );
        }
    }

    /// Compute graphics options to render the VUI space given the user's regular options.
    fn graphics_options(mut options: GraphicsOptions) -> GraphicsOptions {
        // Set FOV to give a predictable, not-too-wide-angle perspective.
        options.fov_y = 30u8.into();

        // Disable fog for maximum clarity and because we shouldn't have any far clipping to hide.
        options.fog = FogOption::None;

        // Fixed view distance for our layout.
        // TODO: Derive this from HudLayout and also FOV (since FOV determines eye-to-space distance).
        options.view_distance = 100u8.into();

        // clutter
        options.debug_chunk_boxes = false;
        options.debug_pixel_cost = false;

        options
    }

    // TODO: This should stop taking a `Tick` and instead expose whatever is necessary for
    // it to be stepped on a schedule independent of the in-game time.
    pub fn step(
        &mut self,
        tick: time::Tick,
        deadline: time::Deadline<impl time::Instant>,
        world_read_ticket: ReadTicket<'_>,
    ) -> UniverseStepInfo {
        self.step_pre_sync(world_read_ticket);
        self.universe.step(tick.paused(), deadline)
    }

    /// Update the UI from its data sources
    #[inline(never)]
    fn step_pre_sync(&mut self, world_read_ticket: ReadTicket<'_>) {
        let mut anything_changed = false;

        if self.changed_graphics_options.get_and_clear() {
            anything_changed = true;
            self.ui_graphics_options
                .set_if_unequal(Arc::new(Self::graphics_options(
                    (*self.hud_inputs.graphics_options.get()).clone(),
                )));
        }

        // TODO: This should possibly be the responsibility of the TooltipState itself?
        if self.changed_character.get_and_clear() {
            if let Some(character_handle) = self.hud_inputs.character_source.get() {
                TooltipState::bind_to_character(
                    world_read_ticket,
                    &self.tooltip_state,
                    character_handle,
                );
            }
        }

        // Drain the control channel.
        loop {
            match self.control_channel.try_recv() {
                Ok(msg) => match msg {
                    VuiMessage::Back => {
                        self.back();
                    }
                    VuiMessage::Open(page) => {
                        // TODO: States should be stackable somehow, and this should not totally overwrite the previous state.
                        self.set_state(page);
                    }
                },
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    // Lack of whatever control sources is non-fatal.
                }
            }
        }

        if self.changed_viewport.get_and_clear() {
            let new_viewport = self.hud_inputs.viewport_source.get();
            let new_size = UiSize::new(new_viewport);
            if new_size != self.last_ui_size {
                anything_changed = true;
                self.last_ui_size = new_size;
                self.current_view.set(Arc::new(UiViewState::default())); // force reconstruction
            }
        }

        // Gather latest notification data.
        self.notif_hub.update();

        // Decide what state we should be in (based on all the stuff we just checked).
        if let Some(new_state) = self.choose_new_page_state(&self.state.get()) {
            self.set_state(new_state);
        } else if anything_changed {
            // We need to update the view state even though it isn't changing what Space it looks
            // at.
            self.compute_view_state();
        }

        if let Some(space_handle) = self.view().get().space.as_ref() {
            if let Ok(space) = space_handle.read(self.universe.read_ticket()) {
                vui::synchronize_widgets(world_read_ticket, self.universe.read_ticket(), &space);
            } else {
                log::error!("failed to synchronize widgets");
            }
        }
    }

    pub fn show_modal_message(&mut self, message: ArcStr) {
        let content =
            EphemeralOpaque::from(Arc::new(pages::new_message_page(message, &self.hud_inputs)));
        self.set_state(VuiPageState::Dump {
            previous: self.state.get(),
            content,
        });
    }

    pub fn show_notification(
        &mut self,
        content: impl Into<notification::NotificationContent>,
    ) -> notification::Notification {
        self.notif_hub.insert(content.into())
    }

    /// Enter some kind of debug view. Not yet defined for the long run exactly what that is.
    pub(crate) fn enter_debug(&mut self, read_ticket: ReadTicket<'_>, cursor: &Cursor) {
        // TODO(read_ticket): kludge; perhaps instead the cursor information provided should
        // include which layer's universe it came from.
        // (Note that the caller can't supply this read ticket selection because it would be a
        // borrow conflict.)
        // This is probably something that will be easier to fix in the shiny ECS future that
        // read_ticket is supposed to be an interim migration mechanism for.
        let read_ticket = if cursor.space().universe_id() == Some(self.universe.universe_id()) {
            self.universe.read_ticket()
        } else {
            read_ticket
        };
        let content = EphemeralOpaque::new(Arc::new(crate::editor::inspect_block_at_cursor(
            read_ticket,
            &self.hud_inputs,
            cursor,
        )));
        self.set_state(VuiPageState::Dump {
            previous: self.state.get(),
            content,
        });
    }

    /// Present the UI visual response to a click (that has already been handled),
    /// either a small indication that a button was pressed or an error message.
    pub fn show_click_result(&self, button: usize, result: Result<(), ToolError>) {
        self.cue_channel.notify(&CueMessage::Clicked(button));
        match result {
            Ok(()) => {}
            Err(error) => self.show_tool_error(&error),
        }
    }

    fn show_tool_error(&self, error: &ToolError) {
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
            read_ticket: self.universe.read_ticket(),
            cursor,
            character: None,
        })?;
        transaction
            .execute(&mut self.universe, (), &mut transaction::no_outputs)
            .map_err(|e| ToolError::Internal(e.to_string()))?;
        Ok(())
    }

    /// Perform the back/escape key action.
    ///
    /// This may cancel out of a menu/dialog, or pause or unpause the game.
    pub fn back(&mut self) {
        match *self.state.get() {
            VuiPageState::Hud => {
                if !self.hud_inputs.paused.get() {
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
                if self.hud_inputs.paused.get() {
                    // Unpause
                    self.hud_inputs
                        .app_control_channel
                        .send(ControlMessage::TogglePause)
                        .unwrap();
                }
            }
            VuiPageState::AboutText | VuiPageState::Options => {
                // The next step will decide whether we should be paused or unpaused.
                // TODO: Instead check right now, but in a reusable fashion.
                self.set_state(VuiPageState::Hud);
            }
            VuiPageState::Progress => {
                log::error!("TODO: need UI state for dismissing notifications");
            }
            VuiPageState::Dump { ref previous, .. } => {
                self.set_state(Arc::clone(previous));
            }
        }
    }

    /// Perform the quit action.
    ///
    /// This may be used in response to a window's close button, for example.
    ///
    /// The UI state *may* decline to react, such as if there are unsaved changes, but it should
    /// be expected to prompt the user in that case.
    ///
    /// The returned future will produce a [`QuitCancelled`] value if quitting was unsuccessful for
    /// any reason. If it is successful, the future never resolves. It is not necessary to poll
    /// the future if the result value is not wanted.
    pub fn quit(&self) -> impl Future<Output = QuitResult> + Send + 'static + use<> {
        if let Some(quit_fn) = self.hud_inputs.quit.as_ref() {
            std::future::ready(quit_fn())
        } else {
            std::future::ready(Err(QuitCancelled::Unsupported))
        }
    }

    /// Compute the wanted page state based on the previous state and external inputs.
    ///
    /// This is how, for example, the pause menu appears when the game is paused by whatever means.
    ///
    /// Returns [`None`] if no change in state should be made.
    fn choose_new_page_state(&self, current_state: &VuiPageState) -> Option<VuiPageState> {
        // Decide whether to display VuiPageState::Progress
        if self.notif_hub.has_interrupt()
            && current_state.freely_replaceable()
            && !matches!(current_state, VuiPageState::Progress)
        {
            return Some(VuiPageState::Progress);
        }

        // Decide whether to stop displaying notifications
        if !self.notif_hub.has_interrupt() && matches!(current_state, VuiPageState::Progress) {
            // TODO: actually we should delegate to the paused-or-not logic...
            return Some(VuiPageState::Hud);
        }

        let paused = self.hud_inputs.paused.get();
        if paused && matches!(current_state, VuiPageState::Hud) {
            // TODO: also do this for lost focus
            Some(VuiPageState::Paused)
        } else if !paused && matches!(current_state, VuiPageState::Paused) {
            Some(VuiPageState::Hud)
        } else {
            None
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

    /// Options/settings/preferences menu.
    Options,

    /// “About All is Cubes” info.
    AboutText,

    /// Displays a task progress bar taken from the [notification] list.
    /// Reverts to [`VuiPageState::Hud`] when there is no notification.
    Progress,

    /// Arbitrary widgets that have already been computed, and which don't demand
    /// any navigation behavior more complex than “cancellable”. This is to be used for
    /// viewing various reports/dialogs until we have a better idea.
    Dump {
        previous: Arc<VuiPageState>,
        content: EphemeralOpaque<vui::Page>,
    },
}

impl VuiPageState {
    /// Whether replacing this state _won't_ lose any important state.
    pub fn freely_replaceable(&self) -> bool {
        match self {
            VuiPageState::Hud => true,
            VuiPageState::Paused => true,
            VuiPageState::AboutText => true,
            VuiPageState::Progress => true,

            VuiPageState::Options => false,
            VuiPageState::Dump { .. } => false,
        }
    }
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
    /// Transition to the specified [`VuiPageState`].
    Open(VuiPageState),
}

/// Channel for broadcasting, from session to widgets, various user interface responses
/// to events (that don't fit into the [`listen::DynSource`] model).
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

    async fn new_vui_for_test(paused: bool) -> (Vui, flume::Receiver<ControlMessage>) {
        let (cctx, ccrx) = flume::bounded(1);
        let vui = Vui::new(UiTargets {
            mouselook_mode: listen::constant(false),
            character_source: listen::constant(None),
            paused: listen::constant(paused),
            graphics_options: listen::constant(Arc::new(GraphicsOptions::default())),
            app_control_channel: cctx,
            viewport_source: listen::constant(Viewport::ARBITRARY),
            fullscreen_mode: listen::constant(None),
            set_fullscreen: None,
            quit: None,
        })
        .await;
        (vui, ccrx)
    }

    #[tokio::test]
    async fn back_pause() {
        let (mut vui, control_channel) = new_vui_for_test(false).await;
        vui.back();
        let msg = control_channel.try_recv().unwrap();
        assert!(matches!(msg, ControlMessage::TogglePause), "{msg:?}");
        assert!(control_channel.try_recv().is_err());
    }

    #[tokio::test]
    async fn back_unpause() {
        let (mut vui, control_channel) = new_vui_for_test(true).await;
        vui.set_state(VuiPageState::Paused);
        vui.back();
        let msg = control_channel.try_recv().unwrap();
        assert!(matches!(msg, ControlMessage::TogglePause), "{msg:?}");
        assert!(control_channel.try_recv().is_err());
    }
}
