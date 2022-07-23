//! Voxel User Interface.
//!
//! We've got all this rendering and interaction code, so let's reuse it for the
//! GUI as well as the game.

use std::sync::{mpsc, Arc, Mutex};

use cgmath::{Angle as _, Decomposed, Deg, Transform, Vector3};
use ordered_float::NotNan;

use crate::apps::{ControlMessage, InputProcessor};
use crate::block::Resolution::R16;
use crate::camera::{FogOption, GraphicsOptions, ViewTransform, Viewport};
use crate::character::{Character, Cursor};
use crate::inv::{Tool, ToolError, ToolInput};
use crate::listen::{DirtyFlag, ListenableCell, ListenableSource};
use crate::math::FreeCoordinate;
use crate::space::Space;
use crate::time::Tick;
use crate::transaction::Transaction;
use crate::universe::{URef, Universe, UniverseStepInfo};
use crate::util::YieldProgress;
use crate::vui::widgets::TooltipState;

mod hud;
use hud::*;
mod icons;
pub use icons::*;
mod layout;
#[doc(hidden)] // going to be used by all-is-cubes-content, but not yet stable
pub use layout::*;
mod widget_trait;
#[doc(hidden)]
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
    current_space: ListenableCell<Option<URef<Space>>>,
    /// Identifies which “page” the UI should be showing — what
    /// should be in `current_space`, taken from one of the [`PageInst`]s.
    state: ListenableCell<VuiPageState>,

    changed_viewport: DirtyFlag,
    viewport_source: ListenableSource<Viewport>,
    /// `HudLayout` computed from `viewport_source`.
    last_hud_layout: HudLayout,
    #[allow(dead_code)] // TODO: probably going to need this for more dynamic UIs
    hud_inputs: HudInputs,

    hud_page: PageInst,
    paused_page: PageInst,

    character_source: ListenableSource<Option<URef<Character>>>,
    changed_character: DirtyFlag,
    tooltip_state: Arc<Mutex<TooltipState>>,
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
    pub async fn new(
        input_processor: &InputProcessor,
        character_source: ListenableSource<Option<URef<Character>>>,
        paused: ListenableSource<bool>,
        graphics_options: ListenableSource<GraphicsOptions>,
        control_channel: mpsc::SyncSender<ControlMessage>,
        viewport_source: ListenableSource<Viewport>,
    ) -> Self {
        let mut universe = Universe::new();
        // TODO: take YieldProgress as a parameter
        let hud_blocks = Arc::new(HudBlocks::new(&mut universe, YieldProgress::noop(), R16).await);

        let tooltip_state = Arc::<Mutex<TooltipState>>::default();

        // TODO: terrible mess of tightly coupled parameters
        let changed_viewport = DirtyFlag::listening(false, |l| viewport_source.listen(l));
        let hud_layout = HudLayout::new(viewport_source.snapshot());
        let hud_inputs = HudInputs {
            hud_blocks,
            control_channel,
            graphics_options,
            paused,
            mouselook_mode: input_processor.mouselook_mode(),
        };
        let hud_widget_tree = new_hud_widget_tree(
            character_source.clone(),
            &hud_inputs,
            &hud_layout,
            &mut universe,
            tooltip_state.clone(),
        );

        let paused_widget_tree = new_paused_widget_tree(&hud_inputs);

        let mut new_self = Self {
            universe,
            current_space: ListenableCell::new(None),
            state: ListenableCell::new(VuiPageState::Hud),

            changed_viewport,
            viewport_source,
            last_hud_layout: hud_layout,
            hud_inputs,

            hud_page: PageInst::new(hud_widget_tree),
            paused_page: PageInst::new(paused_widget_tree),

            changed_character: DirtyFlag::listening(false, |l| character_source.listen(l)),
            character_source,
            tooltip_state,
        };
        new_self.set_space_from_state();
        new_self
    }

    /// The space that should be displayed to the user, drawn on top of the world.
    // TODO: It'd be more encapsulating if we could provide a _read-only_ URef...
    pub fn current_space(&self) -> ListenableSource<Option<URef<Space>>> {
        self.current_space.as_source()
    }

    pub(crate) fn set_state(&mut self, state: VuiPageState) {
        self.state.set(state);
        self.set_space_from_state();
    }

    /// Update `self.current_space` from `self.state` and the source of the selected space.
    fn set_space_from_state(&mut self) {
        let layout = &self.last_hud_layout;
        let universe = &mut self.universe;

        let next_space: Option<URef<Space>> = match &*self.state.get() {
            VuiPageState::Hud => Some(self.hud_page.get_or_create_space(layout, universe)),
            VuiPageState::Paused => Some(self.paused_page.get_or_create_space(layout, universe)),
        };

        if next_space.as_ref() != Option::as_ref(&self.current_space.get()) {
            self.current_space.set(next_space);
            log::trace!(
                "UI switched to {:?} ({:?})",
                self.current_space.get(),
                self.state.get()
            );
        }
    }

    /// Computes a [`ViewTransform`] that should be used to view the [`Vui::current_space`].
    ///
    /// It does not need to be rechecked other than on aspect ratio changes.
    ///
    /// TODO: This is not a method because the code structure makes it inconvenient for
    /// renderers to get access to `Vui` itself. Add some other communication path.
    pub fn view_transform(space: &Space, fov_y: Deg<FreeCoordinate>) -> ViewTransform {
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
    pub fn graphics_options(mut options: GraphicsOptions) -> GraphicsOptions {
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

        if self.changed_viewport.get_and_clear() {
            let new_viewport = self.viewport_source.snapshot();
            let new_layout = HudLayout::new(new_viewport);
            if new_layout != self.last_hud_layout {
                self.last_hud_layout = new_layout;
                self.current_space.set(None); // force reconstruction
                self.set_space_from_state();
            }
        }

        // Decide what state we should be in.
        {
            let current_state: &VuiPageState = &*self.state.get();
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

    pub fn show_tool_error(&mut self, error: ToolError) {
        // TODO: review text formatting
        if let Ok(mut state) = self.tooltip_state.lock() {
            state.set_message(error.to_string().into());
        }
    }

    pub fn click(&mut self, _button: usize, cursor: Option<Cursor>) -> Result<(), ToolError> {
        if cursor.as_ref().map(|c| &c.space) != Option::as_ref(&self.current_space.get()) {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_executor::block_on;

    fn new_vui_for_test() -> Vui {
        block_on(Vui::new(
            &InputProcessor::new(),
            ListenableSource::constant(None),
            ListenableSource::constant(false),
            ListenableSource::constant(GraphicsOptions::default()),
            mpsc::sync_channel(1).0,
            ListenableSource::constant(Viewport::ARBITRARY),
        ))
    }

    #[test]
    fn vui_smoke_test() {
        let _ = new_vui_for_test();
    }
}
