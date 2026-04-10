//! Button widget controllers.

use all_is_cubes::fluff::Fluff;
use alloc::boxed::Box;
use alloc::sync::Arc;
use core::fmt;
use core::sync::atomic::{AtomicU8, Ordering::Relaxed};

use all_is_cubes::behavior::BehaviorSetTransaction;
use all_is_cubes::inv::EphemeralOpaque;
use all_is_cubes::listen;
use all_is_cubes::space::{self, SpaceBehaviorAttachment, SpaceTransaction};
use all_is_cubes::{linking, universe};

use crate::vui;

use super::{Action, ActionButton, ButtonVisualState, ToggleButton, ToggleButtonVisualState};

// -------------------------------------------------------------------------------------------------

impl vui::Widget for ActionButton {
    fn controller(self: Arc<Self>, grant: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ActionButtonController {
            common: CommonController::new(),
            txns: self.common.create_draw_txns(grant),
            definition: self,
        })
    }
}

// TODO: Mess of generic bounds due to the combination of Widget and listen::DynSource
// requirements -- should we make a trait alias for these?
impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::Widget for ToggleButton<D> {
    fn controller(self: Arc<Self>, grant: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ToggleButtonController {
            common: CommonController::new(),
            todo: listen::Flag::listening(true, &self.data_source),
            txns: self.common.create_draw_txns(grant),
            definition: self,
        })
    }
}

// -------------------------------------------------------------------------------------------------

#[derive(Debug)]
struct CommonController {
    // Timer counting down from when the button was pressed.
    recently_pressed: Arc<AtomicU8>,
}

/// [`WidgetController`] for [`ActionButton`].
#[derive(Debug)]
struct ActionButtonController {
    common: CommonController,
    definition: Arc<ActionButton>,
    txns: linking::Provider<ButtonVisualState, vui::WidgetTransaction>,
}

/// [`WidgetController`] for [`ToggleButton`].
#[derive(Debug)]
struct ToggleButtonController<D: Clone + Send + Sync> {
    common: CommonController,
    definition: Arc<ToggleButton<D>>,
    txns: linking::Provider<ToggleButtonVisualState, vui::WidgetTransaction>,
    todo: listen::Flag,
}

impl CommonController {
    const PRESSED_TIMER_DURATION: u8 = 10;

    pub fn new() -> Self {
        Self {
            recently_pressed: Arc::new(AtomicU8::new(0)),
        }
    }

    fn make_activation_behavior(&self, action: Action) -> Arc<space::ActivatableRegion> {
        Arc::new(space::ActivatableRegion {
            effect: {
                let recently_pressed = self.recently_pressed.clone();

                EphemeralOpaque::new(Arc::new(move || {
                    recently_pressed.store(Self::PRESSED_TIMER_DURATION, Relaxed);
                    if let Some(f) = action.try_ref() {
                        f();
                    }
                }))
            },
        })
    }

    /// Step forward and return whether the button state has changed and should be drawn.
    fn step(&mut self) -> ButtonActivity {
        let previous_counter = self
            .recently_pressed
            .update(Relaxed, Relaxed, |counter| counter.saturating_sub(1));

        let changed = previous_counter == Self::PRESSED_TIMER_DURATION || previous_counter == 1;
        let state = self.state();
        ButtonActivity { changed, state }
    }

    fn state(&self) -> ButtonVisualState {
        // TODO: once cursor/click system supports mousedown and up, use that instead
        // of this crude animation behavior (but maybe *also* have a post-press
        // animation, possibly based on block tick_actions instead).
        ButtonVisualState {
            pressed: self.recently_pressed.load(Relaxed) > 0,
        }
    }
}

impl universe::VisitHandles for CommonController {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        let Self {
            recently_pressed: _,
        } = self;
    }
}

/// Results of [`CommonController::step()`].
struct ButtonActivity {
    /// True if we know the state just changed.
    changed: bool,
    state: ButtonVisualState,
}

impl ButtonActivity {
    fn add_fluff(&self, context: &vui::WidgetContext<'_, '_>, txn: &mut SpaceTransaction) {
        if self.changed && self.state.pressed {
            // TODO: dedicated button press fluff/sounds
            if let Some(cube) = context.grant().shrink_to_cube() {
                txn.at(cube).add_fluff(Fluff::Happened);
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------

impl vui::WidgetController for ActionButtonController {
    fn initialize(
        &mut self,
        context: &vui::WidgetContext<'_, '_>,
    ) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let grant = self.definition.common.shrink_bounds(*context.grant());

        Ok(SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            SpaceBehaviorAttachment::new(grant.bounds),
            self.common.make_activation_behavior(self.definition.action.clone()),
        )))
    }

    fn step(
        &mut self,
        context: &vui::WidgetContext<'_, '_>,
    ) -> Result<vui::StepSuccess, vui::StepError> {
        let activity = self.common.step();
        if activity.changed {
            context.request_draw();
        }
        let mut txn = vui::WidgetTransaction::default();
        activity.add_fluff(context, &mut txn);

        // TODO: use waking
        Ok((txn, vui::Then::Step))
    }

    fn draw(
        &mut self,
        _context: &vui::WidgetContext<'_, '_>,
        _from_scratch: bool,
    ) -> vui::WidgetTransaction {
        self.draw_txn(self.common.state())
    }
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::WidgetController
    for ToggleButtonController<D>
{
    fn initialize(
        &mut self,
        context: &vui::WidgetContext<'_, '_>,
    ) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let grant = self.definition.common.shrink_bounds(*context.grant());

        Ok(SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            SpaceBehaviorAttachment::new(grant.bounds),
            self.common.make_activation_behavior(self.definition.action.clone()),
        )))
    }

    fn step(
        &mut self,
        context: &vui::WidgetContext<'_, '_>,
    ) -> Result<vui::StepSuccess, vui::StepError> {
        let activity = self.common.step();
        if self.todo.get_and_clear() || activity.changed {
            context.request_draw();
        }
        let mut txn = vui::WidgetTransaction::default();
        activity.add_fluff(context, &mut txn);

        // TODO: use waking
        Ok((txn, vui::Then::Step))
    }

    fn draw(
        &mut self,
        _context: &vui::WidgetContext<'_, '_>,
        _from_scratch: bool,
    ) -> vui::WidgetTransaction {
        self.draw_txn(self.common.state())
    }
}

impl ActionButtonController {
    fn draw_txn(&self, state: ButtonVisualState) -> vui::WidgetTransaction {
        self.txns[state].clone()
    }
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> ToggleButtonController<D> {
    fn draw_txn(&self, common: ButtonVisualState) -> vui::WidgetTransaction {
        let value = (self.definition.projection)(&self.definition.data_source.get());
        self.txns[ToggleButtonVisualState { common, value }].clone()
    }
}

impl universe::VisitHandles for ActionButtonController {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            common,
            definition,
            txns,
        } = self;
        common.visit_handles(visitor);
        definition.visit_handles(visitor);
        txns.visit_handles(visitor);
    }
}
impl<D: Clone + Send + Sync> universe::VisitHandles for ToggleButtonController<D> {
    fn visit_handles(&self, visitor: &mut dyn universe::HandleVisitor) {
        let Self {
            common,
            definition,
            txns,
            todo: _,
        } = self;
        common.visit_handles(visitor);
        definition.visit_handles(visitor);
        txns.visit_handles(visitor);
    }
}
