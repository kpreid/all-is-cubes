//! Button widget controllers.

use alloc::boxed::Box;
use alloc::sync::Arc;
use core::fmt;
use core::sync::atomic::{AtomicU8, Ordering::Relaxed};

use all_is_cubes::behavior::BehaviorSetTransaction;
use all_is_cubes::inv::EphemeralOpaque;
use all_is_cubes::linking;
use all_is_cubes::listen;
use all_is_cubes::space::{self, SpaceBehaviorAttachment, SpaceTransaction};
use all_is_cubes::transaction::Merge;

use crate::vui;

use super::{ActionButton, ButtonVisualState, ToggleButton, ToggleButtonVisualState};

// -------------------------------------------------------------------------------------------------

impl vui::Widget for ActionButton {
    fn controller(self: Arc<Self>, grant: &vui::LayoutGrant) -> Box<dyn vui::WidgetController> {
        Box::new(ActionButtonController {
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
            todo: listen::Flag::listening(true, &self.data_source),
            txns: self.common.create_draw_txns(grant),
            definition: self,
            recently_pressed: Arc::new(AtomicU8::new(0)),
        })
    }
}

// -------------------------------------------------------------------------------------------------

/// [`WidgetController`] for [`ActionButton`].
#[derive(Debug)]
struct ActionButtonController {
    definition: Arc<ActionButton>,
    txns: linking::Provider<ButtonVisualState, vui::WidgetTransaction>,
}

/// [`WidgetController`] for [`ToggleButton`].
#[derive(Debug)]
struct ToggleButtonController<D: Clone + Send + Sync> {
    definition: Arc<ToggleButton<D>>,
    txns: linking::Provider<ToggleButtonVisualState, vui::WidgetTransaction>,
    todo: listen::Flag,
    recently_pressed: Arc<AtomicU8>,
}

// -------------------------------------------------------------------------------------------------

impl vui::WidgetController for ActionButtonController {
    fn initialize(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let grant = self.definition.common.shrink_bounds(*context.grant());

        // TODO: we never draw the pressed state
        let draw = self.txns[ButtonVisualState { pressed: false }].clone();
        let activatable = SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            SpaceBehaviorAttachment::new(grant.bounds),
            Arc::new(space::ActivatableRegion {
                effect: self.definition.action.clone(),
            }),
        ));
        draw.merge(activatable)
            .map_err(|error| vui::InstallVuiError::Conflict { error })
    }
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> vui::WidgetController
    for ToggleButtonController<D>
{
    fn initialize(
        &mut self,
        context: &vui::WidgetContext<'_>,
    ) -> Result<vui::WidgetTransaction, vui::InstallVuiError> {
        let grant = self.definition.common.shrink_bounds(*context.grant());

        let activatable = SpaceTransaction::behaviors(BehaviorSetTransaction::insert(
            SpaceBehaviorAttachment::new(grant.bounds),
            Arc::new(space::ActivatableRegion {
                effect: {
                    let action = self.definition.action.clone();
                    let recently_pressed = self.recently_pressed.clone();

                    // TODO: awkward lack of composability here.
                    // Perhaps ActivatableRegion should be replaced with being able to
                    // activate any behavior, i.e. this WidgetBehavior?
                    EphemeralOpaque::new(Arc::new(move || {
                        recently_pressed.store(10, Relaxed);
                        if let Some(f) = action.try_ref() {
                            f();
                        }
                    }))
                },
            }),
        ));
        self.draw_txn()
            .merge(activatable)
            .map_err(|error| vui::InstallVuiError::Conflict { error })
    }

    fn step(&mut self, _: &vui::WidgetContext<'_>) -> Result<vui::StepSuccess, vui::StepError> {
        Ok((
            if self.todo.get_and_clear() || self.recently_pressed.load(Relaxed) > 0 {
                self.draw_txn()
            } else {
                SpaceTransaction::default()
            },
            // TODO: use waking
            vui::Then::Step,
        ))
    }
}

impl<D: Clone + fmt::Debug + Send + Sync + 'static> ToggleButtonController<D> {
    fn draw_txn(&self) -> vui::WidgetTransaction {
        let value = (self.definition.projection)(&self.definition.data_source.get());
        self.txns[ToggleButtonVisualState {
            value,
            // TODO: once cursor/click system supports mousedown and up, use that instead
            // of this crude animation behavior (but maybe *also* have a post-press
            // animation, possibly based on block tick_actions instead).
            common: ButtonVisualState {
                pressed: self
                    .recently_pressed
                    .fetch_update(Relaxed, Relaxed, |counter| Some(counter.saturating_sub(1)))
                    .unwrap()
                    > 1,
            },
        }]
        .clone()
    }
}
