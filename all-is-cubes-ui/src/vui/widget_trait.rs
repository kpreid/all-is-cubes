//! UI [`Widget`] trait and related glue.

#![allow(
    unused_assignments,
    reason = "nightly FP <https://github.com/rust-lang/rust/issues/147648>"
)]

use alloc::boxed::Box;
use alloc::sync::Arc;
use core::error::Error;
use core::fmt::Debug;
use core::sync::atomic::{self, AtomicBool};

use bevy_platform::sync::Mutex;

use all_is_cubes::behavior::{self, Behavior};
use all_is_cubes::math::GridAab;
use all_is_cubes::space::{self, Space, SpaceTransaction};
use all_is_cubes::time::Tick;
use all_is_cubes::transaction::{self, Merge as _};
use all_is_cubes::universe::{HandleVisitor, ReadTicket, UniverseTransaction, VisitHandles};

// reused for WidgetController
pub use all_is_cubes::behavior::Then;

use crate::vui::{LayoutGrant, Layoutable, Positioned, validate_widget_transaction};

/// Transaction type produced by [`WidgetController`]s.
/// Placeholder for likely wanting to change this later.
pub type WidgetTransaction = SpaceTransaction;

/// Something that can participate in UI layout (via [`Layoutable`]), and then turn into
/// some interactive contents of a [`Space`] (via [`controller()`]) positioned according
/// to that layout.
///
/// This trait is object-safe so that collections (and in particular [`LayoutTree`]s) of
/// <code>[Arc]&lt;dyn Widget&gt;</code> can be used.
///
/// # Mutability and dependence
///
/// A widget may be instantiated in multiple [`Space`]s by calling [`controller()`] more
/// than once. All such instances should operate independently (not interfering with each
/// other) and equivalently.
///
/// A widget may reference changing data (e.g. the current value of some setting).
/// However, a widget should not behave differently in response to such data, as the
/// systems which manage widgets do not track such changes.
/// In particular, the widget's implementation of [`Layoutable`] should always give the
/// same answer, and the [`WidgetController`] it produces should be equivalent to instances
/// created at other times.
/// Instead, the [`WidgetController`], once created, is responsible for updating the
/// particular piece of [`Space`] granted to the widget whenever the input data changes
/// or the widget is interacted with.
///
/// # Where to find widgets
///
/// Standard widgets may be found in the [`vui::widgets`](crate::vui::widgets) module.
///
/// [`LayoutTree`]: crate::vui::LayoutTree
/// [`controller()`]: Self::controller
pub trait Widget: Layoutable + Debug + Send + Sync {
    /// Create a [`WidgetController`] to manage the widget's existence in a particular
    /// region of a particular [`Space`].
    ///
    /// The difference between a [`Widget`] and its [`WidgetController`]s is that each
    /// [`WidgetController`] must *separately* keep track of which changes need to be
    /// performed within its associated [`Space`]; the [`Widget`] may be instantiated in
    /// any number of [`Space`]s but does not need to keep track of them all. It is
    /// common for a [`WidgetController`] to hold an [`Arc`] pointer to its [`Widget`] to
    /// make use of information from its original definition.
    ///
    /// You should not usually need to call this method, but rather use
    /// [`WidgetTree::installation()`](crate::vui::LayoutTree::installation) to create
    /// controllers and attach them to a [`Space`]. However, it is valid for a widget to
    /// reuse another widget's controller implementation.
    fn controller(self: Arc<Self>, grant: &LayoutGrant) -> Box<dyn WidgetController>;
}

/// Does the work of making a particular region of a [`Space`] behave as a particular
/// [`Widget`].
///
/// Instances of [`WidgetController`] are obtained by calling [`Widget::controller()`].
/// In most cases, [`Widget`] implementations have corresponding [`WidgetController`]
/// implementations, though there are common utilities such as [`OneshotController`].
///
/// Currently, [`WidgetController`]s are expected to manage their state and todo by being
/// mutable — unlike the normal [`Behavior`] contract. This has been chosen as an acceptable
/// compromise for convenience because controllers are required not to operate outside
/// their assigned regions of space and therefore will not experience transaction conflicts.
///
/// [`OneshotController`]: crate::vui::widgets::OneshotController
pub trait WidgetController: Debug + VisitHandles + Send + Sync + 'static {
    /// Write the initial state of the widget to the space.
    /// This is called at most once.
    #[expect(
        clippy::missing_errors_doc,
        reason = "TODO: error type is too broad \
        and allows widgets to fill in fields that should be context"
    )]
    fn initialize(
        &mut self,
        context: &WidgetContext<'_, '_>,
    ) -> Result<WidgetTransaction, InstallVuiError> {
        let _ = context;
        Ok(WidgetTransaction::default())
    }

    /// For widgets which are installed in a session's UI universe,
    /// this is called every frame before [`Self::step()`] to allow the widget controller to fetch
    /// information from the game universe via the provided [`ReadTicket`].
    ///
    /// If this is not overridden, it will do nothing.
    ///
    /// TODO: This is a kludge which should go away soon (or be merged into `step()`)
    /// along with substantial refactoring of the UI as a whole.
    fn synchronize(
        &mut self,
        context: &WidgetContext<'_, '_>,
        world_read_ticket: ReadTicket<'_>,
        ui_read_ticket: ReadTicket<'_>,
    ) {
        _ = context;
        _ = world_read_ticket;
        _ = ui_read_ticket;
    }

    /// Called every frame (except as [`Then`] specifies otherwise)
    /// to update the state of the space to match the current state of
    /// the widget's data sources or user interaction.
    ///
    /// If this is not overridden, it will do nothing and the controller will be dropped.
    ///
    /// # Errors
    ///
    /// If the widget encounters an unexpected problem, it may return an error instead of panicking
    /// so that the rest of the system can continue.
    //---
    // TODO: Make this no longer able to return a transaction, and have `draw()` take over the job.
    fn step(&mut self, context: &WidgetContext<'_, '_>) -> Result<StepSuccess, StepError> {
        let _ = context;
        Ok((WidgetTransaction::default(), Then::Drop))
    }

    /// Produces a [`WidgetTransaction`] that draws the current state of the widget.
    /// If `from_scratch` is true, the widget should disregard any dirty state tracking it has
    /// and draw everything.
    ///
    /// Currently, this is called after every [`Self::initialize()`] and every [`Self::step()`].
    ///
    /// The transaction must not affect any part of the [`Space`] outside of its grant,
    /// or produce blocks that have such effects later.
    /// It must not conflict with the transaction produced by `step()` or `initialize()`.
    //---
    // TODO: Make this able to work without `&mut self`
    fn draw(&mut self, context: &WidgetContext<'_, '_>, from_scratch: bool) -> WidgetTransaction {
        let _ = context;
        let _ = from_scratch;
        WidgetTransaction::default()
    }
}

/// Successful return of [`WidgetController::step()`].
///
/// The [`Then`] determines when `step()` is called again, if it is.
///
/// TODO: This should become a struct that will allow more extensibility for future needs.
pub type StepSuccess = (WidgetTransaction, Then);

/// Error return of [`WidgetController::step()`].
///
/// TODO: This should become a more specific error type.
pub type StepError = Box<dyn Error + Send + Sync>;

impl WidgetController for Box<dyn WidgetController> {
    fn initialize(
        &mut self,
        context: &WidgetContext<'_, '_>,
    ) -> Result<WidgetTransaction, InstallVuiError> {
        (**self).initialize(context)
    }

    fn synchronize(
        &mut self,
        context: &WidgetContext<'_, '_>,
        world_read_ticket: ReadTicket<'_>,
        ui_read_ticket: ReadTicket<'_>,
    ) {
        (**self).synchronize(context, world_read_ticket, ui_read_ticket)
    }

    fn step(&mut self, context: &WidgetContext<'_, '_>) -> Result<StepSuccess, StepError> {
        (**self).step(context)
    }

    fn draw(&mut self, context: &WidgetContext<'_, '_>, from_scratch: bool) -> WidgetTransaction {
        (**self).draw(context, from_scratch)
    }
}

/// Wraps a [`WidgetController`] to make it into a [`Behavior`].
// TODO: Eliminate this iff it doesn't continue to be a useful abstraction.
// TODO: This uses interior mutability when it shouldn't (behaviors are supposed
// to mutate self via transaction); is that fine? It'll certainly mean that failing
// transactions might be lost, but that might be as good as anything.
#[derive(Debug)]
pub(super) struct WidgetBehavior {
    /// Original widget -- not used directly but for error reporting
    widget: Positioned<Arc<dyn Widget>>,
    controller: Mutex<Box<dyn WidgetController>>,
    draw_requested: AtomicBool,
}

impl WidgetBehavior {
    /// Returns a transaction which adds the given widget controller to the space,
    /// or an error if the controller's `initialize()` fails.
    pub(crate) fn installation(
        positioned_widget: Positioned<Arc<dyn Widget>>,
        mut controller: Box<dyn WidgetController>,
        // TODO: unused; revisit this when we have a better story for widget updating/synchronizing
        _read_ticket: ReadTicket<'_>,
    ) -> Result<SpaceTransaction, InstallVuiError> {
        let draw_requested = AtomicBool::new(false);
        let context = &WidgetContext {
            behavior_context: None,
            grant: &positioned_widget.position,
            draw_requested: &draw_requested,
        };
        let init_txn = match controller.initialize(context) {
            Ok(t) => t,
            Err(e) => {
                return Err(InstallVuiError::WidgetInitialization {
                    widget: controller,
                    error: Box::new(e),
                });
            }
        };
        // TODO: should we be calling synchronize() now?
        let draw_txn = controller.draw(context, true);
        let full_init_txn =
            init_txn.merge(draw_txn).map_err(|error| InstallVuiError::Conflict {
                error,
                widget: positioned_widget.value.clone(),
            })?;
        let add_txn = behavior::BehaviorSetTransaction::insert(
            // TODO: widgets should be rotatable and that should go here
            space::SpaceBehaviorAttachment::new(positioned_widget.position.bounds),
            Arc::new(WidgetBehavior {
                widget: positioned_widget.clone(),
                controller: Mutex::new(controller),
                draw_requested,
            }),
        );
        full_init_txn.merge(SpaceTransaction::behaviors(add_txn)).map_err(|error| {
            InstallVuiError::Conflict {
                error,
                widget: positioned_widget.value,
            }
        })
    }
}

impl VisitHandles for WidgetBehavior {
    fn visit_handles(&self, visitor: &mut dyn HandleVisitor) {
        let Self {
            widget: _,
            controller,
            draw_requested: _,
        } = self;
        // Not visiting the widget because it is not used for actual behavior
        // (no handles it may contain will be used *by* this WidgetBehavior).
        controller.lock().unwrap().visit_handles(visitor);
    }
}

impl Behavior<Space> for WidgetBehavior {
    fn step(&self, context: &behavior::Context<'_, '_, Space>) -> (UniverseTransaction, Then) {
        let (txn, then) = {
            let controller = &mut *self.controller.lock().unwrap();
            let widget_context = WidgetContext {
                behavior_context: Some(context),
                grant: &self.widget.position,
                draw_requested: &self.draw_requested,
            };
            let (step_txn, then) = controller
                .step(&widget_context)
                .expect("TODO: behaviors should have an error reporting path");
            if self.draw_requested.fetch_and(false, atomic::Ordering::Acquire) {
                let draw_txn = controller.draw(&widget_context, false);
                (
                    step_txn.merge(draw_txn).expect("step and draw txn should not conflict"),
                    then,
                )
            } else {
                (step_txn, then)
            }
        };
        // TODO: should be using the attachment bounds instead of the layout grant to validate bounds
        validate_widget_transaction(&self.widget.value, &txn, &self.widget.position)
            .expect("transaction validation failed");
        (context.bind_host(txn), then)
    }

    fn persistence(&self) -> Option<behavior::Persistence> {
        None
    }
}

/// Context passed to [`WidgetController::step()`].
#[derive(Debug)]
pub struct WidgetContext<'ctx, 'read> {
    behavior_context: Option<&'ctx behavior::Context<'ctx, 'read, Space>>,
    grant: &'ctx LayoutGrant,
    draw_requested: &'ctx AtomicBool,
}

impl<'a> WidgetContext<'a, '_> {
    /// The time tick that is currently passing, causing this step.
    pub fn tick(&self) -> Tick {
        match self.behavior_context {
            Some(context) => context.tick,
            None => {
                // In this case we are initializing the widget
                // TODO: This violates Tick::from_paused's documented "This should only be used in tests"
                Tick::from_seconds(0.0)
            }
        }
    }

    /// Returns the [`LayoutGrant`] given to this widget; the same value as when
    /// [`Widget::controller()`] was called.
    pub fn grant(&self) -> &'a LayoutGrant {
        self.grant
    }

    /// Causes [`WidgetController::draw()`] to be called after the current operation.
    pub fn request_draw(&self) {
        self.draw_requested.store(true, atomic::Ordering::Release);
    }
}

/// Errors that may arise from setting up [`LayoutTree`]s and [`Widget`]s and installing
/// them in a [`Space`].
///
/// [`LayoutTree`]: crate::vui::LayoutTree
#[derive(Debug, displaydoc::Display)]
#[non_exhaustive]
pub enum InstallVuiError {
    /// The widget failed to initialize for some reason.
    #[displaydoc("error initializing widget ({widget:?})")]
    WidgetInitialization {
        /// TODO: This should be `Arc<dyn Widget>` instead.
        /// Or, if we come up with some way of giving widgets IDs, that.
        widget: Box<dyn WidgetController>,

        /// The error returned by [`WidgetController::initialize()`].
        error: Box<InstallVuiError>,
    },

    /// A transaction conflict arose between two widgets or parts of a widget's installation.
    #[displaydoc("transaction conflict from widget {widget:?}")]
    #[non_exhaustive]
    Conflict {
        // TODO: Now that `ExecuteError` contains conflicts, we should consider making this a
        // sub-case of `ExecuteInstallation`.
        #[allow(missing_docs)]
        error: space::SpaceTransactionConflict,

        /// The widget.
        widget: Arc<dyn Widget>,
    },

    /// The widget attempted to modify space outside its assigned bounds.
    #[displaydoc(
        "widget attempted to write out of bounds\n\
        grant: {grant:?}\n\
        attempted write: {erroneous:?}\n\
        widget: {widget:?}\n\
    "
    )]
    OutOfBounds {
        /// The region given to the widget.
        grant: LayoutGrant,

        /// The region the widget attempted to modify.
        erroneous: GridAab,

        /// The widget.
        widget: Arc<dyn Widget>,
    },

    /// Installing the widget tree failed, because one of the widgets' transactions failed.
    /// This usually indicates a bug in the widget implementation.
    #[displaydoc("installing widget tree failed")]
    #[non_exhaustive]
    ExecuteInstallation {
        #[allow(missing_docs)]
        error: transaction::ExecuteError<SpaceTransaction>,
    },
}

impl Error for InstallVuiError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            InstallVuiError::WidgetInitialization { error, .. } => Some(error),
            InstallVuiError::Conflict { error, .. } => Some(error),
            InstallVuiError::OutOfBounds { .. } => None,
            InstallVuiError::ExecuteInstallation { error } => Some(error),
        }
    }
}

impl From<InstallVuiError> for all_is_cubes::linking::InGenError {
    fn from(value: InstallVuiError) -> Self {
        all_is_cubes::linking::InGenError::other(value)
    }
}

/// Calls [`WidgetController::synchronize()`] on every widget installed in the space.
#[cfg_attr(not(feature = "session"), allow(dead_code))]
pub(crate) fn synchronize_widgets(
    world_read_ticket: ReadTicket<'_>,
    ui_read_ticket: ReadTicket<'_>,
    space: &space::Read<'_>,
) {
    for item in space.behaviors().query::<WidgetBehavior>() {
        let context = &WidgetContext {
            behavior_context: None,
            grant: &item.behavior.widget.position,
            draw_requested: &item.behavior.draw_requested,
        };
        item.behavior.controller.lock().unwrap().synchronize(
            context,
            world_read_ticket,
            ui_read_ticket,
        );
    }
}

/// Create a [`Space`] to put a widget in.
#[cfg(test)]
#[track_caller]
pub(crate) fn instantiate_widget<W: Widget + 'static>(
    read_ticket: ReadTicket<'_>,
    grant: LayoutGrant,
    widget: W,
) -> (Option<GridAab>, Space) {
    use crate::vui;
    use all_is_cubes::transaction::Transaction as _;

    let mut space = Space::builder(grant.bounds).build();
    let txn = vui::install_widgets(grant, &vui::leaf_widget(widget), read_ticket)
        .expect("widget instantiation");
    let bounds = txn.bounds_only_cubes();
    txn.execute(&mut space, read_ticket, &mut transaction::no_outputs)
        .expect("widget transaction");
    (bounds, space)
}

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::util::assert_conditional_send_sync;

    #[test]
    fn error_is_send_sync() {
        assert_conditional_send_sync::<InstallVuiError>()
    }

    fn _assert_widget_trait_is_object_safe(_: &dyn Widget) {}
    fn _assert_controller_trait_is_object_safe(_: &dyn WidgetController) {}
}
