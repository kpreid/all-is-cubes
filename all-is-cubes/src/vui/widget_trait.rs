//! UI [`Widget`] trait and related glue.

use std::error::Error;
use std::fmt::Debug;
use std::sync::{Arc, Mutex};

use crate::behavior::{Behavior, BehaviorContext, BehaviorSetTransaction};
use crate::math::GridAab;
use crate::space::{self, Space, SpaceTransaction};
use crate::time::Tick;
use crate::transaction::{Merge as _, TransactionConflict};
use crate::universe::{RefVisitor, VisitRefs};
use crate::vui::{validate_widget_transaction, LayoutGrant, Layoutable, Positioned};

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
/// Currently, [`WidgetController`]s are expected to manager their state and todo by being
/// mutable — unlike the normal [`Behavior`] contract. This has been chosen as an acceptable
/// compromise for convenience because controllers are required not to operate outside
/// their assigned regions of space and therefore will not experience transaction conflicts.
///
/// [`OneshotController`]: crate::vui::widgets::OneshotController
pub trait WidgetController: Debug + Send + Sync + 'static {
    /// Write the initial state of the widget to the space.
    /// This is called at most once.
    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        Ok(WidgetTransaction::default())
    }

    /// Called every frame to update the state of the space to match the current state of
    /// the widget's data sources or user interaction.
    ///
    /// TODO: Arrange a waking mechanism so that the widget need not be called every frame.
    ///
    /// TODO: Be more specific than `Box<dyn Error>`
    ///
    /// TODO: If this is not overridden, arrange to automatically drop the controller for efficiency
    fn step(&mut self, tick: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        let _ = tick;
        Ok(WidgetTransaction::default())
    }
}

impl WidgetController for Box<dyn WidgetController> {
    fn step(&mut self, tick: Tick) -> Result<WidgetTransaction, Box<dyn Error + Send + Sync>> {
        (**self).step(tick)
    }

    fn initialize(&mut self) -> Result<WidgetTransaction, InstallVuiError> {
        (**self).initialize()
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
}

impl WidgetBehavior {
    /// Returns a transaction which adds the given widget controller to the space,
    /// or an error if the controller's `initialize()` fails.
    pub(crate) fn installation(
        widget: Positioned<Arc<dyn Widget>>,
        mut controller: Box<dyn WidgetController>,
    ) -> Result<SpaceTransaction, InstallVuiError> {
        let init_txn = match controller.initialize() {
            Ok(t) => t,
            Err(e) => {
                return Err(InstallVuiError::WidgetInitialization {
                    widget: controller,
                    error: Box::new(e),
                });
            }
        };
        let add_txn = BehaviorSetTransaction::insert(
            // TODO: widgets should be rotatable and that should go here
            space::SpaceBehaviorAttachment::new(widget.position.bounds),
            Arc::new(WidgetBehavior {
                widget,
                controller: Mutex::new(controller),
            }),
        );
        init_txn
            .merge(SpaceTransaction::behaviors(add_txn))
            .map_err(|error| InstallVuiError::Conflict { error })
    }
}

impl VisitRefs for WidgetBehavior {
    fn visit_refs(&self, _: &mut dyn RefVisitor) {
        // TODO: Do we need to visit the widget controllers?
    }
}

impl Behavior<Space> for WidgetBehavior {
    fn step(
        &self,
        context: &BehaviorContext<'_, Space>,
        tick: Tick,
    ) -> crate::universe::UniverseTransaction {
        let txn = self
            .controller
            .lock()
            .unwrap()
            .step(tick)
            .expect("TODO: behaviors should have an error reporting path");
        // TODO: should be using the attachment bounds instead of the layout grant to validate bounds
        validate_widget_transaction(&self.widget.value, &txn, &self.widget.position)
            .expect("transaction validation failed");
        context.bind_host(txn)
    }

    fn alive(&self, _: &BehaviorContext<'_, Space>) -> bool {
        true
    }

    fn ephemeral(&self) -> bool {
        true
    }
}

/// Errors that may arise from setting up [`LayoutTree`]s and [`Widget`]s and installing
/// them in a [`Space`].
///
/// [`LayoutTree`]: crate::vui::LayoutTree
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum InstallVuiError {
    /// The widget failed to initialize for some reason.
    #[error("error initializing widget ({:?})", .widget)]
    WidgetInitialization {
        /// TODO: This should be `Arc<dyn Widget>` instead.
        /// Or, if we come up with some way of giving widgets IDs, that.
        widget: Box<dyn WidgetController>,

        #[source]
        error: Box<InstallVuiError>,
    },

    /// A transaction conflict arose between two widgets or parts of a widget's installation.
    #[error("transaction conflict involving a widget")]
    #[non_exhaustive]
    Conflict {
        // TODO: Include the widget(s) involved, once `Arc<dyn Widget>` is piped around everywhere
        // and not just sometimes Widget or sometimes WidgetController.
        #[source]
        error: TransactionConflict,
    },

    /// The widget attempted to modify space outside its assigned bounds.
    #[error(
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

    #[error("installing widget tree failed")]
    #[non_exhaustive]
    ExecuteInstallation {
        #[source]
        error: crate::transaction::ExecuteError,
    },
}

/// Create a [`Space`] to put a widget in.
#[cfg(test)]
#[track_caller]
pub(crate) fn instantiate_widget<W: Widget + 'static>(
    grant: LayoutGrant,
    widget: W,
) -> (Option<GridAab>, Space) {
    use crate::transaction::Transaction as _;
    use crate::vui;

    let mut space = Space::builder(grant.bounds).build();
    let txn = vui::install_widgets(grant, &vui::LayoutTree::leaf(Arc::new(widget)))
        .expect("widget instantiation");
    txn.execute(&mut space).expect("widget transaction");
    (txn.bounds(), space)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::assert_send_sync;

    #[test]
    fn error_is_send_sync() {
        assert_send_sync::<InstallVuiError>()
    }

    fn _assert_widget_trait_is_object_safe(_: &dyn Widget) {}
    fn _assert_controller_trait_is_object_safe(_: &dyn WidgetController) {}
}
