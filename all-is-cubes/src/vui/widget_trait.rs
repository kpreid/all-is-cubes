// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! UI [`Widget`] trait and related glue.

use std::error::Error;
use std::fmt::Debug;
use std::sync::{Arc, Mutex};

use crate::apps::Tick;
use crate::behavior::{Behavior, BehaviorContext, BehaviorSetTransaction};
use crate::inv::EphemeralOpaque;
use crate::math::GridPoint;
use crate::space::{Grid, Space, SpaceTransaction};
use crate::transaction::Merge as _;
use crate::universe::{RefVisitor, VisitRefs};
use crate::vui::layout::Layoutable;

// Placeholder for likely wanting to change this later
pub(super) type WidgetTransaction = SpaceTransaction;

/// Something that can participate in layout and turn into some contents of a Space.
///
/// This trait is object-safe so that collections (and in particular [`LayoutTree`]s) of
/// `Arc<dyn Widget>` can be used.
///
/// TODO: Can we name this trait in verb instead of noun form?
///
/// TODO: Explain expectations about interior mutability and sharing.
pub trait Widget: Layoutable + Debug {
    fn controller(self: Arc<Self>, position: GridPoint) -> Box<dyn WidgetController>;
}

/// A form of using a region of a [`Space`] as a UI widget.
///
/// TODO: Merge this into the Behavior trait
pub trait WidgetController: Debug + Send + Sync + 'static {
    /// Write the initial state of the widget to the space.
    ///
    /// TODO: Be more specific than Box<dyn Error> -- perhaps InGenError
    /// TODO: Stop using &mut self in favor of a transaction, like Behavior
    fn initialize(&mut self) -> Result<WidgetTransaction, Box<dyn Error>> {
        Ok(WidgetTransaction::default())
    }

    /// TODO: Be more specific than Box<dyn Error> -- perhaps InGenError
    /// TODO: Stop using &mut self in favor of a transaction, like Behavior
    fn step(&mut self, tick: Tick) -> Result<WidgetTransaction, Box<dyn Error>>;
}

impl WidgetController for Box<dyn WidgetController> {
    fn step(&mut self, tick: Tick) -> Result<WidgetTransaction, Box<dyn Error>> {
        (**self).step(tick)
    }

    fn initialize(&mut self) -> Result<WidgetTransaction, Box<dyn Error>> {
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
    controller: Mutex<Box<dyn WidgetController>>,
}

impl WidgetBehavior {
    /// Returns a transaction which adds the given widget controller to the space,
    /// or an error if the controller's `initialize()` fails.
    pub(crate) fn installation(
        mut controller: Box<dyn WidgetController>,
    ) -> Result<SpaceTransaction, Box<dyn Error>> {
        // TODO: give error context
        let init_txn = controller.initialize()?;
        let add_txn = BehaviorSetTransaction::insert(Arc::new(WidgetBehavior {
            controller: Mutex::new(controller),
        }));
        Ok(init_txn.merge(SpaceTransaction::behaviors(add_txn))?)
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
    ) -> crate::transaction::UniverseTransaction {
        context.bind_host(
            self.controller
                .lock()
                .unwrap()
                .step(tick)
                .expect("TODO: behaviors should have an error reporting path"),
        )
    }

    fn alive(&self, _: &BehaviorContext<'_, Space>) -> bool {
        true
    }

    fn ephemeral(&self) -> bool {
        true
    }
}

/// A region of a [`Space`] that does something if [`Tool::Activate`] is used on it.
///
/// TODO: This is a placeholder for a better design; it's too specific (external side
/// effect) and yet also not general enough (we would like buttons to have detailed
/// reactions to clicking) considering that it's hardcoded in Space.
///
/// TODO: Make the better version of this public
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ActivatableRegion {
    pub(crate) region: Grid,
    pub(crate) effect: EphemeralOpaque<dyn Fn() + Send + Sync>,
}

impl ActivatableRegion {
    pub fn activate(&self) {
        if let Some(f) = &self.effect.0 {
            f();
        }
    }
}

impl Behavior<Space> for ActivatableRegion {
    fn alive(&self, _: &BehaviorContext<'_, Space>) -> bool {
        // TODO: Give a way for this to be deleted automatically
        true
    }

    fn ephemeral(&self) -> bool {
        true
    }
}

impl VisitRefs for ActivatableRegion {
    fn visit_refs(&self, _: &mut dyn RefVisitor) {
        // Our only interesting member is an EphemeralOpaque — which is opaque.
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn _assert_widget_trait_is_object_safe(_: &dyn Widget) {}
    fn _assert_controller_trait_is_object_safe(_: &dyn WidgetController) {}
}
