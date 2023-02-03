//! Broadcasting of notifications of state changes, and other messages.
//!
//! Objects which wish to send notifications use [`Notifier`]s, which manage a collection
//! of [`Listener`]s. Each listener reports when it is no longer needed and may be
//! discarded.
//!
//! When [`Notifier::notify`] is called to send a message, it is synchronously delivered
//! to all listeners; therefore, listeners are obligated to avoid making further
//! significant state changes. The typical pattern is for a listener to contain a
//! `Weak<Mutex<...>>` or similar multiply-owned mutable structure to aggregate incoming
//! messages, which will then be read and cleared by a later task.

use std::fmt;
use std::sync::{Arc, RwLock, Weak};

mod cell;
pub use cell::*;

mod listeners;
pub use listeners::*;

mod util;
pub use util::*;

/// Ability to subscribe to a source of messages, causing a [`Listener`] to receive them
/// as long as it wishes to.
pub trait Listen {
    /// The type of message which may be obtained from this source.
    type Msg: Clone + Send;

    /// Subscribe the given [`Listener`] to this source of messages.
    ///
    /// Note that listeners are removed only via their returning false from
    /// [`Listener::alive()`]; there is no `unlisten()` operation, and identical listeners
    /// are not deduplicated.
    fn listen<L: Listener<Self::Msg> + Send + Sync + 'static>(&self, listener: L);
}

impl<T: Listen> Listen for &T {
    type Msg = T::Msg;

    fn listen<L: Listener<Self::Msg> + Send + Sync + 'static>(&self, listener: L) {
        (**self).listen(listener)
    }
}
impl<T: Listen> Listen for Arc<T> {
    type Msg = T::Msg;

    fn listen<L: Listener<Self::Msg> + Send + Sync + 'static>(&self, listener: L) {
        (**self).listen(listener)
    }
}

/// Mechanism for observing changes to objects. A [`Notifier`] delivers messages
/// of type `M` to a set of listeners, each of which usually holds a weak reference
/// to allow it to be removed when the actual recipient is gone or uninterested.
///
/// TODO: Currently, each message is [`Clone`]d for each recipient. This is fine for
/// most cases, but in some cases it would be cheaper to pass a reference. We could
/// make Notifier and Listener always take `&M`, but it's not clear how to use
/// references *some* of the time — making `M` be a reference type can't have a
/// satisfactory lifetime.
pub struct Notifier<M> {
    listeners: RwLock<Vec<DynListener<M>>>,
}

impl<M: Clone + Send> Notifier<M> {
    /// Constructs a new empty [`Notifier`].
    pub fn new() -> Self {
        Self {
            listeners: Default::default(),
        }
    }

    /// Returns a [`Listener`] which forwards messages to the listeners registered with
    /// this `Notifier`, provided that it is owned by an [`Arc`].
    ///
    /// This may be used together with [`Listener::filter()`] to forward notifications
    /// of changes in dependencies. Using this operation means that the dependent does not
    /// need to fan out listener registrations to all of its current dependencies.
    ///
    /// ```
    /// use std::sync::Arc;
    /// use all_is_cubes::listen::{Listen, Notifier, Sink};
    ///
    /// let notifier_1 = Notifier::new();
    /// let notifier_2 = Arc::new(Notifier::new());
    /// let mut sink = Sink::new();
    /// notifier_1.listen(Notifier::forwarder(Arc::downgrade(&notifier_2)));
    /// notifier_2.listen(sink.listener());
    /// # assert_eq!(notifier_1.count(), 1);
    /// # assert_eq!(notifier_2.count(), 1);
    ///
    /// notifier_1.notify("a");
    /// assert_eq!(sink.drain(), vec!["a"]);
    /// drop(notifier_2);
    /// notifier_1.notify("a");
    /// assert!(sink.drain().is_empty());
    ///
    /// # assert_eq!(notifier_1.count(), 0);
    /// ```
    pub fn forwarder(this: Weak<Self>) -> NotifierForwarder<M> {
        NotifierForwarder(this)
    }

    /// Deliver a message to all [`Listener`]s.
    pub fn notify(&self, message: M) {
        for listener in self.listeners.read().unwrap().iter() {
            listener.receive(message.clone());
        }
    }

    /// Computes the exact count of listeners, including asking all current listeners
    /// if they are [`alive()`](Listener::alive).
    ///
    /// This operation is intended for testing and diagnostic purposes.
    pub fn count(&self) -> usize {
        let mut listeners = self.listeners.write().unwrap();
        Self::cleanup(&mut listeners);
        listeners.len()
    }

    /// Discard all dead weak pointers in `listeners`.
    fn cleanup(listeners: &mut Vec<DynListener<M>>) {
        let mut i = 0;
        while i < listeners.len() {
            if listeners[i].alive() {
                i += 1;
            } else {
                listeners.swap_remove(i);
            }
        }
    }
}

impl<M: Clone + Send> Listen for Notifier<M> {
    type Msg = M;

    fn listen<L: Listener<M> + Send + Sync + 'static>(&self, listener: L) {
        if !listener.alive() {
            return;
        }
        let mut listeners = self.listeners.write().unwrap();
        Self::cleanup(&mut listeners);
        listeners.push(listener.erased());
    }
}

impl<M: Clone + Send> Default for Notifier<M> {
    fn default() -> Self {
        Self::new()
    }
}

impl<M> fmt::Debug for Notifier<M> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // not using fmt.debug_tuple() so this is never printed on multiple lines
        if let Ok(listeners) = self.listeners.try_read() {
            write!(fmt, "Notifier({})", listeners.len())
        } else {
            write!(fmt, "Notifier(?)")
        }
    }
}

/// A receiver of messages (typically from something implementing [`Listen`]) which can
/// indicate when it is no longer interested in them (typically because the associated
/// recipient has been dropped).
///
/// Please note the requirements set out in [`Listener::receive()`].
///
/// Implementors should also implement [`Clone`] whenever possible; this allows
/// for a "listen" operation to be implemented in terms of delegating to several others.
/// This is not required, so that the `Listener` trait remains object-safe.
///
/// Implementors should also implement [`Send`] and [`Sync`], as most usage of listeners
/// might cross threads. However, this is not strictly required.
pub trait Listener<M> {
    /// Process and store a message.
    ///
    /// Note that, since this method takes `&Self`, a `Listener` must use interior
    /// mutability of some variety to store the message. As a `Listener` may be called
    /// from various contexts, and in particular while the sender is still performing
    /// its work, that mutability should in general be limited to setting dirty flags
    /// or inserting into message queues — not attempting to directly perform further
    /// game state changes, and particularly not taking any locks that are not solely
    /// used by the `Listener` and its destination, as that could result in deadlock.
    ///
    /// The typical pattern is for a listener to contain a `Weak<Mutex<...>>` or similar
    /// multiply-owned mutable structure to aggregate incoming messages, which will
    /// then be read and cleared by a later task; see [`FnListener`] for assistance in
    /// implementing this pattern.
    ///
    /// This method should not panic under any circumstances, or inconsistencies may
    /// result due to further work not being done and messages not being sent.
    fn receive(&self, message: M);

    /// Whether the [`Listener`]'s destination is still interested in receiving messages.
    ///
    /// This method should start returning [`false`] as soon as its destination is no
    /// longer interested in them or they would not have any effects on the rest of the
    /// system; this informs [`Notifier`]s that they should drop this listener and avoid
    /// memory leaks in the form of defunct listeners.
    ///
    /// This method should not panic under any circumstances, or inconsistencies may
    /// result due to further work not being done and messages not being sent.
    fn alive(&self) -> bool;

    /// Convert this listener into trait object form, allowing it to be stored in
    /// collections or passed non-generically.
    ///
    /// The purpose of this method over simply calling [`Arc::new()`] is that it will
    /// avoid double-wrapping of a listener that's already in [`Arc`]. **Other
    /// implementors should not override this.**
    fn erased(self) -> DynListener<M>
    where
        Self: Sized + Send + Sync + 'static,
    {
        Arc::new(self)
    }

    /// Apply a map/filter function to incoming messages.
    ///
    /// TODO: Doc test
    fn filter<MI, F>(self, function: F) -> Filter<F, Self>
    where
        Self: Sized,
        F: Fn(MI) -> Option<M> + Sync,
    {
        Filter {
            function,
            target: self,
        }
    }

    /// Wraps `self` to pass messages only until the returned [`Gate`], and any clones
    /// of it, are dropped.
    ///    
    /// This may be used to stop forwarding messages when a dependency no longer exists.
    ///
    /// ```
    /// use all_is_cubes::listen::{Listen, Listener, Gate, Sink};
    ///
    /// let sink = Sink::new();
    /// let (gate, gated) = sink.listener().gate();
    /// gated.receive("kept");
    /// assert!(sink.take_equal("kept"));
    /// drop(gate);
    /// gated.receive("discarded");
    /// assert!(!sink.take_equal("discarded"));
    /// ```
    fn gate(self) -> (Gate, GateListener<Self>)
    where
        Self: Sized,
    {
        Gate::new(self)
    }
}

/// Type-erased form of a [`Listener`] which accepts messages of type `M`.
pub type DynListener<M> = Arc<dyn Listener<M> + Send + Sync>;

impl<M> Listener<M> for DynListener<M> {
    fn receive(&self, message: M) {
        (**self).receive(message)
    }

    fn alive(&self) -> bool {
        (**self).alive()
    }

    fn erased(self) -> DynListener<M> {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn notifier_basics_and_debug() {
        let cn: Notifier<u8> = Notifier::new();
        assert_eq!(format!("{cn:?}"), "Notifier(0)");
        cn.notify(0);
        assert_eq!(format!("{cn:?}"), "Notifier(0)");
        let sink = Sink::new();
        cn.listen(sink.listener());
        assert_eq!(format!("{cn:?}"), "Notifier(1)");
        // type annotation to prevent spurious inference failures in the presence
        // of other compiler errors
        assert_eq!(sink.drain(), Vec::<u8>::new());
        cn.notify(1);
        cn.notify(2);
        assert_eq!(sink.drain(), vec![1, 2]);
        assert_eq!(format!("{cn:?}"), "Notifier(1)");
    }

    #[test]
    #[allow(clippy::vtable_address_comparisons)]
    fn erased_listener() {
        let sink = Sink::new();
        let listener: DynListener<&str> = sink.listener().erased();

        // Should not gain a new wrapper when erased() again.
        assert_eq!(
            Arc::as_ptr(&listener),
            Arc::as_ptr(&listener.clone().erased())
        );

        // Should report alive (and not infinitely recurse).
        assert!(listener.alive());

        // Should deliver messages.
        listener.receive("a");
        assert_eq!(sink.drain(), vec!["a"]);

        // Should report dead
        drop(sink);
        assert!(!listener.alive());
    }
}
