// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Mechanism for receiving notifications of state changes.
//!
//! Objects which wish to send notifications use [`Notifier`]s, which manage a collection
//! of [`Listener`]s. Each listener reports when it is no longer needed and may be
//! discarded.
//!
//! When [`Notifier::notify`] is called to send a message, it is synchronously delivered
//! to all listeners; therefore, listeners are obligated to avoid making further
//! significant state changes. The typical pattern is for a listener to contain a
//! `Weak<RefCell<...>>` or similar multiply-owned mutable structure to aggregate incoming
//! messages, which will then be read and cleared by a separate part of the game loop.

use std::fmt;
use std::sync::{RwLock, Weak};

mod cell;
pub use cell::*;

mod listeners;
pub use listeners::*;

mod util;
pub use util::*;

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
    listeners: RwLock<Vec<Box<dyn Listener<M> + Send + Sync>>>,
}

impl<M: Clone + Send> Notifier<M> {
    /// Constructs a new empty [`Notifier`].
    pub fn new() -> Self {
        Self {
            listeners: Default::default(),
        }
    }

    /// Add a [`Listener`] to this set of listeners.
    pub fn listen<L: Listener<M> + Send + Sync + 'static>(&self, listener: L) {
        if !listener.alive() {
            return;
        }
        let mut listeners = self.listeners.write().unwrap();
        Self::cleanup(&mut listeners);
        listeners.push(Box::new(listener));
    }

    /// Returns a [`Listener`] which forwards messages to the listeners registered with
    /// this `Notifier`, provided that it is owned by an `Rc`.
    ///
    /// This may be used together with [`Listener::filter`] to forward notifications
    /// of changes in dependencies. Using this operation means that the dependent does not
    /// need to fan out listener registrations to all of its current dependencies.
    ///
    /// ```
    /// use std::sync::Arc;
    /// use all_is_cubes::listen::{Notifier, Sink};
    ///
    /// let notifier_1 = Notifier::new();
    /// let notifier_2 = Arc::new(Notifier::new());
    /// let mut sink = Sink::new();
    /// notifier_1.listen(Notifier::forwarder(Arc::downgrade(&notifier_2)));
    /// notifier_2.listen(sink.listener());
    ///
    /// notifier_1.notify("a");
    /// assert_eq!(sink.drain(), vec!["a"]);
    /// drop(notifier_2);
    /// notifier_1.notify("a");
    /// assert!(sink.drain().is_empty());
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

    /// Discard all dead weak pointers in `listeners`.
    fn cleanup(listeners: &mut Vec<Box<dyn Listener<M> + Send + Sync>>) {
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

impl<M: Clone + Send> Default for Notifier<M> {
    fn default() -> Self {
        Self::new()
    }
}

impl<M> fmt::Debug for Notifier<M> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Ok(listeners) = self.listeners.try_read() {
            fmt.debug_tuple("Notifier").field(&listeners.len()).finish()
        } else {
            fmt.debug_tuple("Notifier").field(&"?").finish()
        }
    }
}

/// A receiver of messages which can indicate when it is no longer interested in
/// them (typically because the associated recipient has been dropped).
///
/// Implementors should also implement [`Clone`] whenever possible; this allows
/// for a "listen" operation to be implemented in terms of delegating to several others.
/// This is not required, so that the `Listener` trait remains object-safe.
pub trait Listener<M> {
    /// Process and store a message.
    ///
    /// Note that, since this method takes `&Self`, a Listener must use interior
    /// mutability of some variety to store the message. As a `Listener` may be called
    /// from various contexts, and in particular while the sender is still performing
    /// its work, that mutability should in general be limited to setting dirty flags
    /// or inserting into message queues — not attempting to directly perform further
    /// game state changes, and particularly not taking any locks that are not solely
    /// used by the `Listener` and its destination, as that could result in deadlock.
    fn receive(&self, message: M);

    /// Returns [`false`] if the [`Listener`] should not receive any further messages
    /// because its destination is no longer interested in them or they would not
    /// have any effects on the rest of the system.
    fn alive(&self) -> bool;

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
    /// use all_is_cubes::listen::{Listener, Gate, Sink};
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn notifier_basics_and_debug() {
        let cn: Notifier<u8> = Notifier::new();
        assert_eq!(format!("{:?}", cn), "Notifier(0)");
        cn.notify(0);
        assert_eq!(format!("{:?}", cn), "Notifier(0)");
        let sink = Sink::new();
        cn.listen(sink.listener());
        assert_eq!(format!("{:?}", cn), "Notifier(1)");
        // type annotation to prevent spurious inference failures in the presence
        // of other compiler errors
        assert_eq!(sink.drain(), Vec::<u8>::new());
        cn.notify(1);
        cn.notify(2);
        assert_eq!(sink.drain(), vec![1, 2]);
        assert_eq!(format!("{:?}", cn), "Notifier(1)");
    }
}
