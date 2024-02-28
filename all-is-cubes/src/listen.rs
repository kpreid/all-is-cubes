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

use alloc::sync::{Arc, Weak};
use alloc::vec::Vec;
use core::fmt;
use core::sync::atomic::{AtomicBool, Ordering::Relaxed};

use crate::util::maybe_sync::{RwLock, SendSyncIfStd};

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
    /// Note that listeners are removed only via their returning [`false`] from
    /// [`Listener::receive()`]; there is no operation to remove a listener,
    /// nor are subscriptions deduplicated.
    fn listen<L: Listener<Self::Msg> + 'static>(&self, listener: L);
}

impl<T: Listen> Listen for &T {
    type Msg = T::Msg;

    fn listen<L: Listener<Self::Msg> + 'static>(&self, listener: L) {
        (**self).listen(listener)
    }
}
impl<T: Listen> Listen for Arc<T> {
    type Msg = T::Msg;

    fn listen<L: Listener<Self::Msg> + 'static>(&self, listener: L) {
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
    listeners: RwLock<Vec<NotifierEntry<M>>>,
}

struct NotifierEntry<M> {
    listener: DynListener<M>,
    /// True iff every call to `listener.receive()` has returned true.
    was_alive: AtomicBool,
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
        self.notify_many(&[message])
    }

    /// Deliver multiple messages to all [`Listener`]s.
    pub fn notify_many(&self, messages: &[M]) {
        for NotifierEntry {
            listener,
            was_alive,
        } in self.listeners.read().unwrap().iter()
        {
            // Don't load was_alive before sending, because we assume the common case is that
            // a listener implements receive() cheaply when it is dead.
            let alive = listener.receive(messages);

            was_alive.fetch_and(alive, Relaxed);
        }
    }

    /// Computes the exact count of listeners, including asking all current listeners
    /// if they are alive.
    ///
    /// This operation is intended for testing and diagnostic purposes.
    pub fn count(&self) -> usize {
        let mut listeners = self.listeners.write().unwrap();
        Self::cleanup(&mut listeners);
        listeners.len()
    }

    /// Discard all dead weak pointers in `listeners`.
    fn cleanup(listeners: &mut Vec<NotifierEntry<M>>) {
        let mut i = 0;
        while i < listeners.len() {
            let entry = &listeners[i];
            // We must ask the listener, not just consult was_alive, in order to avoid
            // leaking memory if listen() is called repeatedly without any notify().
            // TODO: But we can skip it if the last operation was notify().
            if entry.was_alive.load(Relaxed) && entry.listener.receive(&[]) {
                i += 1;
            } else {
                listeners.swap_remove(i);
            }
        }
    }
}

impl<M: Clone + Send> Listen for Notifier<M> {
    type Msg = M;

    fn listen<L: Listener<M> + 'static>(&self, listener: L) {
        if !listener.receive(&[]) {
            // skip adding it if it's already dead
            return;
        }
        let mut listeners = self.listeners.write().unwrap();
        // TODO: consider amortization by not doing cleanup every time
        Self::cleanup(&mut listeners);
        listeners.push(NotifierEntry {
            listener: listener.erased(),
            was_alive: AtomicBool::new(true),
        });
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
/// Listeners are typically used in trait object form, which may be created by calling
/// [`erased()`](Self::erased); this is done implicitly by [`Notifier`], but calling it
/// earlier may in some cases be useful to minimize the number of separately allocated
/// clones of the listener.
///
/// Please note the requirements set out in [`Listener::receive()`].
///
/// Implementors must also implement [`Send`] and [`Sync`] if the `std` feature of
/// `all-is-cubes` is enabled. (This non-additive-feature behavior is unfortunately the
/// least bad option available.)
pub trait Listener<M>: fmt::Debug + SendSyncIfStd {
    /// Process and store the given series of messages.
    ///
    /// Returns `true` if the listener is still interested in further messages (“alive”),
    /// and `false` if it should be dropped because these and all future messages would have
    /// no observable effect.
    /// A call of the form `.receive(&[])` may be performed to query aliveness without
    /// delivering any messages.
    ///
    /// # Requirements on implementors
    ///
    /// Messages are provided in a batch for efficiency of dispatch.
    /// Each message in the provided slice should be processed exactly the same as if
    /// it were the only message provided.
    /// If the slice is empty, there should be no observable effect.
    ///
    /// This method should not panic under any circumstances, in order to ensure the sender's
    /// other work is not interfered with.
    /// For example, if the listener accesses a poisoned mutex, it should do nothing or clear
    /// the poison, rather than panicking.
    ///
    /// # Advice for implementors
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
    /// Note that a [`Notifier`] might call `.receive(&[])` at any time, particularly when
    /// listeners are added. Be careful not to cause a deadlock in this case; it may be
    /// necessary to avoid locking in the case where there are no messages to be delivered.
    fn receive(&self, messages: &[M]) -> bool;

    /// Convert this listener into trait object form, allowing it to be stored in
    /// collections or passed non-generically.
    ///
    /// The purpose of this method over simply calling [`Arc::new()`] is that it will
    /// avoid double-wrapping of a listener that's already in [`Arc`]. **Other
    /// implementors should not override this.**
    fn erased(self) -> DynListener<M>
    where
        Self: Sized + 'static,
    {
        Arc::new(self)
    }

    /// Apply a map/filter function to incoming messages.
    ///
    /// TODO: Doc test
    fn filter<MI, F>(self, function: F) -> Filter<F, Self>
    where
        Self: Sized,
        F: for<'a> Fn(&'a MI) -> Option<M> + Sync,
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
    /// gated.receive(&["kept"]);
    /// assert!(sink.take_equal("kept"));
    /// drop(gate);
    /// gated.receive(&["discarded"]);
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
pub type DynListener<M> = Arc<dyn Listener<M>>;

impl<M> Listener<M> for DynListener<M> {
    fn receive(&self, messages: &[M]) -> bool {
        (**self).receive(messages)
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
    #[allow(ambiguous_wide_pointer_comparisons)]
    fn erased_listener() {
        let sink = Sink::new();
        let listener: DynListener<&str> = sink.listener().erased();

        // Should not gain a new wrapper when erased() again.
        assert_eq!(
            Arc::as_ptr(&listener),
            Arc::as_ptr(&listener.clone().erased())
        );

        // Should report alive (and not infinitely recurse).
        assert!(listener.receive(&[]));

        // Should deliver messages.
        assert!(listener.receive(&["a"]));
        assert_eq!(sink.drain(), vec!["a"]);

        // Should report dead
        drop(sink);
        assert!(!listener.receive(&[]));
        assert!(!listener.receive(&["b"]));
    }

    /// Demonstrate that [`DynListener`] implements [`fmt::Debug`].
    #[test]
    fn erased_debug() {
        let sink: Sink<&str> = Sink::new();
        let listener: DynListener<&str> = Arc::new(sink.listener());

        assert_eq!(format!("{listener:?}"), "SinkListener { alive: true }");
    }
}
