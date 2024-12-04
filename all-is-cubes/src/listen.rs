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

use alloc::sync::Arc;
use core::fmt;

use crate::util::maybe_sync::SendSyncIfStd;

mod cell;
pub use cell::*;

mod listeners;
pub use listeners::*;

mod notifier;
pub use notifier::*;

mod store;
pub use store::*;

mod util;
pub use util::*;

// -------------------------------------------------------------------------------------------------

/// Ability to subscribe to a source of messages, causing a [`Listener`] to receive them
/// as long as it wishes to.
pub trait Listen {
    /// The type of message which may be obtained from this source.
    ///
    /// Most message types will satisfy `Copy + Send + Sync + 'static`, but this is not required.
    type Msg;

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

// -------------------------------------------------------------------------------------------------

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
///
/// Consider implementing [`Store`] and using [`StoreLock`] instead of implementing [`Listener`].
/// [`StoreLock`] provides the weak reference and mutex that are needed in the most common
/// kind of use of [`Listener`].
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

    /// Apply a map/filter function (similar to [`Iterator::filter_map()`]) to incoming messages.
    ///
    /// Note: By default, this filter breaks up all message batching into batches of 1.
    /// In order to avoid this and have more efficient message delivery, use
    /// [`Filter::with_stack_buffer()`].
    /// This is unnecessary if `size_of::<M>() == 0`; the buffer is automatically unbounded in
    /// that case.
    ///
    /// TODO: Doc test
    fn filter<MI, F>(self, function: F) -> Filter<F, Self, 1>
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
    /// gated.receive(&["kept1"]);
    /// assert_eq!(sink.drain(), vec!["kept1"]);
    /// gated.receive(&["kept2"]);
    /// drop(gate);
    /// gated.receive(&["discarded"]);
    /// assert_eq!(sink.drain(), vec!["kept2"]);
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

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
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
    fn dyn_listener_debug() {
        let sink: Sink<&str> = Sink::new();
        let listener: DynListener<&str> = Arc::new(sink.listener());

        assert_eq!(format!("{listener:?}"), "SinkListener { alive: true, .. }");
    }
}
