#![allow(
    clippy::module_name_repetitions,
    reason = "false positive; TODO: remove after Rust 1.84 is released"
)]

use alloc::sync::Weak;
use alloc::vec::Vec;
use core::fmt;
use core::sync::atomic::{AtomicBool, Ordering::Relaxed};

#[cfg(doc)]
use alloc::sync::Arc;

use crate::listen::{DynListener, Listen, Listener};
use crate::util::maybe_sync::RwLock;

#[cfg(doc)]
use crate::listen::ListenableCell;

// -------------------------------------------------------------------------------------------------

/// Message broadcaster, usually used for change notifications.
///
/// A `Notifier<M>` delivers messages of type `M` to a dynamic set of [`Listener`]s.
///
/// The `Notifier` is usually owned by some entity which emits messages when it changes,
/// such as a [`ListenableCell`].
/// Each `Listener` usually holds a weak reference to allow it to be removed when the
/// actual recipient is gone or uninterested.
///
/// [`Listener`]s may be added using the [`Listen`] implementation, and are removed when
/// they report themselves as dead.
pub struct Notifier<M> {
    pub(crate) listeners: RwLock<Vec<NotifierEntry<M>>>,
}

pub(crate) struct NotifierEntry<M> {
    pub(crate) listener: DynListener<M>,
    /// True iff every call to `listener.receive()` has returned true.
    pub(crate) was_alive: AtomicBool,
}

impl<M> Notifier<M> {
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
    /// notifier_1.notify(&"a");
    /// assert_eq!(sink.drain(), vec!["a"]);
    /// drop(notifier_2);
    /// notifier_1.notify(&"a");
    /// assert!(sink.drain().is_empty());
    ///
    /// # assert_eq!(notifier_1.count(), 0);
    /// ```
    pub fn forwarder(this: Weak<Self>) -> NotifierForwarder<M> {
        NotifierForwarder(this)
    }

    /// Deliver a message to all [`Listener`]s.
    pub fn notify(&self, message: &M) {
        self.notify_many(core::slice::from_ref(message))
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

    /// Creates a [`Buffer`] which batches messages sent through it.
    /// This may be used as a more convenient interface to [`Notifier::notify_many()`],
    /// at the cost of delaying messages until the buffer is dropped.
    ///
    /// The buffer does not use any heap allocations and will collect up to `CAPACITY` messages
    /// per batch.
    pub fn buffer<const CAPACITY: usize>(&self) -> Buffer<'_, M, CAPACITY> {
        Buffer::new(self)
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
    pub(crate) fn cleanup(listeners: &mut Vec<NotifierEntry<M>>) {
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

impl<M> Listen for Notifier<M> {
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

impl<M> Default for Notifier<M> {
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

// -------------------------------------------------------------------------------------------------

/// A batch of messages of type `M` to be sent through a [`Notifier`].
///
/// Messages may be added to the buffer, and when the buffer is full or when it is dropped,
/// they are sent through the notifier. Creating such a batch is intended to increase performance
/// by not executing dynamic dispatch to every notifier for every message.
#[derive(Debug)]
pub struct Buffer<'notifier, M, const CAPACITY: usize> {
    pub(crate) buffer: arrayvec::ArrayVec<M, CAPACITY>,
    pub(crate) notifier: &'notifier Notifier<M>,
}

impl<'notifier, M, const CAPACITY: usize> Buffer<'notifier, M, CAPACITY> {
    pub(crate) fn new(notifier: &'notifier Notifier<M>) -> Self {
        Self {
            buffer: arrayvec::ArrayVec::new(),
            notifier,
        }
    }

    /// Store a message in this buffer, to be delivered later as if by [`Notifier::notify()`].
    pub fn push(&mut self, message: M) {
        // We don't need to check for fullness before pushing, because we always flush immediately
        // if full.
        self.buffer.push(message);
        if self.buffer.is_full() {
            self.flush();
        }
    }

    #[cold]
    pub(crate) fn flush(&mut self) {
        self.notifier.notify_many(&self.buffer);
        self.buffer.clear();
    }
}

impl<M, const CAPACITY: usize> Drop for Buffer<'_, M, CAPACITY> {
    fn drop(&mut self) {
        // TODO: Should we discard messages if panicking?
        // Currently leaning no, because we've specified that listeners should not panic even under
        // error conditions such as poisoned mutexes.
        if !self.buffer.is_empty() {
            self.flush();
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// A [`Listener`] which forwards messages through a [`Notifier`] to its listeners.
/// Constructed by [`Notifier::forwarder()`].
pub struct NotifierForwarder<M>(pub(super) Weak<Notifier<M>>);

impl<M> fmt::Debug for NotifierForwarder<M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NotifierForwarder")
            .field("alive(shallow)", &(self.0.strong_count() > 0))
            .finish_non_exhaustive()
    }
}

impl<M> Listener<M> for NotifierForwarder<M> {
    fn receive(&self, messages: &[M]) -> bool {
        if let Some(notifier) = self.0.upgrade() {
            notifier.notify_many(messages);
            true
        } else {
            false
        }
    }
}

impl<M> Clone for NotifierForwarder<M> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::listen::Sink;

    #[test]
    fn notifier_basics_and_debug() {
        let cn: Notifier<u8> = Notifier::new();
        assert_eq!(format!("{cn:?}"), "Notifier(0)");
        cn.notify(&0);
        assert_eq!(format!("{cn:?}"), "Notifier(0)");
        let sink = Sink::new();
        cn.listen(sink.listener());
        assert_eq!(format!("{cn:?}"), "Notifier(1)");
        // type annotation to prevent spurious inference failures in the presence
        // of other compiler errors
        assert_eq!(sink.drain(), Vec::<u8>::new());
        cn.notify(&1);
        cn.notify(&2);
        assert_eq!(sink.drain(), vec![1, 2]);
        assert_eq!(format!("{cn:?}"), "Notifier(1)");
    }

    // Test for NotifierForwarder exists as a doc-test.
}
