use std::collections::VecDeque;
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock, Weak};

use crate::listen::Listener;

/// A [`Listener`] which discards all messages and is suitable for filling
/// listener parameters when no listener is needed.
#[allow(clippy::exhaustive_structs)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NullListener;

impl<M> Listener<M> for NullListener {
    fn receive(&self, _message: M) {}
    fn alive(&self) -> bool {
        false
    }
}

/// A [`Listener`] which delivers messages by calling a function on a [`Weak`] reference's
/// referent, and stops when the weak reference breaks.
#[derive(Clone, Debug)]
pub struct FnListener<F, T> {
    function: F,
    weak_target: Weak<T>,
}

impl<F, T> FnListener<F, T> {
    #[allow(missing_docs)]
    pub fn new(target: &Arc<T>, function: F) -> Self {
        Self {
            function,
            weak_target: Arc::downgrade(target),
        }
    }
}

impl<M, F, T> Listener<M> for FnListener<F, T>
where
    F: Fn(&T, M),
{
    fn receive(&self, message: M) {
        if let Some(strong_target) = self.weak_target.upgrade() {
            (self.function)(&*strong_target, message);
        }
    }

    fn alive(&self) -> bool {
        self.weak_target.strong_count() > 0
    }
}

/// A [`Listener`] which stores all the messages it receives.
///
/// This is only intended for testing.
#[derive(Debug)]
pub struct Sink<M> {
    messages: Arc<RwLock<VecDeque<M>>>,
}

/// [`Sink::listener()`] implementation.
#[derive(Debug)]
pub struct SinkListener<M> {
    weak_messages: Weak<RwLock<VecDeque<M>>>,
}

impl<M> Sink<M> {
    /// Constructs a new empty [`Sink`].
    pub fn new() -> Self {
        Self {
            messages: Arc::new(RwLock::new(VecDeque::new())),
        }
    }

    /// Returns a [`Listener`] which records the messages it receives in this Sink.
    pub fn listener(&self) -> SinkListener<M> {
        SinkListener {
            weak_messages: Arc::downgrade(&self.messages),
        }
    }

    /// If the given message was received, remove the first occurrence of it and return true.
    ///
    /// ```
    /// use all_is_cubes::listen::{Listener, Sink};
    ///
    /// let sink = Sink::new();
    /// sink.listener().receive(2);
    /// assert!(!sink.take_equal(1));  // No match
    /// assert!(sink.take_equal(2));   // Match
    /// assert!(!sink.take_equal(2));  // Now removed
    /// ```
    ///
    /// TODO: This is never used and therefore a candidate for removal.
    pub fn take_equal(&self, message: M) -> bool
    where
        M: Eq,
    {
        let mut queue = self.messages.write().unwrap();
        if let Some(index) = queue
            .iter()
            .enumerate()
            .filter_map(|(i, m)| (*m == message).then_some(i))
            .next()
        {
            queue.remove(index);
            true
        } else {
            false
        }
    }

    /// Remove and return all messages returned so far.
    ///
    /// ```
    /// use all_is_cubes::listen::{Listener, Sink};
    ///
    /// let sink = Sink::new();
    /// sink.listener().receive(1);
    /// sink.listener().receive(2);
    /// assert_eq!(sink.drain(), vec![1, 2]);
    /// sink.listener().receive(3);
    /// assert_eq!(sink.drain(), vec![3]);
    /// ```
    pub fn drain(&self) -> Vec<M> {
        self.messages.write().unwrap().drain(..).collect()
    }
}

impl<M> Listener<M> for SinkListener<M> {
    fn receive(&self, message: M) {
        if let Some(cell) = self.weak_messages.upgrade() {
            cell.write().unwrap().push_back(message);
        }
    }
    fn alive(&self) -> bool {
        self.weak_messages.strong_count() > 0
    }
}

impl<M> Clone for SinkListener<M> {
    fn clone(&self) -> Self {
        Self {
            weak_messages: self.weak_messages.clone(),
        }
    }
}

impl<M> Default for Sink<M>
where
    M: Send + Sync,
{
    // This implementation cannot be derived because we do not want M: Default

    fn default() -> Self {
        Self::new()
    }
}

/// A [`Listener`] destination which only stores a single flag indicating if any messages
/// were received.
pub struct DirtyFlag {
    flag: Arc<AtomicBool>,
}
impl fmt::Debug for DirtyFlag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("DirtyFlag")
            .field(&self.flag.load(Ordering::Relaxed))
            .finish()
    }
}

/// [`DirtyFlag::listener()`] implementation.
#[derive(Clone, Debug)]
pub struct DirtyFlagListener {
    weak_flag: Weak<AtomicBool>,
}

impl DirtyFlag {
    /// Constructs a new [`DirtyFlag`] with the given initial value.
    pub fn new(value: bool) -> Self {
        Self {
            flag: Arc::new(AtomicBool::new(value)),
        }
    }

    /// Constructs a new [`DirtyFlag`] with the given initial value
    /// and provides its [`Listener`] to the given function to be installed.
    ///
    /// This is a convenience for calling `new()` followed by `listener()`.
    pub fn listening(value: bool, listener_acceptor: impl FnOnce(DirtyFlagListener)) -> Self {
        let new_self = Self::new(value);
        listener_acceptor(new_self.listener());
        new_self
    }

    /// Returns a [`Listener`] which will set this flag to [`true`] when it receives any
    /// message.
    pub fn listener(&self) -> DirtyFlagListener {
        DirtyFlagListener {
            weak_flag: Arc::downgrade(&self.flag),
        }
    }

    /// Returns the flag value, setting it to [`false`] at the same time.
    pub fn get_and_clear(&self) -> bool {
        self.flag.swap(false, Ordering::Acquire)
    }

    /// Set the flag value to [`true`].
    ///
    /// Usually a [`DirtyFlagListener`] is used instead of this, but it may be useful
    /// in complex situations.
    pub fn set(&self) {
        self.flag.store(true, Ordering::Relaxed);
    }
}
impl<M> Listener<M> for DirtyFlagListener {
    fn receive(&self, _message: M) {
        if let Some(cell) = self.weak_flag.upgrade() {
            cell.store(true, Ordering::Release);
        }
    }
    fn alive(&self) -> bool {
        self.weak_flag.strong_count() > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::listen::Notifier;

    #[test]
    fn null_alive() {
        let notifier: Notifier<()> = Notifier::new();
        notifier.listen(NullListener);
        assert_eq!(notifier.count(), 0);
    }

    #[test]
    fn sink_alive() {
        let notifier: Notifier<()> = Notifier::new();
        let sink = Sink::new();
        notifier.listen(sink.listener());
        assert_eq!(notifier.count(), 1);
        drop(sink);
        assert_eq!(notifier.count(), 0);
    }

    #[test]
    fn dirty_flag_alive() {
        let notifier: Notifier<()> = Notifier::new();
        let flag = DirtyFlag::new(false);
        notifier.listen(flag.listener());
        assert_eq!(notifier.count(), 1);
        drop(flag);
        assert_eq!(notifier.count(), 0);
    }

    #[test]
    fn dirty_flag_debug() {
        assert_eq!(format!("{:?}", DirtyFlag::new(false)), "DirtyFlag(false)");
        assert_eq!(format!("{:?}", DirtyFlag::new(true)), "DirtyFlag(true)");
        let dirtied = DirtyFlag::new(false);
        dirtied.listener().receive(());
        assert_eq!(format!("{dirtied:?}"), "DirtyFlag(true)");
    }
}
