use alloc::sync::{Arc, Weak};
use core::fmt;

use crate::listen::{Listener, Notifier};

/// A [`Listener`] which transforms or discards messages before passing them on.
/// Construct this using [`Listener::filter`].
///
/// This may be used to drop uninteresting messages or reduce their granularity.
///
/// TODO: add doc test
#[derive(Debug)]
pub struct Filter<F, T> {
    /// The function to transform and possibly discard each message.
    pub(super) function: F,
    /// The recipient of the messages.
    pub(super) target: T,
}
impl<MI, MO, F, T> Listener<MI> for Filter<F, T>
where
    F: Fn(MI) -> Option<MO> + Send + Sync,
    T: Listener<MO>,
{
    fn receive(&self, message: MI) {
        if let Some(filtered_message) = (self.function)(message) {
            self.target.receive(filtered_message);
        }
    }
    fn alive(&self) -> bool {
        self.target.alive()
    }
}

/// Controls a [`Listener`] chain by discarding messages when this gate is dropped.
///
/// Construct this using [`Listener::gate`], or if a placeholder instance with no
/// effect is required, [`Gate::default`].
#[derive(Clone, Default)]
pub struct Gate(Arc<()>);

impl fmt::Debug for Gate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Gate")
    }
}

impl Gate {
    pub(super) fn new<L>(listener: L) -> (Gate, GateListener<L>) {
        let signaller = Arc::new(());
        let weak = Arc::downgrade(&signaller);
        (
            Gate(signaller),
            GateListener {
                weak,
                target: listener,
            },
        )
    }
}

/// [`Listener`] implementation which discards messages when the corresponding [`Gate`]
/// is dropped. Construct this using [`Listener::gate()`].
#[derive(Clone, Debug)]
pub struct GateListener<T> {
    weak: Weak<()>,
    target: T,
}
impl<M, T> Listener<M> for GateListener<T>
where
    T: Listener<M>,
{
    fn receive(&self, message: M) {
        if self.alive() {
            self.target.receive(message);
        }
    }
    fn alive(&self) -> bool {
        self.weak.strong_count() > 0 && self.target.alive()
    }
}

/// A [`Listener`] which forwards messages through a [`Notifier`] to its listeners.
/// Constructed by [`Notifier::forwarder()`].
#[derive(Debug)]
pub struct NotifierForwarder<M>(pub(super) Weak<Notifier<M>>);

impl<M: Clone + Send> Listener<M> for NotifierForwarder<M> {
    fn receive(&self, message: M) {
        if let Some(notifier) = self.0.upgrade() {
            notifier.notify(message);
        }
    }
    fn alive(&self) -> bool {
        self.0.strong_count() > 0
    }
}

impl<M> Clone for NotifierForwarder<M> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::listen::{Listen as _, Sink};

    #[test]
    fn filter() {
        let notifier: Notifier<Option<i32>> = Notifier::new();
        let sink = Sink::new();
        notifier.listen(sink.listener().filter(|x| x));
        assert_eq!(notifier.count(), 1);

        // Try delivering messages
        notifier.notify(Some(1));
        notifier.notify(None);
        assert_eq!(sink.drain(), vec![1]);

        // Drop the sink and the notifier should observe it gone
        drop(sink);
        assert_eq!(notifier.count(), 0);
    }

    #[test]
    fn gate() {
        let notifier: Notifier<i32> = Notifier::new();
        let sink = Sink::new();
        let (gate, listener) = Gate::new(sink.listener());
        notifier.listen(listener);
        assert_eq!(notifier.count(), 1);

        // Try delivering messages
        notifier.notify(1);
        assert_eq!(sink.drain(), vec![1]);

        // Drop the gate and messages should stop passing immediately
        // (even though we didn't even trigger notifier cleanup by calling count())
        drop(gate);
        notifier.notify(2);
        assert_eq!(sink.drain(), Vec::<i32>::new());

        assert_eq!(notifier.count(), 0);
    }

    // Test for NotifierForwarder exists as a doc-test.
}
