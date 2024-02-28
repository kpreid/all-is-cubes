use alloc::sync::{Arc, Weak};
use core::fmt;

use manyfmt::formats::Unquote;
use manyfmt::Refmt as _;

use crate::listen::{Listener, Notifier};

/// A [`Listener`] which transforms or discards messages before passing them on.
/// Construct this using [`Listener::filter`].
///
/// This may be used to drop uninteresting messages or reduce their granularity.
///
/// TODO: add doc test
pub struct Filter<F, T> {
    /// The function to transform and possibly discard each message.
    pub(super) function: F,
    /// The recipient of the messages.
    pub(super) target: T,
}

impl<F, T: fmt::Debug> fmt::Debug for Filter<F, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Filter")
            // function's type name may be the function name
            .field("function", &core::any::type_name::<F>().refmt(&Unquote))
            .field("target", &self.target)
            .finish()
    }
}

impl<MI, MO, F, T> Listener<MI> for Filter<F, T>
where
    F: Fn(&MI) -> Option<MO> + Send + Sync,
    T: Listener<MO>,
{
    fn receive(&self, messages: &[MI]) -> bool {
        if messages.is_empty() {
            return self.target.receive(&[]);
        }

        for message in messages {
            if let Some(filtered_message) = (self.function)(message) {
                // TODO: figure out some kind of stack array batching so we don't do a separate
                // receive() for each message.
                let alive = self.target.receive(&[filtered_message]);
                if !alive {
                    return false;
                }
            }
        }
        true
    }
}

/// Controls a [`Listener`] chain by discarding messages when this gate is dropped.
///
/// Construct this using [`Listener::gate`], or if a placeholder instance with no
/// effect is required, [`Gate::default`].
#[derive(Clone, Default)]
pub struct Gate {
    /// By owning this we keep its [`Weak`] peers alive, and thus the GateListener active.
    _strong: Arc<()>,
}

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
            Gate { _strong: signaller },
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
    fn receive(&self, messages: &[M]) -> bool {
        if self.weak.strong_count() > 0 {
            self.target.receive(messages)
        } else {
            false
        }
    }
}

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

impl<M: Clone + Send> Listener<M> for NotifierForwarder<M> {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::listen::{Listen as _, Sink};
    use alloc::vec::Vec;

    #[test]
    fn filter() {
        let notifier: Notifier<Option<i32>> = Notifier::new();
        let sink = Sink::new();
        notifier.listen(sink.listener().filter(|&x| x));
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
