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
/// * `F` is the type of the filter function to use.
/// * `T` is the type of the listener to pass filtered messages to.
/// * `BATCH` is the maximum number of filtered messages to gather before passing them on.
///   It is used as the size of a stack-allocated array, so should be chosen with the size of
///   the message type in mind.
///
/// TODO: add doc test
pub struct Filter<F, T, const BATCH: usize> {
    /// The function to transform and possibly discard each message.
    pub(super) function: F,
    /// The recipient of the messages.
    pub(super) target: T,
}

impl<F, T> Filter<F, T, 1> {
    /// Request that the filter accumulate output messages into a batch.
    ///
    /// This causes each [receive](Listener::receive) operation to allocate an array `[MO; BATCH]`
    /// on the stack, where `MO` is the output message type produced by `F`.
    /// Therefore, the buffer size should be chosen keeping the size of `MO` in mind.
    /// Also, the amount of buffer used cannot exceed the size of the input batch,
    /// so it is not useful to choose a buffer size larger than the expected batch size.
    ///
    /// If `MO` is a zero-sized type, then the buffer is always unbounded,
    /// so `with_stack_buffer()` has no effect and is unnecessary in that case.
    pub fn with_stack_buffer<const BATCH: usize>(self) -> Filter<F, T, BATCH> {
        Filter {
            function: self.function,
            target: self.target,
        }
    }
}

impl<F, T: fmt::Debug, const BATCH: usize> fmt::Debug for Filter<F, T, BATCH> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Filter")
            // function's type name may be the function name
            .field("function", &core::any::type_name::<F>().refmt(&Unquote))
            .field("target", &self.target)
            .finish()
    }
}

impl<MI, MO, F, T, const BATCH: usize> Listener<MI> for Filter<F, T, BATCH>
where
    F: Fn(&MI) -> Option<MO> + Send + Sync,
    T: Listener<MO>,
{
    fn receive(&self, messages: &[MI]) -> bool {
        if const { size_of::<MO>() == 0 } {
            // If the size of the output message is zero, then we can buffer an arbitrary number
            // of them without occupying any memory or performing any allocation, and therefore
            // preserve the input batching for free.
            let mut filtered_messages = alloc::vec::Vec::<MO>::new();
            for message in messages {
                if let Some(filtered_message) = (self.function)(message) {
                    filtered_messages.push(filtered_message);
                }
            }
            // Deliver entire batch of ZST messages.
            self.target.receive(filtered_messages.as_slice())
        } else {
            let mut buffer: arrayvec::ArrayVec<MO, BATCH> = arrayvec::ArrayVec::new();
            for message in messages {
                if let Some(filtered_message) = (self.function)(message) {
                    // Note that we do this fullness check before, not after, pushing a message,
                    // so if the buffer fills up exactly, we will use the receive() call after
                    // the end of the loop, not this one.
                    if buffer.is_full() {
                        let alive = self.target.receive(&buffer);
                        if !alive {
                            // Target doesn’t want any more messages, so we don’t need to filter
                            // them.
                            return false;
                        }
                        buffer.clear();
                    }
                    buffer.push(filtered_message);
                }
            }
            // Deliver final partial batch, if any, and final liveness check.
            self.target.receive(&buffer)
        }
    }
}

/// Breaks a [`Listener`] connection when dropped.
///
/// Construct this using [`Listener::gate()`], or if a placeholder instance with no
/// effect is required, [`Gate::default()`]. Then, drop the [`Gate`] when no more messages
/// should be delivered.
#[derive(Clone, Default)]
pub struct Gate {
    /// By owning this we keep its [`Weak`] peers alive, and thus the [`GateListener`] active.
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

/// A [`Listener`] which forwards messages to another,
/// until the corresponding [`Gate`] is dropped.
///
/// Construct this using [`Listener::gate()`].
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::listen::{Listen as _, Sink};
    use alloc::vec::Vec;

    /// Breaks the listener rules for testing by recording batch boundaries.
    #[derive(Debug)]
    struct CaptureBatch<L>(L);
    impl<M: Clone, L> Listener<M> for CaptureBatch<L>
    where
        L: Listener<Vec<M>>,
    {
        fn receive(&self, messages: &[M]) -> bool {
            self.0.receive(&[Vec::from(messages)])
        }
    }

    #[test]
    fn filter_filtering_and_drop() {
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

    /// Test the behavior when `with_stack_buffer()` is not called,
    /// leaving the buffer size implicitly at 1.
    #[test]
    fn filter_batch_size_1() {
        let notifier: Notifier<i32> = Notifier::new();
        let sink: Sink<Vec<i32>> = Sink::new();
        notifier.listen(CaptureBatch(sink.listener()).filter(|&x: &i32| Some(x)));

        // Send some batches
        notifier.notify_many(&[0, 1]);
        notifier.notify_many(&[]);
        notifier.notify_many(&[2, 3]);

        // Expect the batches to be of size at most 1
        assert_eq!(
            sink.drain(),
            vec![vec![], vec![0], vec![1], vec![], vec![2], vec![3]]
        );
    }

    #[test]
    fn filter_batching_nzst() {
        let notifier: Notifier<i32> = Notifier::new();
        let sink: Sink<Vec<i32>> = Sink::new();
        notifier.listen(
            CaptureBatch(sink.listener())
                .filter(|&x: &i32| Some(x))
                .with_stack_buffer::<2>(),
        );

        // Send some batches
        notifier.notify_many(&[0, 1]);
        notifier.notify_many(&[]);
        notifier.notify_many(&[2, 3, 4]);

        // Expect the batches to be of size at most 2
        assert_eq!(
            sink.drain(),
            vec![vec![], vec![0, 1], vec![], vec![2, 3], vec![4]]
        );
    }

    /// If the message value is a ZST, then batches are unbounded.
    #[test]
    fn filter_batching_zst() {
        let notifier: Notifier<i32> = Notifier::new();
        let sink: Sink<Vec<()>> = Sink::new();
        notifier.listen(
            CaptureBatch(sink.listener()).filter(|&x: &i32| if x == 2 { None } else { Some(()) }),
        );

        // Send some batches
        notifier.notify_many(&[0, 1]);
        notifier.notify_many(&[]);
        notifier.notify_many(&[2, 3, 4, 5]);

        // Expect batches to be preserved and filtered, even though we didn’t set a batch size.
        assert_eq!(
            sink.drain(),
            vec![
                vec![],           // initial liveness check on listen()
                vec![(), ()],     // first nonempty batch
                vec![],           // empty batch
                vec![(), (), ()], // second nonempty batch, with 1 item dropped
            ]
        );
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
