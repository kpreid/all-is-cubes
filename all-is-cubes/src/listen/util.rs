// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::sync::{Arc, Weak};

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
#[derive(Clone, Debug, Default)]
pub struct Gate(Arc<()>);

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
/// is dropped. Construct this using [`Listener::gate`].
#[derive(Debug)]
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

/// A [`Listener`] which forwards messages through a [`Notifier`].
/// Constructed by [`Notifier::forwarder`].
#[derive(Debug)]
pub(super) struct NotifierForwarder<M>(pub(super) Weak<Notifier<M>>);
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

#[cfg(test)]
mod tests {}
