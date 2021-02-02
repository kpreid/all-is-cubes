// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
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

use indexmap::IndexSet;
use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::{Rc, Weak};

/// Mechanism for observing changes to objects. A [`Notifier`] delivers messages
/// to a set of listeners which implement some form of weak-reference semantics
/// to allow cleanup.
pub struct Notifier<M: Clone> {
    listeners: RefCell<Vec<Box<dyn Listener<M>>>>,
}

impl<M: Clone> Notifier<M> {
    /// Constructs a new empty [`Notifier`].
    pub fn new() -> Self {
        Self {
            listeners: Default::default(),
        }
    }

    /// Add a [`Listener`] to this set of listeners.
    pub fn listen<L: Listener<M> + 'static>(&self, listener: L) {
        if !listener.alive() {
            return;
        }
        let mut listeners = self
            .listeners
            .try_borrow_mut()
            .expect("Adding listeners while a notification is being sent is not implemented");
        Self::cleanup(&mut listeners);
        listeners.push(Box::new(listener));
    }

    /// Returns a [`Listener`] which forwards messages to the listeners registered with
    /// this `Notifier`, provided that it is owned by an `Rc`.
    ///
    /// This may be used together with [`ListenerHelper::filter`] to forward notifications
    /// of changes in dependencies. Using this operation means that the dependent does not
    /// need to fan out listener registrations to all of its current dependencies.
    ///
    /// ```
    /// use std::rc::Rc;
    /// use all_is_cubes::listen::{Notifier, Sink};
    ///
    /// let notifier_1 = Notifier::new();
    /// let notifier_2 = Rc::new(Notifier::new());
    /// let mut sink = Sink::new();
    /// notifier_1.listen(Notifier::forwarder(Rc::downgrade(&notifier_2)));
    /// notifier_2.listen(sink.listener());
    ///
    /// notifier_1.notify("a");
    /// assert!(sink.take_equal("a"));
    /// drop(notifier_2);
    /// notifier_1.notify("a");
    /// assert_eq!(None, sink.next());
    /// ```
    pub fn forwarder(this: Weak<Self>) -> impl Listener<M> {
        NotifierForwarder(this)
    }

    /// Deliver a message to all [`Listener`]s.
    pub fn notify(&self, message: M) {
        for listener in self.listeners.borrow().iter() {
            listener.receive(message.clone());
        }
    }

    /// Discard all dead weak pointers in `listeners`.
    fn cleanup(listeners: &mut Vec<Box<dyn Listener<M>>>) {
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

impl<M: Clone> Default for Notifier<M> {
    fn default() -> Self {
        Self::new()
    }
}

impl<M: Clone> Debug for Notifier<M> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(listeners) = self.listeners.try_borrow() {
            fmt.debug_tuple("Notifier").field(&listeners.len()).finish()
        } else {
            fmt.debug_tuple("Notifier").field(&"?").finish()
        }
    }
}

/// A receiver of messages which can indicate when it is no longer interested in
/// them (typically because the associated recipient has been dropped). Note that
/// a Listener must use interior mutability to store the message. As a Listener
/// may be called from various contexts, that mutability should in general be limited
/// to setting dirty flags or inserting into message queues — not triggering any
/// state changes of more general interest.
pub trait Listener<M> {
    /// Process and store a message.
    ///
    /// As a Listener may be called from various contexts, this method should avoid
    /// triggering further side effects, by setting dirty flags or inserting into
    /// message queues — definitely not taking a lock or borrowing a [`RefCell`] that
    /// is not for the sole use of the [`Listener`] and its destination.
    fn receive(&self, message: M);

    /// Returns [`false`] if the [`Listener`] should not receive any further messages
    /// because its destination is no longer interested in them or they would not
    /// have any effects on the rest of the system.
    fn alive(&self) -> bool;
}

/// Methods for adapting listeners that would make `Listener` not [object-safe]
/// (https://doc.rust-lang.org/book/ch17-02-trait-objects.html).
pub trait ListenerHelper<M>
where
    Self: Sized,
{
    /// Apply a map/filter function to incoming messages.
    ///
    /// TODO: Doc test
    fn filter<MI, F>(self, function: F) -> Filter<F, Self>
    where
        F: Fn(MI) -> Option<M>,
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
    /// use all_is_cubes::listen::{Listener, ListenerHelper, Gate, Sink};
    ///
    /// let sink = Sink::new();
    /// let (gate, gated) = sink.listener().gate();
    /// gated.receive("kept");
    /// assert!(sink.take_equal("kept"));
    /// drop(gate);
    /// gated.receive("discarded");
    /// assert!(!sink.take_equal("discarded"));
    /// ```
    fn gate(self) -> (Gate, GateListener<Self>) {
        let signaller = Rc::new(());
        let weak = Rc::downgrade(&signaller);
        (Gate(signaller), GateListener { weak, target: self })
    }
}
impl<M, L: Listener<M> + Sized> ListenerHelper<M> for L {}

/// A [`Listener`] which discards all messages and is suitable for filling
/// listener parameters when no listener is needed.
pub struct NullListener;

impl<M> Listener<M> for NullListener {
    fn receive(&self, _message: M) {}
    fn alive(&self) -> bool {
        false
    }
}

/// A [`Listener`] destination which stores all the messages it receives, deduplicated.
///
/// TODO: This type turns out to be only useful in tests. Rework it to be more fitting.
pub struct Sink<M> {
    messages: Rc<RefCell<IndexSet<M>>>,
}
struct SinkListener<M> {
    weak_messages: Weak<RefCell<IndexSet<M>>>,
}

impl<M> Sink<M>
where
    M: Eq + Hash + Clone,
{
    /// Constructs a new empty [`Sink`].
    pub fn new() -> Self {
        Self {
            messages: Rc::new(RefCell::new(IndexSet::new())),
        }
    }

    /// Returns a [`Listener`] which records the messages it receives in this Sink.
    #[allow(dead_code)] // TODO: only used in tests but maybe should be public
    pub fn listener(&self) -> impl Listener<M> {
        SinkListener {
            weak_messages: Rc::downgrade(&self.messages),
        }
    }

    /// If the given message was received, remove it and return true.
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
    pub fn take_equal(&self, message: M) -> bool {
        self.messages.borrow_mut().swap_remove(&message)
    }
}
/// As an [`Iterator`], yields all messages currently waiting in arbitrary order.
/// TODO: A singular Iterator is not the best way to express polling.
/// Generate independent Iterators (that can be consumed) or use something else.
impl<M> Iterator for Sink<M>
where
    M: Eq + Hash + Clone,
{
    type Item = M;
    fn next(&mut self) -> Option<M> {
        self.messages.borrow_mut().pop()
    }
}
impl<M: Eq + Hash + Clone> Listener<M> for SinkListener<M> {
    fn receive(&self, message: M) {
        if let Some(cell) = self.weak_messages.upgrade() {
            cell.borrow_mut().insert(message);
        }
    }
    fn alive(&self) -> bool {
        self.weak_messages.strong_count() > 0
    }
}
impl<M> Default for Sink<M>
where
    M: Eq + Hash + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

/// A [`Listener`] destination which only stores a single flag indicating if any messages
/// were received.
pub struct DirtyFlag {
    flag: Rc<Cell<bool>>,
}
impl Debug for DirtyFlag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("DirtyFlag").field(&self.flag.get()).finish()
    }
}

struct DirtyFlagListener {
    weak_flag: Weak<Cell<bool>>,
}
impl DirtyFlag {
    /// Constructs a new [`DirtyFlag`] with the flag value set to [`false`].
    pub fn new(value: bool) -> Self {
        Self {
            flag: Rc::new(Cell::new(value)),
        }
    }

    /// Returns a [`Listener`] which will set this flag to [`true`] when it receives any
    /// message.
    pub fn listener<M>(&self) -> impl Listener<M> {
        DirtyFlagListener {
            weak_flag: Rc::downgrade(&self.flag),
        }
    }

    /// Returns the flag value, setting it to [`false`] at the same time.
    pub fn get_and_clear(&self) -> bool {
        self.flag.replace(false)
    }
}
impl<M> Listener<M> for DirtyFlagListener {
    fn receive(&self, _message: M) {
        if let Some(cell) = self.weak_flag.upgrade() {
            cell.set(true);
        }
    }
    fn alive(&self) -> bool {
        self.weak_flag.strong_count() > 0
    }
}

/// A [`Listener`] which transforms messages before passing them on.
///
/// This may be used to drop uninteresting messages or reduce their granularity.
///
/// TODO: add doc test
pub struct Filter<F, T> {
    /// The function to transform and possibly discard each message.
    function: F,
    /// The recipient of the messages.
    target: T,
}
impl<MI, MO, F, T> Listener<MI> for Filter<F, T>
where
    F: Fn(MI) -> Option<MO>,
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
/// Construct this using [`ListenerHelper::gate`], or if a placeholder instance with no
/// effect is required, [`Gate::default`].
#[derive(Clone, Debug, Default)]
pub struct Gate(Rc<()>);

/// [`Listener`] implementation which discards messages when the corresponding [`Gate`]
/// is dropped. Construct this using [`ListenerHelper::gate`].
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
struct NotifierForwarder<M: Clone>(Weak<Notifier<M>>);
impl<M: Clone> Listener<M> for NotifierForwarder<M> {
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
mod tests {
    use super::*;

    #[test]
    fn notifier_basics_and_debug() {
        let cn: Notifier<u8> = Notifier::new();
        assert_eq!(format!("{:?}", cn), "Notifier(0)");
        cn.notify(0);
        assert_eq!(format!("{:?}", cn), "Notifier(0)");
        let mut sink = Sink::new();
        cn.listen(sink.listener());
        assert_eq!(format!("{:?}", cn), "Notifier(1)");
        assert_eq!(None, sink.next());
        cn.notify(1);
        cn.notify(2);
        assert_eq!(Some(2), sink.next());
        assert_eq!(Some(1), sink.next());
        assert_eq!(None, sink.next());
        assert_eq!(format!("{:?}", cn), "Notifier(1)");
    }

    #[test]
    fn dirty_flag_debug() {
        assert_eq!(format!("{:?}", DirtyFlag::new(false)), "DirtyFlag(false)");
        assert_eq!(format!("{:?}", DirtyFlag::new(true)), "DirtyFlag(true)");
        let dirtied = DirtyFlag::new(false);
        dirtied.listener().receive(());
        assert_eq!(format!("{:?}", dirtied), "DirtyFlag(true)");
    }
}
