//! Broadcasting of notifications of state changes, and other messages.
//!
//! This module is a re-export of selected items from [`nosy`].
//! Caution: if the `"std"` feature is disabled, they will change in non-additive ways.

// TODO: Get rid of the renames.

pub use ::nosy::{
    Buffer, Constant, Flag, Gate, GateListener, IntoDynListener, Listen, Listener, NullListener,
    Sink, Source, Store, StoreLock,
};

#[cfg(feature = "std")]
pub use ::nosy::sync::{Cell, CellWithLocal, DynListener, DynSource, Notifier, constant};
#[cfg(not(feature = "std"))]
pub use ::nosy::unsync::{Cell, CellWithLocal, DynListener, DynSource, Notifier, constant};

mod listeners;
pub use listeners::FnListener;
