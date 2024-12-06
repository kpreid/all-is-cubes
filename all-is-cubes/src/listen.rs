//! Broadcasting of notifications of state changes, and other messages.
//!
//! This module is a re-export of selected items from [`nosy`].
//! Caution: if the `"std"` feature is disabled, they will change in non-additive ways.

// TODO: Get rid of the renames.

pub use ::nosy::{
    Buffer, Constant, Flag as DirtyFlag, Gate, GateListener, IntoDynListener, Listen, Listener,
    NullListener, Sink, Source, Store, StoreLock,
};

#[cfg(feature = "std")]
pub use ::nosy::sync::{
    constant, Cell as ListenableCell, CellWithLocal as ListenableCellWithLocal, DynListener,
    DynSource, Notifier,
};
#[cfg(not(feature = "std"))]
pub use ::nosy::unsync::{
    constant, Cell as ListenableCell, CellWithLocal as ListenableCellWithLocal, DynListener,
    DynSource, Notifier,
};

mod listeners;
pub use listeners::FnListener;
