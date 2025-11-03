//! Thread-local state for building graphs of [`Handle`]s.
//!
//! This is used by deserialization and by `arbitrary::Arbitrary` implementations
//! to construct handles in a shared [`Universe`] despite the lack of a place to pass
//! `&mut Universe` directly.
//!
//! In principle, we should be using [`serde::de::DeserializeSeed`] and some kind of wrapper types
//! for `arbitrary`, instead of this mechanism. But that will be a lot of boilerplate work
//! and it can wait for a “1.0” polishing stage.

#![cfg(any(feature = "save", feature = "arbitrary"))]

use alloc::boxed::Box;
use core::cell::RefCell;

use crate::universe::Universe;

// -------------------------------------------------------------------------------------------------

std::thread_local! {
    /// Thread-local state used to communicate from [`Universe`] deserialization to
    /// [`Handle`] deserialization so that the [`Handle`]`] points to a member of that
    /// [`Universe`].
    ///
    /// If [`None`], no [`Universe`] deserialization is currently occurring.
    ///
    /// TODO: Find an alternative not dependent on external state. Perhaps
    /// serde::DeserializeSeed will do, or if necessary we can modify Handle to support
    /// modification after construction.
    static HANDLE_CONTEXT: RefCell<Option<Context>> = const {
        RefCell::new(None)
    };
}

/// Thread-local context value used to deserialize or otherwise construct `Handle`s referring to
/// a universe that is being mutated.
#[derive(Debug)]
pub(crate) struct Context {
    pub(crate) purpose: Purpose,
    pub(crate) universe: Box<Universe>,
}

/// Identifies the purpose of the context, to guard against unintended weird reentrant usage.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum Purpose {
    #[cfg(feature = "save")]
    Deserialization,
    #[cfg(feature = "arbitrary")]
    Arbitrary,
}

/// Guard for an active [`Context`]; create it to install one and destroy it to retrieve the
/// universe.
pub(crate) struct Scope;

impl Scope {
    pub fn install(new_context: Context) -> Self {
        HANDLE_CONTEXT.with_borrow_mut(|tl| {
            if let Some(old_context) = tl {
                panic!(
                    "cannot nest tl::Context.\n\
                     existing: {old_context:#?}\n\
                     attempted: {new_context:#?}"
                );
            }
            assert!(tl.is_none(), "cannot nest Universe deserialization");
            *tl = Some(new_context);
        });
        Self
    }

    /// Uninstall and retrieve the context. Panics if none present.
    pub fn take(self, purpose: Purpose) -> Context {
        let context = HANDLE_CONTEXT
            .with_borrow_mut(|tl| tl.take().expect("something went wrong with HANDLE_CONTEXT"));
        core::mem::forget(self); // don't run Drop
        assert_eq!(context.purpose, purpose);
        context
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        HANDLE_CONTEXT.with(|handle_context| {
            let mut handle_context = handle_context.borrow_mut();
            assert!(
                handle_context.is_some(),
                "something went wrong with HANDLE_CONTEXT"
            );
            *handle_context = None;
        });
    }
}

pub(crate) fn get_from_context<R>(
    purpose: Purpose,
    function: impl FnOnce(&mut Context) -> R,
) -> Option<R> {
    HANDLE_CONTEXT.with(|context_cell| -> Option<R> {
        Option::as_mut(&mut context_cell.borrow_mut()).map(|context| {
            assert_eq!(context.purpose, purpose);
            function(context)
        })
    })
}

// -------------------------------------------------------------------------------------------------
