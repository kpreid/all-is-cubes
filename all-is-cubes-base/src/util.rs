//! Tools that we could imagine being in the Rust standard library, but aren't.

use alloc::sync::Arc;
use core::fmt;
use core::marker::PhantomData;

use futures_core::future::BoxFuture;

// -------------------------------------------------------------------------------------------------

mod custom_format;
pub use custom_format::*;

#[doc(hidden)] // Not intended as public API (yet)
pub mod log;

#[cfg(any(feature = "std", test))]
mod multi_failure;
#[doc(hidden)] // experimental, may become a library
#[cfg(any(feature = "std", test))]
pub use multi_failure::MultiFailure;

// -------------------------------------------------------------------------------------------------

/// Interface to start concurrent tasks.
///
/// In the typical case, applications making use of All is Cubes libraries provide an implementation
/// of this trait to functions which can make use of it.
///
/// Executors are typically passed as [`Arc<dyn Executor>`][Arc].
/// Dropping the `Executor` should not cancel tasks started by it; the `Executor` implementation
/// is a “handle to” the actual executor mechanism.
pub trait Executor: fmt::Debug + Send + Sync {
    /// Create a set of tasks which run the futures created by calling `task_factory` several times,
    /// if possible.
    ///
    /// The given `task_factory` is called some number of times appropriate to the available
    /// parallelism. If only single-threaded asynchronous execution is supported, it will be called
    /// once. It may be called zero times; callers must be able to complete their work without the
    /// assistance of these tasks.
    ///
    /// The future **must periodically yield** by calling [`Executor::yield_now()`].
    /// Otherwise, it may prevent other tasks, even “foreground” ones, from progressing.
    /// This requirement is for the benefit of single-threaded [`Executor`]s.
    ///
    /// The tasks are responsible for terminating on their own when they are no longer needed,
    /// such as by a channel receiver being closed.
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> BoxFuture<'static, ()>);

    /// Grants an opportunity for other tasks to execute instead of the current one.
    ///
    /// This should only be performed from inside of a [`Executor::spawn_background()`] task.
    /// If it is called (or polled) under other circumstances, it may panic or have negative
    /// effects on task scheduling.
    //---
    // If Rust ever gets object-safe async fn in trait without boxing, use it here.
    fn yield_now(&self) -> BoxFuture<'static, ()>;
}
#[allow(clippy::missing_inline_in_public_items)]
impl<T: ?Sized + Executor> Executor for &T {
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> BoxFuture<'static, ()>) {
        (**self).spawn_background(task_factory)
    }
    fn yield_now(&self) -> BoxFuture<'static, ()> {
        (**self).yield_now()
    }
}
#[allow(clippy::missing_inline_in_public_items)]
impl<T: ?Sized + Executor> Executor for Arc<T> {
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> BoxFuture<'static, ()>) {
        (**self).spawn_background(task_factory)
    }
    fn yield_now(&self) -> BoxFuture<'static, ()> {
        (**self).yield_now()
    }
}
/// No-op executor for applications which cannot provide one.
#[allow(clippy::missing_inline_in_public_items)]
impl Executor for () {
    fn spawn_background(&self, _: &mut dyn FnMut() -> BoxFuture<'static, ()>) {}
    fn yield_now(&self) -> BoxFuture<'static, ()> {
        unreachable!(
            "yield_now() should only be called from a task, \
            and this executor does not support tasks"
        )
    }
}

#[doc(hidden)]
pub use error_chain::ErrorChain;
mod error_chain {
    use core::error::Error;
    use core::fmt;

    /// Formatting wrapper which prints an [`Error`] together with its
    /// `source()` chain, with at least one newline between each.
    ///
    /// The text begins with the [`fmt::Display`] format of the error.
    ///
    /// Design note: This is not a [`manyfmt::Fmt`] because that has a blanket implementation
    /// which interferes with this one for [`Error`].
    #[doc(hidden)] // not something we wish to be stable public API
    #[derive(Clone, Copy, Debug)]
    #[expect(clippy::exhaustive_structs)]
    pub struct ErrorChain<'a>(pub &'a (dyn Error + 'a));

    impl fmt::Display for ErrorChain<'_> {
        #[allow(clippy::missing_inline_in_public_items)]
        fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
            format_error_chain(fmt, self.0)
        }
    }
    fn format_error_chain(
        fmt: &mut fmt::Formatter<'_>,
        mut error: &(dyn Error + '_),
    ) -> fmt::Result {
        // Write the error's own message. This is expected NOT to contain the sources itself.
        write!(fmt, "{error}")?;

        while let Some(source) = error.source() {
            error = source;
            write!(fmt, "\n\nCaused by:\n    {error}")?;
        }

        Ok(())
    }
}

/// Transforms the items entering an [`Extend`] implementation with a provided function.
///
/// TODO: this is only used by the wireframe debug mesh mechanism and should be reconsidered
#[doc(hidden)] // pub to be used by all-is-cubes-gpu
#[derive(Debug)]
pub struct MapExtend<'t, In, T, F> {
    target: &'t mut T,
    function: F,
    _input: PhantomData<fn(In)>,
}

impl<'t, In, Out, T, F> MapExtend<'t, In, T, F>
where
    // These bounds are not strictly necessary, but allow better type inference for closures.
    T: Extend<Out>,
    F: Fn(In) -> Out,
{
    #[inline]
    pub fn new(target: &'t mut T, function: F) -> Self {
        Self {
            target,
            function,
            _input: PhantomData,
        }
    }
}

impl<In, Out, T, F> Extend<In> for MapExtend<'_, In, T, F>
where
    T: Extend<Out>,
    F: Fn(In) -> Out,
{
    #[inline]
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = In>,
    {
        self.target.extend(iter.into_iter().map(&self.function));
    }
}

#[doc(hidden)] // for use in internal tests only
#[allow(clippy::missing_inline_in_public_items)]
pub fn assert_send_sync<T: Send + Sync>() {
    // We don't need to do anything in this function; the call to it having been successfully
    // compiled is the assertion.
}

/// Check that the future is Send. This function does not need to be actually run;
/// a call to it only needs to be compiled, not run.
///
/// Strictly, this function does not need a [`Future`] bound, but having it catches the mistake
/// of passing something other than a future (e.g. the return value of a non-async function).
#[doc(hidden)] // for use in internal tests only
#[allow(clippy::missing_inline_in_public_items)]
pub fn assert_send_future<T: Send + Future>(_: T) {
    // We don't need to do anything in this function; the call to it having been successfully
    // compiled is the assertion.
}

// Assert `Send + Sync` only if the `std` feature is active.
#[cfg(feature = "std")]
#[doc(hidden)] // for use in internal tests only
#[allow(clippy::missing_inline_in_public_items)]
pub fn assert_conditional_send_sync<T: Send + Sync>() {}
#[cfg(not(feature = "std"))]
#[doc(hidden)] // for use in internal tests only
#[allow(clippy::missing_inline_in_public_items)]
pub fn assert_conditional_send_sync<T>() {}

#[cfg(test)]
mod tests {
    use super::*;

    fn _assert_executor_trait_is_object_safe(_: &dyn Executor) {}

    #[test]
    fn error_chain() {
        use core::error::Error;
        use core::fmt;

        #[derive(Debug)]
        struct TestError1;
        impl Error for TestError1 {}
        impl fmt::Display for TestError1 {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "TestError1")
            }
        }

        #[derive(Debug)]
        struct TestError2(TestError1);
        impl Error for TestError2 {
            fn source(&self) -> Option<&(dyn Error + 'static)> {
                Some(&self.0)
            }
        }
        impl fmt::Display for TestError2 {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "TestError2")
            }
        }

        assert_eq!(
            format!("{}", ErrorChain(&TestError2(TestError1))),
            "TestError2\n\nCaused by:\n    TestError1"
        );
    }
}
