//! Tools that we could imagine being in the Rust standard library, but aren't.

use alloc::sync::Arc;
use core::fmt;
use core::marker::PhantomData;
use core::ops::AddAssign;
use core::time::Duration;

// Note that this is not the `maybe_sync::BoxFuture`!
use futures_core::future::BoxFuture as SyncBoxFuture;
use manyfmt::Refmt as _;

// -------------------------------------------------------------------------------------------------

mod custom_format;
pub use custom_format::*;

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
/// Executors should generally implement `Clone`.
pub trait Executor: fmt::Debug + Send + Sync {
    /// Create a set of tasks which runds the provided `future`, if possible.
    ///
    /// The given `task_factory` is called some number of times appropriate to the available
    /// parallelism. If only single-threaded asynchronous execution is supported, it will be called
    /// once. It may be called zero times; callers must be able to complete their work without the
    /// assistance of these tasks.
    ///
    /// The future **must periodically yield** by calling [`Executor::yield_now()`].
    /// Otherwise, it may prevent other tasks, even “foreground” ones, from progressing.
    /// This requirement is for the benefit of single-threaded [`Executor`]s.
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> SyncBoxFuture<'static, ()>);

    /// Grants an opportunity for other tasks to execute instead of the current one.
    ///
    /// This should only be performed from inside of a [`Executor::spawn_background()`] task.
    /// If it is called (or polled) under other circumstances, it may panic or have negative
    /// effects on task scheduling.
    //---
    // If Rust ever gets object-safe async fn in trait without boxing, use it here.
    fn yield_now(&self) -> SyncBoxFuture<'static, ()>;
}
#[allow(clippy::missing_inline_in_public_items)]
impl<T: ?Sized + Executor> Executor for &T {
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> SyncBoxFuture<'static, ()>) {
        (**self).spawn_background(task_factory)
    }
    fn yield_now(&self) -> futures_util::future::BoxFuture<'static, ()> {
        (**self).yield_now()
    }
}
#[allow(clippy::missing_inline_in_public_items)]
impl<T: ?Sized + Executor> Executor for Arc<T> {
    fn spawn_background(&self, task_factory: &mut dyn FnMut() -> SyncBoxFuture<'static, ()>) {
        (**self).spawn_background(task_factory)
    }
    fn yield_now(&self) -> futures_util::future::BoxFuture<'static, ()> {
        (**self).yield_now()
    }
}
/// No-op executor for applications which cannot provide one.
#[allow(clippy::missing_inline_in_public_items)]
impl Executor for () {
    fn spawn_background(&self, _: &mut dyn FnMut() -> SyncBoxFuture<'static, ()>) {}
    fn yield_now(&self) -> futures_util::future::BoxFuture<'static, ()> {
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

/// Equivalent of [`Iterator::map`] but applied to an [`Extend`] instead, transforming
/// the incoming elements.
///
/// TODO: this is only used by the wireframe debug mesh mechanism and should be reconsidered
#[doc(hidden)] // pub to be used by all-is-cubes-gpu
#[derive(Debug)]
pub struct MapExtend<'a, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
    target: &'a mut T,
    function: F,
    _input: PhantomData<fn(A)>,
}

impl<'a, A, B, T, F> MapExtend<'a, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
    #[inline]
    pub fn new(target: &'a mut T, function: F) -> Self {
        Self {
            target,
            function,
            _input: PhantomData,
        }
    }
}

impl<A, B, T, F> Extend<A> for MapExtend<'_, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
    #[inline]
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = A>,
    {
        self.target.extend(iter.into_iter().map(&self.function));
    }
}

/// Aggregation of the time taken by a set of events.
///
/// TODO: Consider including an identifier for the longest.
/// TODO: Consider generalizing this to quantities other than time? Probably not.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
pub struct TimeStats {
    /// The number of events aggregated into this [`TimeStats`].
    pub count: usize,
    /// The sum of the durations of all events.
    pub sum: Duration,
    /// The minimum duration of all events, or [`None`] if there were no events.
    pub min: Option<Duration>,
    /// The maximum duration of all events, or [`Duration::ZERO`] if there were no events.
    pub max: Duration,
}

impl TimeStats {
    /// Constructs a [`TimeStats`] for a single event.
    ///
    /// Multiple of these may then be aggregated using the `+=` operator.
    #[inline]
    pub const fn one(duration: Duration) -> Self {
        Self {
            count: 1,
            sum: duration,
            min: Some(duration),
            max: duration,
        }
    }

    /// Record an event based on the given previous time and current time, then update
    /// the previous time value.
    ///
    /// Returns the duration that was recorded.
    #[doc(hidden)] // for now, not making writing conveniences public
    #[inline]
    pub fn record_consecutive_interval(
        &mut self,
        last_marked_instant: &mut crate::time::Instant,
        now: crate::time::Instant,
    ) -> Duration {
        let previous = *last_marked_instant;
        *last_marked_instant = now;

        let duration = now.saturating_duration_since(previous);
        *self += Self::one(duration);
        duration
    }
}

impl AddAssign for TimeStats {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        *self = TimeStats {
            count: self.count + rhs.count,
            sum: self.sum + rhs.sum,
            min: self.min.map_or(rhs.min, |value| Some(value.min(rhs.min?))),
            max: self.max.max(rhs.max),
        };
    }
}

impl fmt::Display for TimeStats {
    #[allow(clippy::missing_inline_in_public_items)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.min {
            None => write!(
                f,
                "(-------- .. {}) for {:3}, total {}",
                self.max.refmt(&ConciseDebug),
                self.count,
                self.sum.refmt(&ConciseDebug),
            ),
            Some(min) => write!(
                f,
                "({} .. {}) for {:3}, total {}",
                min.refmt(&ConciseDebug),
                self.max.refmt(&ConciseDebug),
                self.count,
                self.sum.refmt(&ConciseDebug),
            ),
        }
    }
}

#[doc(hidden)] // for use in internal tests only
#[allow(clippy::missing_inline_in_public_items)]
pub fn assert_send_sync<T: Send + Sync>() {
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
