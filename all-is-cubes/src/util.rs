//! Tools that we could imagine being in the Rust standard library, but aren't.

use core::fmt;
use core::marker::PhantomData;
use core::ops::AddAssign;
use core::time::Duration;

mod custom_format;
pub use custom_format::*;

#[doc(no_inline)]
pub use yield_progress::{Builder as YieldProgressBuilder, YieldProgress};

#[doc(hidden)]
pub fn yield_progress_for_testing() -> YieldProgress {
    // Theoretically we should use Tokio's yield function, but it shouldn't matter for
    // tests and I don't want the dependency here.
    yield_progress::Builder::new().build()
}

#[cfg(feature = "std")]
pub use error_chain::ErrorChain;
#[cfg(feature = "std")]
mod error_chain {
    use core::fmt;
    use std::error::Error;

    /// Formatting wrapper which prints an [`Error`] together with its
    /// `source()` chain, with at least one newline between each.
    ///
    /// The text begins with the [`core::fmt::Display`] format of the error.
    ///
    /// Design note: This is not a [`CustomFormat`] because that has a blanket implementation
    /// which interferes with this one for [`Error`].
    #[doc(hidden)] // not something we wish to be stable public API
    #[derive(Clone, Copy, Debug)]
    #[allow(clippy::exhaustive_structs)]
    pub struct ErrorChain<'a>(pub &'a (dyn Error + 'a));

    impl fmt::Display for ErrorChain<'_> {
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

cfg_if::cfg_if! {
    if #[cfg(feature = "std")] {
        /// Alias for [`std::error::Error`] that is a substitute when not on `std`.
        #[doc(hidden)]
        pub use std::error::Error as ErrorIfStd;
    } else {
        use alloc::boxed::Box;

        /// Substitute for [`std::error::Error`] with the same supertraits but no methods.
        #[doc(hidden)]
        pub trait ErrorIfStd: fmt::Debug + fmt::Display {}
        impl<T> ErrorIfStd for T where T: fmt::Debug + fmt::Display {}

        impl From<&str> for Box<dyn ErrorIfStd + Send + Sync> {
            fn from(s: &str) -> Self {
                Box::new(alloc::string::String::from(s))
            }
        }
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
    pub fn new(target: &'a mut T, function: F) -> Self {
        Self {
            target,
            function,
            _input: PhantomData,
        }
    }
}

impl<'a, A, B, T, F> Extend<A> for MapExtend<'a, A, B, T, F>
where
    T: Extend<B>,
    F: Fn(A) -> B,
{
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
    pub fn record_consecutive_interval<I: crate::time::Instant>(
        &mut self,
        last_marked_instant: &mut I,
        now: I,
    ) -> Duration {
        let previous = *last_marked_instant;
        *last_marked_instant = now;

        let duration = now.saturating_duration_since(previous);
        *self += Self::one(duration);
        duration
    }
}

impl AddAssign for TimeStats {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.min {
            None => write!(
                f,
                "(-------- .. {}) for {:3}, total {}",
                self.max.custom_format(StatusText),
                self.count,
                self.sum.custom_format(StatusText),
            ),
            Some(min) => write!(
                f,
                "({} .. {}) for {:3}, total {}",
                min.custom_format(StatusText),
                self.max.custom_format(StatusText),
                self.count,
                self.sum.custom_format(StatusText),
            ),
        }
    }
}

#[doc(hidden)]
pub fn assert_send_sync<T: Send + Sync>() {
    // We don't need to do anything in this function; the call to it having been successfully
    // compiled is the assertion.
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;
    use std::fmt;

    #[test]
    fn error_chain() {
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
