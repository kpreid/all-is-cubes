use core::cmp::Ordering;
use core::{fmt, ops};

#[doc(no_inline)]
pub use core::time::Duration;

/// A timestamp of some sort that can be used like [`std::time::Instant`].
///
/// Because this trait is not object-safe, it is usually used with the `Deadline` wrapper,
/// which provides higher-level Operations to avoid needing to compare two instants.
pub trait Instant: Copy + Ord + Send + Sync + fmt::Debug + 'static
where
    Self: ops::Add<Duration, Output = Self>,
    Self: ops::Sub<Duration, Output = Self>,
{
    /// Returns the current time.
    fn now() -> Self;

    /// Subtracts `other` from `self`, returning [`Duration::ZERO`] if the difference is
    /// negative.
    fn saturating_duration_since(self, other: Self) -> Duration;
}

#[cfg(any(feature = "std", test))]
impl Instant for std::time::Instant {
    #[inline]
    fn now() -> Self {
        std::time::Instant::now()
    }

    #[inline]
    fn saturating_duration_since(self, other: Self) -> Duration {
        std::time::Instant::saturating_duration_since(&self, other)
    }
}

/// Trivial implementation of [`Instant`] where everything happens in zero time.
///
/// Use this for specifying [`Deadline::Asap`] or [`Deadline::Whenever`] when no instant type
/// is available **and it's okay for all time measurements to be recorded as zero**.
#[doc(hidden)]
#[derive(Debug, Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
#[expect(clippy::exhaustive_structs)]
pub struct NoTime;

impl Instant for NoTime {
    #[inline]
    fn now() -> Self {
        Self
    }

    #[inline]
    fn saturating_duration_since(self, _: Self) -> Duration {
        Duration::ZERO
    }
}

impl ops::Add<Duration> for NoTime {
    type Output = Self;
    #[inline]
    fn add(self, _: Duration) -> Self::Output {
        NoTime
    }
}
impl ops::Sub<Duration> for NoTime {
    type Output = Self;
    #[inline]
    fn sub(self, _: Duration) -> Self::Output {
        NoTime
    }
}

/// A request regarding how much real time should be spent on a computation.
#[derive(Debug, Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub enum Deadline<I> {
    /// Stop immediately after the minimum necessary activities.
    ///
    /// Arithmetically, this is “negative infinity”; it is less than all finite deadlines.
    Asap,
    /// Stop as close to the given time (before or after) as is feasible.
    At(I),
    /// Don't stop until all the work is done.
    ///
    /// This choice is appropriate when deterministic results are desired.
    ///
    /// Arithmetically, this is “positive infinity”; it is greater than all finite deadlines.
    Whenever,
}
impl<I: Instant> Deadline<I> {
    /// Returns the time between `start` and the deadline, or [`None`] if there is no
    /// deadline and the remaining time is unbounded.
    ///
    /// If the deadline is already past, returns `Some(Duration::ZERO)`
    ///
    /// (This does not return [`Duration::MAX`] since that would be likely to cause
    /// unintended arithmetic overflows.)
    #[inline]
    pub fn remaining_since(&self, start: I) -> Option<Duration> {
        match self {
            Deadline::Asap => Some(Duration::ZERO),
            Deadline::At(deadline) => Some(deadline.saturating_duration_since(start)),
            Deadline::Whenever => None,
        }
    }
}

impl<I: Instant> ops::Add<Duration> for Deadline<I> {
    type Output = Self;
    #[inline]
    fn add(self, rhs: Duration) -> Self::Output {
        match self {
            Deadline::Asap => Deadline::Asap,
            Deadline::At(i) => Deadline::At(i + rhs),
            Deadline::Whenever => Deadline::Whenever,
        }
    }
}
impl<I: Instant> ops::Sub<Duration> for Deadline<I> {
    type Output = Self;
    #[inline]
    fn sub(self, rhs: Duration) -> Self::Output {
        match self {
            Deadline::Asap => Deadline::Asap,
            Deadline::At(i) => Deadline::At(i - rhs),
            Deadline::Whenever => Deadline::Whenever,
        }
    }
}

// Allow comparing `Deadline` and `Instant` without wrapping.
impl<I: Instant> PartialEq<I> for Deadline<I> {
    #[mutants::skip] // trivial
    #[inline]
    fn eq(&self, other: &I) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}
/// Note: The reverse `PartialOrd<Deadline<I>> for I` is not permitted by the orphan rules.
impl<I: Instant> PartialOrd<I> for Deadline<I> {
    #[inline]
    fn partial_cmp(&self, other: &I) -> Option<Ordering> {
        Some(match self {
            Deadline::Asap => Ordering::Less,
            Deadline::At(i) => i.cmp(other),
            Deadline::Whenever => Ordering::Greater,
        })
    }
}

/// Convenience alias for [`Deadline`] with a dummy instant type.
///
/// Use this for specifying [`Deadline::Asap`] or [`Deadline::Whenever`] when no instant type
/// is available **and it's okay for all time measurements to be recorded as zero**.
pub type DeadlineNt = Deadline<NoTime>;

/// Convenience alias for [`Deadline`] with [`std::time::Instant`].
///
/// Use this for specifying [`Deadline::Asap`] or [`Deadline::Whenever`] when no instant type
/// is available **and it's okay for all time measurements to be recorded as zero**.
#[cfg(any(feature = "std", test))]
pub type DeadlineStd = Deadline<std::time::Instant>;

impl<I: Instant> From<I> for Deadline<I> {
    #[inline]
    fn from(value: I) -> Self {
        Self::At(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deadline_ordering() {
        use std::time::Instant;
        let i = Instant::now();
        let mut deadlines = [
            Deadline::At(i + Duration::from_secs(1)),
            Deadline::Asap,
            Deadline::Whenever,
            Deadline::At(i),
        ];
        deadlines.sort();
        assert_eq!(
            deadlines,
            [
                Deadline::Asap,
                Deadline::At(i),
                Deadline::At(i + Duration::from_secs(1)),
                Deadline::Whenever,
            ]
        );
    }
}
