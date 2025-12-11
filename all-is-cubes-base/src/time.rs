use core::cmp::Ordering;
use core::fmt;
use core::ops;

use crate::util::ConciseDebug;
use manyfmt::Refmt as _;

// -------------------------------------------------------------------------------------------------

#[doc(no_inline)]
pub use bevy_platform::time::Instant;
#[doc(no_inline)]
pub use core::time::Duration;

// -------------------------------------------------------------------------------------------------

/// A request regarding how much real time should be spent on a computation.
#[derive(Debug, Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub enum Deadline {
    /// Stop immediately after the minimum necessary activities.
    ///
    /// Arithmetically, this is “negative infinity”; it is less than all finite deadlines.
    Asap,
    /// Stop as close to the given time (before or after) as is feasible.
    At(Instant),
    /// Don't stop until all the work is done.
    ///
    /// This choice is appropriate when deterministic results are desired.
    ///
    /// Arithmetically, this is “positive infinity”; it is greater than all finite deadlines.
    Whenever,
}

impl Deadline {
    /// Returns the time between `start` and the deadline, or [`None`] if there is no
    /// deadline and the remaining time is unbounded.
    ///
    /// If the deadline is already past, returns `Some(Duration::ZERO)`
    ///
    /// (This does not return [`Duration::MAX`] since that would be likely to cause
    /// unintended arithmetic overflows.)
    #[inline]
    pub fn remaining_since(&self, start: Instant) -> Option<Duration> {
        match self {
            Deadline::Asap => Some(Duration::ZERO),
            Deadline::At(deadline) => Some(deadline.saturating_duration_since(start)),
            Deadline::Whenever => None,
        }
    }
}

impl ops::Add<Duration> for Deadline {
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
impl ops::Sub<Duration> for Deadline {
    type Output = Self;
    #[inline]
    fn sub(self, rhs: Duration) -> Self::Output {
        match self {
            Deadline::Asap => Deadline::Asap,
            #[allow(clippy::unchecked_time_subtraction, reason = "TODO: can we do better?")]
            Deadline::At(i) => Deadline::At(i - rhs),
            Deadline::Whenever => Deadline::Whenever,
        }
    }
}

// Allow comparing `Deadline` and `Instant` without wrapping.
impl PartialEq<Instant> for Deadline {
    #[mutants::skip] // trivial
    #[inline]
    fn eq(&self, other: &Instant) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}
impl PartialEq<Deadline> for Instant {
    #[mutants::skip] // trivial
    #[inline]
    fn eq(&self, other: &Deadline) -> bool {
        other.eq(self)
    }
}
impl PartialOrd<Instant> for Deadline {
    #[inline]
    fn partial_cmp(&self, other: &Instant) -> Option<Ordering> {
        Some(match self {
            Deadline::Asap => Ordering::Less,
            Deadline::At(i) => i.cmp(other),
            Deadline::Whenever => Ordering::Greater,
        })
    }
}
impl PartialOrd<Deadline> for Instant {
    #[inline]
    fn partial_cmp(&self, other: &Deadline) -> Option<Ordering> {
        other.partial_cmp(self).map(Ordering::reverse)
    }
}

impl From<Instant> for Deadline {
    #[inline]
    fn from(value: Instant) -> Self {
        Self::At(value)
    }
}

// -------------------------------------------------------------------------------------------------

/// Summary of the time taken by a set of events.
///
/// This type is produced by performance measurements within several subsystems of All is Cubes.
///
/// It may be created by [`TimeStats::default()`] (empty), or [`TimeStats::one()`] (single event),
/// and multiple events may be aggregated using the `+=` operator.
/// It may be formatted for reading using the [`fmt::Display`] implementation.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
#[non_exhaustive]
#[expect(clippy::module_name_repetitions, reason = "TODO: find a better name")]
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
        last_marked_instant: &mut Instant,
        now: Instant,
    ) -> Duration {
        let previous = *last_marked_instant;
        *last_marked_instant = now;

        let duration = now.saturating_duration_since(previous);
        *self += Self::one(duration);
        duration
    }
}

impl ops::AddAssign for TimeStats {
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
        let max = self.max.refmt(&ConciseDebug);
        let count = self.count;
        let sum = self.sum.refmt(&ConciseDebug);
        match self.min {
            None => write!(f, "(-------- .. {max}) for {count:3}, total {sum}"),
            Some(min) => {
                let min = min.refmt(&ConciseDebug);
                write!(f, "({min} .. {max}) for {count:3}, total {sum}")
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deadline_ordering() {
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
