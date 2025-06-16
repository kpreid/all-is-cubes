use core::cmp::Ordering;
use core::ops;

#[doc(no_inline)]
pub use bevy_platform::time::Instant;
#[doc(no_inline)]
pub use core::time::Duration;

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
            #[expect(
                clippy::unchecked_duration_subtraction,
                reason = "TODO: can we do better?"
            )]
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
