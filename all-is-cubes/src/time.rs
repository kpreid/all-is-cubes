//! Time passing “in game”, i.e. in a [`Universe`] and its contents.

use std::fmt;

pub use instant::{Duration, Instant};

#[cfg(doc)]
use crate::universe::Universe;

/// Specifies an amount of time passing in a [`Universe`]
/// and its contents.
///
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Tick {
    /// Duration of this tick. This should be used in simulation/animation calculations
    /// and not assumed to have any relationship to “wall” time.
    delta_t: Duration,

    /// Whether game time is paused, and `delta_t` should not be considered
    /// as an amount of game time passing. See [`Self::paused()`] for details.
    paused: bool,
}

impl Tick {
    /// A tick of arbitrary length, for testing purposes. Do not use this for actual gameplay.
    pub const fn arbitrary() -> Self {
        Self {
            delta_t: Duration::from_secs(1),
            paused: false,
        }
    }

    /// Construct a [`Tick`] of the specified length.
    pub const fn from_duration(delta_t: Duration) -> Self {
        Self {
            delta_t,
            paused: false,
        }
    }

    /// Construct a non-paused [`Tick`] from a duration expressed in fractional seconds.
    pub fn from_seconds(dt: f64) -> Self {
        Self {
            delta_t: Duration::from_micros((dt * 1e6) as u64),
            paused: false,
        }
    }

    /// Return the amount of time passed as a [`Duration`].
    pub fn delta_t(self) -> Duration {
        self.delta_t
    }

    /// Set the paused flag. See [`Tick::paused`] for more information.
    #[must_use]
    pub fn pause(self) -> Self {
        Self {
            paused: true,
            ..self
        }
    }

    /// Returns the "paused" state of this Tick. If true, then step operations should
    /// not perform any changes that reflect "in-game" time passing. They should still
    /// take care of the side effects of other mutations/transactions, particularly where
    /// not doing so might lead to a stale or inconsistent view.
    ///
    /// Note that functions which propagate ticks to subordinate game objects are free to
    /// not propagate paused ticks. TODO: The exact policies are not yet settled.
    pub fn paused(&self) -> bool {
        self.paused
    }
}

/// Defines how time passes in a [`Universe`].
///
/// Specifically, it defines a base real-time duration (for example, it could be 1 second),
/// and a divisor with which to subdivide this duration into individual [`Tick`]s.
///
/// The significance of the base duration is that events which proceed on regular global
/// intervals less frequent than every tick (e.g. some interaction between blocks) are
/// required to proceed at a rate which the base duration is a multiple of. For example,
/// if the base duration is 1 second and the divisor is 60, then "every 10 ticks, or 6
/// times per second" is a valid schedule, but "every 45 ticks, or 3/4 of a second" is not.
///
/// This design provides the following properties:
///
/// * Simulation systems with different schedules will proceed in simple ratios to each
///   other.
/// * The only information which needs to be persistently stored is the _phase_ of the
///   clock — that is, how many ticks have elapsed since the last whole base duration.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct TickSchedule {
    base_duration: Duration,
    divisor: u16,
}

impl TickSchedule {
    /// Construct a [`TickSchedule`] which specifies `divisor` ticks per second.
    pub const fn per_second(divisor: u16) -> Self {
        Self {
            base_duration: Duration::from_secs(1),
            divisor,
        }
    }

    /// Returns the length of a [`Tick`] in this schedule.
    pub fn delta_t(&self) -> Duration {
        self.base_duration / u32::from(self.divisor)
    }
}

impl fmt::Debug for TickSchedule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let TickSchedule {
            base_duration,
            divisor,
        } = *self;
        write!(f, "TickSchedule({base_duration:?} / {divisor})")
    }
}

/// Defines the passage of time in a [`Universe`].
///
/// See [`TickSchedule`] for details on what is possible and why.
///
/// ---
///
/// TODO: Should `paused` be part of the clock's state?
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Clock {
    schedule: TickSchedule,

    /// Specifies how the current instant relates to the schedule.
    ///
    /// * It should always be the case that `phase < schedule.divisor`.
    /// * The initial phase, such as for a universe that has just been been created but
    ///   not yet stepped, is `0`.
    phase: u16,
}

impl Clock {
    /// Creates a new [`Clock`] with the given state.
    ///
    /// If the `phase` is out of range, it is reduced modulo `schedule.divisor`.
    pub const fn new(schedule: TickSchedule, phase: u16) -> Self {
        Self {
            schedule,
            phase: phase.rem_euclid(schedule.divisor),
        }
    }

    /// Returns the schedule which this clock obeys.
    pub fn schedule(&self) -> TickSchedule {
        self.schedule
    }

    /// If `paused` is false, advances this clock to the next instant (as defined by
    /// the schedule) and returns the [`Tick`] defining the transition between those
    /// instants.
    ///
    /// If `paused` is true, returns a paused [`Tick`] for the current instant.
    /// See [`Tick::paused`] for information on the meaning of pausing.
    pub fn advance(&mut self, paused: bool) -> Tick {
        let tick = self.next_tick(paused);
        if !paused {
            self.phase = (self.phase + 1).rem_euclid(self.schedule.divisor);
        }
        tick
    }

    /// Returns the tick that will happen the next time [`Self::advance()`] is called,
    /// if the schedule is not changed before then.
    pub fn next_tick(&self, paused: bool) -> Tick {
        let mut tick = Tick::from_duration(self.schedule.delta_t());

        if paused {
            tick = tick.pause();
        }

        tick
    }
}

impl fmt::Debug for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Clock {
            schedule:
                TickSchedule {
                    base_duration,
                    divisor,
                },
            phase,
        } = *self;
        write!(f, "Clock({phase}/{divisor} of {base_duration:?})")
    }
}

#[doc(hidden)] // test helper
pub fn practically_infinite_deadline() -> Instant {
    /// A Duration long enough that it is not interesting in questions of testing, but not
    /// so long that adding a reasonable number of it to an [`Instant`] will overflow.
    const VERY_LONG: Duration = Duration::from_secs(86400 * 7);

    Instant::now() + VERY_LONG
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clock_debug() {
        let mut clock = Clock::new(TickSchedule::per_second(25), 0);
        assert_eq!(format!("{clock:?}"), "Clock(0/25 of 1s)");
        clock.advance(false);
        assert_eq!(format!("{clock:?}"), "Clock(1/25 of 1s)");
    }

    #[test]
    fn tick_schedule_duration() {
        let schedule = TickSchedule::per_second(60);
        assert_eq!(schedule.delta_t(), Duration::from_nanos(1_000_000_000 / 60));
    }
}
