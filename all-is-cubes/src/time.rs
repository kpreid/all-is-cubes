//! Data types for simulated and real time.

use core::fmt;
use core::num::NonZeroU16;

use bevy_ecs::prelude as ecs;

#[cfg(doc)]
use crate::universe::Universe;

// -------------------------------------------------------------------------------------------------

#[doc(inline)]
pub use all_is_cubes_base::time::*;

/// Specifies an amount of time passing “in game” in a [`Universe`] and its contents.
///
/// [`Tick`] values are passed along through the `step()` operations that advance time.
/// They are produced by a [`Clock`] which has a [`TickSchedule`], and the clock generates
/// distinct but eventually repeating ticks based on its *phase*.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Tick {
    /// Schedule from which this tick was derived, which also determines its length.
    schedule: TickSchedule,

    /// The phase of the clock *before* this tick happens.
    /// (After this tick happens, the phase is this value plus 1.)
    prev_phase: u16,

    /// Whether game time is paused, and `delta_t` should not be considered
    /// as an amount of game time passing. See [`Self::paused()`] for details.
    paused: bool,
}

impl Tick {
    fn new(schedule: TickSchedule, prev_phase: u16) -> Self {
        Self {
            schedule,
            prev_phase,
            paused: false,
        }
    }

    /// A tick of arbitrary length, for testing purposes. Do not use this for actual gameplay.
    pub const fn arbitrary() -> Self {
        Self {
            schedule: TickSchedule::per_second(1),
            prev_phase: 0,
            paused: false,
        }
    }

    /// Construct a non-paused [`Tick`] from a duration expressed in fractional seconds,
    /// and phase 0 as if it were the first tick in a universe.
    ///
    /// This should only be used for tests.
    pub fn from_seconds(dt: f64) -> Self {
        Self {
            schedule: TickSchedule {
                base_duration: Duration::from_micros((dt * 1e6) as u64),
                divisor: NonZeroU16::MIN,
            },
            prev_phase: 0,
            paused: false,
        }
    }

    /// Return the amount of time passed as a [`Duration`].
    pub fn delta_t(self) -> Duration {
        self.schedule.delta_t()
    }

    /// Returns the phase of the originating clock *before* this tick happens.
    /// (After this tick happens, the phase is this value plus 1.)
    pub fn prev_phase(self) -> u16 {
        self.prev_phase
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

// -------------------------------------------------------------------------------------------------

/// Specifies which [`Tick`]s a repeating event occurs on.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Schedule {
    period: NonZeroU16,
    // TODO: add a relative-phase field. Or, consider replacing `period` with a bitmask
    // covering all ticks (if we restrict the max divisor to a size for which that is sane).
}

impl Schedule {
    /// Schedule which includes every tick.
    pub const EVERY_TICK: Self = Self {
        period: NonZeroU16::MIN,
    };

    /// Creates a schedule which specifies executing some action every `period` ticks.
    ///
    /// The `period` should be divisible by the universe’s [`TickSchedule`]’s divisor.
    /// If it is not, then schedule will have uneven periods.
    /// If it is zero, it will be replaced with 1.
    pub fn from_period(period: NonZeroU16) -> Self {
        Schedule { period }
    }

    pub(crate) fn contains(self, tick: Tick) -> bool {
        tick.prev_phase().rem_euclid(self.period.get()) != 0
    }

    /// If this schedule is of the form “every N ticks”, return N.
    #[expect(clippy::unnecessary_wraps, reason = "the None cases don't exist yet")]
    pub fn to_period(self) -> Option<NonZeroU16> {
        Some(self.period)
    }
}

// -------------------------------------------------------------------------------------------------

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
/// * Simulation systems with different [`Schedule`]s will proceed in simple ratios to each
///   other.
/// * The only information which needs to be persistently stored is the _phase_ of the
///   clock — that is, how many ticks have elapsed since the last whole base duration.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct TickSchedule {
    base_duration: Duration,
    divisor: NonZeroU16,
}

impl TickSchedule {
    /// Construct a [`TickSchedule`] which specifies `divisor` ticks per second.
    pub const fn per_second(divisor: u16) -> Self {
        Self {
            base_duration: Duration::from_secs(1),
            divisor: match NonZeroU16::new(divisor) {
                Some(x) => x,
                None => panic!("divisor must be nonzero"),
            },
        }
    }

    /// Returns the length of a [`Tick`] in this schedule.
    pub fn delta_t(&self) -> Duration {
        self.base_duration / u32::from(self.divisor.get())
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

// -------------------------------------------------------------------------------------------------

/// Defines the passage of time in a [`Universe`].
///
/// See [`TickSchedule`] for details on what is possible and why.
///
/// ---
///
/// TODO: Should `paused` be part of the clock's state?
#[derive(Clone, Copy, Eq, Hash, PartialEq, ecs::Resource)]
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
            phase: phase % schedule.divisor.get(),
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
            self.phase = (self.phase + 1) % self.schedule.divisor;
        }
        tick
    }

    /// Returns the tick that will happen the next time [`Self::advance()`] is called,
    /// if the schedule is not changed before then.
    pub fn next_tick(&self, paused: bool) -> Tick {
        let mut tick = Tick::new(self.schedule, self.phase);

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

// -------------------------------------------------------------------------------------------------

/// Resource which contains a `Tick` when, and only when, the [`Universe`] is being stepped.
//---
#[derive(Clone, Copy, Default, ecs::Resource)]
pub(crate) struct CurrentTick(pub Option<Tick>);

impl CurrentTick {
    pub(crate) fn get(
        self,
    ) -> Result<Tick, alloc::boxed::Box<dyn core::error::Error + Send + Sync>> {
        self.0
            .ok_or_else(|| "attempted to get current Tick when there is no step in progress".into())
    }
}

// -------------------------------------------------------------------------------------------------

/// [`ScheduleLabel`]s relating to time passing in a [`Universe`].
pub(crate) mod schedule {
    use bevy_ecs::schedule::ScheduleLabel;

    /// Schedule label for systems executed immediately before [`Step`].
    ///
    /// Use this schedule to collect information about the current state
    /// and synchronize with any edits that happened outside of stepping.
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, ScheduleLabel)]
    pub struct BeforeStep;

    /// Most simulation logic goes here.
    ///
    /// If time is paused, this is not called, but [`BeforeStep`] and [`AfterStep`] still are.
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, ScheduleLabel)]
    pub struct Step;

    /// Executed immediately after [`Step`].
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, ScheduleLabel)]
    pub struct AfterStep;
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;

    #[test]
    fn clock_debug() {
        let mut clock = Clock::new(TickSchedule::per_second(25), 0);
        assert_eq!(format!("{clock:?}"), "Clock(0/25 of 1s)");
        clock.advance(false);
        assert_eq!(format!("{clock:?}"), "Clock(1/25 of 1s)");
    }

    #[test]
    fn clock_phase_advance() {
        let mut clock = Clock::new(TickSchedule::per_second(3), 0);
        assert_eq!(
            std::iter::repeat_with(|| clock.advance(false).prev_phase())
                .take(10)
                .collect::<Vec<_>>(),
            vec![0, 1, 2, 0, 1, 2, 0, 1, 2, 0],
        );
    }

    #[test]
    fn tick_schedule_duration() {
        let schedule = TickSchedule::per_second(60);
        assert_eq!(schedule.delta_t(), Duration::from_nanos(1_000_000_000 / 60));
    }
}
