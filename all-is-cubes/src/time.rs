// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Time passing “in game”, i.e. in a [`Universe`] and its contents.
//!
//! [`Universe`]: crate::universe::Universe

// TODO: This module exists because I am intending to add complications to Tick
// like having multiple subdivisions of time (to allow efficient slower-running
// yet synchronized game systems). If that doesn't happen, it should be merged
// into universe.rs or something like that.

use instant::Duration;

/// Specifies an amount of time passing in a [`Universe`](crate::universe::Universe)
/// and its contents.
///
/// [`Universe`]: crate::universe::Universe
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Tick {
    // TODO: Replace this with a rational-number-based system so that we can
    // (1) step in exact 60ths or other frame rate fractions
    // (2) have a standard subdivision for slower-than-every-frame events
    pub(crate) delta_t: Duration,

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

    pub(crate) fn from_duration(delta_t: Duration) -> Self {
        Self {
            delta_t,
            paused: false,
        }
    }

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
