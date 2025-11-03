//! Definition of a short sound that may be played by game events.
//!
//! [`SoundDef`]s are used as members of [`Universe`s][crate::universe::Universe].

use core::fmt;

/// Acts as polyfill for float methods used in synthesis such as `sin()`
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::Float as _;

use crate::math::{PositiveSign, ZeroOne};
use crate::transaction::{self, Equal, Transaction};
use crate::universe;

// -------------------------------------------------------------------------------------------------

/// A sound effect or grain.
#[derive(Clone, Debug, Eq, Hash, PartialEq, bevy_ecs::component::Component)]
#[expect(clippy::module_name_repetitions)]
#[non_exhaustive]
pub struct SoundDef {
    /// The total duration of the sound, in seconds. It may not be longer than one second.
    pub duration: ZeroOne<f32>,

    /// The frequency of the oscillator, in hertz.
    pub frequency: PositiveSign<f32>,

    /// TODO: Define amplitude scaling.
    /// We probably need some physical reference because the obvious alternative, 0 dBFS,
    /// is a bad idea. But some sounds are spatial and some are not...perhaps "1 meter/cube away
    /// is the reference distance" will solve that.
    pub amplitude: ZeroOne<f32>,
    // TODO: More properties:
    // * envelope attack
    // * envelope decay *shape*
    // * oscillator type(s)
    //   * and an option to use a recording instead of an oscillator
    // * modulation
}

impl SoundDef {
    #[doc(hidden)] // API design still experimental
    pub fn synthesize(&self, sample_rate: f32) -> impl Iterator<Item = [f32; 2]> {
        let sample_count = (self.duration.into_inner() * sample_rate).round() as usize;
        let sample_index_to_radians =
            self.frequency.into_inner() * core::f32::consts::TAU / sample_rate;
        let amplitude = self.amplitude.into_inner();

        (0..sample_count).map(move |sample_index| {
            let time_in_fraction = sample_index as f32 / (sample_count - 1).max(1) as f32;
            let time_in_radians = sample_index as f32 * sample_index_to_radians;

            let envelope: f32 = (1.0 - time_in_fraction).sqrt();

            let wave: f32 = time_in_radians.sin() * amplitude;
            [wave * envelope; 2]
        })
    }
}

impl universe::VisitHandles for SoundDef {
    fn visit_handles(&self, _: &mut dyn universe::HandleVisitor) {
        let Self {
            duration: _,
            frequency: _,
            amplitude: _,
        } = self;
    }
}

// -------------------------------------------------------------------------------------------------

/// A [`Transaction`] which replaces (or checks) a [`SoundDef`].
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DefTransaction {
    old: Equal<SoundDef>,
    new: Equal<SoundDef>,
}

impl transaction::Transactional for SoundDef {
    type Transaction = DefTransaction;
}

impl DefTransaction {
    /// Returns a transaction which fails if the current value of the [`SoundDef`] is not
    /// equal to `old`.
    pub fn expect(old: SoundDef) -> Self {
        Self {
            old: Equal(Some(old)),
            new: Equal(None),
        }
    }

    /// Returns a transaction which replaces the current value of the [`SoundDef`] with `new`.
    pub fn overwrite(new: SoundDef) -> Self {
        Self {
            old: Equal(None),
            new: Equal(Some(new)),
        }
    }

    /// Returns a transaction which replaces the value of the [`SoundDef`] with `new`,
    /// if it is equal to `old`, and otherwise fails.
    pub fn replace(old: SoundDef, new: SoundDef) -> Self {
        Self {
            old: Equal(Some(old)),
            new: Equal(Some(new)),
        }
    }
}

impl Transaction for DefTransaction {
    type Target = SoundDef;
    type CommitCheck = ();
    // This ReadTicket is not currently used, but at least for now, *all* universe member transactions are to have ReadTicket as their context type.
    type Context<'a> = universe::ReadTicket<'a>;
    type Output = transaction::NoOutput;
    type Mismatch = Mismatch;

    fn check(&self, target: &SoundDef) -> Result<Self::CommitCheck, Self::Mismatch> {
        self.old.check(target).map_err(|_| Mismatch::Unexpected)
    }

    fn commit(
        self,
        target: &mut SoundDef,
        _: Self::Context<'_>,
        (): Self::CommitCheck,
        _outputs: &mut dyn FnMut(Self::Output),
    ) -> Result<(), transaction::CommitError> {
        if let Equal(Some(new)) = self.new {
            *target = new;
            // Note there is no change notification.
            // It would be nice if we could arrange such notification to happen via the containing
            // Handle instead of implementing it anew -- but for now, this should not be too bad
            // except for editors.
        }
        Ok(())
    }
}

impl universe::TransactionOnEcs for DefTransaction {
    fn check(
        &self,
        target: universe::ReadGuard<'_, SoundDef>,
    ) -> Result<Self::CommitCheck, Self::Mismatch> {
        Transaction::check(self, &*target)
    }

    fn commit(
        self,
        target: &mut SoundDef,
        read_ticket: universe::ReadTicket<'_>,
        check: Self::CommitCheck,
    ) -> Result<(), transaction::CommitError> {
        Transaction::commit(
            self,
            target,
            read_ticket,
            check,
            &mut transaction::no_outputs,
        )
    }
}

impl transaction::Merge for DefTransaction {
    type MergeCheck = ();
    type Conflict = Conflict;

    fn check_merge(&self, other: &Self) -> Result<Self::MergeCheck, Self::Conflict> {
        let conflict = Conflict {
            old: self.old.check_merge(&other.old).is_err(),
            new: self.new.check_merge(&other.new).is_err(),
        };

        if (conflict
            != Conflict {
                old: false,
                new: false,
            })
        {
            Err(conflict)
        } else {
            Ok(())
        }
    }

    fn commit_merge(&mut self, other: Self, (): Self::MergeCheck) {
        let Self { old, new } = self;
        old.commit_merge(other.old, ());
        new.commit_merge(other.new, ());
    }
}

/// Transaction precondition error type for a [`DefTransaction`].
#[derive(Clone, Debug, Eq, PartialEq, displaydoc::Display)]
#[non_exhaustive]
pub enum Mismatch {
    /// old definition not as expected
    Unexpected,
}

/// Transaction conflict error type for a [`DefTransaction`].
// ---
// TODO: this is identical to `BlockDefConflict` and `CubeConflict` but for the names
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub struct Conflict {
    /// The transactions have conflicting preconditions (`old` definitions).
    pub(crate) old: bool,
    /// The transactions are attempting to provide two different `new` definitions.
    pub(crate) new: bool,
}

impl core::error::Error for Mismatch {}
impl core::error::Error for Conflict {}

impl fmt::Display for Conflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Conflict {
                old: true,
                new: false,
            } => write!(f, "different preconditions for SoundDef"),
            Conflict {
                old: false,
                new: true,
            } => write!(f, "cannot write different new values to the same SoundDef"),
            Conflict {
                old: true,
                new: true,
            } => write!(f, "different preconditions (with write)"),
            Conflict {
                old: false,
                new: false,
            } => unreachable!(),
        }
    }
}
