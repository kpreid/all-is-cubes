//! Momentary decorative effects produced by the game world, such as sound and particles.

/// Momentary decorative effects produced by the game world, such as sound and particles.
///
/// Each [`Fluff`] value represents the beginning of such an effect. It does not specify
/// anything about the exact duration; the intent is that they should all be negligibly
/// short.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
#[non_exhaustive]
pub enum Fluff {
    /// A standard beep/“bell” sound, as might be used for a notification or error.
    Beep,
    /// A sound suitable for “something was activated or done”, e.g. a button was clicked.
    Happened,
}
