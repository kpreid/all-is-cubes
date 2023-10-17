//! Momentary decorative effects produced by the game world, such as sound and particles.

/// Momentary decorative effects produced by the game world, such as sound and particles.
///
/// Each [`Fluff`] value represents the beginning of such an effect. It does not specify
/// anything about the exact duration; the intent is that they should all be negligibly
/// short.
///
/// Currently, all `Fluff` is an item from a fixed list. In the future, it will be able
/// to refer to audio and visual assets defined in a `Universe`.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
#[non_exhaustive]
pub enum Fluff {
    /// A standard beep/“bell” sound, as might be used for a notification or error.
    Beep,

    /// A sound suitable for “something was activated or done”, e.g. a button was clicked.
    Happened,

    /// Sound and visual effect from a block having been placed in the game world
    /// by player action, without any more specific overriding styling.
    PlaceBlockGeneric,
}
