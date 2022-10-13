//! Momentary decorative effects produced by the game world, such as sound and particles.
//!
//! Each [`Fluff`] value represents the beginning of such an effect. It does not
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Fluff {
    /// A standard beep/“bell” sound, as might be used for a notification or error.
    Beep,
}
