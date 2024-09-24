//! Serialization/persistence/saved games.

#[cfg(feature = "save")]
mod compress;
#[cfg(feature = "save")]
pub(crate) mod conversion;
#[cfg(feature = "save")]
pub(crate) mod schema;

#[cfg(test)]
#[cfg(feature = "save")]
mod tests;

mod whence;
pub use whence::WhenceUniverse;
