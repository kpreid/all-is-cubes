//! Serialization/persistence/saved games.
//!
//! This module is not exported because it is entirely made of trait impls,
//! helpers, and tests.

pub(crate) mod conversion;
pub(crate) mod schema;

#[cfg(test)]
mod tests;
