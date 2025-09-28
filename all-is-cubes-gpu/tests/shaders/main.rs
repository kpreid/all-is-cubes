#![feature(macro_attr)]

//! Unit tests for our shader code.
//!
//! These work by appending an extra test entry-point to the shader under test.

// Helpers
mod harness;
mod wgsl;

// Individual tests
mod tests;
