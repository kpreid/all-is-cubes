// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Web server and game server (multiplayer/server-side storage) for All is Cubes.

#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
// TODO: warn(missing_docs), eventually
#![warn(noop_method_call)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::wrong_self_convention)]

pub mod webserver;
