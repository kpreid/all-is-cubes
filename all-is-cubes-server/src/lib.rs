// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! The primary purpose of this crate is to be a web server and game server
//! (multiplayer/server-side storage) for All is Cubes.
//!
//! It also contains tools that are not runnable in a browser environment.
//! Eventually this may include save file editing tools and such, but currently
//! only `aic-console`, an ASCII-art raytracer.

#![warn(clippy::cast_lossless)]

pub mod console;
pub mod webserver;
