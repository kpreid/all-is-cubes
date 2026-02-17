//! Web server and game server (multiplayer/server-side storage) for All is Cubes.

#![feature(doc_cfg)]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]
#![cfg_attr(test, allow(dead_code_pub_in_binary, reason = "FP on test binaries"))]

mod webserver;
pub use webserver::start_server;

mod client_static;
pub use client_static::AicClientSource;
