//! Web server and game server (multiplayer/server-side storage) for All is Cubes.

#![feature(doc_cfg)]
// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

mod webserver;
pub use webserver::start_server;

mod client_static;
pub use client_static::AicClientSource;
