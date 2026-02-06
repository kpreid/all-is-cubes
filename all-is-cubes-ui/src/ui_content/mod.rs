//! TODO: Find this module a better name.
//! It is an incomplete extraction of the “application specific” parts of the [`crate::vui`]
//! module, which I am intending to convert into being just the widget-library part.

pub(crate) mod hud;
pub mod notification;
pub(crate) mod pages;
pub(crate) mod settings;
pub(crate) mod vui_manager;
pub use vui_manager::Command;
pub(crate) use vui_manager::*;
