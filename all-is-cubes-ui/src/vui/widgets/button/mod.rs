mod draw;
pub(crate) use draw::{ButtonBase, ButtonIcon, make_button_label_block};
mod r#impl;
mod widget;
pub use widget::*;

type Action = all_is_cubes::inv::EphemeralOpaque<dyn Fn() + Send + Sync>;
