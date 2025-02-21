#![allow(
    exported_private_dependencies,
    reason = "false positive on crossterm via ratatui"
)]

use all_is_cubes_ui::apps::Key;

use ratatui::crossterm::event::{Event, KeyCode, KeyModifiers, MouseButton};

/// Converts [`Event`] to [`Key`].
///
/// Returns `None` if there is no corresponding value.
pub fn event_to_key(event: &Event) -> Option<Key> {
    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Char(c)) => Some(Key::Character(c.to_ascii_lowercase())),
            (_, KeyCode::Esc) => Some(Key::Escape),
            (_, KeyCode::Up) => Some(Key::Up),
            (_, KeyCode::Down) => Some(Key::Down),
            (_, KeyCode::Left) => Some(Key::Left),
            (_, KeyCode::Right) => Some(Key::Right),
            _ => None,
        },
        _ => None,
    }
}

pub fn map_mouse_button(button: MouseButton) -> usize {
    match button {
        MouseButton::Left => 0,
        MouseButton::Right => 1,
        MouseButton::Middle => 2,
    }
}
