//! Glue between [`gilrs`] gamepad input and our [`InputProcessor`].
//!
//! TODO: This is very incomplete; nearly unusable.

use gilrs::EventType;

use all_is_cubes_ui::apps::{InputProcessor, Key};

pub(crate) fn apply_gilrs_events(gilrs: &mut gilrs::Gilrs, input_processor: &mut InputProcessor) {
    // Apply input that is state and not events.
    // TODO: InputProcessor has no place for gamepad input
    // if let Some((id, some_gamepad)) = gilrs.gamepads().next() {
    //     if let Some(ad) = some_gamepad.state().axis_data(axis) {
    //         input_processor.mouselook_delta(vec2(ad.value()
    //     }
    // }

    // Consult button press events
    while let Some(event) = gilrs.next_event() {
        match event.event {
            EventType::ButtonPressed(button, _) => {
                if let Some(key) = map_button(button) {
                    input_processor.key_down(key);
                }
            }
            EventType::ButtonRepeated(_, _) => {}
            EventType::ButtonReleased(button, _) => {
                if let Some(key) = map_button(button) {
                    input_processor.key_up(key);
                }
            }

            EventType::ButtonChanged(_, _, _) => {}
            EventType::AxisChanged(_, _, _) => {}

            // TODO: send key-ups on disconnect
            EventType::Connected | EventType::Disconnected => {
                log::debug!("Gamepad: {event:?}")
            }
            EventType::Dropped => {}
            _ => {}
        }
    }
}

fn map_button(button: gilrs::Button) -> Option<Key> {
    use gilrs::Button as B;
    // TODO: bad assumption of InputProcessor's fixed key bindings.
    // InputProcessor needs to be able to be commanded in terms of .
    match button {
        B::South => Some(Key::Character(' ')),
        B::East => None,
        B::North => Some(Key::Character('e')),
        B::West => None,
        B::C => None,
        B::Z => None,
        B::LeftTrigger => None,
        B::LeftTrigger2 => None,
        B::RightTrigger => None,
        B::RightTrigger2 => None,
        B::Select => Some(Key::Character('\t')),
        B::Start => Some(Key::Escape),
        B::Mode => None,
        B::LeftThumb => None,
        B::RightThumb => None,
        B::DPadUp => Some(Key::Character('w')),
        B::DPadDown => Some(Key::Character('s')),
        B::DPadLeft => Some(Key::Character('a')),
        B::DPadRight => Some(Key::Character('d')),
        B::Unknown => None,
    }
}
