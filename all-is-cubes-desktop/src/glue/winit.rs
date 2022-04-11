// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use winit::dpi::{LogicalSize, PhysicalSize};
use winit::monitor::MonitorHandle;

use all_is_cubes::apps::InputProcessor;
use all_is_cubes::camera::Viewport;
use all_is_cubes::cgmath::Vector2;

pub fn physical_size_to_viewport(scale_factor: f64, size: PhysicalSize<u32>) -> Viewport {
    let size: [u32; 2] = size.into();
    Viewport {
        nominal_size: Vector2::from(size).map(|c| f64::from(c) / scale_factor),
        // max(1) because wgpu likes nonzero viewports
        framebuffer_size: Vector2::from(size).map(|c| c.max(1)),
    }
}

pub fn logical_size_from_vec(size: Vector2<u32>) -> LogicalSize<u32> {
    LogicalSize {
        width: size.x,
        height: size.y,
    }
}

pub fn monitor_size_for_window(monitor: MonitorHandle) -> Vector2<u32> {
    // TODO: monitor.size() doesn't specify whether it's the usable-area or the outer size
    physical_size_to_viewport(monitor.scale_factor(), monitor.size())
        .nominal_size
        .map(|c| c as u32)
}

pub fn map_mouse_button(button: winit::event::MouseButton) -> usize {
    match button {
        winit::event::MouseButton::Left => 0,
        winit::event::MouseButton::Right => 1,
        winit::event::MouseButton::Middle => 2,
        winit::event::MouseButton::Other(other) => other.into(),
    }
}

pub fn map_key(key: winit::event::VirtualKeyCode) -> Option<all_is_cubes::apps::Key> {
    use all_is_cubes::apps::Key as A;
    use winit::event::VirtualKeyCode as V;
    Some(match key {
        V::Key1 => A::Character('1'),
        V::Key2 => A::Character('2'),
        V::Key3 => A::Character('3'),
        V::Key4 => A::Character('4'),
        V::Key5 => A::Character('5'),
        V::Key6 => A::Character('6'),
        V::Key7 => A::Character('7'),
        V::Key8 => A::Character('8'),
        V::Key9 => A::Character('9'),
        V::Key0 => A::Character('0'),
        V::A => A::Character('a'),
        V::B => A::Character('b'),
        V::C => A::Character('c'),
        V::D => A::Character('d'),
        V::E => A::Character('e'),
        V::F => A::Character('f'),
        V::G => A::Character('g'),
        V::H => A::Character('h'),
        V::I => A::Character('i'),
        V::J => A::Character('j'),
        V::K => A::Character('k'),
        V::L => A::Character('l'),
        V::M => A::Character('m'),
        V::N => A::Character('n'),
        V::O => A::Character('o'),
        V::P => A::Character('p'),
        V::Q => A::Character('q'),
        V::R => A::Character('r'),
        V::S => A::Character('s'),
        V::T => A::Character('t'),
        V::U => A::Character('u'),
        V::V => A::Character('v'),
        V::W => A::Character('w'),
        V::X => A::Character('x'),
        V::Y => A::Character('y'),
        V::Z => A::Character('z'),
        V::Escape => return None,
        V::F1 => return None,
        V::F2 => return None,
        V::F3 => return None,
        V::F4 => return None,
        V::F5 => return None,
        V::F6 => return None,
        V::F7 => return None,
        V::F8 => return None,
        V::F9 => return None,
        V::F10 => return None,
        V::F11 => return None,
        V::F12 => return None,
        V::F13 => return None,
        V::F14 => return None,
        V::F15 => return None,
        V::F16 => return None,
        V::F17 => return None,
        V::F18 => return None,
        V::F19 => return None,
        V::F20 => return None,
        V::F21 => return None,
        V::F22 => return None,
        V::F23 => return None,
        V::F24 => return None,
        V::Snapshot => return None,
        V::Scroll => return None,
        V::Pause => return None,
        V::Insert => return None,
        V::Home => return None,
        V::Delete => return None,
        V::End => return None,
        V::PageDown => return None,
        V::PageUp => return None,
        V::Left => A::Left,
        V::Up => A::Up,
        V::Right => A::Right,
        V::Down => A::Down,
        V::Back => return None,
        V::Return => A::Character('\r'),
        V::Space => A::Character(' '),
        V::Compose => return None,
        V::Caret => A::Character('^'),
        V::Numlock => return None,
        V::Numpad0 => A::Character('0'),
        V::Numpad1 => A::Character('1'),
        V::Numpad2 => A::Character('2'),
        V::Numpad3 => A::Character('3'),
        V::Numpad4 => A::Character('4'),
        V::Numpad5 => A::Character('5'),
        V::Numpad6 => A::Character('6'),
        V::Numpad7 => A::Character('7'),
        V::Numpad8 => A::Character('8'),
        V::Numpad9 => A::Character('9'),
        V::NumpadAdd => A::Character(']'),
        V::NumpadDivide => A::Character('/'),
        V::NumpadDecimal => A::Character('.'),
        V::NumpadComma => A::Character(','),
        V::NumpadEnter => A::Character('\r'),
        V::NumpadEquals => A::Character('='),
        V::NumpadMultiply => A::Character('*'),
        V::NumpadSubtract => A::Character('-'),
        V::AbntC1 => return None,
        V::AbntC2 => return None,
        V::Apostrophe => A::Character('\''),
        V::Apps => return None,
        V::Asterisk => A::Character('*'),
        V::At => A::Character('@'),
        V::Ax => return None,
        V::Backslash => A::Character('\\'),
        V::Calculator => return None,
        V::Capital => return None,
        V::Colon => A::Character(':'),
        V::Comma => A::Character(','),
        V::Convert => return None,
        V::Equals => A::Character('='),
        V::Grave => A::Character('`'),
        V::Kana => return None,
        V::Kanji => return None,
        V::LAlt => return None,
        V::LBracket => return None,
        V::LControl => return None,
        V::LShift => return None,
        V::LWin => return None,
        V::Mail => return None,
        V::MediaSelect => return None,
        V::MediaStop => return None,
        V::Minus => A::Character('-'),
        V::Mute => return None,
        V::MyComputer => return None,
        V::NavigateForward => return None,
        V::NavigateBackward => return None,
        V::NextTrack => return None,
        V::NoConvert => return None,
        V::OEM102 => return None,
        V::Period => A::Character('.'),
        V::PlayPause => return None,
        V::Plus => A::Character('+'),
        V::Power => return None,
        V::PrevTrack => return None,
        V::RAlt => return None,
        V::RBracket => return None,
        V::RControl => return None,
        V::RShift => return None,
        V::RWin => return None,
        V::Semicolon => A::Character(';'),
        V::Slash => A::Character('/'),
        V::Sleep => return None,
        V::Stop => return None,
        V::Sysrq => return None,
        V::Tab => A::Character('\t'),
        V::Underline => A::Character('_'),
        V::Unlabeled => return None,
        V::VolumeDown => return None,
        V::VolumeUp => return None,
        V::Wake => return None,
        V::WebBack => return None,
        V::WebFavorites => return None,
        V::WebForward => return None,
        V::WebHome => return None,
        V::WebRefresh => return None,
        V::WebSearch => return None,
        V::WebStop => return None,
        V::Yen => return None,
        V::Copy => return None,
        V::Paste => return None,
        V::Cut => return None,
    })
}

pub fn sync_cursor_grab(window: &winit::window::Window, input_processor: &mut InputProcessor) {
    let wants = input_processor.wants_pointer_lock();
    match window.set_cursor_grab(wants) {
        Ok(()) => {
            window.set_cursor_visible(!wants);
            input_processor.has_pointer_lock(wants);
        }
        Err(_) => {
            // TODO: log error
            window.set_cursor_visible(true);
            input_processor.has_pointer_lock(false);
        }
    }
}
