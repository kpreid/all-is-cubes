#![allow(
    exported_private_dependencies,
    reason = "false positive on dpi via winit"
)]

use winit::dpi::{LogicalSize, PhysicalSize};
use winit::monitor::MonitorHandle;

use all_is_cubes::euclid::Size2D;
use all_is_cubes_render::camera::{self, Viewport};

pub fn physical_size_to_viewport(scale_factor: f64, size: PhysicalSize<u32>) -> Viewport {
    let size: camera::ImageSize = <[u32; 2]>::from(size).into();
    // max(1) because wgpu wants guaranteed nonzero viewports
    Viewport::with_scale(scale_factor, size.max(Size2D::splat(1)))
}

pub fn to_logical_size(size: Size2D<u32, camera::NominalPixel>) -> LogicalSize<u32> {
    LogicalSize {
        width: size.width,
        height: size.height,
    }
}

pub fn monitor_size_for_window(monitor: &MonitorHandle) -> Size2D<u32, camera::NominalPixel> {
    // TODO: monitor.size() doesn't specify whether it's the usable-area or the outer size
    physical_size_to_viewport(monitor.scale_factor(), monitor.size())
        .nominal_size
        .to_u32()
}

pub fn map_mouse_button(button: winit::event::MouseButton) -> usize {
    match button {
        winit::event::MouseButton::Left => 0,
        winit::event::MouseButton::Right => 1,
        winit::event::MouseButton::Middle => 2,
        winit::event::MouseButton::Back => 3,
        winit::event::MouseButton::Forward => 4,
        winit::event::MouseButton::Other(other) => other.into(),
    }
}

pub fn map_key(key: winit::keyboard::PhysicalKey) -> Option<all_is_cubes_ui::apps::Key> {
    use all_is_cubes_ui::apps::Key as A;
    use winit::keyboard::KeyCode as V;
    let key = match key {
        winit::keyboard::PhysicalKey::Code(key_code) => key_code,
        winit::keyboard::PhysicalKey::Unidentified(_) => return None,
    };
    Some(match key {
        V::Digit0 => A::Character('0'),
        V::Digit1 => A::Character('1'),
        V::Digit2 => A::Character('2'),
        V::Digit3 => A::Character('3'),
        V::Digit4 => A::Character('4'),
        V::Digit5 => A::Character('5'),
        V::Digit6 => A::Character('6'),
        V::Digit7 => A::Character('7'),
        V::Digit8 => A::Character('8'),
        V::Digit9 => A::Character('9'),
        V::KeyA => A::Character('a'),
        V::KeyB => A::Character('b'),
        V::KeyC => A::Character('c'),
        V::KeyD => A::Character('d'),
        V::KeyE => A::Character('e'),
        V::KeyF => A::Character('f'),
        V::KeyG => A::Character('g'),
        V::KeyH => A::Character('h'),
        V::KeyI => A::Character('i'),
        V::KeyJ => A::Character('j'),
        V::KeyK => A::Character('k'),
        V::KeyL => A::Character('l'),
        V::KeyM => A::Character('m'),
        V::KeyN => A::Character('n'),
        V::KeyO => A::Character('o'),
        V::KeyP => A::Character('p'),
        V::KeyQ => A::Character('q'),
        V::KeyR => A::Character('r'),
        V::KeyS => A::Character('s'),
        V::KeyT => A::Character('t'),
        V::KeyU => A::Character('u'),
        V::KeyV => A::Character('v'),
        V::KeyW => A::Character('w'),
        V::KeyX => A::Character('x'),
        V::KeyY => A::Character('y'),
        V::KeyZ => A::Character('z'),
        V::Escape => A::Escape,
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
        V::ArrowLeft => A::Left,
        V::ArrowUp => A::Up,
        V::ArrowRight => A::Right,
        V::ArrowDown => A::Down,
        V::Space => A::Character(' '),
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
        V::NumpadAdd => A::Character('+'),
        V::NumpadDivide => A::Character('/'),
        V::NumpadDecimal => A::Character('.'),
        V::NumpadComma => A::Character(','),
        V::NumpadEnter => A::Character('\r'),
        V::NumpadEqual => A::Character('='),
        V::NumpadMultiply => A::Character('*'),
        V::NumpadSubtract => A::Character('-'),
        V::Quote => A::Character('\''),
        V::Backslash => A::Character('\\'),
        V::Comma => A::Character(','),
        V::Equal => A::Character('='),
        V::Backquote => A::Character('`'),
        V::Minus => A::Character('-'),
        V::Period => A::Character('.'),
        V::Semicolon => A::Character(';'),
        V::Slash => A::Character('/'),
        V::Tab => A::Character('\t'),
        _ => return None,
    })
}

pub fn cursor_icon_to_winit(icon: &all_is_cubes_ui::apps::CursorIcon) -> winit::window::CursorIcon {
    use all_is_cubes_ui::apps::CursorIcon as A;
    use winit::window::CursorIcon as W;
    match icon {
        A::Crosshair => W::Crosshair,
        A::PointingHand => W::Pointer,
        /* A::Normal | */ _ => W::Default,
    }
}
