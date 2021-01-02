// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::io;
use termion::color;
use termion::event::{Event, Key as TermionKey};

use all_is_cubes::camera::{Camera, Key, ProjectionHelper};
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::math::{NotNan, RGBA};
use all_is_cubes::raytracer::{CharacterBuf, ColorBuf, PixelBuf, SpaceRaytracer};
use all_is_cubes::space::SpaceBlockData;

/// Converts `termion::Event` to `all_is_cubes::camera::Key`.
///
/// Returns `None` if there is no corresponding value.
pub fn map_termion_event(event: &Event) -> Option<Key> {
    match event {
        Event::Key(key) => match key {
            TermionKey::Char(c) => Some(Key::Character(c.to_ascii_lowercase())),
            TermionKey::Up => Some(Key::Up),
            TermionKey::Down => Some(Key::Down),
            TermionKey::Left => Some(Key::Left),
            TermionKey::Right => Some(Key::Right),
            _ => None,
        },
        _ => None,
    }
}

/// Obtain the terminal size and adjust it such that it is suitable for a
/// `ProjectionHelper::set_viewport` followed by `draw_space`.
pub fn viewport_from_terminal_size() -> io::Result<Vector2<usize>> {
    let (w, h) = termion::terminal_size()?;
    // max(1) is to keep the projection math from blowing up.
    // Subtracting some height allows for info text.
    Ok(Vector2::new(w.max(1) as usize, (h - 5).max(1) as usize))
}

/// Draw the camera's space to an ANSI terminal using raytracing.
pub fn draw_space<O: io::Write>(
    projection: &mut ProjectionHelper,
    camera: &Camera,
    out: &mut O,
) -> io::Result<()> {
    let space = &*camera.space.borrow_mut();
    projection.set_view_matrix(camera.view());

    write!(out, "{}", termion::cursor::Goto(1, 1))?;
    let info = SpaceRaytracer::<ColorCharacterBuf>::new(space).trace_scene_to_text(
        projection,
        &*END_OF_LINE,
        |s| write!(out, "{}", s),
    )?;
    write!(
        out,
        "{}{}{:?}\r\n",
        *END_OF_LINE,
        termion::clear::AfterCursor,
        info
    )?;
    out.flush()?;

    Ok(())
}

/// Implements `PixelBuf` for colored text output using `termion`.
#[derive(Clone, Debug, Default, PartialEq)]
struct ColorCharacterBuf {
    color: ColorBuf,
    text: CharacterBuf,

    /// Disables normal colorization.
    override_color: bool,
}

impl PixelBuf for ColorCharacterBuf {
    type Pixel = String;
    type BlockData = <CharacterBuf as PixelBuf>::BlockData;

    fn compute_block_data(s: &SpaceBlockData) -> Self::BlockData {
        CharacterBuf::compute_block_data(s)
    }

    fn error_block_data() -> Self::BlockData {
        CharacterBuf::error_block_data()
    }

    fn sky_block_data() -> Self::BlockData {
        CharacterBuf::sky_block_data()
    }

    #[inline]
    fn opaque(&self) -> bool {
        self.color.opaque()
    }

    #[inline]
    fn result(self) -> String {
        if self.override_color {
            return self.text.result();
        }

        let final_rgb = self.color.result().to_rgb();
        // TODO: Pick 8/256/truecolor based on what the terminal supports.
        fn scale(x: NotNan<f32>) -> u8 {
            let scale = 5.0;
            (x.into_inner() * scale).max(0.0).min(scale) as u8
        }
        let converted_color = color::AnsiValue::rgb(
            scale(final_rgb.red()),
            scale(final_rgb.green()),
            scale(final_rgb.blue()),
        );
        if self.text.opaque() {
            format!(
                "{}{}{}",
                color::Bg(converted_color),
                color::Fg(color::Black),
                self.text.result()
            )
        } else {
            format!(
                "{}{}.",
                color::Bg(converted_color),
                color::Fg(color::AnsiValue::rgb(5, 5, 5))
            )
        }
    }

    #[inline]
    fn add(&mut self, surface_color: RGBA, text: &Self::BlockData) {
        if self.override_color {
            return;
        }

        self.color.add(surface_color, &());
        self.text.add(surface_color, text);
    }

    fn hit_nothing(&mut self) {
        self.text.add(
            RGBA::TRANSPARENT,
            &Cow::Owned(format!(
                "{}{} ",
                color::Bg(color::Reset),
                color::Fg(color::Reset)
            )),
        );
        self.override_color = true;
    }
}

static END_OF_LINE: Lazy<String> =
    Lazy::new(|| format!("{}{}\r\n", color::Bg(color::Reset), color::Fg(color::Reset)));
