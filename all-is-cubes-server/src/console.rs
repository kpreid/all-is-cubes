// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use cgmath::Vector2;
use once_cell::sync::Lazy;
use ordered_float::NotNan;
use std::borrow::Cow;
use std::io;
use termion::color;
use termion::event::{Event, Key};

use all_is_cubes::camera::{Camera, ProjectionHelper};
use all_is_cubes::math::RGBA;
use all_is_cubes::raytracer::{raytrace_space, ColorBuf, PixelBuf};
use all_is_cubes::space::SpaceBlockData;

/// Processes events for moving a camera. Returns all those events it does not process.
#[rustfmt::skip]
pub fn controller(camera: &mut Camera, event: Event) -> Option<Event> {
    camera.auto_rotate = false;  // stop on any keypress
    match event {
        Event::Key(key) => match key {
            Key::Char('w') | Key::Char('W') => { camera.set_velocity_input(( 0.0,  0.0, -1.0)); },
            Key::Char('a') | Key::Char('A') => { camera.set_velocity_input((-1.0,  0.0,  0.0)); },
            Key::Char('s') | Key::Char('S') => { camera.set_velocity_input(( 0.0,  0.0,  1.0)); },
            Key::Char('d') | Key::Char('D') => { camera.set_velocity_input(( 1.0,  0.0,  0.0)); },
            Key::Char('e') | Key::Char('E') => { camera.set_velocity_input(( 0.0,  1.0,  0.0)); },
            Key::Char('c') | Key::Char('C') => { camera.set_velocity_input(( 0.0, -1.0,  0.0)); },
            Key::Up => { camera.body.pitch += 5.0; },
            Key::Down => { camera.body.pitch -= 5.0; },
            Key::Left => { camera.body.yaw -= 5.0; },
            Key::Right => { camera.body.yaw += 5.0; },
            _ => { return Some(event); },
        },
        _ => { return Some(event); },
    }
    None  // match branches default to consuming event
}

pub fn viewport_from_terminal_size() -> io::Result<Vector2<usize>> {
    let (w, h) = termion::terminal_size()?;
    Ok(Vector2::new(w.max(1) as usize, (h - 5).max(1) as usize))
}

// Draw the camera's space to an ANSI terminal using raytracing.
pub fn draw_space<O: io::Write>(
    projection: &mut ProjectionHelper,
    camera: &Camera,
    out: &mut O,
) -> io::Result<()> {
    let space = &*camera.space.borrow_mut();
    projection.set_view_matrix(camera.view());

    // Diagnostic info accumulators
    let mut number_of_cubes_examined: usize = 0;

    write!(out, "{}", termion::cursor::Goto(1, 1))?;
    for (xch, ych, text, count) in raytrace_space::<CharacterBuf>(projection, space) {
        if xch == 0 && ych != 0 {
            write!(out, "{}", *END_OF_LINE)?;
        }
        number_of_cubes_examined += count;
        write!(out, "{}", text)?;
    }
    write!(
        out,
        "{}{}Cubes traced through: {}\r\n",
        *END_OF_LINE,
        termion::clear::AfterCursor,
        number_of_cubes_examined
    )?;
    out.flush()?;

    Ok(())
}

/// Implements `PixelBuf` for text output.
#[derive(Clone, Debug, Default, PartialEq)]
struct CharacterBuf {
    color: ColorBuf,

    /// Text to draw, if determined yet.
    hit_text: Option<String>,

    /// Disables normal colorization.
    override_color: bool,
}

impl PixelBuf for CharacterBuf {
    type Pixel = String;
    type BlockData = Cow<'static, str>;

    fn compute_block_data(s: &SpaceBlockData) -> Self::BlockData {
        // TODO: For more Unicode correctness, index by grapheme cluster...
        // ...and do something clever about double-width characters.
        s.evaluated()
            .attributes
            .display_name
            .chars()
            .next()
            .map(|c| Cow::Owned(c.to_string()))
            .unwrap_or(Cow::Borrowed(&" "))
    }

    fn error_block_data() -> Self::BlockData {
        Cow::Borrowed(&"X")
    }

    fn sky_block_data() -> Self::BlockData {
        Cow::Borrowed(&" ")
    }

    fn opaque(&self) -> bool {
        self.color.opaque()
    }

    fn result(self) -> String {
        if self.override_color {
            return self.hit_text.unwrap();
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
        if let Some(text) = self.hit_text {
            format!(
                "{}{}{}",
                color::Bg(converted_color),
                color::Fg(color::Black),
                text
            )
        } else {
            format!(
                "{}{}.",
                color::Bg(converted_color),
                color::Fg(color::AnsiValue::rgb(5, 5, 5))
            )
        }
    }

    fn add(&mut self, surface_color: RGBA, text: &Self::BlockData) {
        if self.override_color {
            return;
        }

        self.color.add(surface_color, &());

        if self.hit_text.is_none() {
            self.hit_text = Some(text.to_owned().to_string());
        }
    }

    fn hit_nothing(&mut self) {
        self.hit_text = Some(format!(
            "{}{} ",
            color::Bg(color::Reset),
            color::Fg(color::Reset)
        ));
        self.override_color = true;
    }
}

static END_OF_LINE: Lazy<String> =
    Lazy::new(|| format!("{}{}\r\n", color::Bg(color::Reset), color::Fg(color::Reset)));
