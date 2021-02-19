// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use crossterm::cursor::MoveTo;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::style::{Color, Colors, SetColors};
use crossterm::terminal::{Clear, ClearType};
use crossterm::QueueableCommand;
use std::borrow::Cow;
use std::error::Error;
use std::io;
use std::sync::mpsc;
use std::thread;
use std::time::Instant;

use all_is_cubes::apps::{AllIsCubesAppState, Key};
use all_is_cubes::camera::{Camera, ProjectionHelper, Viewport};
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::math::{FreeCoordinate, NotNan, Rgb, Rgba};
use all_is_cubes::raytracer::{CharacterBuf, ColorBuf, PixelBuf, SpaceRaytracer};
use all_is_cubes::space::SpaceBlockData;

pub fn terminal_main_loop(app: AllIsCubesAppState) -> Result<(), Box<dyn Error>> {
    // TODO: Leftovers from early input-less days.
    app.camera().borrow_mut().auto_rotate = true;

    crossterm::terminal::enable_raw_mode()?;

    // Run the actual work in a function so we can make really sure
    // that we always disable_raw_mode. (TODO: How about a Drop hook...?)
    let result = real_main_loop(app);
    let _ = crossterm::terminal::disable_raw_mode();
    result
}

fn real_main_loop(mut app: AllIsCubesAppState) -> Result<(), Box<dyn Error>> {
    let mut proj: ProjectionHelper =
        ProjectionHelper::new(viewport_from_terminal_size(crossterm::terminal::size()?));
    let mut out = io::stdout();

    // Park stdin blocking reads on another thread.
    let (event_tx, event_rx) = mpsc::sync_channel(0);
    thread::spawn(move || {
        loop {
            match crossterm::event::read() {
                Ok(event) => event_tx.send(event).unwrap(),
                Err(err) => {
                    eprintln!("stdin read error: {}", err);
                    break;
                }
            }
        }
        eprintln!("read thread exiting");
    });

    out.queue(Clear(ClearType::All))?;

    loop {
        'input: loop {
            match event_rx.try_recv() {
                Ok(event) => {
                    if let Some(aic_event) = map_crossterm_event(&event) {
                        if app.input_processor.key_momentary(aic_event) {
                            // Handled by input_processor
                            continue 'input;
                        }
                    }
                    match event {
                        Event::Key(KeyEvent {
                            code: KeyCode::Esc, ..
                        })
                        | Event::Key(KeyEvent {
                            code: KeyCode::Char('c'),
                            modifiers: KeyModifiers::CONTROL,
                        })
                        | Event::Key(KeyEvent {
                            code: KeyCode::Char('d'),
                            modifiers: KeyModifiers::CONTROL,
                        }) => {
                            return Ok(());
                        }
                        Event::Key(_) => {}
                        Event::Resize(w, h) => {
                            proj.set_viewport(viewport_from_terminal_size((w, h)));
                            out.queue(Clear(ClearType::All))?;
                        }
                        Event::Mouse(_) => {}
                    }
                }
                Err(mpsc::TryRecvError::Disconnected) => {
                    eprintln!("input disconnected");
                    return Ok(());
                }
                Err(mpsc::TryRecvError::Empty) => {
                    break 'input;
                }
            }
        }

        // TODO: sleep instead of spinning, and maybe put a general version of this in AllIsCubesAppState.
        app.frame_clock.advance_to(Instant::now());
        app.maybe_step_universe();
        if app.frame_clock.should_draw() {
            draw_space(&mut proj, &*app.camera().borrow(), &mut out)?;
            app.frame_clock.did_draw();
        } else {
            std::thread::yield_now();
        }
    }
}

/// Converts [`Event`] to [`all_is_cubes::camera::Key`].
///
/// Returns `None` if there is no corresponding value.
pub fn map_crossterm_event(event: &Event) -> Option<Key> {
    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Char(c)) => Some(Key::Character(c.to_ascii_lowercase())),
            (_, KeyCode::Up) => Some(Key::Up),
            (_, KeyCode::Down) => Some(Key::Down),
            (_, KeyCode::Left) => Some(Key::Left),
            (_, KeyCode::Right) => Some(Key::Right),
            _ => None,
        },
        _ => None,
    }
}

/// Obtain the terminal size and adjust it such that it is suitable for a
/// `ProjectionHelper::set_viewport` followed by `draw_space`.
pub fn viewport_from_terminal_size((w, h): (u16, u16)) -> Viewport {
    // max(1) is to keep the projection math from blowing up.
    // Subtracting some height allows for info text.
    let w = w.max(1);
    let h = h.saturating_sub(5).max(1);
    Viewport {
        framebuffer_size: Vector2::new(w.into(), h.into()),
        nominal_size: Vector2::new(FreeCoordinate::from(w) * 0.5, h.into()),
    }
}

/// Draw the camera's space to an ANSI terminal using raytracing.
pub fn draw_space<O: io::Write>(
    projection: &mut ProjectionHelper,
    camera: &Camera,
    out: &mut O,
) -> crossterm::Result<()> {
    let space = &*camera.space.borrow_mut();
    projection.set_view_matrix(camera.view());

    let (image, info) =
        SpaceRaytracer::<ColorCharacterBuf>::new(space).trace_scene_to_image(projection);

    out.queue(MoveTo(0, 0))?;
    let mut current_color = Colors::new(Color::Reset, Color::Reset);
    let fs = projection.viewport().framebuffer_size;
    for y in 0..fs.y as usize {
        for x in 0..fs.x as usize {
            let (ref pixel, color) = image[y * fs.x as usize + x];

            let mapped_color = match color {
                Some(color) => ColorMode::TwoFiftySix.convert(color),
                None => Colors::new(Color::Reset, Color::Reset),
            };
            if mapped_color != current_color {
                current_color = mapped_color;
                out.queue(SetColors(current_color))?;
            }
            write!(out, "{}", pixel)?;
        }
        current_color = Colors::new(Color::Reset, Color::Reset);
        out.queue(SetColors(current_color))?;
        write!(out, "\r\n")?;
    }
    write!(out, "\r\n{:?}\r\n", info)?;
    out.flush()?;

    Ok(())
}

/// Which type of color control sequences to use.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum ColorMode {
    None,
    // TODO: Sixteen,
    TwoFiftySix,
    Rgb,
}

impl ColorMode {
    fn convert(self, rgb: Rgb) -> Colors {
        // TODO: Pick 8/256/truecolor based on what the terminal supports.
        fn scale(x: NotNan<f32>, scale: f32) -> u8 {
            (x.into_inner() * scale).max(0.0).min(scale) as u8
        }

        match self {
            ColorMode::None => Colors {
                foreground: None,
                background: None,
            },
            // ColorMode::Sixteen => {}
            ColorMode::TwoFiftySix => {
                // Crossterm doesn't have a convenient 216-color table. Use Termion's.
                use termion::color::AnsiValue;
                let AnsiValue(byte) = AnsiValue::rgb(
                    scale(rgb.red(), 5.),
                    scale(rgb.green(), 5.),
                    scale(rgb.blue(), 5.),
                );
                Colors::new(Color::Black, Color::AnsiValue(byte))
            }
            ColorMode::Rgb => {
                let c = Color::Rgb {
                    r: scale(rgb.red(), 255.),
                    g: scale(rgb.green(), 255.),
                    b: scale(rgb.blue(), 255.),
                };
                Colors::new(Color::Black, c)
            }
        }
    }
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
    type Pixel = (String, Option<Rgb>);
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
    fn result(self) -> (String, Option<Rgb>) {
        // TODO: override_color should be less clunky
        if self.override_color {
            (self.text.result(), None)
        } else {
            (self.text.result(), Some(self.color.result().to_rgb()))
        }
    }

    #[inline]
    fn add(&mut self, surface_color: Rgba, text: &Self::BlockData) {
        if self.override_color {
            return;
        }

        self.color.add(surface_color, &());
        self.text.add(surface_color, text);
    }

    fn hit_nothing(&mut self) {
        self.text.add(Rgba::TRANSPARENT, &Cow::Borrowed(" "));
        self.override_color = true;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn viewport_no_panic() {
        viewport_from_terminal_size((0, 0));
        viewport_from_terminal_size((0, 1));
        viewport_from_terminal_size((1, 0));
        viewport_from_terminal_size((1, 1));
    }

    // TODO: add tests of color calculation
}
