// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use crossterm::cursor::{self, MoveTo};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::style::{Attribute, Color, Colors, SetAttribute, SetColors};
use crossterm::terminal::{Clear, ClearType};
use crossterm::QueueableCommand as _;
use std::borrow::Cow;
use std::error::Error;
use std::io;
use std::io::Write as _;
use std::sync::mpsc;
use std::thread;
use std::time::Instant;

use all_is_cubes::apps::{AllIsCubesAppState, Key};
use all_is_cubes::camera::{ProjectionHelper, Viewport};
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::math::{FreeCoordinate, NotNan, Rgb, Rgba};
use all_is_cubes::raytracer::{CharacterBuf, ColorBuf, PixelBuf, SpaceRaytracer};
use all_is_cubes::space::SpaceBlockData;

/// Options for the terminal UI.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TerminalOptions {
    /// Color escapes supported by the terminal.
    colors: ColorMode,
}

impl TerminalOptions {
    /// Obtain the terminal size and adjust it such that it is suitable for a
    /// `ProjectionHelper::set_viewport` followed by `draw_space`.
    fn viewport_from_terminal_size(&self, size: Vector2<u16>) -> Viewport {
        // max(1) is to keep the projection math from blowing up.
        // Subtracting some height allows for info text.
        let w = size.x.max(1);
        let h = size.y.saturating_sub(5).max(1);
        Viewport {
            framebuffer_size: Vector2::new(w.into(), h.into()),
            nominal_size: Vector2::new(FreeCoordinate::from(w) * 0.5, h.into()),
        }
    }
}

impl Default for TerminalOptions {
    fn default() -> Self {
        Self {
            colors: ColorMode::TwoFiftySix, // TODO: default to 16-color mode once we have it implemented
        }
    }
}

pub fn terminal_main_loop(
    app: AllIsCubesAppState,
    options: TerminalOptions,
) -> Result<(), Box<dyn Error>> {
    // TODO: Leftovers from early input-less days.
    app.camera().borrow_mut().auto_rotate = true;

    let mut main = TerminalMain::new(app, options)?;
    main.run()?;
    main.clean_up_terminal()?; // note this is _also_ run on drop
    Ok(())
}

#[derive(Debug)]
struct TerminalMain {
    app: AllIsCubesAppState,
    options: TerminalOptions,
    out: io::Stdout,
    terminal_size: Vector2<u16>,
    projection: ProjectionHelper,
    terminal_state_dirty: bool,
}

impl TerminalMain {
    fn new(app: AllIsCubesAppState, options: TerminalOptions) -> crossterm::Result<Self> {
        crossterm::terminal::enable_raw_mode()?;

        let terminal_size = Vector2::from(crossterm::terminal::size()?);

        Ok(Self {
            projection: ProjectionHelper::new(options.viewport_from_terminal_size(terminal_size)),
            app,
            options,
            out: io::stdout(),
            terminal_size,
            terminal_state_dirty: true,
        })
    }

    /// Reset terminal state, as before exiting.
    /// This will be run on drop but errors will not be reported in that case.
    fn clean_up_terminal(&mut self) -> crossterm::Result<()> {
        self.out.queue(SetAttribute(Attribute::Reset))?;
        self.out
            .queue(SetColors(Colors::new(Color::Reset, Color::Reset)))?;
        self.out.queue(cursor::Show)?;
        crossterm::terminal::disable_raw_mode()?;
        self.terminal_state_dirty = false;
        Ok(())
    }

    fn run(&mut self) -> crossterm::Result<()> {
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

        self.out.queue(Clear(ClearType::All))?;

        loop {
            'input: loop {
                match event_rx.try_recv() {
                    Ok(event) => {
                        if let Some(aic_event) = map_crossterm_event(&event) {
                            if self.app.input_processor.key_momentary(aic_event) {
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
                            Event::Key(KeyEvent {
                                code: KeyCode::Char('m'),
                                modifiers: _,
                            }) => self.options.colors = self.options.colors.cycle(),
                            Event::Key(_) => {}
                            Event::Resize(w, h) => {
                                self.terminal_size = Vector2::new(w, h);
                                self.projection.set_viewport(
                                    self.options.viewport_from_terminal_size(self.terminal_size),
                                );
                                self.out.queue(Clear(ClearType::All))?;
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
            self.app.frame_clock.advance_to(Instant::now());
            self.app.maybe_step_universe();
            if self.app.frame_clock.should_draw() {
                self.draw()?;
                self.app.frame_clock.did_draw();
            } else {
                std::thread::yield_now();
            }
        }
    }

    pub fn draw(&mut self) -> crossterm::Result<()> {
        let projection = &mut self.projection;
        let camera = &*self.app.camera().borrow();
        let out = &mut self.out;
        let options = &self.options;
        let space = &*camera.space.borrow_mut();

        projection.set_view_matrix(camera.view());

        let (image, info) =
            SpaceRaytracer::<ColorCharacterBuf>::new(space).trace_scene_to_image(projection);

        let mut current_color = Colors::new(Color::Reset, Color::Reset);
        out.queue(cursor::Hide)?;
        out.queue(SetAttribute(Attribute::Reset))?;
        out.queue(SetColors(current_color))?;
        out.queue(MoveTo(0, 0))?;

        let fs = projection.viewport().framebuffer_size;
        for y in 0..fs.y as usize {
            for x in 0..fs.x as usize {
                let (ref pixel, color) = image[y * fs.x as usize + x];

                let mapped_color = match color {
                    Some(color) => options.colors.convert(color),
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

        // TODO: Allocate footer space explicitly, don't wrap on small widths, clear, etc.
        write!(
            out,
            "\r\nColors: {:?}    Frame: {:?}\r\n",
            options.colors, info,
        )?;
        out.flush()?;

        Ok(())
    }
}

impl Drop for TerminalMain {
    fn drop(&mut self) {
        if self.terminal_state_dirty {
            let _ = self.clean_up_terminal();
        }
    }
}

/// Converts [`Event`] to [`all_is_cubes::camera::Key`].
///
/// Returns `None` if there is no corresponding value.
fn map_crossterm_event(event: &Event) -> Option<Key> {
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

/// Which type of color control sequences to use.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum ColorMode {
    None,
    // TODO: Sixteen,
    TwoFiftySix,
    Rgb,
}

impl ColorMode {
    fn cycle(self) -> Self {
        use ColorMode::*;
        match self {
            None => TwoFiftySix,
            TwoFiftySix => Rgb,
            Rgb => None,
        }
    }

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
        let o = TerminalOptions::default();
        o.viewport_from_terminal_size((0, 0).into());
        o.viewport_from_terminal_size((0, 1).into());
        o.viewport_from_terminal_size((1, 0).into());
        o.viewport_from_terminal_size((1, 1).into());
    }

    // TODO: add tests of color calculation
}
