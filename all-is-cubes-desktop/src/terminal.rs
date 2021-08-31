// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use cgmath::ElementWise as _;
use crossterm::cursor::{self, MoveTo};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::style::{Attribute, Color, Colors, SetAttribute, SetColors};
use crossterm::terminal::{Clear, ClearType};
use crossterm::QueueableCommand as _;
use cursor::position;
use std::borrow::Cow;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::io::Write as _;
use std::sync::mpsc::{self, TrySendError};
use std::time::{Duration, Instant};

use all_is_cubes::apps::{AllIsCubesAppState, Key};
use all_is_cubes::camera::{Camera, Viewport};
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::math::{FreeCoordinate, Rgba};
use all_is_cubes::raytracer::{CharacterBuf, ColorBuf, PixelBuf, RaytraceInfo, SpaceRaytracer};
use all_is_cubes::space::SpaceBlockData;

/// Options for the terminal UI.
///
/// TODO: Migrate all of this into `GraphicsOptions`? Add an extension mechanism?
/// In any case, we shouldn't have two separately-designed mechanisms, but at most two
/// parallel ones.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TerminalOptions {
    /// Color escapes supported by the terminal.
    colors: ColorMode,

    /// Whether to use block drawing characters (e.g. “▄”) for increased resolution.
    ///
    /// This may cause artifacts in some terminals, and increases the CPU requirement
    /// since more rays are used per frame.
    graphic_characters: bool,
}

impl TerminalOptions {
    /// Obtain the terminal size and adjust it such that it is suitable for a
    /// [`Camera::set_viewport`] followed by [`TerminalMain::draw`].
    fn viewport_from_terminal_size(&self, size: Vector2<u16>) -> Viewport {
        // max(1) is to keep the projection math from blowing up.
        // Subtracting some height allows for info text.
        let w = size.x.max(1);
        let h = size.y.saturating_sub(5).max(1);
        Viewport {
            framebuffer_size: Vector2::new(w.into(), h.into())
                .mul_element_wise(self.rays_per_character().map(u32::from)),
            nominal_size: Vector2::new(FreeCoordinate::from(w) * 0.5, h.into()),
        }
    }

    fn rays_per_character(&self) -> Vector2<u8> {
        if self.graphic_characters {
            Vector2::new(1, 2)
        } else {
            Vector2::new(1, 1)
        }
    }
}

impl Default for TerminalOptions {
    fn default() -> Self {
        Self {
            colors: ColorMode::TwoFiftySix, // TODO: default to 16-color mode once we have it implemented
            graphic_characters: false,      // TODO: default false
        }
    }
}

pub fn terminal_main_loop(
    app: AllIsCubesAppState,
    options: TerminalOptions,
) -> Result<(), Box<dyn Error>> {
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
    camera: Camera,
    /// True if we should clean up on drop.
    terminal_state_dirty: bool,

    // Tracking terminal state.
    terminal_size: Vector2<u16>,
    current_color: Option<Colors>,

    /// The widths of "single characters" according to the terminal's interpretation
    /// (e.g. emoji might be 2 wide), empirically determined by querying the cursor
    /// position.
    widths: HashMap<String, usize>,

    render_pipe_in: mpsc::SyncSender<FrameInput>,
    render_pipe_out: mpsc::Receiver<FrameOutput>,
}

struct FrameInput {
    camera: Camera,
    options: TerminalOptions,
    scene: SpaceRaytracer<ColorCharacterBuf>,
}

/// A frame's pixels and all the context needed to interpret it (which duplicates fields
/// in TerminalMain, but must be carried due to the pipelined rendering).
struct FrameOutput {
    viewport: Viewport,
    options: TerminalOptions,
    image: Box<[<ColorCharacterBuf as PixelBuf>::Pixel]>,
    info: RaytraceInfo,
}

impl TerminalMain {
    fn new(app: AllIsCubesAppState, options: TerminalOptions) -> crossterm::Result<Self> {
        crossterm::terminal::enable_raw_mode()?;

        let terminal_size = Vector2::from(crossterm::terminal::size()?);

        // Create thread for making the raytracing operation concurrent with main loop
        // Note: this doesn't really need a thread but rayon doesn't have a
        // "start but don't block on this" operation.
        let (render_pipe_in, render_thread_in) = mpsc::sync_channel::<FrameInput>(1);
        let (render_thread_out, render_pipe_out) = mpsc::sync_channel(1);
        std::thread::spawn({
            move || {
                while let Ok(FrameInput {
                    camera,
                    options,
                    scene,
                }) = render_thread_in.recv()
                {
                    let (image, info) = scene.trace_scene_to_image(&camera);
                    // Ignore send errors as they just mean we're shutting down or died elsewhere
                    let _ = render_thread_out.send(FrameOutput {
                        viewport: camera.viewport(),
                        options,
                        image,
                        info,
                    });
                }
            }
        });

        Ok(Self {
            camera: Camera::new(
                app.graphics_options().snapshot(),
                options.viewport_from_terminal_size(terminal_size),
            ),
            app,
            options,
            out: io::stdout(),
            terminal_size,
            terminal_state_dirty: true,
            current_color: None,
            widths: HashMap::new(),
            render_pipe_in,
            render_pipe_out,
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
        self.out.queue(Clear(ClearType::All))?;

        loop {
            'input: while crossterm::event::poll(Duration::ZERO)? {
                let event = crossterm::event::read()?;
                if let Some(aic_event) = map_crossterm_event(&event) {
                    if self.app.input_processor.key_momentary(aic_event) {
                        // Handled by input_processor
                        continue 'input;
                    }
                }
                match event {
                    Event::Key(
                        KeyEvent {
                            code: KeyCode::Esc, ..
                        }
                        | KeyEvent {
                            code: KeyCode::Char('c' | 'd'),
                            modifiers: KeyModifiers::CONTROL,
                        },
                    ) => {
                        return Ok(());
                    }
                    Event::Key(KeyEvent {
                        code: KeyCode::Char('n'),
                        modifiers: _,
                    }) => self.options.colors = self.options.colors.cycle(),
                    Event::Key(KeyEvent {
                        code: KeyCode::Char('m'),
                        modifiers: _,
                    }) => {
                        self.options.graphic_characters = !self.options.graphic_characters;
                        self.sync_options();
                    }
                    Event::Key(_) => {}
                    Event::Resize(w, h) => {
                        self.terminal_size = Vector2::new(w, h);
                        self.sync_options();
                        self.out.queue(Clear(ClearType::All))?;
                    }
                    Event::Mouse(_) => {}
                }
            }

            // TODO: sleep instead of spinning, and maybe put a general version of this in AllIsCubesAppState.
            self.app.frame_clock.advance_to(Instant::now());
            self.app.maybe_step_universe();

            match self.render_pipe_out.try_recv() {
                Ok(frame) => self.write_frame(frame)?,
                Err(mpsc::TryRecvError::Empty) => {}
                Err(mpsc::TryRecvError::Disconnected) => panic!("render thread died"),
            }

            if self.app.frame_clock.should_draw() {
                self.app.update_cursor(&self.camera, &self.camera); // TODO: wrong UI camera ...
                self.send_frame_to_render();
            } else {
                std::thread::yield_now();
            }
        }
    }

    fn sync_options(&mut self) {
        self.camera
            .set_viewport(self.options.viewport_from_terminal_size(self.terminal_size));
    }

    fn send_frame_to_render(&mut self) {
        let character = match self.app.character() {
            Some(character_ref) => character_ref.borrow(),
            None => {
                return;
            }
        };
        self.camera.set_view_matrix(character.view());

        let space = &*character.space.borrow();

        match self.render_pipe_in.try_send(FrameInput {
            camera: self.camera.clone(),
            options: self.options.clone(),
            scene: SpaceRaytracer::<ColorCharacterBuf>::new(
                space,
                self.app.graphics_options().snapshot(),
            ),
        }) {
            Ok(()) => {
                self.app.frame_clock.did_draw();
            }
            Err(TrySendError::Disconnected(_)) => {
                // Ignore send errors as they should be detected on the receive side
            }
            Err(TrySendError::Full(_)) => {
                // Skip this frame
            }
        }
    }

    fn write_frame(&mut self, frame: FrameOutput) -> crossterm::Result<()> {
        let FrameOutput {
            viewport,
            options,
            image,
            info,
        } = frame;
        let color_mode = self.options.colors;

        self.out.queue(cursor::Hide)?;
        self.out.queue(SetAttribute(Attribute::Reset))?;
        self.out.queue(MoveTo(0, 0))?;
        self.current_color = None; // pessimistic about prior state

        // TODO: aim for less number casting
        let character_size = viewport
            .framebuffer_size
            .map(|s| s as usize)
            .div_element_wise(options.rays_per_character().map(usize::from));
        for y in 0..character_size.y {
            let mut x = 0;
            while x < character_size.x {
                let (text, color) = Self::image_patch_to_character(
                    &options,
                    &*image,
                    Vector2::new(x, y),
                    character_size,
                );

                let width = self.write_with_color_and_measure(text, color, character_size.x - x)?;
                x += width.max(1); // max(1) prevents infinite looping in edge case
            }
            self.write_with_color("\r\n", Colors::new(Color::Reset, Color::Reset))?;
        }

        // TODO: Allocate footer space explicitly, don't wrap on small widths, clear, etc.
        write!(
            self.out,
            "\r\nColors: {:?}    Frame: {:?}  {:5.1} FPS",
            color_mode,
            info,
            self.app.draw_fps_counter().frames_per_second(),
        )?;
        self.out.queue(Clear(ClearType::UntilNewLine))?;
        write!(self.out, "\r\n")?;
        if let Some(cursor) = self.app.cursor_result() {
            // TODO: design good formatting for cursor data
            write!(
                self.out,
                "{:?} : {}",
                cursor.place, cursor.evaluated.attributes.display_name
            )?;
        }
        self.out.queue(Clear(ClearType::UntilNewLine))?;
        self.out.flush()?;

        Ok(())
    }

    fn image_patch_to_character<'i>(
        options: &TerminalOptions,
        image: &'i [(String, Option<Rgba>)],
        char_pos: Vector2<usize>,
        char_size: Vector2<usize>,
    ) -> (&'i str, Colors) {
        let row = char_size.x;
        // This condition must match TerminalOptions::rays_per_character
        // TODO: Refactor to read patches separately from picking graphic characters
        if options.graphic_characters {
            // TODO: This is a mess
            let (_, color1) = image[(char_pos.y * 2) * row + char_pos.x];
            let (_, color2) = image[(char_pos.y * 2 + 1) * row + char_pos.x];
            let color1 = options.colors.convert(color1);
            let color2 = options.colors.convert(color2);
            if color1 == color2 {
                (
                    // TODO: Offer choice of showing character sometimes. Also use characters for dithering.
                    " ", // text.as_str(),
                    Colors {
                        // Emit color at all only if we're not doing no-colors
                        foreground: color1.map(|_| Color::Black),
                        background: color1,
                    },
                )
            } else {
                (
                    "▄",
                    Colors {
                        foreground: color2,
                        background: color1,
                    },
                )
            }
        } else {
            let (ref text, color) = image[char_pos.y * row + char_pos.x];
            let mapped_color = match options.colors.convert(color) {
                Some(color) => Colors::new(Color::Black, color),
                None => Colors::new(Color::Reset, Color::Reset),
            };
            (text, mapped_color)
        }
    }

    fn write_with_color(&mut self, text: &str, color: Colors) -> crossterm::Result<()> {
        if self.current_color != Some(color) {
            self.current_color = Some(color);
            self.out.queue(SetColors(color))?;
        }
        write!(self.out, "{}", text)?;
        Ok(())
    }

    fn write_with_color_and_measure(
        &mut self,
        text: &str,
        color: Colors,
        max_width: usize,
    ) -> crossterm::Result<usize> {
        if let Some(&w) = self.widths.get(text) {
            if w <= max_width {
                self.write_with_color(text, color)?;
            }
            Ok(w)
        } else {
            // Measure the de-facto width of the string by measuring how far the cursor advances.
            let before = position()?;
            self.write_with_color(text, color)?;
            let after = position()?;
            let w = usize::from(after.0.saturating_sub(before.0));
            if after.1 == before.1 && w > 0 {
                self.widths.insert(text.to_owned(), w);
            }
            Ok(w)
        }
    }
}

impl Drop for TerminalMain {
    fn drop(&mut self) {
        if self.terminal_state_dirty {
            let _ = self.clean_up_terminal();
        }
    }
}

/// Converts [`Event`] to [`Key`].
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

    /// Convert RGB color to a Crossterm [`Color`] value according to this mode.
    ///
    /// The input and output [`Option`]s have different meanings:
    ///
    /// * If the input color is [`None`] then the output is "reset"
    ///   (i.e. the "not colored" colors).
    /// * This function returns [`None`] if color is disabled and no color control
    ///   sequences should be produced — i.e. the input is ignored.
    fn convert(self, input: Option<Rgba>) -> Option<Color> {
        match (input, self) {
            // Mode None produces no output no matter what.
            (_, ColorMode::None) => None,
            // Input None means Reset.
            (None, _) => Some(Color::Reset),
            // ColorMode::Sixteen => {}
            (Some(rgba), ColorMode::TwoFiftySix) => {
                let [r, g, b, _] = rgba.to_srgb_32bit();

                // Crossterm doesn't have a convenient 216-color table. Use Termion's.
                use termion::color::AnsiValue;
                fn scale(x: u8) -> u8 {
                    ((u16::from(x) * 5) / 255) as u8
                }
                let AnsiValue(byte) = AnsiValue::rgb(scale(r), scale(g), scale(b));

                Some(Color::AnsiValue(byte))
            }
            (Some(rgba), ColorMode::Rgb) => {
                let [r, g, b, _] = rgba.to_srgb_32bit();
                let c = Color::Rgb { r, g, b };
                Some(c)
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
    type Pixel = (String, Option<Rgba>);
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
    fn result(self) -> (String, Option<Rgba>) {
        // TODO: override_color should be less clunky
        if self.override_color {
            (self.text.result(), None)
        } else {
            (self.text.result(), Some(self.color.result()))
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
