// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::{self, Write as _};
use std::sync::mpsc::{self, TrySendError};
use std::time::Duration;

use crossterm::cursor::{self, MoveTo};
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent, MouseEventKind};
use crossterm::style::{Attribute, Color, Colors, SetAttribute, SetColors};
use crossterm::QueueableCommand as _;
use tui::layout::{Constraint, Direction, Layout, Rect};
use tui::style::{Color as TuiColor, Modifier, Style};
use tui::text::{Span, Spans};
use tui::widgets::{Borders, Paragraph};
use tui::{backend::CrosstermBackend, Terminal};

use all_is_cubes::apps::{Session, StandardCameras};
use all_is_cubes::camera::{Camera, Viewport};
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::cgmath::{ElementWise as _, Point2};
use all_is_cubes::inv::Slot;
use all_is_cubes::listen::{ListenableCell, ListenableSource};
use all_is_cubes::math::{FreeCoordinate, Rgba};
use all_is_cubes::raytracer::{
    CharacterBuf, CharacterRtData, ColorBuf, PixelBuf, RaytraceInfo, RtRenderer,
};

use crate::glue::crossterm::{event_to_key, map_mouse_button};
use crate::session::{ClockSource, DesktopSession};

/// Options for the terminal UI.
///
/// TODO: Migrate all of this into `GraphicsOptions`? Add an extension mechanism?
/// In any case, we shouldn't have two separately-designed mechanisms, but at most two
/// parallel ones.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TerminalOptions {
    /// Color escapes supported by the terminal.
    colors: ColorMode,

    characters: CharacterMode,
}

impl TerminalOptions {
    /// Compute viewport to use for raytracing, given the size in characters of the
    /// drawing area.
    fn viewport_from_terminal_size(&self, size: Vector2<u16>) -> Viewport {
        // max(1) is to keep the projection math from blowing up.
        // TODO: Remove this and make it more robust instead.
        let size = size.map(|c| c.max(1));
        Viewport {
            framebuffer_size: size
                .map(u32::from)
                .mul_element_wise(self.characters.rays_per_character().map(u32::from)),

            // Assume that characters are approximately twice as tall as they are wide.
            nominal_size: size
                .map(FreeCoordinate::from)
                .mul_element_wise(Vector2::new(0.5, 1.0)),
        }
    }
}

impl Default for TerminalOptions {
    fn default() -> Self {
        Self {
            colors: ColorMode::TwoFiftySix, // TODO: default to 16-color mode once we have it implemented
            characters: CharacterMode::Split,
        }
    }
}

pub fn terminal_main_loop(session: Session, options: TerminalOptions) -> Result<(), anyhow::Error> {
    let mut main = TerminalMain::new(session, options)?;
    main.run()?;
    main.clean_up_terminal()?; // note this is _also_ run on drop
    Ok(())
}

/// Print the scene to stdout and return, instead of starting any interaction.
///
/// TODO: This shouldn't need to take ownership of the `session`; it does so because it is
/// built on the same components as [`terminal_main_loop`].
pub fn terminal_print_once(
    session: Session,
    options: TerminalOptions,
    display_size: Vector2<u16>,
) -> Result<(), anyhow::Error> {
    let mut main = TerminalMain::new(session, options)?;
    main.print_once(display_size)?;
    main.clean_up_terminal()?; // note this is _also_ run on drop
    Ok(())
}

struct TerminalMain {
    dsession: DesktopSession<TerminalRenderer, ()>,

    options: TerminalOptions,
    tuiout: tui::Terminal<CrosstermBackend<io::Stdout>>,
    /// True if we should clean up on drop.
    terminal_state_dirty: bool,

    // Tracking terminal state.
    /// Regionof the terminal the scene is drawn into.
    viewport_position: Rect,
}

/// Fills the renderer slot of [`DesktopSession`].
///
/// TODO: Refactor this 'till we can merge it with the record mode
/// raytracing.
struct TerminalRenderer {
    /// Redundant with the RtRenderer's cameras, but is a copy that
    /// isn't hopping around threads.
    cameras: StandardCameras,

    /// The widths of "single characters" according to the terminal's interpretation
    /// (e.g. emoji might be 2 wide), empirically determined by querying the cursor
    /// position.
    widths: HashMap<String, u16>,

    buffer_reuse_out: mpsc::Receiver<RtRenderer<CharacterRtData>>,
    render_pipe_in: mpsc::SyncSender<FrameInput>,
    render_pipe_out: mpsc::Receiver<FrameOutput>,
}

struct FrameInput {
    options: TerminalOptions,
    scene: RtRenderer<CharacterRtData>,
}

/// A frame's pixels and all the context needed to interpret it (which duplicates fields
/// in TerminalMain, but must be carried due to the pipelined rendering).
struct FrameOutput {
    /// The `framebuffer_size` of this viewport is equal to the size of the `image` data.
    viewport: Viewport,
    options: TerminalOptions,
    image: Vec<TextAndColor>,
    info: RaytraceInfo,
}

impl TerminalMain {
    fn new(session: Session, options: TerminalOptions) -> crossterm::Result<Self> {
        crossterm::terminal::enable_raw_mode()?;

        let viewport_position = Rect::default();
        let viewport_cell =
            ListenableCell::new(options.viewport_from_terminal_size(rect_size(viewport_position)));
        let cameras = StandardCameras::from_session(&session, viewport_cell.as_source()).unwrap();

        // Generate reusable buffers for scene.
        // They are recirculated through the channels so that one can be updated while another is being raytraced.
        let number_of_scene_buffers = 3;
        let (buffer_reuse_in, buffer_reuse_out) =
            mpsc::sync_channel::<RtRenderer<CharacterRtData>>(number_of_scene_buffers);
        for _ in 0..number_of_scene_buffers {
            buffer_reuse_in
                .send(RtRenderer::new(
                    cameras.clone(),
                    Box::new(|v| v),
                    ListenableSource::constant(()),
                ))
                .unwrap();
        }

        // Create thread for making the raytracing operation concurrent with main loop
        // Note: this doesn't really need a thread but rayon doesn't have a
        // "start but don't block on this" operation.
        let (render_pipe_in, render_thread_in) = mpsc::sync_channel::<FrameInput>(1);
        let (render_thread_out, render_pipe_out) = mpsc::sync_channel(1);
        std::thread::Builder::new()
            .name("raytracer".to_string())
            .spawn({
                move || {
                    while let Ok(FrameInput { options, scene }) = render_thread_in.recv() {
                        // TODO: it isn't actually correct to use the world camera always,
                        // once UI exists because it will use the world exposure for UI,
                        // but this will require a rethink of the raytracer interface.
                        let camera = &scene.cameras().cameras().world;
                        let (viewport, image, info) =
                            scene.draw::<ColorCharacterBuf, _, _>("", |b| b.output(camera));
                        // Ignore send errors as they just mean we're shutting down or died elsewhere
                        let _ = render_thread_out.send(FrameOutput {
                            viewport,
                            options,
                            image,
                            info,
                        });
                        let _ = buffer_reuse_in.send(scene);
                    }
                }
            })?;

        Ok(Self {
            dsession: DesktopSession {
                session,
                renderer: TerminalRenderer {
                    cameras,
                    widths: HashMap::new(),
                    buffer_reuse_out,
                    render_pipe_in,
                    render_pipe_out,
                },
                window: (),
                viewport_cell,
                clock_source: ClockSource::Instant,
            },
            options,
            tuiout: Terminal::new(CrosstermBackend::new(io::stdout()))?,
            viewport_position,
            terminal_state_dirty: true,
        })
    }

    /// Reset terminal state, as before exiting.
    /// This will be run on drop but errors will not be reported in that case.
    fn clean_up_terminal(&mut self) -> crossterm::Result<()> {
        let out = self.tuiout.backend_mut();
        out.queue(SetAttribute(Attribute::Reset))?;
        out.queue(SetColors(Colors::new(Color::Reset, Color::Reset)))?;
        out.queue(cursor::Show)?;
        out.queue(crossterm::event::DisableMouseCapture)?;
        crossterm::terminal::disable_raw_mode()?;
        self.terminal_state_dirty = false;
        Ok(())
    }

    /// Run the simulation and interactive UI. Returns after user's quit command.
    fn run(&mut self) -> crossterm::Result<()> {
        self.tuiout
            .backend_mut()
            .queue(crossterm::event::EnableMouseCapture)?;
        self.tuiout.clear()?;

        loop {
            'input: while crossterm::event::poll(Duration::ZERO)? {
                let event = crossterm::event::read()?;
                if let Some(aic_event) = event_to_key(&event) {
                    if self
                        .dsession
                        .session
                        .input_processor
                        .key_momentary(aic_event)
                    {
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
                        self.options.characters = self.options.characters.cycle();
                        self.sync_viewport();
                    }
                    Event::Key(_) => {}
                    Event::Resize(..) => { /* tui handles this */ }
                    Event::Mouse(MouseEvent {
                        kind,
                        column,
                        row,
                        modifiers: _,
                    }) => {
                        // TODO: there's a scaling of nominal_size in viewport_from_terminal_size that we
                        // need to undo here, but it would be better to directly input the right coordinate
                        // system. Define a version of mouse_pixel_position which takes sizes directly?
                        let position =
                            Point2::new((f64::from(column) - 0.5) * 0.5, f64::from(row) - 0.5);
                        self.dsession.session.input_processor.mouse_pixel_position(
                            *self.dsession.viewport_cell.get(),
                            Some(position),
                            true,
                        );
                        match kind {
                            MouseEventKind::Down(button) => {
                                self.dsession.session.click(map_mouse_button(button));
                            }
                            MouseEventKind::Up(_)
                            | MouseEventKind::Drag(_)
                            | MouseEventKind::Moved
                            | MouseEventKind::ScrollDown
                            | MouseEventKind::ScrollUp => {}
                        }
                    }
                }
            }

            self.dsession.advance_time_and_maybe_step();

            match self.dsession.renderer.render_pipe_out.try_recv() {
                Ok(frame) => {
                    self.write_ui(&frame)?;
                    self.write_frame(frame, true)?;
                }
                // TODO: Even if we don't have a frame, we might want to update the UI anyway.
                Err(mpsc::TryRecvError::Empty) => {}
                Err(mpsc::TryRecvError::Disconnected) => panic!("render thread died"),
            }

            if self.dsession.session.frame_clock.should_draw() {
                // TODO: this is the only reason .cameras exists
                let c = &mut self.dsession.renderer.cameras;
                c.update();
                self.dsession.session.update_cursor(&*c);

                self.send_frame_to_render();
            } else {
                // TODO: sleep instead of spinning.
                std::thread::yield_now();
            }
        }
    }

    /// Prints one frame, without clearing the terminal, and returns.
    fn print_once(&mut self, image_size_in_characters: Vector2<u16>) -> crossterm::Result<()> {
        self.viewport_position =
            Rect::new(0, 0, image_size_in_characters.x, image_size_in_characters.y);
        self.sync_viewport();

        self.send_frame_to_render();
        let frame = self
            .dsession
            .renderer
            .render_pipe_out
            .recv()
            .expect("Internal error in rendering");
        self.write_frame(frame, false)?;

        Ok(())
    }

    fn sync_viewport(&mut self) {
        self.dsession.viewport_cell.set(
            self.options
                .viewport_from_terminal_size(rect_size(self.viewport_position)),
        );
    }

    fn send_frame_to_render(&mut self) {
        // Fetch and update one of our recirculating renderers.
        let mut renderer = self.dsession.renderer.buffer_reuse_out.recv().unwrap();
        renderer
            .update(self.dsession.session.cursor_result())
            .unwrap();

        match self.dsession.renderer.render_pipe_in.try_send(FrameInput {
            options: self.options.clone(),
            scene: renderer,
        }) {
            Ok(()) => {
                self.dsession.session.frame_clock.did_draw();
            }
            Err(TrySendError::Disconnected(_)) => {
                // Ignore send errors as they should be detected on the receive side
            }
            Err(TrySendError::Full(_)) => {
                // Skip this frame
            }
        }
    }

    /// Lay out and write the UI using [`tui`] -- everything on screen *except* for
    /// the raytraced scene.
    ///
    /// This function also stores the current scene viewport for future frames.
    fn write_ui(&mut self, frame: &FrameOutput) -> crossterm::Result<()> {
        let FrameOutput { info, .. } = frame;
        let mut viewport_rect = None;
        self.tuiout.draw(|f| {
            const HELP_TEXT: &str = "\
                Move: WS AD EC  Turn: ←→ ↑↓\n\
                Term color: N   Term chars: M\n\
                Quit: Esc, ^C, or ^D";

            let [viewport_rect_tmp, toolbar_rect, gfx_info_rect, cursor_and_help_rect]: [Rect; 4] =
                Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Min(1),
                        Constraint::Length(3),
                        Constraint::Length(2),
                        Constraint::Length(3),
                    ])
                    .split(f.size())
                    .try_into()
                    .unwrap();
            let [cursor_rect, help_rect]: [Rect; 2] = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Min(0), Constraint::Length(30)])
                .split(cursor_and_help_rect)
                .try_into()
                .unwrap();
            // Toolbar
            {
                const SLOTS: usize = 10; // TODO: link with other UI and gameplay code

                const SELECTED_BLANK: Span<'static> = Span {
                    content: Cow::Borrowed(" "),
                    style: STYLE_NONE,
                };
                const SELECTED_0: Span<'static> = Span {
                    content: Cow::Borrowed("1"),
                    style: Style {
                        fg: Some(TuiColor::Black),
                        bg: Some(TuiColor::Red),
                        ..STYLE_NONE
                    },
                };
                const SELECTED_1: Span<'static> = Span {
                    content: Cow::Borrowed("2"),
                    style: Style {
                        fg: Some(TuiColor::Black),
                        bg: Some(TuiColor::Yellow),
                        ..STYLE_NONE
                    },
                };

                let slot_rects = Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([Constraint::Ratio(1, SLOTS as u32); SLOTS])
                    .split(toolbar_rect);

                if let Some(character_ref) = self.dsession.session.character().snapshot() {
                    let character = character_ref.borrow();
                    let selected_slots = character.selected_slots();
                    let slots = &character.inventory().slots;
                    for (i, rect) in slot_rects.into_iter().enumerate() {
                        let slot = slots.get(i).unwrap_or(&Slot::Empty);
                        let slot_info = Spans::from(vec![
                            if selected_slots[0] == i {
                                SELECTED_0
                            } else {
                                SELECTED_BLANK
                            },
                            Span::from(format!(" {} ", i)),
                            if selected_slots[1] == i {
                                SELECTED_1
                            } else {
                                SELECTED_BLANK
                            },
                        ]);
                        let block = tui::widgets::Block::default()
                            .title(slot_info)
                            .borders(Borders::ALL);
                        f.render_widget(
                            match slot {
                                // TODO: Use item icon text -- we need a way to access the predefined icons from here
                                Slot::Empty => Paragraph::new(""),
                                Slot::Stack(count, item) if count.get() == 1 => {
                                    Paragraph::new(format!("{:?}", item))
                                }
                                Slot::Stack(count, item) => {
                                    Paragraph::new(format!("{} × {:?}", count, item))
                                }

                                // Fallback
                                slot => Paragraph::new(format!("{:?}", slot)),
                            }
                            .block(block),
                            rect,
                        );
                    }
                } else {
                    // TODO: render blank slots
                }
            }

            viewport_rect = Some(viewport_rect_tmp);

            // Graphics info line
            {
                let [frame_info_rect, colors_info_rect, render_info_rect]: [Rect; 3] =
                    Layout::default()
                        .direction(Direction::Horizontal)
                        .constraints([
                            Constraint::Percentage(30),
                            Constraint::Percentage(30),
                            Constraint::Percentage(30),
                        ])
                        .split(gfx_info_rect)
                        .try_into()
                        .unwrap();

                f.render_widget(
                    Paragraph::new(format!(
                        "{:5.1} FPS",
                        self.dsession.session.draw_fps_counter().frames_per_second()
                    )),
                    frame_info_rect,
                );

                f.render_widget(
                    Paragraph::new(format!(
                        "Colors: {:?}\nChars: {:?}",
                        self.options.colors, self.options.characters
                    )),
                    colors_info_rect,
                );

                f.render_widget(Paragraph::new(format!("{:?}", info)), render_info_rect);
            }

            // Cursor info
            f.render_widget(
                if let Some(cursor) = self.dsession.session.cursor_result() {
                    // TODO: design good formatting for cursor data
                    Paragraph::new(format!(
                        "{:?} : {}",
                        cursor.place, cursor.evaluated.attributes.display_name
                    ))
                } else {
                    Paragraph::new("")
                },
                cursor_rect,
            );

            // Help text
            f.render_widget(Paragraph::new(HELP_TEXT), help_rect);
        })?;

        // Store latest layout position so it can be used for choosing what size to render and for
        // independent drawing.
        let viewport_rect = viewport_rect.expect("layout failed to update");
        if self.viewport_position != viewport_rect {
            self.viewport_position = viewport_rect;
            self.sync_viewport();
        }

        Ok(())
    }

    /// Actually write image data to the terminal.
    ///
    /// If `draw_into_rect` is true, moves the cursor to fit into `self.viewport_position`.
    /// If it is false, does not affect the cursor position and uses newlines.
    fn write_frame(&mut self, frame: FrameOutput, draw_into_rect: bool) -> crossterm::Result<()> {
        let FrameOutput {
            viewport,
            options,
            image,
            info: _,
        } = frame;

        // Now separately draw the frame data. This is done because we want to use a precise
        // strategy for measuring the width of characters (draw them and read back the cursor
        // position) whereas tui-rs assumes that `unicode_width`'s answers match the terminal.
        // For all UI text, we run with that since it should be only minor glitches, but the
        // scene display needs accurate horizontal alignment.

        let backend = self.tuiout.backend_mut();
        backend.queue(cursor::Hide)?;
        backend.queue(SetAttribute(Attribute::Reset))?;
        let mut current_color = None;

        // TODO: aim for less number casting
        let image_size_in_characters = viewport
            .framebuffer_size
            .map(|s| s as usize)
            .div_element_wise(options.characters.rays_per_character().map(usize::from));

        let mut rect = self.viewport_position;
        // Clamp rect to match the actual image data size, to avoid trying to read the
        // image data out of bounds. (This should be only temporary while resizing.)
        rect.width = rect.width.min(image_size_in_characters.x as u16);
        rect.height = rect.height.min(image_size_in_characters.y as u16);

        for y in 0..rect.height {
            if draw_into_rect {
                backend.queue(MoveTo(rect.x, rect.y + y))?;
            }
            let mut x = 0;
            while x < rect.width {
                let (text, color) = Self::image_patch_to_character(
                    &options,
                    &*image,
                    Vector2::new(x as usize, y as usize),
                    image_size_in_characters,
                );

                let width = write_colored_and_measure(
                    backend,
                    &mut self.dsession.renderer.widths,
                    &mut current_color,
                    color,
                    text,
                    rect.width - x,
                )?;
                x += width.max(1); // max(1) prevents infinite looping in edge case
            }
            if !draw_into_rect {
                // don't colorize the rest of the line
                current_color = None;
                backend.queue(SetAttribute(Attribute::Reset))?;

                backend.write_all(b"\r\n")?;
            }
        }

        // If we don't do this, the other text might become wrongly colored.
        // TODO: Is this actually sufficient if the UI ever has colored parts — that is,
        // are we agreeing adequately with tui's state tracking?
        backend.queue(SetAttribute(Attribute::Reset))?;

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
        if !matches!(
            options.characters,
            CharacterMode::Names | CharacterMode::Shades
        ) {
            // TODO: This is a mess
            let (ref text1, color1) = image[(char_pos.y * 2) * row + char_pos.x];
            let (_, color2) = image[(char_pos.y * 2 + 1) * row + char_pos.x];
            if options.colors == ColorMode::None {
                // Use "black and white" graphic characters — the alternative would be a blank screen.

                let lum1 = color1.unwrap_or(Rgba::TRANSPARENT).luminance();
                let lum2 = color2.unwrap_or(Rgba::TRANSPARENT).luminance();
                // Checkerboard dithering.
                let [threshold1, threshold2] = if char_pos.x % 2 == 0 {
                    [0.5, 0.7]
                } else {
                    [0.7, 0.5]
                };

                let text = match &options.characters {
                    CharacterMode::Names | CharacterMode::Shades => unreachable!(),
                    CharacterMode::Split => match (lum1 > threshold1, lum2 > threshold2) {
                        // Assume that background is black and foreground is white (this could be an option).
                        (true, true) => "█",  // U+2588 FULL BLOCK
                        (true, false) => "▀", // U+2580 UPPER HALF BLOCK
                        (false, true) => "▄", // U+2584 LOWER HALF BLOCK
                        (false, false) if lum2 > 0.2 => text1.as_str(),
                        (false, false) => " ", // U+0020 SPACE
                    },
                    CharacterMode::Shapes => {
                        let avg_lum = ((lum1 + lum2) / 2.0).clamp(0.0, 1.0);

                        if (lum1 - lum2).abs() < 0.05 {
                            [" ", "░", "▒", "▓", "█"][(avg_lum * 4.999) as usize]
                            //[" ", "▏", "▎", "▍", "▌", "▋", "▊", "▉", "█"][(avg_lum * 8.999) as usize]
                        } else if lum1 < lum2 {
                            [" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"]
                                [(avg_lum * 8.999) as usize]
                        } else {
                            [" ", "▔", "▘", "▘", "▀", "▀", "▛", "▛", "█"]
                                [(avg_lum * 8.999) as usize]
                        }
                    }
                };

                (
                    text,
                    Colors {
                        foreground: None,
                        background: None,
                    },
                )
            } else {
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
            }
        } else {
            let (ref text, color) = image[char_pos.y * row + char_pos.x];
            match options.characters {
                CharacterMode::Names => {
                    let mapped_color = match options.colors.convert(color) {
                        Some(color) => Colors::new(Color::Black, color),
                        None => Colors::new(Color::Reset, Color::Reset),
                    };
                    (text, mapped_color)
                }
                CharacterMode::Shades => (
                    [" ", "░", "▒", "▓", "█"]
                        [(color.unwrap_or(Rgba::TRANSPARENT).luminance() * 4.999) as usize],
                    Colors::new(Color::Reset, Color::Reset),
                ),
                CharacterMode::Split | CharacterMode::Shapes => unreachable!(),
            }
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

fn write_colored_and_measure<B: tui::backend::Backend + io::Write>(
    backend: &mut B,
    width_table: &mut HashMap<String, u16>,
    current_color: &mut Option<Colors>,
    wanted_color: Colors,
    text: &str,
    max_width: u16,
) -> Result<u16, io::Error> {
    if *current_color != Some(wanted_color) {
        *current_color = Some(wanted_color);
        backend.queue(SetColors(wanted_color))?;
    }
    write_and_measure(backend, width_table, text, max_width)
}

/// Write a string and report how far this advanced the cursor,
/// unless the string was previously discovered to advance it more than `max_width`.
///
/// `width_table` is used to memoize the previously measured widths. Because of this,
/// strings should be kept short enough to be repetitive (e.g. single characters).
///
/// Returns an error if the string could not be written. If an error was encountered
/// measuring the width, returns an estimate instead.
fn write_and_measure<B: tui::backend::Backend + io::Write>(
    backend: &mut B,
    width_table: &mut HashMap<String, u16>,
    text: &str,
    max_width: u16,
) -> Result<u16, io::Error> {
    if text.len() == 1 && (b' '..=b'~').contains(&text.as_bytes()[0]) {
        // Hardcode the ASCII case.
        if 1 <= max_width {
            backend.write_all(text.as_bytes())?;
        }
        Ok(1)
    } else if let Some(&w) = width_table.get(text) {
        // Use and report already-computed width.
        if w <= max_width {
            backend.write_all(text.as_bytes())?;
        }
        Ok(w)
    } else {
        let before = backend.get_cursor();
        backend.write_all(text.as_bytes())?;
        let after = backend.get_cursor();

        // Compute width from cursor position, if available.
        match (before, after) {
            (Ok((before_x, before_y)), Ok((after_x, after_y))) => {
                if before_y == after_y {
                    match after_x.checked_sub(before_x) {
                        Some(width) => {
                            width_table.insert(text.to_owned(), width);
                            Ok(width)
                        }
                        None => {
                            // Cursor moved leftward. Did we print a backspace, perhaps? Or did we
                            // end up on the last line and provoke scrolling? No way to recover
                            // information from this.
                            // (TODO: Add a text filter that prevents reaching this case by avoiding
                            // printing control characters.)
                            Ok(fallback_measure_str(text))
                        }
                    }
                } else {
                    // The character caused moving to a new line, perhaps because we incorrectly
                    // assumed its width was not greater than 1 and it was on the right edge.
                    Ok(fallback_measure_str(text))
                }
            }
            (_, Err(_)) | (Err(_), _) => {
                // Ignore IO error, which might be a timeout inside get_cursor ... such as due to
                // high response latency due to running this raytracer on a slow system.
                Ok(fallback_measure_str(text))
            }
        }
    }
}

fn fallback_measure_str(text: &str) -> u16 {
    unicode_width::UnicodeWidthStr::width(text)
        .try_into()
        .unwrap_or(u16::MAX)
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
                let [r, g, b, _] = rgba.to_srgb8();

                // Crossterm doesn't have a convenient 216-color table. Use Termion's.
                use termion::color::AnsiValue;
                fn scale(x: u8) -> u8 {
                    ((u16::from(x) * 5) / 255) as u8
                }
                let AnsiValue(byte) = AnsiValue::rgb(scale(r), scale(g), scale(b));

                Some(Color::AnsiValue(byte))
            }
            (Some(rgba), ColorMode::Rgb) => {
                let [r, g, b, _] = rgba.to_srgb8();
                let c = Color::Rgb { r, g, b };
                Some(c)
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
enum CharacterMode {
    /// Show the first characters of the names of the blocks.
    Names,

    /// Use the characters `  ░░▒▒▓▓██` to represent brightnesses.
    Shades,

    /// Use block drawing characters (e.g. “▄”) for double resolution and squarer pixels.
    ///
    /// This may cause artifacts in some terminals, and increases the CPU requirement
    /// since more rays are used per frame.
    Split,

    /// If colors are enabled, equivalent to [`CharacterMode::Split`].
    /// Otherwise, use block drawing characters to approximately represent brightness.
    Shapes,
}

impl CharacterMode {
    fn cycle(self) -> Self {
        use CharacterMode::*;
        match self {
            Names => Shades,
            Shades => Split,
            Split => Shapes,
            Shapes => Names,
        }
    }

    fn rays_per_character(&self) -> Vector2<u8> {
        use CharacterMode::*;
        match self {
            Names | Shades => Vector2::new(1, 1),
            Split | Shapes => Vector2::new(1, 2),
        }
    }
}

/// Output of ColorCharacterBuf
type TextAndColor = (String, Option<Rgba>);

/// Implements `PixelBuf` for colored text output using `termion`.
#[derive(Clone, Debug, Default, PartialEq)]
struct ColorCharacterBuf {
    color: ColorBuf,
    text: CharacterBuf,

    /// Disables normal colorization.
    override_color: bool,
}

impl ColorCharacterBuf {
    fn output(self, camera: &Camera) -> TextAndColor {
        // TODO: override_color should be less clunky
        if self.override_color {
            (self.text.into(), None)
        } else {
            (
                self.text.into(),
                Some(camera.post_process_color(Rgba::from(self.color))),
            )
        }
    }
}

impl PixelBuf for ColorCharacterBuf {
    type BlockData = <CharacterBuf as PixelBuf>::BlockData;

    #[inline]
    fn opaque(&self) -> bool {
        self.color.opaque()
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
        self.text
            .add(Rgba::TRANSPARENT, &CharacterRtData(Cow::Borrowed(" ")));
        self.override_color = true;
    }
}

fn rect_size(rect: Rect) -> Vector2<u16> {
    Vector2::new(rect.width, rect.height)
}

/// An empty style value (which `tui` doesn't provide as a constant)
const STYLE_NONE: Style = Style {
    fg: None,
    bg: None,
    add_modifier: Modifier::empty(),
    sub_modifier: Modifier::empty(),
};

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
