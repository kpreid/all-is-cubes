// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::{self, Write as _};
use std::sync::mpsc::{self, TrySendError};
use std::time::Duration;

use all_is_cubes::util::{CustomFormat, StatusText};
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
use all_is_cubes::cgmath::{ElementWise as _, Point2, Vector2};
use all_is_cubes::inv::Slot;
use all_is_cubes::listen::{ListenableCell, ListenableSource};
use all_is_cubes::math::Rgba;
use all_is_cubes::raytracer::{
    CharacterBuf, CharacterRtData, ColorBuf, PixelBuf, RaytraceInfo, RtRenderer,
};

use crate::glue::crossterm::{event_to_key, map_mouse_button};
use crate::session::{ClockSource, DesktopSession};

mod options;
pub(crate) use options::TerminalOptions;
mod output;
use output::{image_patch_to_character, write_colored_and_measure};

/// Print the scene to stdout and return, instead of starting any interaction.
///
/// TODO: This shouldn't need to take ownership of the `session`; it does so because it is
/// built on the same components as [`terminal_main_loop`].
pub(crate) fn terminal_print_once(
    mut dsession: DesktopSession<TerminalRenderer, TerminalWindow>,
    display_size: Vector2<u16>,
) -> Result<(), anyhow::Error> {
    dsession.window.viewport_position = Rect::new(0, 0, display_size.x, display_size.y);
    sync_viewport(&mut dsession);

    dsession
        .renderer
        .send_frame_to_render(&mut dsession.session);
    let frame = dsession
        .renderer
        .render_pipe_out
        .recv()
        .expect("Internal error in rendering");
    dsession.window.write_frame(frame, false)?;

    dsession.window.clean_up_terminal()?; // note this is _also_ run on drop
    Ok(())
}

/// Fills the window slot of [`DesktopSession`].
/// Tracks the terminal state and acts as communication channel.
pub(crate) struct TerminalWindow {
    tui: tui::Terminal<CrosstermBackend<io::Stdout>>,

    /// True if we should clean up on drop.
    terminal_state_dirty: bool,

    /// Region of the terminal the scene is drawn into;
    /// updated when `tui` layout runs.
    viewport_position: Rect,

    /// The widths of "single characters" according to the terminal's interpretation
    /// (e.g. emoji might be 2 wide), empirically determined by querying the cursor
    /// position.
    widths: HashMap<String, u16>,
}

/// Fills the renderer slot of [`DesktopSession`].
///
/// TODO: Refactor this 'till we can merge it with the record mode
/// raytracing.
pub(crate) struct TerminalRenderer {
    /// Redundant with the RtRenderer's cameras, but is a copy that
    /// isn't hopping around threads.
    cameras: StandardCameras,

    /// Options pertaining to how to draw.
    ///
    /// TODO: Unclear whether this belongs in Renderer or Window
    options: TerminalOptions,

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

pub(crate) fn create_terminal_session(
    session: Session,
    options: TerminalOptions,
) -> crossterm::Result<DesktopSession<TerminalRenderer, TerminalWindow>> {
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
                    let viewport = scene.modified_viewport();
                    let mut image = vec![(String::new(), None); viewport.pixel_count().unwrap()];
                    let info = scene.draw::<ColorCharacterBuf, _, _, _>(
                        |_| String::new(),
                        |b| b.output(camera),
                        &mut image,
                    );
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

    Ok(DesktopSession {
        session,
        renderer: TerminalRenderer {
            cameras,
            options,
            buffer_reuse_out,
            render_pipe_in,
            render_pipe_out,
        },
        window: TerminalWindow {
            tui: Terminal::new(CrosstermBackend::new(io::stdout()))?,
            viewport_position,
            terminal_state_dirty: true,
            widths: HashMap::new(),
        },
        viewport_cell,
        clock_source: ClockSource::Instant,
        recorder: None,
    })
}

/// Run the simulation and interactive UI. Returns after user's quit command.
pub(crate) fn terminal_main_loop(
    mut dsession: DesktopSession<TerminalRenderer, TerminalWindow>,
) -> Result<(), anyhow::Error> {
    run(&mut dsession)?;
    dsession.window.clean_up_terminal()?; // note this is _also_ run on drop
    Ok(())
}

/// Run the simulation and interactive UI. Returns after user's quit command.
/// Caller is responsible for `clean_up_terminal()`.
fn run(dsession: &mut DesktopSession<TerminalRenderer, TerminalWindow>) -> crossterm::Result<()> {
    dsession.window.begin_fullscreen()?;

    loop {
        'input: while crossterm::event::poll(Duration::ZERO)? {
            let event = crossterm::event::read()?;
            if let Some(aic_event) = event_to_key(&event) {
                if dsession.session.input_processor.key_momentary(aic_event) {
                    // Handled by input_processor
                    continue 'input;
                }
            }
            let options = &mut dsession.renderer.options;
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
                }) => options.colors = options.colors.cycle(),
                Event::Key(KeyEvent {
                    code: KeyCode::Char('m'),
                    modifiers: _,
                }) => {
                    options.characters = options.characters.cycle();
                    sync_viewport(dsession);
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
                    dsession.session.input_processor.mouse_pixel_position(
                        *dsession.viewport_cell.get(),
                        Some(position),
                        true,
                    );
                    match kind {
                        MouseEventKind::Down(button) => {
                            dsession.session.click(map_mouse_button(button));
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

        dsession.advance_time_and_maybe_step();

        match dsession.renderer.render_pipe_out.try_recv() {
            Ok(frame) => {
                write_ui(dsession, &frame)?;
                dsession.window.write_frame(frame, true)?;
            }
            // TODO: Even if we don't have a frame, we might want to update the UI anyway.
            Err(mpsc::TryRecvError::Empty) => {}
            Err(mpsc::TryRecvError::Disconnected) => panic!("render thread died"),
        }

        if dsession.session.frame_clock.should_draw() {
            // TODO: this is the only reason .cameras exists
            let c = &mut dsession.renderer.cameras;
            c.update();
            dsession.session.update_cursor(&*c);

            dsession
                .renderer
                .send_frame_to_render(&mut dsession.session);
        } else {
            // TODO: sleep instead of spinning.
            std::thread::yield_now();
        }
    }
}

impl TerminalRenderer {
    /// Read the latest data from the session (mut be equal to previously provided one)
    /// and send it off to the raytracing threads.
    fn send_frame_to_render(&mut self, session: &mut Session) {
        // Fetch and update one of our recirculating renderers.
        let mut renderer = self.buffer_reuse_out.recv().unwrap();
        renderer.update(session.cursor_result()).unwrap();

        match self.render_pipe_in.try_send(FrameInput {
            options: self.options.clone(),
            scene: renderer,
        }) {
            Ok(()) => {
                session.frame_clock.did_draw();
            }
            Err(TrySendError::Disconnected(_)) => {
                // Ignore send errors as they should be detected on the receive side
            }
            Err(TrySendError::Full(_)) => {
                // Skip this frame
            }
        }
    }
}

impl TerminalWindow {
    fn begin_fullscreen(&mut self) -> crossterm::Result<()> {
        self.terminal_state_dirty = true;
        crossterm::terminal::enable_raw_mode()?;
        self.tui
            .backend_mut()
            .queue(crossterm::event::EnableMouseCapture)?;
        self.tui.clear()?;
        Ok(())
    }

    /// Reset terminal state, as before exiting.
    /// This will also be run on drop but errors will not be reported in that case.
    fn clean_up_terminal(&mut self) -> crossterm::Result<()> {
        let out = self.tui.backend_mut();
        out.queue(SetAttribute(Attribute::Reset))?;
        out.queue(SetColors(Colors::new(Color::Reset, Color::Reset)))?;
        out.queue(cursor::Show)?;
        out.queue(crossterm::event::DisableMouseCapture)?;
        crossterm::terminal::disable_raw_mode()?;
        self.terminal_state_dirty = false;
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

        let backend = self.tui.backend_mut();
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
                let (text, color) = image_patch_to_character(
                    &options,
                    &*image,
                    Vector2::new(x as usize, y as usize),
                    image_size_in_characters,
                );

                let width = write_colored_and_measure(
                    backend,
                    &mut self.widths,
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
}

impl Drop for TerminalWindow {
    fn drop(&mut self) {
        if self.terminal_state_dirty {
            let _ = self.clean_up_terminal();
        }
    }
}

/// Copy session's current viewport state to the viewport cell.
fn sync_viewport(dsession: &mut DesktopSession<TerminalRenderer, TerminalWindow>) {
    dsession.viewport_cell.set(
        dsession
            .renderer
            .options
            .viewport_from_terminal_size(rect_size(dsession.window.viewport_position)),
    );
}

/// Lay out and write the UI using [`tui`] -- everything on screen *except* for
/// the raytraced scene.
///
/// This function also stores the current scene viewport for future frames.
fn write_ui(
    dsession: &mut DesktopSession<TerminalRenderer, TerminalWindow>,
    frame: &FrameOutput,
) -> crossterm::Result<()> {
    let FrameOutput { info, .. } = frame;
    let mut viewport_rect = None;
    dsession.window.tui.draw(|f| {
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

            if let Some(character_ref) = dsession.session.character().snapshot() {
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
                    dsession.session.draw_fps_counter().frames_per_second()
                )),
                frame_info_rect,
            );

            f.render_widget(
                Paragraph::new(format!(
                    "Colors: {:?}\nChars: {:?}",
                    dsession.renderer.options.colors, dsession.renderer.options.characters
                )),
                colors_info_rect,
            );

            f.render_widget(
                Paragraph::new(format!("{}", info.custom_format(StatusText))),
                render_info_rect,
            );
        }

        // Cursor info
        f.render_widget(
            if let Some(cursor) = dsession.session.cursor_result() {
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
    if dsession.window.viewport_position != viewport_rect {
        dsession.window.viewport_position = viewport_rect;
        sync_viewport(dsession);
    }

    Ok(())
}

/// Output of ColorCharacterBuf
type TextAndColor = (String, Option<Rgba>);

/// Implements `PixelBuf` for colored text output.
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
