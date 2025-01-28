use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{self, Write as _};
use std::sync::mpsc;

use anyhow::Context as _;
use crossterm::QueueableCommand as _;
use crossterm::cursor::{self, MoveTo};
use crossterm::style::{Attribute, Color, Colors, SetAttribute, SetColors};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;
use ratatui::crossterm;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color as TuiColor, Modifier, Style};
use ratatui::text::Span;
use ratatui::widgets::{Borders, Paragraph};

use all_is_cubes::character::{Character, Cursor};
use all_is_cubes::euclid::Vector2D;
use all_is_cubes::inv;
use all_is_cubes::universe::Handle;
use all_is_cubes::util::{Refmt as _, StatusText};

use crate::terminal::TextRayImage;
use crate::terminal::chars::{image_patch_to_character, write_colored_and_measure};

/// Fills the window slot of [`DesktopSession`](crate::DesktopSession) for terminal sessions.
///
/// Tracks the terminal state and acts as communication channel.
#[derive(Debug)]
pub struct TerminalWindow {
    out_sender: Option<mpsc::SyncSender<OutMsg>>,
    in_receiver: mpsc::Receiver<InMsg>,
    thread: Option<std::thread::JoinHandle<()>>,

    /// Last retrieved size of the terminal.
    viewport_position: Rect,
}

#[derive(Debug)]
pub(super) enum OutMsg {
    BeginFullscreen,
    WriteUiAndFrame(UiFrame),
    WriteFrameOnly(TextRayImage),
    OverrideViewport(Rect),
    Ping(mpsc::SyncSender<()>),
}

#[derive(Debug)]
pub(crate) enum InMsg {
    Viewport(Rect),
}

/// All the information to draw one frame of world and UI.
#[derive(Debug)]
pub(super) struct UiFrame {
    pub(super) frame: TextRayImage,
    pub(super) cursor: Option<Cursor>,
    pub(super) frames_per_second: f64,
    pub(super) terminal_options: super::TerminalOptions,
    pub(super) inventory: InventoryDisplay,
}

/// Inner state of the UI and terminal IO thread.
struct TerminalState {
    tui: MaybeTui,
    in_sender: mpsc::SyncSender<InMsg>,

    /// True if stdin is a terminal and we are allowed to use things that query it.
    has_terminal_stdin: bool,

    /// True if we should clean up on drop.
    reset_terminal_on_drop: bool,

    /// Region of the terminal the scene is drawn into;
    /// updated when `tui` layout runs.
    pub(crate) viewport_position: Rect,

    /// The widths of "single characters" according to the terminal's interpretation
    /// (e.g. emoji might be 2 wide), empirically determined by querying the cursor
    /// position.
    widths: HashMap<String, u16>,
}
enum MaybeTui {
    Tui(Terminal<CrosstermBackend<io::Stdout>>),
    NoTui(CrosstermBackend<io::Stdout>),
}

impl TerminalWindow {
    pub(super) fn new() -> Result<Self, anyhow::Error> {
        let tui = match Terminal::new(CrosstermBackend::new(io::stdout())) {
            Ok(tui) => MaybeTui::Tui(tui),
            Err(io_error) => {
                log::debug!("Terminal::new() failed ({io_error}); falling back to non-TUI output");
                MaybeTui::NoTui(CrosstermBackend::new(io::stdout()))
            }
        };

        let (out_sender, out_receiver) = mpsc::sync_channel(3); // TODO: tune capacity
        let (in_sender, in_receiver) = mpsc::sync_channel(50);

        let state = TerminalState {
            tui,
            in_sender,
            has_terminal_stdin: io::IsTerminal::is_terminal(&io::stdin()),
            viewport_position: Rect::default(),
            reset_terminal_on_drop: true,
            widths: HashMap::new(),
        };

        let thread = std::thread::Builder::new()
            .name("all-is-cubes terminal output".into())
            .spawn(move || terminal_thread_loop(out_receiver, state))
            .context("failed to create terminal output thread")?;

        Ok(TerminalWindow {
            out_sender: Some(out_sender),
            in_receiver,
            thread: Some(thread),
            viewport_position: Rect::default(),
        })
    }

    /// Send an [`OutMsg`] (pertaining to what should be written to stdout) to the thread
    /// that is responsible for that.
    #[track_caller]
    pub(super) fn send(&self, msg: OutMsg) {
        // TODO: handle error — don't return it to the caller here, to keep things write-only,
        // but signal that we should be shutting down.
        match self
            .out_sender
            .as_ref()
            .expect("TerminalWindow already shut down")
            .send(msg)
        {
            Ok(()) => {}
            Err(mpsc::SendError(msg)) => {
                log::trace!("TerminalWindow output thread not listening; message = {msg:?}");
            }
        }
    }

    pub(crate) fn wait_for_sync(&self) {
        let (ping_sender, ping_receiver) = mpsc::sync_channel(1);
        let _ = self
            .out_sender
            .as_ref()
            .unwrap()
            .send(OutMsg::Ping(ping_sender));
        // TODO: error handling
        ping_receiver.recv().unwrap();
    }

    /// Send the cleanup command and wait for the thread to exit.
    #[expect(clippy::unnecessary_wraps)] // see TODO below
    pub(crate) fn stop(mut self) -> Result<(), anyhow::Error> {
        // Drop channel to signal thread to shut down
        self.out_sender = None;
        if let Some(handle) = self.thread.take() {
            // TODO: propagate error properly
            log::trace!("TerminalWindow::stop() blocking on thread join");
            handle.join().unwrap();
        }

        Ok(())
    }

    pub(crate) fn viewport_position(&mut self) -> Rect {
        self.update();
        self.viewport_position
    }

    fn update(&mut self) {
        #[expect(clippy::while_let_loop, reason = "this is clearer about its behavior")]
        loop {
            match self.in_receiver.try_recv() {
                Ok(InMsg::Viewport(v)) => self.viewport_position = v,
                Err(mpsc::TryRecvError::Disconnected | mpsc::TryRecvError::Empty) => break,
            }
        }
    }
}

impl Drop for TerminalWindow {
    fn drop(&mut self) {
        // Drop channel to signal thread to shut down
        self.out_sender = None;
        if let Some(handle) = self.thread.take() {
            log::trace!("TerminalWindow::drop() blocking on thread join");
            let _ = handle.join();
        }
    }
}

impl crate::glue::Window for TerminalWindow {
    fn set_title(&self, _: String) {
        // TODO: add setting title
    }
}

impl MaybeTui {
    fn backend_mut(&mut self) -> &mut CrosstermBackend<io::Stdout> {
        match self {
            MaybeTui::Tui(terminal) => terminal.backend_mut(),
            MaybeTui::NoTui(backend) => backend,
        }
    }
}

fn terminal_thread_loop(output_channel: mpsc::Receiver<OutMsg>, mut state: TerminalState) {
    log::trace!("entering terminal_thread_loop()");
    for command in output_channel {
        // TODO: handle IO errors
        log::trace!("received command {command:?}");
        match command {
            OutMsg::BeginFullscreen => state.begin_fullscreen().unwrap(),
            OutMsg::WriteUiAndFrame(ui_frame) => {
                state.write_ui(&ui_frame).unwrap();
                state.write_frame(&ui_frame.frame, true).unwrap();
            }
            OutMsg::WriteFrameOnly(frame) => state.write_frame(&frame, false).unwrap(),
            OutMsg::OverrideViewport(rect) => {
                // TODO: this should permanently override and not be able to be overwritten on resize
                state.viewport_position = rect;
                let _ = state.in_sender.send(InMsg::Viewport(rect));
            }
            OutMsg::Ping(sender) => {
                let _ = sender.send(());
            }
        }
    }
    state.clean_up_terminal();
    log::trace!("exiting terminal_thread_loop()");
}

impl TerminalState {
    fn begin_fullscreen(&mut self) -> anyhow::Result<()> {
        match &mut self.tui {
            MaybeTui::Tui(terminal) => {
                self.reset_terminal_on_drop = true;
                if self.has_terminal_stdin {
                    crossterm::terminal::enable_raw_mode()?;
                }
                terminal
                    .backend_mut()
                    .queue(crossterm::event::EnableMouseCapture)?;
                terminal.clear()?;
                Ok(())
            }
            MaybeTui::NoTui(_) => Err(anyhow::anyhow!("cannot fullscreen without terminal")),
        }
    }

    /// Reset terminal state, as before exiting.
    fn clean_up_terminal(&mut self) {
        if self.reset_terminal_on_drop {
            fn log_if_fails<T, E: std::error::Error>(r: Result<T, E>) {
                match r {
                    Ok(_) => {}
                    Err(e) => {
                        log::error!("error detected while cleaning up terminal state: {e}");
                    }
                }
            }

            let out = self.tui.backend_mut();
            log_if_fails(out.queue(SetAttribute(Attribute::Reset)));
            log_if_fails(out.queue(SetColors(Colors::new(Color::Reset, Color::Reset))));
            log_if_fails(out.queue(cursor::Show));
            log_if_fails(out.queue(crossterm::event::DisableMouseCapture));
            log_if_fails(crossterm::terminal::disable_raw_mode());
            self.reset_terminal_on_drop = false;
        }
    }

    /// Actually write image data to the terminal. Does not write UI.
    ///
    /// If `draw_into_rect` is true, moves the cursor to fit into `self.viewport_position`.
    /// If it is false, does not affect the cursor position and uses newlines.
    fn write_frame(&mut self, image: &TextRayImage, draw_into_rect: bool) -> io::Result<()> {
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
        let image_size_in_characters = image
            .viewport
            .framebuffer_size
            .to_usize()
            .to_vector()
            .component_div(image.options.characters.rays_per_character().to_usize());

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
                let (text, color) = image_patch_to_character(image, Vector2D::new(x, y).to_usize());

                let width = write_colored_and_measure(
                    backend,
                    self.has_terminal_stdin,
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

    /// Lay out and write the UI using [`tui`] -- everything on screen *except* for
    /// the raytraced scene.
    ///
    /// This function also stores the current scene viewport for future frames.
    fn write_ui(&mut self, ui_frame: &UiFrame) -> anyhow::Result<()> {
        let terminal = match &mut self.tui {
            MaybeTui::Tui(terminal) => terminal,
            MaybeTui::NoTui(_) => {
                return Err(anyhow::anyhow!(
                    "cannot draw terminal UI when not attached to a terminal"
                ));
            }
        };
        let TextRayImage { info, .. } = ui_frame.frame;
        let mut viewport_rect = None;
        terminal.draw(|f| {
            const HELP_TEXT: &str = "\
                Move: WS AD EC  Turn: ←→ ↑↓\n\
                Term color: N   Term chars: M\n\
                Quit: ^C, or ^D";

            let [
                viewport_rect_tmp,
                toolbar_rect,
                gfx_info_rect,
                cursor_and_help_rect,
            ] = *Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Min(1),
                    Constraint::Length(3),
                    Constraint::Length(2),
                    Constraint::Length(3),
                ])
                .split(f.area())
            else {
                unreachable!()
            };
            let [cursor_rect, help_rect] = *Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Min(0), Constraint::Length(30)])
                .split(cursor_and_help_rect)
            else {
                unreachable!()
            };
            // Toolbar
            {
                const SLOTS: usize = InventoryDisplay::SLOTS;

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

                let selected_slots = ui_frame.inventory.selected_slots;
                for ((&rect, i), slot) in slot_rects.iter().zip(0..).zip(&ui_frame.inventory.slots)
                {
                    let slot_info: Vec<Span<'_>> = vec![
                        if selected_slots[0] == i {
                            SELECTED_0
                        } else {
                            SELECTED_BLANK
                        },
                        Span::from(format!(" {i} ")),
                        if selected_slots[1] == i {
                            SELECTED_1
                        } else {
                            SELECTED_BLANK
                        },
                    ];
                    let block = ratatui::widgets::Block::default()
                        .title(slot_info)
                        .borders(Borders::ALL);
                    f.render_widget(
                        match slot {
                            // TODO: Use item icon text -- we need a way to access the predefined icons from here
                            inv::Slot::Empty => Paragraph::new(""),
                            inv::Slot::Stack(count, item) if count.get() == 1 => {
                                Paragraph::new(format!("{item:?}"))
                            }
                            inv::Slot::Stack(count, item) => {
                                Paragraph::new(format!("{count} × {item:?}"))
                            }

                            // Fallback
                            slot => Paragraph::new(format!("{slot:?}")),
                        }
                        .block(block),
                        rect,
                    );
                }
            }

            viewport_rect = Some(viewport_rect_tmp);

            // Graphics info line
            {
                let [frame_info_rect, colors_info_rect, render_info_rect] = *Layout::default()
                    .direction(Direction::Horizontal)
                    .constraints([
                        Constraint::Percentage(30),
                        Constraint::Percentage(30),
                        Constraint::Percentage(30),
                    ])
                    .split(gfx_info_rect)
                else {
                    unreachable!()
                };

                f.render_widget(
                    Paragraph::new(format!("{:5.1} FPS", ui_frame.frames_per_second)),
                    frame_info_rect,
                );

                f.render_widget(
                    Paragraph::new(format!(
                        "Colors: {:?}\nChars: {:?}",
                        ui_frame.terminal_options.colors, ui_frame.terminal_options.characters
                    )),
                    colors_info_rect,
                );

                f.render_widget(
                    Paragraph::new(format!("{}", info.refmt(&StatusText::ALL))),
                    render_info_rect,
                );
            }

            // Cursor info
            f.render_widget(
                if let Some(cursor) = &ui_frame.cursor {
                    // TODO: design good formatting for cursor data
                    Paragraph::new(format!(
                        "{:?} {:?} : {}",
                        cursor.cube(),
                        cursor.face_selected(),
                        cursor.hit().evaluated.attributes().display_name
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
            let _ = self.in_sender.send(InMsg::Viewport(viewport_rect));
        }

        Ok(())
    }
}

/// Snapshot of a [`Character`] inventory for the UI.
#[derive(Clone, Debug)]
pub(super) struct InventoryDisplay {
    slots: [inv::Slot; Self::SLOTS],
    selected_slots: [inv::Ix; 3],
}

impl InventoryDisplay {
    const SLOTS: usize = 10; // TODO: link with other UI and gameplay code

    pub fn new(character_handle: Option<Handle<Character>>) -> Self {
        // TODO: reimplement character inventory viewing in a safely asynchronous way
        if let Some(character_handle) = character_handle {
            if let Ok(character) = character_handle.read() {
                let slots = character.inventory().slots();

                return Self {
                    slots: std::array::from_fn(|i| {
                        slots.get(i).unwrap_or(&inv::Slot::Empty).clone()
                    }),
                    selected_slots: character.selected_slots(),
                };
            }
        }

        Self {
            slots: std::array::from_fn(|_| inv::Slot::Empty),
            selected_slots: [0; 3],
        }
    }
}

/// An empty style value (which `tui` doesn't provide as a constant)
const STYLE_NONE: Style = Style {
    fg: None,
    bg: None,
    add_modifier: Modifier::empty(),
    sub_modifier: Modifier::empty(),
};
