use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::{self, Write as _};
use std::sync::mpsc;

use crossterm::cursor::{self, MoveTo};
use crossterm::style::{Attribute, Color, Colors, SetAttribute, SetColors};
use crossterm::QueueableCommand as _;
use tui::backend::CrosstermBackend;
use tui::layout::{Constraint, Direction, Layout, Rect};
use tui::style::{Color as TuiColor, Modifier, Style};
use tui::text::{Span, Spans};
use tui::widgets::{Borders, Paragraph};
use tui::Terminal;

use all_is_cubes::cgmath::{ElementWise as _, Vector2};
use all_is_cubes::character::{Character, Cursor};
use all_is_cubes::inv::Slot;
use all_is_cubes::universe::URef;
use all_is_cubes::util::{CustomFormat, StatusText};

use crate::terminal::chars::{image_patch_to_character, write_colored_and_measure};
use crate::terminal::TextRayImage;

/// Fills the window slot of [`DesktopSession`].
/// Tracks the terminal state and acts as communication channel.
#[derive(Debug)]
pub(crate) struct TerminalWindow {
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
    tui: tui::Terminal<CrosstermBackend<io::Stdout>>,
    in_sender: mpsc::SyncSender<InMsg>,

    /// True if we should clean up on drop.
    terminal_state_dirty: bool,

    /// Region of the terminal the scene is drawn into;
    /// updated when `tui` layout runs.
    pub(crate) viewport_position: Rect,

    /// The widths of "single characters" according to the terminal's interpretation
    /// (e.g. emoji might be 2 wide), empirically determined by querying the cursor
    /// position.
    widths: HashMap<String, u16>,
}

impl TerminalWindow {
    pub(super) fn new() -> crossterm::Result<Self> {
        let (out_sender, out_receiver) = mpsc::sync_channel(3); // TODO: tune capacity
        let (in_sender, in_receiver) = mpsc::sync_channel(50);
        let state = TerminalState {
            tui: Terminal::new(CrosstermBackend::new(io::stdout()))?,
            in_sender,
            viewport_position: Rect::default(),
            terminal_state_dirty: true,
            widths: HashMap::new(),
        };
        let thread = std::thread::Builder::new()
            .name("all-is-cubes terminal IO".into())
            .spawn(move || terminal_thread_loop(out_receiver, state))
            .unwrap();
        Ok(TerminalWindow {
            out_sender: Some(out_sender),
            in_receiver,
            thread: Some(thread),
            viewport_position: Rect::default(),
        })
    }

    pub(super) fn send(&self, msg: OutMsg) {
        // TODO: handle error — don't return it to the caller here, to keep things write-only,
        // but signal that we should be shutting down.
        self.out_sender
            .as_ref()
            .expect("TerminalWindow already shut down")
            .send(msg)
            .unwrap();
    }

    pub fn wait_for_sync(&self) {
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
    pub fn stop(mut self) -> Result<(), anyhow::Error> {
        // Drop channel to signal thread to shut down
        self.out_sender = None;
        if let Some(handle) = self.thread.take() {
            // TODO: propagate error properly
            log::trace!("TerminalWindow::stop() blocking on thread join");
            handle.join().unwrap();
        }

        Ok(())
    }

    pub fn viewport_position(&mut self) -> Rect {
        self.update();
        self.viewport_position
    }

    fn update(&mut self) {
        #[allow(clippy::while_let_loop)] // this is clearer about its behavior
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
    state.clean_up_terminal().unwrap();
    log::trace!("exiting terminal_thread_loop()");
}

impl TerminalState {
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
        if self.terminal_state_dirty {
            let out = self.tui.backend_mut();
            out.queue(SetAttribute(Attribute::Reset))?;
            out.queue(SetColors(Colors::new(Color::Reset, Color::Reset)))?;
            out.queue(cursor::Show)?;
            out.queue(crossterm::event::DisableMouseCapture)?;
            crossterm::terminal::disable_raw_mode()?;
            self.terminal_state_dirty = false;
        }
        Ok(())
    }

    /// Actually write image data to the terminal. Does not write UI.
    ///
    /// If `draw_into_rect` is true, moves the cursor to fit into `self.viewport_position`.
    /// If it is false, does not affect the cursor position and uses newlines.
    fn write_frame(&mut self, image: &TextRayImage, draw_into_rect: bool) -> crossterm::Result<()> {
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
            .map(|s| s as usize)
            .div_element_wise(
                image
                    .options
                    .characters
                    .rays_per_character()
                    .map(usize::from),
            );

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
                let (text, color) =
                    image_patch_to_character(image, Vector2::new(x as usize, y as usize));

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

    /// Lay out and write the UI using [`tui`] -- everything on screen *except* for
    /// the raytraced scene.
    ///
    /// This function also stores the current scene viewport for future frames.
    fn write_ui(&mut self, ui_frame: &UiFrame) -> crossterm::Result<()> {
        let TextRayImage { info, .. } = ui_frame.frame;
        let mut viewport_rect = None;
        self.tui.draw(|f| {
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
                for ((i, rect), slot) in slot_rects
                    .into_iter()
                    .enumerate()
                    .zip(&ui_frame.inventory.slots)
                {
                    let slot_info = Spans::from(vec![
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
                    ]);
                    let block = tui::widgets::Block::default()
                        .title(slot_info)
                        .borders(Borders::ALL);
                    f.render_widget(
                        match slot {
                            // TODO: Use item icon text -- we need a way to access the predefined icons from here
                            Slot::Empty => Paragraph::new(""),
                            Slot::Stack(count, item) if count.get() == 1 => {
                                Paragraph::new(format!("{item:?}"))
                            }
                            Slot::Stack(count, item) => {
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
                    Paragraph::new(format!("{}", info.custom_format(StatusText))),
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
                        cursor.hit().evaluated.attributes.display_name
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
    slots: [Slot; Self::SLOTS],
    selected_slots: [usize; 3],
}

impl InventoryDisplay {
    const SLOTS: usize = 10; // TODO: link with other UI and gameplay code

    pub fn new(character_ref: Option<URef<Character>>) -> Self {
        // TODO: reimplement character inventory viewing in a safely asynchronous way
        if let Some(character_ref) = character_ref {
            if let Ok(character) = character_ref.read() {
                let slots = &character.inventory().slots;

                return Self {
                    slots: std::array::from_fn(|i| slots.get(i).unwrap_or(&Slot::Empty).clone()),
                    selected_slots: character.selected_slots(),
                };
            }
        }

        Self {
            slots: std::array::from_fn(|_| Slot::Empty),
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
