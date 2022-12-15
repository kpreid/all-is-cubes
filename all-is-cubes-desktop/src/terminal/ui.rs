use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::{self, Write as _};

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
use all_is_cubes::inv::Slot;
use all_is_cubes::util::{CustomFormat, StatusText};

use crate::session::DesktopSession;
use crate::terminal::chars::{image_patch_to_character, write_colored_and_measure};
use crate::terminal::{sync_viewport, TerminalRenderer, TextRayImage};

/// Fills the window slot of [`DesktopSession`].
/// Tracks the terminal state and acts as communication channel.
pub(crate) struct TerminalWindow {
    tui: tui::Terminal<CrosstermBackend<io::Stdout>>,

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
    pub fn new() -> crossterm::Result<Self> {
        Ok(TerminalWindow {
            tui: Terminal::new(CrosstermBackend::new(io::stdout()))?,
            viewport_position: Rect::default(),
            terminal_state_dirty: true,
            widths: HashMap::new(),
        })
    }

    pub(super) fn begin_fullscreen(&mut self) -> crossterm::Result<()> {
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
    pub(super) fn clean_up_terminal(&mut self) -> crossterm::Result<()> {
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
    pub(super) fn write_frame(
        &mut self,
        image: TextRayImage,
        draw_into_rect: bool,
    ) -> crossterm::Result<()> {
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
                    image_patch_to_character(&image, Vector2::new(x as usize, y as usize));

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

/// Lay out and write the UI using [`tui`] -- everything on screen *except* for
/// the raytraced scene.
///
/// This function also stores the current scene viewport for future frames.
pub(super) fn write_ui(
    dsession: &mut DesktopSession<TerminalRenderer, TerminalWindow>,
    frame: &TextRayImage,
) -> crossterm::Result<()> {
    let TextRayImage { info, .. } = frame;
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

/// An empty style value (which `tui` doesn't provide as a constant)
const STYLE_NONE: Style = Style {
    fg: None,
    bg: None,
    add_modifier: Modifier::empty(),
    sub_modifier: Modifier::empty(),
};
