//! Rendering as terminal text. Why not? Turn cubes into rectangles.

use std::borrow::Cow;
use std::sync::mpsc::{self, TrySendError};
use std::time::Duration;

use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent, MouseEventKind};

use all_is_cubes::apps::{Session, StandardCameras};
use all_is_cubes::camera::{Camera, Viewport};
use all_is_cubes::cgmath::{Point2, Vector2};
use all_is_cubes::listen::{ListenableCell, ListenableSource};
use all_is_cubes::math::Rgba;
use all_is_cubes::raytracer::{
    CharacterBuf, CharacterRtData, ColorBuf, PixelBuf, RaytraceInfo, RtRenderer,
};
use tui::layout::Rect;

use crate::glue::crossterm::{event_to_key, map_mouse_button};
use crate::session::{ClockSource, DesktopSession};

mod chars;
mod options;
pub(crate) use options::TerminalOptions;
mod ui;
use ui::{write_ui, TerminalWindow};

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
    viewport_cell: ListenableCell<Viewport>,
) -> crossterm::Result<DesktopSession<TerminalRenderer, TerminalWindow>> {
    viewport_cell.set(options.viewport_from_terminal_size(rect_size(Rect::default())));
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
        window: TerminalWindow::new()?,
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

/// Copy session's current viewport state to the viewport cell.
fn sync_viewport(dsession: &mut DesktopSession<TerminalRenderer, TerminalWindow>) {
    dsession.viewport_cell.set(
        dsession
            .renderer
            .options
            .viewport_from_terminal_size(rect_size(dsession.window.viewport_position)),
    );
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
