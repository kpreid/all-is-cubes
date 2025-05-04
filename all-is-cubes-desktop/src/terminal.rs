//! Rendering as styled terminal text.

#![expect(clippy::module_name_repetitions)] // TODO: consider renaming relevant items

use std::sync::Arc;
use std::sync::mpsc::{self, TrySendError};
use std::time::{Duration, Instant};

use anyhow::Context;
use crossterm::event::{Event, KeyCode, KeyEvent, KeyModifiers, MouseEvent, MouseEventKind};
use ratatui::crossterm;
use ratatui::layout::Rect;

use all_is_cubes::arcstr::literal_substr;
use all_is_cubes::euclid::{Point2D, Size2D};
use all_is_cubes::listen;
use all_is_cubes::math::Rgba;
use all_is_cubes_render::camera::{self, Camera, StandardCameras, Viewport};
use all_is_cubes_render::raytracer::{
    Accumulate, CharacterBuf, CharacterRtData, ColorBuf, RtRenderer,
};

use crate::Session;
use crate::glue::crossterm::{event_to_key, map_mouse_button};
use crate::session::DesktopSession;

mod chars;
mod options;
pub use options::TerminalOptions;
mod ray_image;
use ray_image::TextRayImage;
mod ui;
pub use ui::TerminalWindow;
use ui::{InventoryDisplay, OutMsg, UiFrame};

/// Print the scene to stdout and return, instead of starting any interaction.
///
/// TODO: This shouldn't need to take ownership of the `session`; it does so because it is
/// built on the same components as [`terminal_main_loop`].
pub fn terminal_print_once(
    mut dsession: DesktopSession<TerminalRenderer, TerminalWindow>,
    display_size: Size2D<u16, camera::ImagePixel>,
) -> Result<DesktopSession<(), ()>, anyhow::Error> {
    let rect = Rect::new(0, 0, display_size.width, display_size.height);
    dsession.window.send(OutMsg::OverrideViewport(rect));
    dsession.window.wait_for_sync();
    sync_viewport(&mut dsession);

    dsession
        .renderer
        .send_frame_to_render(&mut dsession.session);
    let frame = dsession
        .renderer
        .render_pipe_out
        .recv()
        .expect("Internal error in rendering");
    dsession.window.send(OutMsg::WriteFrameOnly(frame));

    let (dsession, _, window) = dsession.into_renderer_and_window();

    window
        .stop()
        .context("failed to stop TerminalWindow after printing")?;
    Ok(dsession)
}

/// Fills the renderer slot of [`DesktopSession`] for terminal sessions.
#[derive(Debug)]
pub struct TerminalRenderer {
    /// Redundant with the [`RtRenderer`]'s cameras, but is a copy that
    /// isn't hopping around threads.
    cameras: StandardCameras,

    /// Options pertaining to how to draw.
    ///
    /// TODO: Unclear whether this belongs in `Renderer` or `Window`
    options: TerminalOptions,

    buffer_reuse_out: mpsc::Receiver<RtRenderer<CharacterRtData>>,
    render_pipe_in: mpsc::SyncSender<FrameInput>,
    render_pipe_out: mpsc::Receiver<TextRayImage>,
}

impl crate::glue::Renderer for TerminalRenderer {}

struct FrameInput {
    options: TerminalOptions,
    scene: RtRenderer<CharacterRtData>,
}

/// Creates a [`DesktopSession`] which can be used with [`terminal_main_loop`].
pub fn create_terminal_session(
    executor: Arc<crate::Executor>,
    session: Session,
    options: TerminalOptions,
    viewport_cell: listen::Cell<Viewport>,
) -> Result<DesktopSession<TerminalRenderer, TerminalWindow>, anyhow::Error> {
    viewport_cell.set(options.viewport_from_terminal_size(rect_size(Rect::default())));
    let cameras = session.create_cameras(viewport_cell.as_source());

    // Generate reusable buffers for scene.
    // They are recirculated through the channels so that one can be updated while another is being raytraced.
    let number_of_scene_buffers = 3;
    let (buffer_reuse_in, buffer_reuse_out) =
        mpsc::sync_channel::<RtRenderer<CharacterRtData>>(number_of_scene_buffers);
    for _ in 0..number_of_scene_buffers {
        buffer_reuse_in
            .send(RtRenderer::new(
                cameras.clone_unupdated(),
                Box::new(|v| v),
                listen::constant(Arc::new(())),
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
            #[expect(clippy::shadow_unrelated)]
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
                    let _ = render_thread_out.send(TextRayImage {
                        viewport,
                        options,
                        image,
                        info,
                    });
                    let _ = buffer_reuse_in.send(scene);
                }
            }
        })
        .context("failed to create terminal raytracer thread")?;

    Ok(DesktopSession::new(
        executor,
        TerminalRenderer {
            cameras,
            options,
            buffer_reuse_out,
            render_pipe_in,
            render_pipe_out,
        },
        TerminalWindow::new().context("failed to create TerminalWindow")?,
        session,
        viewport_cell,
        false, // tty might be remote, so no gamepad input
    ))
}

/// Run the simulation and interactive UI. Returns after user's quit command.
pub fn terminal_main_loop(
    mut dsession: DesktopSession<TerminalRenderer, TerminalWindow>,
    mut universe_ready_rx: tokio::sync::oneshot::Receiver<Result<(), anyhow::Error>>,
) -> Result<(), anyhow::Error> {
    // Wait for the universe to be finished before starting the terminal UI, to avoid conflict
    // with the indicatif progress bars.
    // TODO: Instead of this, use ratatui progress bar widgets when in terminal mode!
    while let Err(tokio::sync::oneshot::error::TryRecvError::Empty) = universe_ready_rx.try_recv() {
        // TODO: this semi-spin-loop is bad; we should be able to just ask the session to poll its
        // main task with appropriate Waker usage, instead.
        std::thread::yield_now();

        dsession.advance_time_and_maybe_step();
    }

    run(&mut dsession)?;
    dsession
        .window
        .stop()
        .context("failed to stop TerminalWindow after main loop")?;
    Ok(())
}

/// Run the simulation and interactive UI. Returns after user's quit command.
/// Caller is responsible for `clean_up_terminal()`.
fn run(
    dsession: &mut DesktopSession<TerminalRenderer, TerminalWindow>,
) -> Result<(), anyhow::Error> {
    dsession.window.send(OutMsg::BeginFullscreen);

    loop {
        'input: while crossterm::event::poll(Duration::ZERO)
            .context("crossterm input poll() failed")?
        {
            let event = crossterm::event::read().context("crossterm input read() failed")?;
            if let Some(aic_event) = event_to_key(&event) {
                if dsession.session.input_processor.key_momentary(aic_event) {
                    // Handled by input_processor
                    continue 'input;
                }
            }
            let options = &mut dsession.renderer.options;
            match event {
                Event::Key(KeyEvent {
                    code: KeyCode::Char('c' | 'd'),
                    modifiers: KeyModifiers::CONTROL,
                    ..
                }) => {
                    return Ok(());
                }
                Event::Key(KeyEvent {
                    code: KeyCode::Char('n'),
                    modifiers: _,
                    ..
                }) => options.colors = options.colors.cycle(),
                Event::Key(KeyEvent {
                    code: KeyCode::Char('m'),
                    modifiers: _,
                    ..
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
                        Point2D::new((f64::from(column) - 0.5) * 0.5, f64::from(row) - 0.5);
                    dsession.session.input_processor.mouse_pixel_position(
                        dsession.viewport_cell.get(),
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
                        | MouseEventKind::ScrollLeft
                        | MouseEventKind::ScrollRight
                        | MouseEventKind::ScrollDown
                        | MouseEventKind::ScrollUp => {}
                    }
                }
                Event::FocusGained | Event::FocusLost => {}
                Event::Paste(_) => {}
            }
        }

        dsession.advance_time_and_maybe_step();
        sync_viewport(dsession); // TODO: do this on a notification

        match dsession.renderer.render_pipe_out.try_recv() {
            Ok(frame) => dsession.window.send(OutMsg::WriteUiAndFrame(UiFrame {
                frame,
                cursor: dsession.session.cursor_result().cloned(),
                frames_per_second: dsession.session.draw_fps_counter().frames_per_second(),
                terminal_options: dsession.renderer.options.clone(),
                inventory: InventoryDisplay::new(
                    dsession.session.universe().read_ticket(),
                    dsession.session.character().get(),
                ),
            })),
            // TODO: Even if we don't have a frame, we might want to update the UI anyway.
            Err(mpsc::TryRecvError::Empty) => {}
            Err(mpsc::TryRecvError::Disconnected) => panic!("render thread died"),
        }

        if dsession.session.frame_clock.should_draw() {
            // TODO: this is the only reason .cameras exists
            let c = &mut dsession.renderer.cameras;
            c.update(dsession.session.read_tickets());
            dsession.session.update_cursor(&*c);

            dsession
                .renderer
                .send_frame_to_render(&mut dsession.session);
        } else if let Some(t) = dsession.session.frame_clock.next_step_or_draw_time() {
            std::thread::sleep(t - Instant::now());
        }
    }
}

impl TerminalRenderer {
    /// Read the latest data from the session (mut be equal to previously provided one)
    /// and send it off to the raytracing threads.
    fn send_frame_to_render(&mut self, session: &mut Session) {
        // Fetch and update one of our recirculating renderers.
        let mut renderer = self.buffer_reuse_out.recv().unwrap();
        renderer
            .update(session.read_tickets(), session.cursor_result())
            .unwrap();

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
    let new = dsession
        .renderer
        .options
        .viewport_from_terminal_size(rect_size(dsession.window.viewport_position()));
    if dsession.viewport_cell.get() != new {
        dsession.viewport_cell.set(new);
    }
}

/// Output of [`ColorCharacterBuf`]
type TextAndColor = (String, Option<Rgba>);

/// Implements [`Accumulate`] for colored text output.
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

impl Accumulate for ColorCharacterBuf {
    type BlockData = <CharacterBuf as Accumulate>::BlockData;

    #[inline]
    fn opaque(&self) -> bool {
        self.color.opaque()
    }

    #[inline]
    fn add(&mut self, surface: ColorBuf, text: &Self::BlockData) {
        if self.override_color {
            return;
        }

        self.color.add(surface, &());
        self.text.add(surface, text);
    }

    fn hit_nothing(&mut self) {
        self.text.add(
            Rgba::TRANSPARENT.into(),
            &CharacterRtData(literal_substr!(" ")),
        );
        self.override_color = true;
    }

    fn mean<const N: usize>(mut items: [Self; N]) -> Self {
        use std::{array::from_fn, mem::take};
        Self {
            color: ColorBuf::mean::<N>(from_fn(|i| items[i].color)),
            text: CharacterBuf::mean::<N>(from_fn(|i| take(&mut items[i].text))),
            override_color: items.into_iter().all(|ccb| ccb.override_color),
        }
    }
}

fn rect_size(rect: Rect) -> Size2D<u16, camera::ImagePixel> {
    Size2D::new(rect.width, rect.height)
}
