//! Binary for All is Cubes desktop app.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::Context;
#[allow(unused, reason = "may be unused with some features")]
use clap::{CommandFactory as _, Parser as _};
use futures_channel::oneshot;

use all_is_cubes::euclid::Size2D;
use all_is_cubes::listen;
use all_is_cubes_render::camera::Viewport;

#[cfg(feature = "record")]
use all_is_cubes_desktop::record;
#[cfg(feature = "terminal")]
use all_is_cubes_desktop::terminal::{
    TerminalOptions, create_terminal_session, terminal_main_loop, terminal_print_once,
};
use all_is_cubes_desktop::winit::{
    self as aic_winit, create_winit_wgpu_desktop_session, winit_main_loop_and_init,
};
use all_is_cubes_desktop::{
    DesktopSession, InnerMainParams, Session, UniverseTask, inner_main, logging,
};

mod command_options;
use command_options::{AicDesktopArgs, DisplaySizeArg, GraphicsType, parse_universe_source};

static TITLE: &str = "All is Cubes";
fn title_and_version() -> String {
    // TODO: include git version if different
    format!("{TITLE} v{v}", v = clap::crate_version!())
}

fn main() -> Result<(), anyhow::Error> {
    let executor = all_is_cubes_desktop::Executor::new();

    // Parse and transform command-line arguments.
    let options = AicDesktopArgs::parse();
    // Destructure as a check that we're using/skipping all the args
    #[cfg_attr(not(feature = "record"), allow(unused_mut))]
    let AicDesktopArgs {
        graphics: graphics_type,
        display_size: DisplaySizeArg(display_size),
        fullscreen,
        template,
        template_size,
        seed,
        mut precompute_light,
        input_file,
        #[cfg(feature = "record")]
            output_file_and_format: _, // used in RecordOptions
        #[cfg(feature = "record")]
            save_all: _, // used in RecordOptions
        duration,
        logging: logging_args,
        settings: settings_args,
    } = options.clone();

    // Initialize logging -- telling it to suppress actual output in terminal mode.
    let late_logging = logging::install(&logging_args, graphics_type.uses_terminal())?;

    // After setting up logging, do other option interpretation steps.

    let input_source = parse_universe_source(input_file, template, template_size, seed);

    #[cfg(feature = "record")]
    let record_options: Option<record::RecordOptions> =
        options.record_options().inspect(|optropt| {
            if graphics_type == GraphicsType::Record
                && optropt.as_ref().is_some_and(|ropt| ropt.output_format.includes_light())
            {
                precompute_light = true;
            }
        })?;

    let settings = settings_args.build_settings()?;

    // Done with options; now start creating the session.

    let mut universe_task = UniverseTask::new(&executor, input_source, precompute_light);

    // This cell will be moved into the session after (possibly) being reset to the actual
    // window size. This is a kludge because the `Session`'s `Vui` wants to be able to track
    // the viewport aspect ratio. It would be nice to have a better strategy, but at least
    // this is mostly confined to initialization.
    let viewport_cell = listen::Cell::new(Viewport::with_scale(
        1.0,
        display_size.unwrap_or_else(Size2D::zero).cast_unit(),
    ));

    let start_session_time = Instant::now();
    let mut session = async_io::block_on(
        Session::builder()
            .ui(viewport_cell.as_source())
            .settings_from(settings)
            .quit(Arc::new(|| {
                // TODO: command the event loop to exit instead
                std::process::exit(0)
            }))
            .build(),
    );
    universe_task.attach_to_session(&mut session);
    let dsession = DesktopSession::new(executor.clone(), session, viewport_cell);
    let session_done_time = Instant::now();
    log::debug!(
        "Initialized session ({:.3} s)",
        session_done_time.duration_since(start_session_time).as_secs_f32()
    );

    // Bundle of inputs to `inner_main()`, which — unlike this function — is generic over
    // the kind of window system we're using.
    #[cfg_attr(not(feature = "terminal"), allow(unused_variables, unused_mut))]
    let (universe_ready_tx, mut universe_ready_rx) = oneshot::channel();
    #[cfg_attr(not(feature = "terminal"), allow(unused_variables, unused_mut))]
    let (task_done_tx, mut task_done_rx) = oneshot::channel();
    let inner_params = InnerMainParams {
        application_title: title_and_version(),
        executor,
        before_loop_time: Instant::now(),
        universe_task,
        headless: options.is_headless(),
        logging: late_logging,
        #[cfg(feature = "record")]
        recording: record_options,
        #[cfg(not(feature = "record"))]
        recording: None,
        universe_ready_signal: universe_ready_tx,
        task_done_signal: task_done_tx,
    };

    // The graphics type selects not only the kind of 'window' we create, but also the
    // type of event loop to run. Hence, this match combines
    // * creating a window
    // * parameterizing the DesktopSession with the type of window
    // * calling `inner_main()` to do the rest, including starting the event loop
    //
    // Note that _every_ branch of this match calls `inner_main()`.
    //
    // Note that while its return type is nominally Result<()>, it does not necessarily
    // ever return “successfully”, so no code should follow it.
    match graphics_type {
        GraphicsType::Window | GraphicsType::WindowRt => {
            winit_main_loop_and_init(
                Box::new(move |_inner_params, elwt| {
                    // TODO: this logic should not be inside main(), really, it should be part
                    // of the winit module — it just has a mess of deps
                    // TODO: don't block_on, be async?
                    let dsession = async_io::block_on(create_winit_wgpu_desktop_session(
                        dsession,
                        // TODO: turn this inside out and stop having `WinAndState` exposed
                        aic_winit::WinAndState::new(
                            elwt,
                            title_and_version(), // this will be overwritten with more detail later
                            display_size,
                            fullscreen,
                        )
                        .context("failed to create window")?,
                    ))
                    .context("failed to create session")?;
                    if graphics_type == GraphicsType::WindowRt {
                        // TODO: improve on this kludge by just having a general cmdline graphics config
                        dsession.session.settings().mutate_graphics_options(|o| {
                            o.render_method = all_is_cubes_render::camera::RenderMethod::Reference;
                        });
                    }
                    Ok(dsession)
                }),
                inner_params,
            )
        }
        #[cfg(feature = "terminal")]
        GraphicsType::Terminal => {
            let dsession = create_terminal_session(dsession, TerminalOptions::default())
                .context("failed to create session")?;
            inner_main(
                inner_params,
                |dsession| terminal_main_loop(dsession, universe_ready_rx),
                dsession,
            )
        }
        #[cfg(feature = "terminal")]
        GraphicsType::Print => {
            // TODO: Replace having a special mode with this being a kind of record/export running
            // under the headless main loop. We're not doing that yet because, currently, "renderer"
            // and "recorder" are handled very differently.
            let dsession = create_terminal_session(dsession, TerminalOptions::default())
                .context("failed to create session")?;
            inner_main(
                inner_params,
                |mut dsession| {
                    // Wait for the universe to be finished before capturing.
                    // TODO: How about we make this a special case of recording instead?
                    while let Ok(None) = universe_ready_rx.try_recv() {
                        dsession.advance_time_and_maybe_step();
                    }

                    let mut dsession = terminal_print_once(
                        dsession,
                        // TODO: Default display size should be based on terminal width
                        // (but not necessarily the full height)
                        display_size
                            .unwrap_or_else(|| Size2D::new(80, 24))
                            .min(Size2D::splat(u16::MAX.into()))
                            .cast::<u16>()
                            .cast_unit(),
                    )?;

                    // Wait for any other activities to complete.
                    while let Ok(None) = task_done_rx.try_recv() {
                        dsession.advance_time_and_maybe_step();
                    }

                    Ok(())
                },
                dsession,
            )
        }

        // TODO: remove these identical arms by getting rid of Record as a distinct type
        #[cfg(feature = "record")]
        GraphicsType::Record | GraphicsType::Headless => inner_main(
            inner_params,
            |dsession| {
                all_is_cubes_desktop::headless_main_loop(
                    dsession,
                    duration.map(Duration::from_secs_f64),
                )
            },
            dsession,
        ),

        #[cfg(not(feature = "record"))]
        GraphicsType::Headless => inner_main(
            inner_params,
            |dsession| {
                all_is_cubes_desktop::headless_main_loop(
                    dsession,
                    duration.map(Duration::from_secs_f64),
                )
            },
            dsession,
        ),
    }
}
