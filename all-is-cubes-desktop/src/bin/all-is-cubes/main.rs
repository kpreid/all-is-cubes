//! Binary for All is Cubes desktop app.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::Context;
use clap::{CommandFactory as _, Parser as _};

use all_is_cubes::camera::{GraphicsOptions, Viewport};
use all_is_cubes::euclid::Size2D;
use all_is_cubes::listen::ListenableCell;
use all_is_cubes::math::VectorOps;

use all_is_cubes_desktop::terminal::{
    create_terminal_session, terminal_main_loop, terminal_print_once, TerminalOptions,
};
use all_is_cubes_desktop::winit::{
    self as aic_winit, create_winit_wgpu_desktop_session, winit_main_loop_and_init,
};
use all_is_cubes_desktop::{
    inner_main, load_config, logging, record, DesktopSession, InnerMainParams, Session,
};

mod command_options;
use command_options::{
    determine_record_format, parse_universe_source, AicDesktopArgs, DisplaySizeArg, GraphicsType,
};

static TITLE: &str = "All is Cubes";
fn title_and_version() -> String {
    // TODO: include git version if different
    format!("{TITLE} v{v}", v = clap::crate_version!())
}

fn main() -> Result<(), anyhow::Error> {
    let runtime = tokio::runtime::Builder::new_multi_thread().build().unwrap();
    let executor = all_is_cubes_desktop::Executor::new(runtime.handle().clone());

    // Parse and transform command-line arguments.
    let options = AicDesktopArgs::parse();
    // Destructure as a check that we're using/skipping all the args
    let AicDesktopArgs {
        graphics: graphics_type,
        display_size: DisplaySizeArg(display_size),
        fullscreen,
        template,
        template_size,
        seed,
        precompute_light,
        input_file,
        output_file,
        save_all: _, // used in RecordOptions
        duration,
        logging: logging_args,
        no_config_files,
    } = options.clone();

    // Initialize logging -- telling it to suppress actual output in terminal mode.
    let late_logging = logging::install(&logging_args, graphics_type == GraphicsType::Terminal)?;

    // After setting up logging, do other option interpretation steps.

    let input_source = parse_universe_source(input_file, template, template_size, seed);

    // TODO: record_options validation should just be part of the regular arg parsing
    // (will need a wrapper type)
    let record_options: Option<record::RecordOptions> = options
        .record_options()
        .map_err(|e| e.format(&mut AicDesktopArgs::command()))?;

    let graphics_options = if no_config_files {
        GraphicsOptions::default()
    } else {
        load_config().context("Error loading configuration files")?
    };

    // Done with options; now start creating the session.

    // Kick off constructing the universe in the background.
    let universe_future = {
        // TODO: refactor this to access fewer deep details
        let precompute_light = precompute_light
            || (graphics_type == GraphicsType::Record
                && output_file.as_ref().map_or(false, |file| {
                    determine_record_format(file).map_or(false, |fmt| fmt.includes_light())
                }));

        runtime.spawn(input_source.create_universe(precompute_light))
    };

    // This cell will be moved into the session after (possibly) being reset to the actual
    // window size. This is a kludge because the `Session`'s `Vui` wants to be able to track
    // the viewport aspect ratio. It would be nice to have a better strategy, but at least
    // this is mostly confined to initialization.
    let viewport_cell = ListenableCell::new(Viewport::with_scale(
        1.0,
        display_size.unwrap_or_else(Size2D::zero).cast_unit(),
    ));

    let start_session_time = Instant::now();
    let session = runtime.block_on(
        Session::builder()
            .ui(viewport_cell.as_source())
            .quit(Arc::new(|| {
                // TODO: command the event loop to exit instead
                std::process::exit(0)
            }))
            .build(),
    );
    session.graphics_options_mut().set(graphics_options);
    let session_done_time = Instant::now();
    log::debug!(
        "Initialized session ({:.3} s)",
        session_done_time
            .duration_since(start_session_time)
            .as_secs_f32()
    );

    // Bundle of inputs to `inner_main()`, which — unlike this function — is generic over
    // the kind of window system we're using.
    let (universe_ready_tx, mut universe_ready_rx) = tokio::sync::oneshot::channel();
    let inner_params = InnerMainParams {
        application_title: title_and_version(),
        runtime,
        before_loop_time: Instant::now(),
        universe_future,
        headless: options.is_headless(),
        logging: late_logging,
        recording: record_options,
        universe_ready_signal: universe_ready_tx,
    };

    // The graphics type selects not only the kind of 'window' we create, but also the
    // type of event loop to run. Hence, this match combines
    // * creating a window
    // * creating a DesktopSession
    // * calling `inner_main()` to do the rest, including starting the event loop
    //
    // Note that _every_ branch of this match calls `inner_main()`.
    //
    // Note that while its return type is nominally Result<()>, it does not necessarily
    // ever return “successfully”, so no code should follow it.
    match graphics_type {
        GraphicsType::Window | GraphicsType::WindowRt => {
            winit_main_loop_and_init(
                move |inner_params, elwt| {
                    // TODO: this logic should not be inside main(), really, it should be part
                    // of the winit module — it just has a mess of deps
                    // TODO: don't block_on, be async?
                    let dsession = inner_params
                        .runtime
                        .block_on(create_winit_wgpu_desktop_session(
                            executor.clone(),
                            session,
                            // TODO: turn this inside out and stop having `WinAndState` exposed
                            aic_winit::WinAndState::new(
                                elwt,
                                &title_and_version(), // this will be overwritten with more detail later
                                display_size,
                                fullscreen,
                            )
                            .context("failed to create window")?,
                            viewport_cell,
                        ))
                        .context("failed to create session")?;
                    if graphics_type == GraphicsType::WindowRt {
                        // TODO: improve on this kludge by just having a general cmdline graphics config
                        dsession.session.graphics_options_mut().update_mut(|o| {
                            o.render_method = all_is_cubes::camera::RenderMethod::Reference;
                        });
                    }
                    Ok(dsession)
                },
                inner_params,
            )
        }
        GraphicsType::Terminal => {
            let dsession = create_terminal_session(
                executor,
                session,
                TerminalOptions::default(),
                viewport_cell,
            )
            .context("failed to create session")?;
            inner_main(
                inner_params,
                |dsession| terminal_main_loop(dsession, universe_ready_rx),
                dsession,
            )
        }
        GraphicsType::Print => {
            let dsession = create_terminal_session(
                executor,
                session,
                TerminalOptions::default(),
                viewport_cell,
            )
            .context("failed to create session")?;
            inner_main(
                inner_params,
                |mut dsession| {
                    // Wait for the universe to be finished before capturing.
                    // TODO: How about we make this a special case of recording instead?
                    while let Err(tokio::sync::oneshot::error::TryRecvError::Empty) =
                        universe_ready_rx.try_recv()
                    {
                        dsession.advance_time_and_maybe_step();
                    }

                    terminal_print_once(
                        dsession,
                        // TODO: Default display size should be based on terminal width
                        // (but not necessarily the full height)
                        display_size
                            .unwrap_or_else(|| Size2D::new(80, 24))
                            .map(|component| component.min(u16::MAX.into()) as u16)
                            .cast_unit(),
                    )
                },
                dsession,
            )
        }
        GraphicsType::Record | GraphicsType::Headless => inner_main(
            inner_params,
            |dsession| {
                all_is_cubes_desktop::headless_main_loop(
                    dsession,
                    duration.map(Duration::from_secs_f64),
                )
            },
            DesktopSession::new(executor, (), (), session, viewport_cell),
        ),
    }
}
