//! Binary for All is Cubes desktop app.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
// * This crate does not forbid(unsafe_code) because wgpu initialization requires it.

use std::sync::Arc;
use std::time::{Duration, Instant};

use anyhow::Context;
use clap::{CommandFactory as _, Parser as _};
use indicatif::ProgressBar;
use rand::Rng;

use all_is_cubes::camera::{self, GraphicsOptions, Viewport};
use all_is_cubes::euclid::{vec2, Vector2D};
use all_is_cubes::listen::ListenableCell;
use all_is_cubes::math::VectorOps;
use all_is_cubes::space::{LightUpdatesInfo, Space};
use all_is_cubes::universe::Universe;
use all_is_cubes_content::TemplateParameters;

mod aic_winit;
use aic_winit::winit_main_loop;
mod command_options;
use command_options::GraphicsType;
mod audio;
mod config_files;
mod glue;
mod logging;
mod record;
mod session;
mod terminal;

use crate::aic_winit::{create_winit_rt_desktop_session, create_winit_wgpu_desktop_session};
use crate::command_options::{
    determine_record_format, parse_universe_source, AicDesktopArgs, DisplaySizeArg, UniverseSource,
};
use crate::session::DesktopSession;
use crate::terminal::{
    create_terminal_session, terminal_main_loop, terminal_print_once, TerminalOptions,
};

type Session = all_is_cubes_ui::apps::Session<Instant>;

// TODO: put version numbers in the title when used as a window title
static TITLE: &str = "All is Cubes";

fn title_and_version() -> String {
    // TODO: include git version if different
    format!("{TITLE} v{v}", v = clap::crate_version!())
}

fn main() -> Result<(), anyhow::Error> {
    let runtime = tokio::runtime::Builder::new_multi_thread().build().unwrap();

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
        rerun,
    } = options.clone();

    // Initialize logging -- but only if it won't interfere.
    if graphics_type != GraphicsType::Terminal || logging_args.verbose {
        logging::install(logging_args)?;
    }

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
        config_files::load_config().expect("Error loading configuration files")
    };

    // Done with options; now start creating the session.

    // Kick off constructing the universe in the background.
    let universe_future = {
        // TODO: refactor this to work through RecordOptions
        let precompute_light = precompute_light
            || (graphics_type == GraphicsType::Record
                && output_file.as_ref().map_or(false, |file| {
                    determine_record_format(file).map_or(false, |fmt| fmt.includes_light())
                }));

        runtime.spawn(create_universe(input_source, precompute_light))
    };

    // This cell will be moved into the session after (possibly) being reset to the actual
    // window size. This is a kludge because the `Session`'s `Vui` wants to be able to track
    // the viewport aspect ratio. It would be nice to have a better strategy, but at least
    // this is mostly confined to initialization.
    let viewport_cell = ListenableCell::new(Viewport::with_scale(
        1.0,
        display_size.unwrap_or_else(Vector2D::zero).cast_unit(),
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
    let inner_params = InnerMainParams {
        runtime,
        before_loop_time: Instant::now(),
        universe_future,
        headless: options.is_headless(),
        rerun,
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
        GraphicsType::Window => {
            let event_loop = winit::event_loop::EventLoop::new()?;
            let dsession = inner_params
                .runtime
                .block_on(create_winit_wgpu_desktop_session(
                    session,
                    aic_winit::create_window(
                        &event_loop,
                        &title_and_version(),
                        display_size,
                        fullscreen,
                    )
                    .context("failed to create window")?,
                    viewport_cell,
                ))
                .context("failed to create session")?;
            inner_main(
                inner_params,
                move |dsession| winit_main_loop(event_loop, dsession),
                dsession,
            )
        }
        GraphicsType::WindowRt => {
            let event_loop = winit::event_loop::EventLoop::new()?;
            let dsession = create_winit_rt_desktop_session(
                session,
                aic_winit::create_window(
                    &event_loop,
                    &title_and_version(),
                    display_size,
                    fullscreen,
                )
                .context("failed to create window")?,
                viewport_cell,
            )
            .context("failed to create session")?;
            inner_main(
                inner_params,
                move |dsession| winit_main_loop(event_loop, dsession),
                dsession,
            )
        }
        GraphicsType::Terminal => {
            let dsession =
                create_terminal_session(session, TerminalOptions::default(), viewport_cell)
                    .context("failed to create session")?;
            inner_main(inner_params, terminal_main_loop, dsession)
        }
        GraphicsType::Record => {
            let record_options =
                record_options.expect("arg validation did not require output with -g record");

            let dsession = DesktopSession::new((), (), session, viewport_cell);
            let handle = inner_params.runtime.handle().clone();

            inner_main(
                inner_params,
                move |dsession| record::record_main(dsession, record_options, &handle),
                dsession,
            )
        }
        GraphicsType::Print => {
            let dsession =
                create_terminal_session(session, TerminalOptions::default(), viewport_cell)
                    .context("failed to create session")?;
            inner_main(
                inner_params,
                |dsession| {
                    terminal_print_once(
                        dsession,
                        // TODO: Default display size should be based on terminal width
                        // (but not necessarily the full height)
                        display_size
                            .unwrap_or_else(|| Vector2D::new(80, 24))
                            .map(|component| component.min(u16::MAX.into()) as u16)
                            .cast_unit(),
                    )
                },
                dsession,
            )
        }
        GraphicsType::Headless => inner_main(
            inner_params,
            |dsession| headless_main_loop(dsession, duration),
            DesktopSession::new((), (), session, viewport_cell),
        ),
    }
}

/// Given a [`DesktopSession`] and an event loop type already decided, run the remainder
/// of main operations.
///
/// This function may or may not ever return, depending on the type of event loop.
fn inner_main<Ren, Win>(
    params: InnerMainParams,
    looper: impl FnOnce(DesktopSession<Ren, Win>) -> Result<(), anyhow::Error>,
    mut dsession: DesktopSession<Ren, Win>,
) -> Result<(), anyhow::Error> {
    let InnerMainParams {
        runtime,
        before_loop_time,
        universe_future,
        headless,
        rerun,
    } = params;

    // At this point we have just finished whatever the GraphicsType did before calling
    // inner_main().
    let entered_inner_time = Instant::now();
    log::debug!(
        "Initialized graphics ({:.3} s)",
        entered_inner_time
            .duration_since(before_loop_time)
            .as_secs_f64()
    );

    if !headless {
        match audio::init_sound(&dsession.session) {
            Ok(audio_out) => dsession.audio = Some(audio_out),
            Err(e) => log::error!(
                // note that `e` is an anyhow::Error and will benefit from its
                // chain printing
                "Failed to initialize audio. Will proceed without.\n{e:#}",
            ),
        };
    }

    // TODO: don't do this as a block_on, but await it from inside of the event loop
    // (this will require further work on the record event loop)
    let mut universe: Universe = runtime
        .block_on(universe_future)
        .context("internal error inside of universe creation/loading task")?
        .context("failed to create universe from requested template or file")?;

    if rerun {
        cfg_if::cfg_if! {
            if #[cfg(feature = "rerun")] {
                connect_rerun(&mut universe);
            } else {
                // suppress warning
                let _ = &mut universe;

                // TODO: cleaner error handling from this point
                panic!("not compiled with rerun logging support");
            }
        }
    }

    dsession.session.set_universe(universe);

    log::trace!("Entering event loop.");

    looper(dsession)
}

/// Ad-hoc struct of arguments to [`inner_main`] that can be constructed beforehand.
struct InnerMainParams {
    runtime: tokio::runtime::Runtime,
    before_loop_time: Instant,
    universe_future: tokio::task::JoinHandle<Result<Universe, anyhow::Error>>,
    headless: bool,
    rerun: bool,
}

/// Perform and log the creation of the universe.
async fn create_universe(
    input_source: UniverseSource,
    precompute_light: bool,
) -> Result<Universe, anyhow::Error> {
    let start_time = Instant::now();
    let universe_progress_bar = logging::new_progress_bar(100)
        .with_style(
            logging::common_progress_style()
                .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}% {msg:36}")
                .unwrap(),
        )
        .with_prefix("Building");
    universe_progress_bar.set_position(0);
    let yield_progress = {
        let universe_progress_bar = universe_progress_bar.clone();
        glue::tokio_yield_progress()
            .progress_using(move |info| {
                universe_progress_bar.set_position((info.fraction() * 100.0) as u64);
                universe_progress_bar.set_message(String::from(info.label_str()));
            })
            .build()
    };
    let universe = match input_source.clone() {
        UniverseSource::Template(template, TemplateParameters { seed, size }) => {
            let seed: u64 = seed.unwrap_or_else(|| {
                let seed = rand::thread_rng().gen();
                log::info!("Randomly chosen universe seed: {seed}");
                seed
            });
            template
                .clone()
                .build::<Instant>(
                    yield_progress,
                    TemplateParameters {
                        seed: Some(seed),
                        size,
                    },
                )
                .await
                .with_context(|| {
                    format!("failed while constructing universe from template {template:?}")
                })
        }
        UniverseSource::File(path) => {
            let path = Arc::new(path);
            all_is_cubes_port::load_universe_from_file(yield_progress, path.clone())
                .await
                .with_context(|| format!("could not load universe from file {path:?}"))
        }
    }?;
    universe_progress_bar.finish();
    let universe_done_time = Instant::now();
    log::debug!(
        "Initialized game state with {:?} ({:.3} s)",
        input_source,
        universe_done_time.duration_since(start_time).as_secs_f32()
    );

    if precompute_light {
        if let Some(c) = universe.get_default_character() {
            c.read()
                .unwrap()
                .space
                .try_modify(evaluate_light_with_progress)
                .unwrap();
        }
    }

    Ok(universe)
}

/// Main loop that does nothing but run the simulation.
/// This may be used in case the simulation has interesting side-effects.
fn headless_main_loop(
    mut dsession: DesktopSession<(), ()>,
    duration: Option<f64>,
) -> Result<(), anyhow::Error> {
    // TODO: Right now this is useless. Eventually, we may have other paths for side
    // effects from the universe, or interesting logging.
    log::info!("Simulating a universe nobody's looking at...");

    let duration = duration.map(Duration::from_secs_f64);

    let t0 = Instant::now();
    loop {
        dsession.advance_time_and_maybe_step();

        if duration
            .map(|d| Instant::now().duration_since(t0) > d)
            .unwrap_or(false)
        {
            break;
        } else if let Some(t) = dsession.session.frame_clock.next_step_or_draw_time() {
            std::thread::sleep(t - Instant::now());
        }
    }

    Ok(())
}

#[cfg(feature = "rerun")]
fn connect_rerun(universe: &mut Universe) {
    use all_is_cubes::rerun_glue as rg;

    let stream = re_sdk::RecordingStreamBuilder::new("all-is-cubes")
        .default_enabled(true)
        .connect(re_sdk::default_server_addr(), Some(Duration::from_secs(1)))
        .unwrap();
    let destination = rg::Destination {
        stream,
        // Note: This must not be empty for ViewCoordinates to work
        // https://github.com/rerun-io/rerun/issues/3538
        path: rg::entity_path!["dt"],
    };

    // Log timeless configuration
    destination.log_initialization();

    // Attach to universe elements
    universe.log_to_rerun(destination.clone());
    if let Some(c) = universe.get_default_character() {
        c.try_modify(|c| c.log_to_rerun(destination.child(&rg::entity_path!["character"])))
            .unwrap();
    }
}

fn evaluate_light_with_progress(space: &mut Space) {
    let light_progress = logging::new_progress_bar(100).with_prefix("Lighting");
    space.evaluate_light::<Instant>(1, lighting_progress_adapter(&light_progress));
    light_progress.finish();
}

/// Convert `LightUpdatesInfo` data to an approximate completion progress.
/// TODO: Improve this and put it in the lighting module (independent of indicatif).
fn lighting_progress_adapter(progress: &ProgressBar) -> impl FnMut(LightUpdatesInfo) + '_ {
    let mut worst = 1;
    move |info| {
        worst = worst.max(info.queue_count);
        progress.set_length(worst as u64);
        progress.set_position((worst - info.queue_count) as u64);
    }
}

/// Choose a window size (in terms of viewport size) when the user did not request one.
///
/// The given dimensions are of the maximum possible viewport size, if known.
fn choose_graphical_window_size(
    maximum_size: Option<Vector2D<u32, camera::NominalPixel>>,
) -> Vector2D<u32, camera::NominalPixel> {
    match maximum_size {
        Some(maximum_size) => {
            // TODO: consider constraining the aspect ratio, setting a maximum size, and other caveats
            maximum_size * 7 / 10
        }
        None => vec2(800, 600),
    }
}
