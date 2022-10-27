//! Binary for All is Cubes desktop app.

// Basic lint settings, which should be identical across all all-is-cubes project crates.
// This list is sorted.
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::needless_update)]
#![allow(clippy::single_match)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::doc_markdown)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::return_self_not_must_use)]
#![warn(clippy::wrong_self_convention)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(trivial_numeric_casts)]
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
// Lenience for tests.
#![cfg_attr(test,
    allow(clippy::float_cmp), // deterministic tests
    allow(clippy::redundant_clone), // prefer regularity over efficiency
)]
// TODO: warn(missing_docs), eventually
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review

use std::time::{Duration, Instant};

use all_is_cubes::universe::Universe;
use clap::{CommandFactory as _, Parser as _};
use futures::executor::block_on;
use indicatif::{ProgressBar, ProgressStyle};
use rand::{thread_rng, Rng};

use all_is_cubes::apps::Session;
use all_is_cubes::camera::{GraphicsOptions, Viewport};
use all_is_cubes::cgmath::{Vector2, Zero as _};
use all_is_cubes::listen::ListenableCell;
use all_is_cubes::space::{LightUpdatesInfo, Space};
use all_is_cubes::util::YieldProgress;

mod aic_glfw;
use aic_glfw::glfw_main_loop;
mod aic_winit;
use aic_winit::winit_main_loop;
mod command_options;
use command_options::GraphicsType;
mod config_files;
mod glue;
mod record;
use record::record_main;
mod session;
mod terminal;

use crate::aic_glfw::create_glfw_desktop_session;
use crate::aic_winit::{create_winit_rt_desktop_session, create_winit_wgpu_desktop_session};
use crate::command_options::{
    determine_record_format, parse_universe_source, AicDesktopArgs, DisplaySizeArg, UniverseSource,
};
use crate::record::{create_recording_session, RecordFormat};
use crate::session::{ClockSource, DesktopSession};
use crate::terminal::{
    create_terminal_session, terminal_main_loop, terminal_print_once, TerminalOptions,
};

// TODO: put version numbers in the title when used as a window title
static TITLE: &str = "All is Cubes";

fn title_and_version() -> String {
    // TODO: include git version if different
    format!("{} v{}", TITLE, clap::crate_version!())
}

fn main() -> Result<(), anyhow::Error> {
    // Parse and transform command-line arguments.
    let options = AicDesktopArgs::parse();
    // Destructure as a check that we're using/skipping all the args
    let AicDesktopArgs {
        graphics: graphics_type,
        display_size: DisplaySizeArg(display_size),
        fullscreen,
        template,
        precompute_light,
        input_file,
        output_file,
        duration,
        verbose,
        no_config_files,
    } = options.clone();
    let input_source = parse_universe_source(input_file, template);

    // Initialize logging -- but only if it won't interfere.
    if graphics_type != GraphicsType::Terminal || verbose {
        // Note: Something like this log configuration also appears in other binaries.
        // Unclear how to deduplicate since we don't want to have a library-level dep on
        // simplelog. For now, just remember to consider updating other instances.
        use simplelog::LevelFilter::{Debug, Off, Trace};
        simplelog::TermLogger::init(
            match verbose {
                // TODO: When we're closer to 1.0, change the default level to `Info`
                false => Debug,
                true => Trace,
            },
            simplelog::ConfigBuilder::new()
                .set_target_level(Off)
                .set_location_level(Off)
                .add_filter_ignore_str("wgpu") // noisy
                .add_filter_ignore_str("naga") // noisy
                .add_filter_ignore_str("winit") // noisy at Trace level only
                .build(),
            simplelog::TerminalMode::Stderr,
            simplelog::ColorChoice::Auto,
        )?;
    }

    let graphics_options = if no_config_files {
        GraphicsOptions::default()
    } else {
        config_files::load_config().expect("Error loading configuration files")
    };

    // This cell will be moved into the session after (possibly) being reset to the actual
    // window size. This is a kludge because the `Session`'s `Vui` wants to be able to track
    // the viewport aspect ratio. It would be nice to have a better strategy, but at least
    // this is mostly confined to initialization.
    let viewport_cell = ListenableCell::new(Viewport::with_scale(
        1.0,
        display_size.unwrap_or_else(Vector2::zero),
    ));

    let start_session_time = Instant::now();
    let mut session = block_on(Session::builder().ui(viewport_cell.as_source()).build());
    session.graphics_options_mut().set(graphics_options);
    let session_done_time = Instant::now();
    log::debug!(
        "Initialized session ({:.3} s)",
        session_done_time
            .duration_since(start_session_time)
            .as_secs_f32()
    );

    // TODO: refactor this to work through RecordOptions
    let precompute_light = precompute_light
        || (graphics_type == GraphicsType::Record
            && output_file
                .as_ref()
                .and_then(|f| determine_record_format(f).ok())
                != Some(RecordFormat::Gltf));
    let universe = create_universe(input_source, precompute_light)?;
    session.set_universe(universe);

    // The graphics type selects not only the kind of 'window' we create, but also the
    // type of event loop to run. Hence, this match combines
    // * creating a window
    // * creating a DesktopSession
    // * starting the main loop
    // Note that while its return type is nominally Result<()>, it does not necessarily
    // ever return “successfully”, so no code should follow it.
    match graphics_type {
        GraphicsType::WindowGl => {
            let dsession = create_glfw_desktop_session(
                session,
                &title_and_version(),
                display_size,
                fullscreen,
                viewport_cell,
            )?;
            glfw_main_loop(dsession)
        }
        GraphicsType::Window => {
            let event_loop = winit::event_loop::EventLoop::new();
            let dsession = block_on(create_winit_wgpu_desktop_session(
                session,
                aic_winit::create_window(
                    &event_loop,
                    &title_and_version(),
                    display_size,
                    fullscreen,
                )?,
                viewport_cell,
            ))?;
            winit_main_loop(event_loop, dsession)
        }
        GraphicsType::WindowRt => {
            let event_loop = winit::event_loop::EventLoop::new();
            let dsession = create_winit_rt_desktop_session(
                session,
                aic_winit::create_window(
                    &event_loop,
                    &title_and_version(),
                    display_size,
                    fullscreen,
                )?,
                viewport_cell,
            )?;
            winit_main_loop(event_loop, dsession)
        }
        GraphicsType::Terminal => {
            let dsession =
                create_terminal_session(session, TerminalOptions::default(), viewport_cell)?;
            terminal_main_loop(dsession)
        }
        GraphicsType::Record => {
            // TODO: record_options validation should just be part of the regular arg parsing
            // (will need a wrapper type)
            let record_options = options
                .record_options()
                .map_err(|e| e.format(&mut AicDesktopArgs::command()))?;
            let (dsession, sr) = create_recording_session(session, &record_options, viewport_cell)?;
            record_main(dsession, record_options, sr)
        }
        GraphicsType::Print => {
            let dsession =
                create_terminal_session(session, TerminalOptions::default(), viewport_cell)?;
            terminal_print_once(
                dsession,
                // TODO: Default display size should be based on terminal width
                // (but not necessarily the full height)
                display_size
                    .unwrap_or_else(|| Vector2::new(80, 24))
                    .map(|component| component.min(u16::MAX.into()) as u16),
            )
        }
        GraphicsType::Headless => {
            let mut dsession = DesktopSession {
                session,
                renderer: (),
                window: (),
                viewport_cell,
                clock_source: ClockSource::Instant,
                recorder: None,
            };

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
    }
}

/// Perform and log the creation of the universe.
fn create_universe(
    input_source: UniverseSource,
    precompute_light: bool,
) -> Result<Universe, anyhow::Error> {
    let start_time = Instant::now();
    let universe_progress_bar = ProgressBar::new(100)
        .with_style(
            common_progress_style()
                .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}%      ")
                .unwrap(),
        )
        .with_prefix("Building");
    universe_progress_bar.set_position(0);
    let yield_progress = {
        let universe_progress_bar = universe_progress_bar.clone();
        YieldProgress::new(
            || std::future::ready(()),
            move |fraction| universe_progress_bar.set_position((fraction * 100.0) as u64),
        )
    };
    let universe = block_on(async {
        match input_source.clone() {
            UniverseSource::Template(template) => template
                .build(yield_progress, thread_rng().gen())
                .await
                .map_err(anyhow::Error::from),
            UniverseSource::File(path) => {
                all_is_cubes_port::load_universe_from_file(yield_progress, &path).await
            }
        }
    })?;
    universe_progress_bar.finish();
    let universe_done_time = Instant::now();
    log::debug!(
        "Initialized game state with {:?} ({:.3} s)",
        input_source,
        universe_done_time.duration_since(start_time).as_secs_f32()
    );

    if precompute_light {
        if let Some(c) = universe.get_default_character() {
            c.borrow()
                .space
                .try_modify(evaluate_light_with_progress)
                .unwrap();
        }
    }

    Ok(universe)
}

fn evaluate_light_with_progress(space: &mut Space) {
    let light_progress = ProgressBar::new(100)
        .with_style(common_progress_style())
        .with_prefix("Lighting");
    space.evaluate_light(1, lighting_progress_adapter(&light_progress));
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

/// [`ProgressStyle`] for progress bars we display.
fn common_progress_style() -> ProgressStyle {
    ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6}")
        .unwrap()
}

/// Choose a window size (in terms of viewport size) when the user did not request one.
///
/// The given dimensions are of the maximum possible viewport size, if known.
fn choose_graphical_window_size(maximum_size: Option<Vector2<u32>>) -> Vector2<u32> {
    match maximum_size {
        Some(maximum_size) => {
            // TODO: consider constraining the aspect ratio, setting a maximum size, and other caveats
            maximum_size * 7 / 10
        }
        None => Vector2::new(800, 600),
    }
}
