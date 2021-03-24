// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Binary for All is Cubes desktop app.

#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::wrong_self_convention)]

use std::time::{Duration, Instant};

use clap::{CommandFactory as _, Parser as _};
use futures::executor::block_on;
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
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
mod data_files;
mod glue;
mod record;
use record::record_main;
mod session;
mod terminal;
use terminal::{terminal_main_loop, TerminalOptions};

use crate::aic_glfw::create_glfw_desktop_session;
use crate::aic_winit::{create_winit_rt_desktop_session, create_winit_wgpu_desktop_session};
use crate::command_options::{
    determine_record_format, parse_universe_source, AicDesktopArgs, DisplaySizeArg, UniverseSource,
};
use crate::record::RecordFormat;
use crate::session::{ClockSource, DesktopSession};
use crate::terminal::terminal_print_once;

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
        // simplelog.
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

    let start_session_time = Instant::now();
    let mut session = block_on(Session::new());
    session.graphics_options_mut().set(graphics_options);
    let session_done_time = Instant::now();
    log::debug!(
        "Initialized session ({:.3} s)",
        session_done_time
            .duration_since(start_session_time)
            .as_secs_f32()
    );

    let universe_progress_bar = ProgressBar::new(100)
        .with_style(
            common_progress_style().template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}%      "),
        )
        .with_prefix("Building");
    universe_progress_bar.set_position(0); // Show bar promptly
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
                data_files::load_universe_from_file(yield_progress, &path).await
            }
        }
    })?;
    session.set_universe(universe);
    universe_progress_bar.finish();
    let universe_done_time = Instant::now();
    log::debug!(
        "Initialized game state with {:?} ({:.3} s)",
        input_source,
        universe_done_time
            .duration_since(session_done_time)
            .as_secs_f32()
    );

    // TODO: kludgy condition; this should be a question we ask about the record_options()
    if precompute_light
        || (graphics_type == GraphicsType::Record
            && output_file
                .as_ref()
                .and_then(|f| determine_record_format(f).ok())
                != Some(RecordFormat::Gltf))
    {
        session
            .character()
            .snapshot()
            .expect("no character to record the viewpoint of")
            .borrow()
            .space
            .try_modify(evaluate_light_with_progress)
            .unwrap();
    }

    match graphics_type {
        GraphicsType::Window => glfw_main_loop(create_glfw_desktop_session(
            session,
            &title_and_version(),
            display_size,
        )?),
        GraphicsType::WindowWgpu => {
            let event_loop = winit::event_loop::EventLoop::new();
            let dsession = block_on(create_winit_wgpu_desktop_session(
                session,
                aic_winit::create_window(&event_loop, &title_and_version(), display_size)?,
            ))?;
            winit_main_loop(event_loop, dsession)
        }
        GraphicsType::WindowRt => {
            let event_loop = winit::event_loop::EventLoop::new();
            let dsession = create_winit_rt_desktop_session(
                session,
                aic_winit::create_window(&event_loop, &title_and_version(), display_size)?,
            )?;
            winit_main_loop(event_loop, dsession)
        }
        GraphicsType::Terminal => terminal_main_loop(session, TerminalOptions::default()),
        GraphicsType::Record => record_main(
            session,
            options
                .record_options()
                .map_err(|e| e.format(&mut AicDesktopArgs::command()))?,
        ),
        GraphicsType::Print => terminal_print_once(
            session,
            TerminalOptions::default(),
            // TODO: Default display size should be based on terminal width
            // (but not necessarily the full height)
            display_size
                .unwrap_or_else(|| Vector2::new(80, 24))
                .map(|component| component.min(u16::MAX.into()) as u16),
        ),
        GraphicsType::Headless => {
            let mut dsession = DesktopSession {
                session,
                renderer: (),
                window: (),
                // dummy value
                viewport_cell: ListenableCell::new(Viewport::with_scale(
                    1.0,
                    display_size.unwrap_or_else(Vector2::zero),
                )),
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
                } else {
                    // TODO: sleep instead of spinning (provide a general implementation)
                    std::thread::yield_now();
                }
            }

            Ok(())
        }
    }
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
        .on_finish(ProgressFinish::AtCurrentPos)
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
