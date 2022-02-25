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

use clap::Parser as _;
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
use rand::{thread_rng, Rng};

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::space::{LightUpdatesInfo, Space};
use all_is_cubes::util::YieldProgress;

mod aic_glfw;
use aic_glfw::glfw_main_loop;
mod command_options;
use command_options::GraphicsType;
mod config_files;
mod data_files;
mod glue;
mod record;
use record::record_main;
mod terminal;
use terminal::{terminal_main_loop, TerminalOptions};

use crate::command_options::{
    parse_universe_source, AicDesktopArgs, DisplaySizeArg, UniverseSource,
};
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
        output_file: _,
        duration,
        verbose,
        no_config_files,
    } = options.clone();
    let input_source = parse_universe_source(input_file, template);

    // Convert options we will consult multiple times.

    // Initialize logging -- but only if it won't interfere.
    if graphics_type != GraphicsType::Terminal || verbose {
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

    let start_app_time = Instant::now();
    let mut app = futures_executor::block_on(AllIsCubesAppState::new());
    app.graphics_options_mut().set(graphics_options);
    let app_done_time = Instant::now();
    log::debug!(
        "Initialized app state ({:.3} s)",
        app_done_time.duration_since(start_app_time).as_secs_f32()
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
    let universe = futures_executor::block_on(async {
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
    app.set_universe(universe);
    universe_progress_bar.finish();
    let universe_done_time = Instant::now();
    log::debug!(
        "Initialized game state with {:?} ({:.3} s)",
        input_source,
        universe_done_time
            .duration_since(app_done_time)
            .as_secs_f32()
    );

    if precompute_light || graphics_type == GraphicsType::Record {
        app.character()
            .snapshot()
            .expect("no character to record the viewpoint of")
            .borrow()
            .space
            .try_modify(evaluate_light_with_progress)
            .unwrap();
    }

    match graphics_type {
        GraphicsType::Window => glfw_main_loop(app, &title_and_version(), display_size),
        GraphicsType::Terminal => terminal_main_loop(app, TerminalOptions::default()),
        GraphicsType::Record => record_main(app, options.record_options()),
        GraphicsType::Print => terminal_print_once(
            app,
            TerminalOptions::default(),
            // TODO: Default display size should be based on terminal width
            // (but not necessarily the full height)
            display_size
                .unwrap_or_else(|| Vector2::new(80, 24))
                .map(|component| component.min(u16::MAX.into()) as u16),
        ),
        GraphicsType::Headless => {
            // TODO: Right now this is useless. Eventually, we may have other paths for side
            // effects from the universe, or interesting logging.
            log::info!("Simulating a universe nobody's looking at...");

            let duration = duration.map(Duration::from_secs_f64);

            let t0 = Instant::now();
            loop {
                // TODO: sleep instead of spinning, and maybe put a general version of this in AllIsCubesAppState.
                // TODO: Offer a faster-than-real-time option. (Right now, FrameClock bakes in a slowdown policy that would need adjustment.)
                let t = Instant::now();
                app.frame_clock.advance_to(t);
                app.maybe_step_universe();

                if duration.map(|d| t.duration_since(t0) > d).unwrap_or(false) {
                    break;
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
