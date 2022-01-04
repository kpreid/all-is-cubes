// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Binary for All is Cubes desktop app.

#![deny(rust_2018_idioms)]
#![warn(unused_extern_crates)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]

use std::time::Instant;

use all_is_cubes::cgmath::Vector2;
use clap::value_t;
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
use rand::{thread_rng, Rng};

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::space::{LightUpdatesInfo, Space};
use all_is_cubes::util::YieldProgress;

mod aic_glfw;
use aic_glfw::glfw_main_loop;
mod command_options;
use command_options::{parse_dimensions, parse_record_options, GraphicsType};
mod config_files;
mod data_files;
mod record;
use record::record_main;
mod terminal;
use terminal::{terminal_main_loop, TerminalOptions};

use crate::command_options::{parse_universe_source, UniverseSource};
use crate::terminal::terminal_print_once;

// TODO: put version numbers in the title when used as a window title
static TITLE: &str = "All is Cubes";

fn title_and_version() -> String {
    // TODO: include git version if different
    format!("{} v{}", TITLE, clap::crate_version!())
}

fn main() -> Result<(), anyhow::Error> {
    let options = command_options::app().get_matches();

    // Convert options we will consult multiple times.
    let display_size = parse_dimensions(options.value_of("display_size").unwrap()).unwrap();
    let graphics_type = value_t!(options, "graphics", GraphicsType).unwrap_or_else(|e| e.exit());
    let input_source = parse_universe_source(&options).unwrap_or_else(|e| e.exit());

    // Initialize logging -- but only if it won't interfere.
    if graphics_type != GraphicsType::Terminal || options.is_present("verbose") {
        simplelog::TermLogger::init(
            match options.occurrences_of("verbose") {
                // TODO: When we're closer to 1.0, change the default level to `Info`
                0 => simplelog::LevelFilter::Debug,
                _ => simplelog::LevelFilter::Trace,
            },
            simplelog::Config::default(),
            simplelog::TerminalMode::Stderr,
            simplelog::ColorChoice::Auto,
        )?;
    }

    let graphics_options = if options.is_present("no_config_files") {
        GraphicsOptions::default()
    } else {
        config_files::load_config().expect("Error loading configuration files")
    };

    let start_app_time = Instant::now();
    let mut app = AllIsCubesAppState::new();
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
            UniverseSource::File(path) => data_files::load_universe_from_file(&path).await,
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

    if options.is_present("precompute_light") || graphics_type == GraphicsType::Record {
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
        GraphicsType::Record => record_main(app, parse_record_options(options, display_size)?),
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

            loop {
                // TODO: sleep instead of spinning, and maybe put a general version of this in AllIsCubesAppState.
                app.frame_clock.advance_to(Instant::now());
                app.maybe_step_universe();
            }
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
