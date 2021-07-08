// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Binary for All is Cubes desktop app.

#![deny(rust_2018_idioms)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]

use std::convert::TryInto;
use std::error::Error;
use std::fs::create_dir_all;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::path::PathBuf;
use std::time::{Duration, Instant};

use cgmath::Vector2;
use clap::{value_t, Arg, ErrorKind};
use directories_next::ProjectDirs;
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
use serde::{de::DeserializeOwned, Serialize};
use strum::IntoEnumIterator;

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::camera::GraphicsOptions;
use all_is_cubes::content::UniverseTemplate;
use all_is_cubes::space::{LightUpdatesInfo, Space};

use crate::record::{RecordAnimationOptions, RecordOptions};

mod aic_glfw;
use aic_glfw::glfw_main_loop;
mod record;
use record::record_main;
mod terminal;
use terminal::{terminal_main_loop, TerminalOptions};

#[derive(Debug, PartialEq, strum::EnumString, strum::EnumIter, strum::IntoStaticStr)]
#[strum(serialize_all = "kebab-case")]
#[non_exhaustive]
pub enum GraphicsType {
    Headless,
    Window,
    Terminal,
    Record,
}

// TODO: put version numbers in the title when used as a window title
static TITLE: &str = "All is Cubes";

fn app() -> clap::App<'static, 'static> {
    clap::App::new(TITLE)
        .version(clap::crate_version!()) // TODO: include all_is_cubes library version
        .author(clap::crate_authors!())
        .about(clap::crate_description!())
        .arg(
            Arg::with_name("graphics")
                .long("graphics")
                .short("g")
                .possible_values(&GraphicsType::iter().map(<&str>::from).collect::<Vec<_>>())
                .default_value("window")
                .value_name("MODE")
                .hide_possible_values(true)
                .help(
                    "Graphics/UI mode; one of the following keywords:\n\
                    window   - Open a window (uses OpenGL)\n\
                    terminal - Colored text in this terminal (uses raytracing)\n\
                    headless - Non-interactive; don't draw anything but only simulates\n\
                    record   - Non-interactive; save an image or video (uses raytracing)\n\
                    ",
                ),
        )
        .arg(
            Arg::with_name("display_size")
                .long("display-size")
                .value_name("W×H")
                .default_value("auto")
                .validator(|s| parse_dimensions(&s).map(|_| ()))
                .help("Window size, if applicable to the selected graphics mode."),
        )
        .arg(
            Arg::with_name("template")
                .long("template")
                .short("t")
                .possible_values(
                    &UniverseTemplate::iter()
                        .map(<&str>::from)
                        .collect::<Vec<_>>(),
                )
                .default_value("demo-city")
                .help("Which world template to use."),
        )
        .arg(Arg::with_name("precompute_light")
            .long("precompute-light")
            .help("Fully calculate light before starting the game."))
        .arg(
            Arg::with_name("output")
                .long("output")
                .short("o")
                .required_if("graphics", "record")
                .value_name("FILE")
                .help("Output PNG file name for 'record' mode. If animating, a frame number will be inserted."),
        )
        .arg(
            Arg::with_name("duration")
                .long("duration")
                .value_name("SECONDS")
                // TODO: Generalize this to "exit after this much time has passed".
                .help("Time to record video, in 'record' mode only (omit for still image)."),
        )
        .arg(
            Arg::with_name("verbose")
                .long("verbose")
                .short("v")
                .help("Additional logging to stderr."),
        )
        .arg(
            Arg::with_name("no_config_files")
                .long("no-config-files")
                .help("Ignore all configuration files, using only defaults and command-line options."),
        )
}

fn main() -> Result<(), Box<dyn Error>> {
    let options = app().get_matches();

    // Convert options we will consult multiple times.
    let display_size = parse_dimensions(options.value_of("display_size").unwrap()).unwrap();
    let graphics_type = value_t!(options, "graphics", GraphicsType).unwrap_or_else(|e| e.exit());
    let universe_template =
        value_t!(options, "template", UniverseTemplate).unwrap_or_else(|e| e.exit());

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
        load_config().expect("Error loading configuration files")
    };

    let start_time = Instant::now();
    let mut app = AllIsCubesAppState::new(universe_template.clone());
    app.graphics_options_mut().set(graphics_options);
    let app_done_time = Instant::now();
    log::debug!(
        "Initialized game state with {:?} ({:.3} s)",
        universe_template,
        app_done_time.duration_since(start_time).as_secs_f32()
    );

    if options.is_present("precompute_light") || graphics_type == GraphicsType::Record {
        evaluate_light_with_progress(&mut app.character().unwrap().borrow().space.borrow_mut());
    }

    match graphics_type {
        GraphicsType::Window => glfw_main_loop(app, TITLE, display_size),
        GraphicsType::Terminal => terminal_main_loop(app, TerminalOptions::default()),
        GraphicsType::Record => record_main(app, parse_record_options(options, display_size)?),
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

fn parse_dimensions(input: &str) -> Result<Option<Vector2<u32>>, String> {
    if input.to_ascii_lowercase() == "auto" {
        Ok(None)
    } else {
        let dims: [u32; 2] = input
            .split(&['×', 'x', ',', ';', ' '][..])
            .map(|s| {
                s.parse::<u32>()
                    .map_err(|_| format!("{:?} not an integer or \"auto\"", s))
            })
            .collect::<Result<Vec<u32>, String>>()?
            .try_into()
            .map_err(|_| String::from("must be two integers or \"auto\""))?;
        Ok(Some(Vector2::from(dims)))
    }
}

// TODO: write tests for this (requires factoring out the App definition from main)
fn parse_record_options(
    options: clap::ArgMatches<'_>,
    display_size: Option<Vector2<u32>>,
) -> Result<RecordOptions, clap::Error> {
    Ok(RecordOptions {
        output_path: PathBuf::from(options.value_of_os("output").unwrap()),
        image_size: display_size.unwrap_or_else(|| Vector2::new(640, 480)),
        animation: match value_t!(options, "duration", f64) {
            Ok(duration) => Some(RecordAnimationOptions {
                frame_count: ((duration * 60.0).round() as usize).max(1),
                frame_period: Duration::from_nanos((1e9 / 60.0) as u64),
            }),
            Err(clap::Error {
                kind: ErrorKind::ArgumentNotFound,
                ..
            }) => None,
            Err(e) => return Err(e),
        },
    })
}

fn load_config() -> Result<GraphicsOptions, Box<dyn Error>> {
    // TODO: make testable
    let project_dirs = ProjectDirs::from("org.switchb", "", "all-is-cubes")
        .ok_or_else(|| <Box<dyn Error>>::from("could not find configuration directory"))?;
    create_dir_all(project_dirs.config_dir())?;

    let graphics_options = read_or_create_default_json_file(
        "graphics options",
        &project_dirs.config_dir().join("graphics.json"),
        GraphicsOptions::default,
    );

    Ok(graphics_options)
}

fn read_or_create_default_json_file<V: DeserializeOwned + Serialize>(
    description: &str,
    path: &Path,
    default: fn() -> V,
) -> V {
    match File::open(path) {
        Ok(file) => match serde_json::from_reader(BufReader::new(file)) {
            Ok(value) => {
                log::trace!("Loaded {} from {}", description, path.to_string_lossy());
                value
            }
            Err(e) => {
                log::warn!(
                    "Syntax error in {} loaded from {}; using default values. Error: {}",
                    description,
                    path.to_string_lossy(),
                    e
                );
                default()
            }
        },
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            log::info!(
                "No {} file found; creating {}",
                description,
                path.to_string_lossy()
            );
            let value = default();
            let json_text = serde_json::to_string_pretty(&value).unwrap();
            std::fs::write(path, json_text.as_bytes()).expect("Error writing default file");
            value
        }
        Err(e) => {
            log::error!(
                "Error while reading {} file {}: {}",
                description,
                path.to_string_lossy(),
                e
            );
            default()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_dimensions() {
        let err = |s: &str| Err(s.to_owned());
        assert_eq!(parse_dimensions("1,2"), Ok(Some(Vector2::new(1, 2))));
        assert_eq!(parse_dimensions("30x93"), Ok(Some(Vector2::new(30, 93))));
        assert_eq!(parse_dimensions("30×93"), Ok(Some(Vector2::new(30, 93))));
        assert_eq!(parse_dimensions(""), err("\"\" not an integer or \"auto\""));
        assert_eq!(
            parse_dimensions("1"),
            err("must be two integers or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("a"),
            err("\"a\" not an integer or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("1a1"),
            err("\"1a1\" not an integer or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("1×1×1"),
            err("must be two integers or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("a×b"),
            err("\"a\" not an integer or \"auto\"")
        );
        assert_eq!(
            parse_dimensions("1×b"),
            err("\"b\" not an integer or \"auto\"")
        );
    }
}
