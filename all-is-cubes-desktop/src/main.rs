// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Binary for All is Cubes desktop app.

#![deny(rust_2018_idioms)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]

use cgmath::Vector2;
use clap::{value_t, Arg, ErrorKind};
use std::convert::TryInto;
use std::error::Error;
use std::path::PathBuf;
use std::time::{Duration, Instant};
use strum::IntoEnumIterator;

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::content::UniverseTemplate;

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

fn main() -> Result<(), Box<dyn Error>> {
    // TODO: put version numbers in the title when used as a window title
    let title = "All is Cubes";

    let options = clap::App::new(title)
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
        .get_matches();

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

    let start_time = Instant::now();
    let mut app = AllIsCubesAppState::new(universe_template.clone());
    let app_done_time = Instant::now();
    log::debug!(
        "Initialized game state with {:?} ({:.3} s)",
        universe_template,
        app_done_time.duration_since(start_time).as_secs_f32()
    );

    match graphics_type {
        GraphicsType::Window => glfw_main_loop(app, title, display_size),
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
