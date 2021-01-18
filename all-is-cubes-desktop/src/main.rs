// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Binary for All is Cubes desktop app.

#![warn(clippy::cast_lossless)]

use clap::{arg_enum, value_t, Arg};
use std::error::Error;
use std::time::Instant;

use all_is_cubes::apps::AllIsCubesAppState;

mod aic_glfw;
use aic_glfw::glfw_main_loop;
mod terminal;
use terminal::terminal_main_loop;

arg_enum! {
    #[derive(Debug, PartialEq)]
    pub enum GraphicsType {
        Headless,
        Window,
        Terminal
    }
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
                .possible_values(
                    &GraphicsType::variants()
                        .iter()
                        .map(|s| s.to_ascii_lowercase())
                        .collect::<Vec<_>>() // makes the strings live long enough
                        .iter()
                        .map(String::as_ref)
                        .collect::<Vec<_>>(),
                )
                .case_insensitive(true)
                .default_value("window")
                .help("Graphics/UI mode."),
        )
        .get_matches();

    let mut app = AllIsCubesAppState::new();
    match value_t!(options, "graphics", GraphicsType).unwrap_or_else(|e| e.exit()) {
        GraphicsType::Window => glfw_main_loop(app, title),
        GraphicsType::Terminal => terminal_main_loop(app),
        GraphicsType::Headless => {
            // TODO: Right now this is useless. Eventually, we may have other paths for side
            // effects from the universe, or interesting logging.
            eprintln!("Simulating a universe nobody's looking at...");

            loop {
                // TODO: sleep instead of spinning, and maybe put a general version of this in AllIsCubesAppState.
                app.frame_clock.advance_to(Instant::now());
                app.maybe_step_universe();
            }
        }
    }
}
