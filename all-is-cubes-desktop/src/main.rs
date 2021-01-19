// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Binary for All is Cubes desktop app.

#![warn(clippy::cast_lossless)]

use clap::{value_t, Arg};
use std::error::Error;
use std::time::Instant;
use strum::IntoEnumIterator;

use all_is_cubes::apps::AllIsCubesAppState;
use all_is_cubes::content::demo::UniverseTemplate;

mod aic_glfw;
use aic_glfw::glfw_main_loop;
mod terminal;
use terminal::terminal_main_loop;

#[derive(Debug, PartialEq, strum::EnumString, strum::EnumIter, strum::IntoStaticStr)]
#[strum(serialize_all = "kebab-case")]
pub enum GraphicsType {
    Headless,
    Window,
    Terminal,
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
                    &GraphicsType::iter()
                        .map(|t| <&str>::from(t))
                        .collect::<Vec<_>>(),
                )
                .default_value("window")
                .help("Graphics/UI mode."),
        )
        .arg(
            Arg::with_name("template")
                .long("template")
                .short("t")
                .possible_values(
                    &UniverseTemplate::iter()
                        .map(|t| <&str>::from(t))
                        .collect::<Vec<_>>(),
                )
                .default_value("demo-city")
                .help("Which world template to use."),
        )
        .get_matches();

    let mut app = AllIsCubesAppState::new(
        value_t!(options, "template", UniverseTemplate).unwrap_or_else(|e| e.exit()),
    );
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
