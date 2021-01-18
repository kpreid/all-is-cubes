// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Binary for All is Cubes desktop app.

use std::error::Error;

use all_is_cubes::apps::AllIsCubesAppState;

mod aic_glfw;
use aic_glfw::glfw_main_loop;

fn main() -> Result<(), Box<dyn Error>> {
    // TODO: add command line argument processing (display options, initial world options)

    let app = AllIsCubesAppState::new();
    // TODO: put version numbers in the title
    glfw_main_loop(app, "All is Cubes")
}
