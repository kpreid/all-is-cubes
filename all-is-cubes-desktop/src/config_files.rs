// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use std::error::Error;
use std::fs::create_dir_all;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use directories_next::ProjectDirs;
use serde::{de::DeserializeOwned, Serialize};

use all_is_cubes::camera::GraphicsOptions;

pub fn load_config() -> Result<GraphicsOptions, Box<dyn Error>> {
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
