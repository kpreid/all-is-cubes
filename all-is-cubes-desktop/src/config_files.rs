use std::fs::create_dir_all;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use directories_next::ProjectDirs;
use serde::{de::DeserializeOwned, Serialize};

use all_is_cubes_render::camera::GraphicsOptions;

/// Load preferences/settings/config files from a platform-appropriate read/write location.
pub fn load_config() -> Result<GraphicsOptions, anyhow::Error> {
    // TODO: make testable
    // TODO: allow users of this library function to pick their own config dir

    if std::env::var("AIC_DO_NOT_USE_CONFIG_FILES_IN_TESTS").is_ok() {
        panic!("tests should be hermetic and not touch user config files \
        (environment variable AIC_DO_NOT_USE_CONFIG_FILES_IN_TESTS set but --no-config-files not passed)");
    }

    let project_dirs = ProjectDirs::from("org.switchb", "", "all-is-cubes")
        .ok_or_else(|| anyhow::anyhow!("could not find configuration directory"))?;
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
