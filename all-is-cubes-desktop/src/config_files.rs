use std::fs;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::sync::Arc;

use all_is_cubes_ui::apps::Settings;
use directories_next::ProjectDirs;
use serde::{de::DeserializeOwned, Serialize};

use all_is_cubes_render::camera::GraphicsOptions;

/// Load preferences/settings/config files from a platform-appropriate read/write location.
pub fn load_config() -> Result<Settings, anyhow::Error> {
    // TODO: make testable
    // TODO: allow users of this library function to pick their own config dir

    if std::env::var("AIC_DO_NOT_USE_CONFIG_FILES_IN_TESTS").is_ok() {
        panic!(
            "tests should be hermetic and not touch user config files \
            (environment variable AIC_DO_NOT_USE_CONFIG_FILES_IN_TESTS set, \
            but --no-config-files not passed)"
        );
    }

    let project_dirs = ProjectDirs::from("org.switchb", "", "all-is-cubes")
        .ok_or_else(|| anyhow::anyhow!("could not find configuration directory"))?;
    fs::create_dir_all(project_dirs.config_dir())?;

    let settings_path = project_dirs.config_dir().join("graphics.json");

    let graphics_options = read_or_create_default_json_file(
        "graphics options",
        &settings_path,
        GraphicsOptions::default,
    );

    Ok(Settings::with_persistence(
        Arc::new(graphics_options),
        // TODO: ideally, writes would be performed asynchronously and with rate-limiting
        Arc::new(move |data| {
            write_json_file("graphics options", &settings_path, &data);
        }),
    ))
}

fn read_or_create_default_json_file<V: DeserializeOwned + Serialize>(
    description: &str,
    path: &Path,
    default: fn() -> V,
) -> V {
    match File::open(path) {
        Ok(file) => match serde_json::from_reader(BufReader::new(file)) {
            Ok(value) => {
                log::trace!(
                    "Loaded {description} from {path}",
                    path = path.to_string_lossy()
                );
                value
            }
            Err(e) => {
                log::warn!(
                    "Syntax error in {description} loaded from {path}; \
                    using default values. Error: {e}",
                    path = path.to_string_lossy(),
                );
                default()
            }
        },
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            log::info!(
                "No {description} file found; creating {path}",
                path = path.to_string_lossy()
            );
            let value = default();
            let json_text = serde_json::to_string_pretty(&value).unwrap();
            fs::write(path, json_text.as_bytes()).expect("Error writing default file");
            value
        }
        Err(e) => {
            log::error!(
                "Error while reading {description} file {path}: {e}",
                path = path.to_string_lossy(),
            );
            default()
        }
    }
}

fn write_json_file<V: Serialize>(description: &str, path: &Path, value: &V) {
    match fs::OpenOptions::new().write(true).truncate(true).open(path) {
        Ok(file) => match serde_json::to_writer_pretty(file, &value) {
            Ok(()) => log::trace!(
                "Wrote {description} to {path}",
                path = path.to_string_lossy()
            ),
            Err(e) => {
                log::error!(
                    "Error while writing {description} file {path}: {e}",
                    path = path.to_string_lossy(),
                );
            }
        },
        Err(e) => {
            log::error!(
                "Error while opening {description} file {path}: {e}",
                path = path.to_string_lossy(),
            );
        }
    }
}
