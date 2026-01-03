use std::fs;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use std::sync::Arc;

use anyhow::Context as _;
use directories_next::ProjectDirs;
use serde::{Serialize, de::DeserializeOwned};

use all_is_cubes_render::camera::GraphicsOptions;
use all_is_cubes_ui::settings::Settings;

// -------------------------------------------------------------------------------------------------

/// Load preferences/settings/config files from a platform-appropriate read/write location.
///
/// This does not respect command-line options. Use [`SettingsArgs`] for that.
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

// -------------------------------------------------------------------------------------------------

/// [`clap::Args`] argument group struct for args that affect what settings are used.
#[derive(Clone, Debug, clap::Args)]
pub struct SettingsArgs {
    /// Ignore all configuration files, using only defaults and command-line options.
    #[arg(long = "no-config-files")]
    pub(crate) no_config_files: bool,

    #[expect(clippy::doc_markdown, reason = "will be displayed in --help")]
    /// Override the value of a setting for this session, instead of taking it from files
    /// or defaults.
    ///
    /// The value is specified as a key-value pair where the key is an unquoted string, the
    /// separator is “=”, and the value is a JSON value (which, if a string, must be quoted);
    /// for example: -Slighting_display='"None"'
    ///
    /// Using this option disables writing settings back to disk.
    #[arg(long = "set", short = 'S', value_parser = parse_configure, value_name="NAME=JSON")]
    pub(crate) set: Vec<(String, serde_json::Value)>,
}

impl SettingsArgs {
    /// Constructs the [`Settings`] a session with these args should use.
    pub fn build_settings(self) -> Result<Settings, anyhow::Error> {
        let Self {
            no_config_files,
            set: to_override,
        } = self;

        let persisted_settings = if no_config_files {
            Settings::new(Arc::new(GraphicsOptions::default()))
        } else {
            load_config().context("Error loading configuration files")?
        };

        let session_settings = Settings::inherit(persisted_settings);

        if !to_override.is_empty() {
            session_settings.disinherit();
            // TODO: this is weirdly specific because right now, graphics options are the *only*
            // settings. This code *should* change when we have more settings and the data model
            // is more like a generic key-value store.
            let Ok(serde_json::Value::Object(mut current_settings)) =
                serde_json::to_value(session_settings.get_graphics_options())
            else {
                unreachable!("graphis options should appear as a json object");
            };
            for (key, value) in to_override {
                current_settings.insert(key, value);
            }
            session_settings.set_graphics_options(
                serde_json::from_value(serde_json::Value::Object(current_settings))
                    .context("--set did not produce valid settings")?,
            );
        }

        Ok(session_settings)
    }
}

fn parse_configure(arg: &str) -> Result<(String, serde_json::Value), anyhow::Error> {
    let (key, value) = arg.split_once('=').ok_or_else(|| anyhow::anyhow!("missing '='"))?;
    let value = serde_json::from_str(value)?;
    Ok((key.to_owned(), value))
}

// -------------------------------------------------------------------------------------------------

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
        Err(open_for_read_error) if open_for_read_error.kind() == std::io::ErrorKind::NotFound => {
            log::info!(
                "No {description} file found; creating {path}",
                path = path.to_string_lossy()
            );
            let value = default();
            let json_text = serde_json::to_string_pretty(&value).unwrap();
            match fs::write(path, json_text.as_bytes()) {
                Ok(()) => log::trace!(
                    "Wrote default {description} to {path}",
                    path = path.to_string_lossy()
                ),
                Err(write_error) => {
                    log::error!(
                        "Error while writing default {description} file {path}: {write_error}",
                        path = path.to_string_lossy(),
                    );
                }
            }
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
