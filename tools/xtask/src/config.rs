use std::fs;
use std::io::{self, Write as _};
use std::path::PathBuf;

use anyhow::Context as _;

use crate::ActionError;

// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub(crate) struct Config {
    pub browser: Browser,

    pub verbosity: Verbosity,
}

impl Config {
    fn new_from_env() -> Self {
        Self {
            browser: if option_env!("CI").is_some() && cfg!(target_os = "macos") {
                // 2024-04: GitHub's new Apple Silicon macOS CI images don't have Firefox,
                // only Chrome and Safari.
                Browser::Chrome
            } else {
                Browser::Firefox
            },
            verbosity: Verbosity {},
        }
    }
}

/// One of the browsers supported by `wasm-pack test`
#[derive(Clone, Copy, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum Browser {
    /// Don’t run browser tests.
    Skip,
    Chrome,
    Firefox,
    Safari,
}

impl Browser {
    pub fn to_wasm_pack_argument(self) -> Option<&'static str> {
        Some(match self {
            Browser::Skip => return None,
            Browser::Chrome => "--chrome",
            Browser::Firefox => "--firefox",
            Browser::Safari => "--safari",
        })
    }
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub(crate) struct Verbosity {
    // TODO
}

// -------------------------------------------------------------------------------------------------

fn config_path() -> PathBuf {
    crate::PROJECT_DIR.join("tools/xtask-config.toml")
}

pub(crate) fn load_user_config() -> Result<Config, ActionError> {
    let path = config_path();

    match fs::read_to_string(&path) {
        Ok(contents) => {
            let config: Config = toml_edit::de::from_str(&contents).with_context(|| {
                format!("failed to parse configuration file “{}”", path.display())
            })?;
            Ok(config)
        }
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(Config::new_from_env()),
        Err(error) => Err(ActionError::from(error).context(format!(
            "failed to read configuration file “{}”",
            path.display()
        ))),
    }
}

pub(crate) fn create_user_config_if_not_present(config: &Config) -> Result<(), ActionError> {
    let path = config_path();
    match fs::File::options().write(true).create_new(true).open(&path) {
        Ok(mut file) => {
            let text = toml_edit::ser::to_string_pretty(config)
                .context("failed to serialize configuration")?;
            file.write_all(text.as_bytes()).with_context(|| {
                format!("failed to write configuration file “{}”", path.display())
            })?;
            eprintln!(
                "[xtask] Created user configuration file “{}”",
                path.display()
            );
        }
        Err(error) if error.kind() == io::ErrorKind::AlreadyExists => {}
        Err(error) => {
            return Err(anyhow::Error::new(error)
                .context(format!("creating configuration file “{}”", path.display())));
        }
    }

    Ok(())
}
