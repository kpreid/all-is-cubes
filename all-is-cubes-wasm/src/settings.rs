use std::sync::Arc;

use send_wrapper::SendWrapper;
use web_sys::window;

use all_is_cubes_render::camera::GraphicsOptions;
use all_is_cubes_ui::apps::Settings;

/// `localStorage` key we stash the graphics options in.
///
/// This is namespace-prefixed to reduce the chances of conflicting with e.g.
/// other servers run on random `localhost` ports.
///
/// When we have The Real Settings System and no longer use this, we should remember to
/// migrate and delete it.
const GRAPHICS_OPTIONS_KEY: &str = "all-is-cubes.settings.graphics-options";

/// Returns `None` if access to `localStorage` failed for any reason.
fn load_settings_from_local_storage() -> Option<Settings> {
    let storage: web_sys::Storage = window()?.local_storage().ok()??;

    let initial_data: GraphicsOptions = match storage.get_item(GRAPHICS_OPTIONS_KEY).ok()? {
        Some(data_string) => match serde_json::from_str(&data_string) {
            Ok(value) => {
                log::trace!("Loaded settings from {GRAPHICS_OPTIONS_KEY}");
                value
            }
            Err(e) => {
                log::warn!(
                    "Syntax error in settings loaded from {GRAPHICS_OPTIONS_KEY}; using default values. Error: {e}",
                );
                GraphicsOptions::default()
            }
        },
        None => {
            log::trace!("No settings found in {GRAPHICS_OPTIONS_KEY}; using defaults");
            GraphicsOptions::default()
        }
    };

    let storage = SendWrapper::new(storage);

    Some(Settings::with_persistence(
        Arc::new(initial_data),
        Arc::new(
            move |data: &Arc<GraphicsOptions>| match serde_json::to_string(data) {
                Ok(data_string) => match storage.set_item(GRAPHICS_OPTIONS_KEY, &data_string) {
                    Ok(()) => log::trace!("Stored settings to {GRAPHICS_OPTIONS_KEY}"),
                    Err(e) => log::error!("Failed to store settings: {e:?}"),
                },
                Err(e) => log::error!("Failed to serialize settings: {e}"),
            },
        ),
    ))
}

pub(crate) fn load_settings_from_local_storage_if_possible() -> Settings {
    load_settings_from_local_storage().unwrap_or_default()
}
