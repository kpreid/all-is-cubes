use std::sync::Arc;

use js_sys::JsString;
use send_wrapper::SendWrapper;
use web_sys::{console, window};

use all_is_cubes::arcstr::ArcStr;
use all_is_cubes_ui::settings::{Data, Settings};

/// `localStorage` key prefix we stash our settings values in.
///
/// This is namespace-prefixed to reduce the chances of conflicting with e.g.
/// other servers run on random `localhost` ports.
const PREFIX: &str = "all-is-cubes.settings.";

/// Returns `None` if access to `localStorage` failed for any reason.
fn load_settings_from_local_storage() -> Option<Settings> {
    let storage: web_sys::Storage = window()?.local_storage().ok()??;

    let len = storage.length().ok()?;
    let initial_data: Data =
        Data::from_iter((0..len).filter_map(|i: u32| -> Option<(ArcStr, ArcStr)> {
            match storage.key(i) {
                Err(error) => {
                    console::warn_2(&JsString::from("localStorage.key() failed: %o"), &error);
                    None
                }
                Ok(None) => None, // length changed while iterating?
                Ok(Some(storage_key)) => {
                    if let Some(settings_key) = storage_key.strip_prefix(PREFIX) {
                        match storage.get_item(&storage_key) {
                            Err(error) => {
                                console::warn_2(
                                    &JsString::from("localStorage.getItem() failed: %o"),
                                    &error,
                                );
                                None
                            }
                            Ok(None) => None, // value deleted while iterating?
                            Ok(Some(storage_value_string)) => {
                                let value = ArcStr::from(storage_value_string);
                                Some((ArcStr::from(settings_key), value))
                            }
                        }
                    } else {
                        None
                    }
                }
            }
        }));

    log::trace!("Loaded settings: {initial_data:?}");

    let storage = SendWrapper::new(storage);

    Some(Settings::with_persistence(
        initial_data,
        Arc::new(move |data: &Data| {
            for (key, value) in data.iter_set() {
                let storage_key = format!("{PREFIX}{key}");
                match storage.set_item(&storage_key, value.as_str()) {
                    Ok(()) => log::trace!("Stored {storage_key}"),
                    Err(error) => {
                        console::error_2(&JsString::from("Failed to store setting: %o"), &error)
                    }
                }
            }
        }),
    ))
}

pub(crate) fn load_settings_from_local_storage_if_possible() -> Settings {
    load_settings_from_local_storage().unwrap_or_default()
}
