use std::sync::Arc;

use send_wrapper::SendWrapper;
use web_sys::window;

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

    let len = storage.length().unwrap();
    let initial_data: Data = Data::from_iter((0..len).into_iter().filter_map(
        |i: u32| -> Option<(ArcStr, ArcStr)> {
            let storage_key: String = storage.key(i).unwrap().unwrap();
            if let Some(settings_key) = storage_key.strip_prefix(PREFIX) {
                let value = ArcStr::from(storage.get_item(&storage_key).unwrap().unwrap());
                Some((ArcStr::from(settings_key), value))
            } else {
                None
            }
        },
    ));

    log::trace!("Loaded settings: {initial_data:?}");

    let storage = SendWrapper::new(storage);

    Some(Settings::with_persistence(
        initial_data,
        Arc::new(move |data: &Data| {
            for (key, value) in data.iter_set() {
                let storage_key = format!("{PREFIX}{key}");
                match storage.set_item(&storage_key, value.as_str()) {
                    Ok(()) => log::trace!("Stored {storage_key}"),
                    Err(e) => log::error!("Failed to store setting: {e:?}"),
                }
            }
        }),
    ))
}

pub(crate) fn load_settings_from_local_storage_if_possible() -> Settings {
    load_settings_from_local_storage().unwrap_or_default()
}
