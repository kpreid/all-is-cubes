//! “Hot-reloadable” data sources such as shaders.
//!
//! This module builds on top of the `resource` library to add change notification
//! via a background thread and all-is-cubes's `ListenableSource` mechanism.

use std::sync::{Arc, Mutex, PoisonError};
use std::time::Duration;

use once_cell::sync::Lazy;
use resource::Resource;

use all_is_cubes::listen::{ListenableCell, ListenableSource};

#[derive(Clone)]
pub(crate) struct Reloadable(Arc<ReloadableInner>);

pub(crate) struct ReloadableInner {
    resource: Mutex<Resource<str>>,
    cell: ListenableCell<Arc<str>>,
}

/// `file_path` is relative to the Cargo package root.
macro_rules! reloadable_str {
    ($file_path:literal) => {
        $crate::reloadable::Reloadable::new(::resource::resource_str!($file_path))
    };
}
pub(crate) use reloadable_str;

impl Reloadable {
    pub fn new(resource: Resource<str>) -> Self {
        let this = Reloadable(Arc::new(ReloadableInner {
            cell: ListenableCell::new(res_arc_str(&resource)),
            resource: Mutex::new(resource),
        }));

        // TODO: make this optional if we ever care
        if let Ok(mut reloadables) = POLLED_RELOADABLES.lock() {
            reloadables.push(this.clone());
        }

        this
    }

    pub fn poll(&self) -> bool {
        match self.0.resource.lock().as_deref_mut() {
            Ok(resource) => {
                let changed = resource.reload_if_changed();
                if changed {
                    // unfortunately ListenableCell wants an Arc with Sized contents
                    self.0.cell.set(res_arc_str(resource));
                }
                changed
            }
            Err(PoisonError { .. }) => false,
        }
    }

    pub fn as_source(&self) -> ListenableSource<Arc<str>> {
        self.0.cell.as_source()
    }
}

fn res_arc_str(r: &Resource<str>) -> Arc<str> {
    Arc::from(<Resource<str> as AsRef<str>>::as_ref(r))
}

static POLLED_RELOADABLES: Lazy<Mutex<Vec<Reloadable>>> = Lazy::new(|| {
    // Spawn a thread to poll for reloading, as soon as anyone cares,
    // but only if it's possible and useful.
    if cfg!(all(not(target_family = "wasm"), debug_assertions)) {
        std::thread::spawn(poll_reloadables_loop);
    }

    Mutex::new(Vec::new())
});

fn poll_reloadables_loop() {
    loop {
        std::thread::sleep(Duration::from_secs(1));

        // If this expect fails it'll just kill the thread
        let v = POLLED_RELOADABLES
            .lock()
            .expect("could not lock POLLED_RELOADABLES from polling thread");
        let mut any = false;
        for reloadable in v.iter() {
            any |= reloadable.poll();
        }
        if any {
            log::debug!("Reloadable polled and found changed");
        }
    }
}
