use std::sync::Weak;

/// Start polling the given [`wgpu::Device`] until it is dropped or its queue is empty,
/// as reported by [`wgpu::Device::poll()`], to ensure that callbacks are invoked promptly.
///
/// This should be called immediately *after* each operation of interest, such as
/// [`wgpu::BufferView::map_async()`].
///
/// Calling this function again with the same device will not create redundant work.
/// It may block briefly if demand is high or if a callback function takes a long time
/// (which they should avoid).
pub(crate) fn ensure_polled(#[allow(unused)] device: Weak<wgpu::Device>) {
    // TODO: Implement async version for web. Right now, this is blocking.
    // (The wgpu docs say polling is automatic “on web” but they mean WebGPU, not WebGL.)
    #[cfg(target_family = "wasm")]
    if let Some(device) = device.upgrade() {
        device.poll(wgpu::Maintain::Wait);
    }

    #[cfg(not(target_family = "wasm"))]
    inner::ensure_polled(device);
}

#[cfg(not(target_family = "wasm"))]
mod inner {
    use std::collections::HashSet;
    use std::sync::{OnceLock, Weak};
    use std::time::Duration;

    /// Crossbeam, unlike `std::sync::mpsc`, has a `Sender` that is `Sync`.
    use crossbeam_channel as channel;

    pub(crate) fn ensure_polled(device: Weak<wgpu::Device>) {
        POLLER_THREAD
            .get_or_init(init_poller_thread)
            .send(device)
            .unwrap()
    }

    static POLLER_THREAD: OnceLock<channel::Sender<Weak<wgpu::Device>>> = OnceLock::new();

    fn init_poller_thread() -> channel::Sender<Weak<wgpu::Device>> {
        let (tx, rx) = channel::bounded(4);
        std::thread::Builder::new()
            .name("all-is-cubes wgpu poller".into())
            .spawn(move || {
                let mut to_poll: HashSet<WeakIdentityDevice> = HashSet::new();
                let mut to_drop: Vec<WeakIdentityDevice> = Vec::new();
                loop {
                    // Poll all the devices to poll.
                    // eprintln!("poller: polling {}", to_poll.len());
                    for device_ref in to_poll.iter() {
                        match device_ref.0.upgrade() {
                            Some(device) => {
                                let queue_empty = device.poll(wgpu::Maintain::Poll);
                                if queue_empty {
                                    to_drop.push(device_ref.clone());
                                }
                            }
                            None => to_drop.push(device_ref.clone()),
                        }
                    }

                    // Remove no-longer-necessary entries.
                    // if !to_drop.is_empty() {
                    //     eprintln!("poller: dropping {}", to_drop.len());
                    // }
                    for device_ref in to_drop.drain(..) {
                        to_poll.remove(&device_ref);
                    }

                    // While waiting for it to be time to poll again, check for incoming requests.
                    let recv_result = if to_poll.is_empty() {
                        // If we have nothing to poll, then we don't need to wake up.
                        // eprintln!("poller: sleeping indefinitely");
                        rx.recv().map_err(channel::RecvTimeoutError::from)
                    } else {
                        rx.recv_timeout(Duration::from_millis(1))
                    };

                    match recv_result {
                        Ok(device) => {
                            // eprintln!("poller: got another device");
                            to_poll.insert(WeakIdentityDevice(device));
                        }
                        Err(channel::RecvTimeoutError::Disconnected) => {
                            log::warn!("shouldn't happen: wgpu poller thread disconnected");
                        }
                        Err(channel::RecvTimeoutError::Timeout) => {
                            // eprintln!("poller: time to poll");
                            // continue to poll
                        }
                    }
                }
            })
            .unwrap();
        tx
    }

    /// Compare a `Weak<wgpu::Device>` by pointer identity.
    #[derive(Clone, Debug)]
    struct WeakIdentityDevice(Weak<wgpu::Device>);

    impl PartialEq for WeakIdentityDevice {
        fn eq(&self, other: &Self) -> bool {
            Weak::ptr_eq(&self.0, &other.0)
        }
    }
    impl Eq for WeakIdentityDevice {}
    impl std::hash::Hash for WeakIdentityDevice {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            let ptr: *const wgpu::Device = self.0.as_ptr();
            ptr.hash(state);
        }
    }
}
