use std::collections::HashSet;
use std::sync::{OnceLock, Weak};

use cfg_if::cfg_if;
#[cfg(target_family = "wasm")]
use futures_util::StreamExt as _;

/// Start polling the given [`wgpu::Device`] until it is dropped or its queue is empty,
/// as reported by [`wgpu::Device::poll()`], to ensure that callbacks are invoked promptly.
///
/// This should be called immediately *after* each operation of interest, such as
/// [`wgpu::BufferView::map_async()`].
///
/// Calling this function again with the same device will not create redundant work.
/// It may block briefly if demand is high or if a callback function takes a long time
/// (which they should avoid).
pub(crate) fn ensure_polled(device: Weak<wgpu::Device>) {
    // (The wgpu docs say polling is automatic “on web” but they mean WebGPU, not WebGL,
    // so we need to do this on all platforms.)
    inner::ensure_polled(device)
}

#[cfg(not(target_family = "wasm"))]
mod inner {
    use super::*;

    static POLLER_CHANNEL: OnceLock<flume::Sender<Weak<wgpu::Device>>> = OnceLock::new();

    pub(super) fn ensure_polled(device: Weak<wgpu::Device>) {
        POLLER_CHANNEL
            .get_or_init(init_poller_task)
            .send(device)
            .expect("shouldn't happen: wgpu channel poller task is dead")
    }

    fn init_poller_task() -> flume::Sender<Weak<wgpu::Device>> {
        let (tx, rx) = flume::bounded(4);
        std::thread::Builder::new()
            .name("all-is-cubes wgpu poller".into())
            // We do not need to use any fancier async runtime, because the only thing
            // polling_task() does is read a channel, and the only reason this is async is for
            // wasm compatibility.
            .spawn(move || pollster::block_on(polling_task(rx)))
            .expect("shouldn't happen: wgpu channel poller task is dead");
        tx
    }
}

#[cfg(target_family = "wasm")]
mod inner {
    use super::*;

    thread_local! {
        /// On Wasm there is only one thread (or more precisely, `wgpu` only supports running in the
        /// event loop thread, not a worker), so we make the channel thread-local to avoid the unmet
        /// bound `wgpu::Device: Send`.
        static POLLER_CHANNEL: OnceLock<flume::Sender<Weak<wgpu::Device>>> =
            const { OnceLock::new() };
    }

    pub(super) fn ensure_polled(device: Weak<wgpu::Device>) {
        POLLER_CHANNEL.with(|ch| ch.get_or_init(init_poller_task).send(device).unwrap())
    }

    fn init_poller_task() -> flume::Sender<Weak<wgpu::Device>> {
        // Using unbounded channel does no harm since we have only one thread, so
        // cannot have any relevantly unfair scheduling between sender and receiver.
        let (tx, rx) = flume::unbounded();
        wasm_bindgen_futures::spawn_local(polling_task(rx));
        tx
    }
}

/// Polls all [`wgpu::Device`]s delivered to it on `POLLING_CHANNEL`.
/// To be run on a thread or async task as the platform permits.
#[allow(clippy::infinite_loop)]
async fn polling_task(rx: flume::Receiver<Weak<wgpu::Device>>) {
    // 10 milliseconds is much better than acceptable latency for our applications, which are all
    // non-realtime headless rendering.
    let polling_interval = core::time::Duration::from_millis(10);

    let mut to_poll: HashSet<WeakIdentityDevice> = HashSet::new();
    let mut to_drop: Vec<WeakIdentityDevice> = Vec::new();
    #[cfg(target_family = "wasm")]
    let mut tick_stream =
        gloo_timers::future::IntervalStream::new(polling_interval.as_millis().try_into().unwrap())
            .fuse();

    loop {
        // Poll all the devices to poll.
        // eprintln!("poller: polling {}", to_poll.len());
        for device_ref in to_poll.iter() {
            match device_ref.0.upgrade() {
                Some(device) => {
                    // Kludge: As of wgpu 0.18, using Maintain::Poll doesn't actually have any
                    // effect (doesn't cause the map callbacks to run) on WebGL on Firefox.
                    // So, for now, use Maintain::Wait (which also doesn't actually do any waiting
                    // (wgpu bug), but *does* trigger the callbacks).
                    // TODO: See if we can remove this in the next wgpu version.
                    let maintain = if cfg!(target_family = "wasm") {
                        wgpu::Maintain::Wait
                    } else {
                        wgpu::Maintain::Poll
                    };

                    let queue_empty = device.poll(maintain).is_queue_empty();
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
            rx.recv_async().await.map_err(flume::RecvTimeoutError::from)
        } else {
            cfg_if! {
                if #[cfg(target_family = "wasm")] {
                    // On wasm, we must not block, which means we must instead use async timers
                    futures_util::select! {
                        result = rx.recv_async() => {
                            result.map_err(flume::RecvTimeoutError::from)
                        }
                        _ = tick_stream.next() => {
                            Err(flume::RecvTimeoutError::Timeout)
                        }
                    }
                } else {
                    // The function is async, but in this case, we are running it in a dedicated
                    // thread, so it's okay to block and saves us needing to pull in another
                    // async timer implementation for non-Wasm targets.
                    rx.recv_timeout(polling_interval)
                }
            }
        };

        match recv_result {
            Ok(device) => {
                // eprintln!("poller: got another device");
                to_poll.insert(WeakIdentityDevice(device));
            }
            Err(flume::RecvTimeoutError::Disconnected) => {
                // This shouldn't happen because the sender is never dropped
                log::warn!("shouldn't happen: wgpu poller channel disconnected");
            }
            Err(flume::RecvTimeoutError::Timeout) => {
                // eprintln!("poller: time to poll");
                // continue to poll
            }
        }
    }
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
