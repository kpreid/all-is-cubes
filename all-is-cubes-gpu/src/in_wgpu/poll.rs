use alloc::sync::{Arc, Weak};
use alloc::vec::Vec;
use std::sync::OnceLock;

use cfg_if::cfg_if;
#[cfg(target_family = "wasm")]
use futures_util::StreamExt as _;
use hashbrown::HashMap;

/// Start polling the given [`wgpu::Device`] in the background until the returned guard is dropped.
///
/// Calling this function again with the same device will not create redundant work.
/// It may block briefly if demand is high or if a callback function takes a long time
/// (which they should avoid).
pub(crate) fn start_polling(device: wgpu::Device) -> Poller {
    let poller = Poller(Arc::new(device));

    // (The wgpu docs say polling is automatic “on web” but they mean WebGPU, not WebGL,
    // so we need to do this on all platforms.)
    inner::send_to_poller_task(Arc::downgrade(&poller.0));

    poller
}

/// As long as this guard is not dropped,
/// the contained [`wgpu::Device`] will be polled periodically.
#[derive(Debug)]
pub(crate) struct Poller(Arc<wgpu::Device>);

#[cfg(not(target_family = "wasm"))]
mod inner {
    use super::*;

    static POLLER_CHANNEL: OnceLock<flume::Sender<Weak<wgpu::Device>>> = OnceLock::new();

    pub(super) fn send_to_poller_task(device: Weak<wgpu::Device>) {
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

    std::thread_local! {
        /// On Wasm there is only one thread (or more precisely, `wgpu` only supports running in the
        /// event loop thread, not a worker), so we make the channel thread-local to avoid the unmet
        /// bound `wgpu::Device: Send`.
        static POLLER_CHANNEL: OnceLock<flume::Sender<Weak<wgpu::Device>>> =
            const { OnceLock::new() };
    }

    pub(super) fn send_to_poller_task(device: Weak<wgpu::Device>) {
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

/// Polls all [`wgpu::Device`]s delivered to it on `POLLING_CHANNEL`,
/// as long as each device has at least one weak handle to it.
/// To be run on a thread or async task as the platform permits.
#[expect(clippy::infinite_loop)]
async fn polling_task(rx: flume::Receiver<Weak<wgpu::Device>>) {
    // 10 milliseconds is much better than acceptable latency for our applications, which are all
    // non-realtime headless rendering.
    let polling_interval = core::time::Duration::from_millis(10);

    // We want to poll each device only once per polling interval, but there may be multiple
    // requests to poll the same device. This table has devices as keys and requests as values.
    let mut to_poll: HashMap<wgpu::Device, Vec<Weak<wgpu::Device>>> = HashMap::new();

    #[cfg(target_family = "wasm")]
    let mut tick_stream =
        gloo_timers::future::IntervalStream::new(polling_interval.as_millis().try_into().unwrap())
            .fuse();

    loop {
        // Prune dropped requests and their devices.
        to_poll.retain(|_, requests| {
            requests.retain(|request| Weak::upgrade(request).is_some());
            !requests.is_empty()
        });

        // Poll all the devices to poll.
        // eprintln!("poller: polling {}", to_poll.len());
        for device in to_poll.keys() {
            device
                .poll(wgpu::PollType::Poll)
                .expect("unreachable: poll() timed out despite not using timeout");
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
            Ok(request) => {
                if let Some(device) = request.upgrade() {
                    // eprintln!("poller: got request for {device:?}");
                    to_poll
                        .entry(wgpu::Device::clone(&*device))
                        .or_default()
                        .push(request);
                }
            }
            Err(flume::RecvTimeoutError::Disconnected) => {
                // This shouldn't happen because the sender is static and never dropped
                log::warn!("shouldn't happen: wgpu poller channel disconnected");
            }
            Err(flume::RecvTimeoutError::Timeout) => {
                // eprintln!("poller: time to poll");
                // continue to poll
            }
        }
    }
}
