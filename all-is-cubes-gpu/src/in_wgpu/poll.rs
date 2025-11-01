use alloc::sync::{Arc, Weak};
use alloc::vec::Vec;
use std::sync::OnceLock;

use cfg_if::cfg_if;
#[cfg(target_family = "wasm")]
use futures_util::StreamExt as _;

// -------------------------------------------------------------------------------------------------

/// Common point for defining presence and level of logging of poller activity.
macro_rules! log {
    ($($args:tt)*) => {
        ::log::trace!("[poll] {}", format_args!($($args)*));
    }
}

/// Start polling the given [`wgpu::Device`] in the background until the returned guard is dropped.
///
/// This function may block briefly if demand is high or if a callback function provided to `wgpu`
/// takes a long time (which they should avoid).
// ---
// We used to offer that
// /// Calling this function again with the same device will not create redundant work.
// but <https://github.com/gfx-rs/wgpu/issues/8461> broke that, and it wasn’t ever very useful
// because all of the uses of this are tests that use their device sequentially so there would
// never be concurrent registrations of the same device.
#[must_use = "the returned `Poller` must be kept as long as polling is desired"]
pub(crate) fn start_polling(device: wgpu::Device) -> Poller {
    log!("start_polling({device:?})");

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
        log!(
            "{tid:?} init_poller_task()",
            tid = std::thread::current().id()
        );

        let (tx, rx) = flume::bounded(4);
        std::thread::Builder::new()
            .name("all-is-cubes wgpu poller".into())
            // We do not need to use any fancier async runtime, because the only thing
            // polling_task() does is read a channel, and the only reason this is async is for
            // wasm compatibility.
            .spawn(move || pollster::block_on(polling_task(rx)))
            .expect("failed to spawn wgpu poller task");
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
        log!("init_poller_task()");

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
#[expect(
    clippy::infinite_loop,
    reason = "spawn_local() requires () as return type"
)]
async fn polling_task(rx: flume::Receiver<Weak<wgpu::Device>>) {
    // 10 milliseconds is much better than acceptable latency for our applications, which are all
    // non-realtime headless rendering.
    let polling_interval = core::time::Duration::from_millis(10);

    let mut to_poll: Vec<Weak<wgpu::Device>> = Vec::new();

    #[cfg(target_family = "wasm")]
    let mut tick_stream =
        gloo_timers::future::IntervalStream::new(polling_interval.as_millis().try_into().unwrap())
            .fuse();

    loop {
        // Poll devices and prune ones for which the `Poller` was dropped.
        let count_before_prune = to_poll.len();
        to_poll.retain(|weak_device| {
            let upgrade: Option<Arc<wgpu::Device>> = weak_device.upgrade();
            if let Some(device) = upgrade {
                device
                    .poll(wgpu::PollType::Poll)
                    .expect("unreachable: poll() timed out despite not using timeout");
                true
            } else {
                false
            }
        });
        let count_after_prune = to_poll.len();
        if count_after_prune < count_before_prune {
            log!(
                "PT pruned {} devices",
                count_before_prune - count_after_prune
            );
        }

        // While waiting for it to be time to poll again, check for incoming requests.
        let recv_result = if to_poll.is_empty() {
            // If we have nothing to poll, then we don't need to wake up.
            log!("PT has nothing to poll; waiting for new devices");
            rx.recv_async().await.map_err(flume::RecvTimeoutError::from)
        } else {
            log!("PT polled {count_after_prune} devices and will recv with timeout");
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
            Ok(weak_device) => {
                log!(
                    "PT received request to poll {device:?}",
                    device = weak_device.upgrade()
                );
                to_poll.push(weak_device);
            }
            Err(flume::RecvTimeoutError::Disconnected) => {
                // This shouldn't happen because the sender is static and never dropped
                log::warn!("shouldn't happen: wgpu poller channel disconnected");
            }
            Err(flume::RecvTimeoutError::Timeout) => {
                log!("PT got timeout");
                // continue to poll
            }
        }
    }
}
