use std::fmt;
use std::panic::Location;
use std::sync::{Arc, Mutex};

use futures_core::future::{BoxFuture, Future};
use instant::{Duration, Instant};

/// Allows a long-running async task to report its progress, while also yielding to the
/// scheduler (e.g. for single-threaded web environment) and introducing cancellation
/// points.
///
/// These go together because the rate at which it makes sense to yield (to avoid event
/// loop hangs) is similar to the rate at which it makes sense to report progress.
///
/// Note that while a [`YieldProgress`] is `Send` and `Sync`, it does not currently
/// support meaningfully being used from multiple threads or futures at once — only
/// reporting the progress of, and yielding periodically within, a fully sequential
/// operation. This might change in the future, but for now, it will just output
/// inconsistent results if you try to use it otherwise.
pub struct YieldProgress {
    start: f32,
    end: f32,

    /// Name given to this specific portion of work. Inherited from the parent if not
    /// overridden.
    ///
    /// TODO: Eventually we will want to have things like "label this segment as a
    /// fallback if it has no better label", which will require some notion of distinguishing
    /// inheritance from having been explicitly set.
    label: Option<Arc<str>>,

    yielding: Arc<Yielding<dyn Fn() -> BoxFuture<'static, ()> + Send + Sync>>,
    // TODO: change progress reporting interface to support efficient handling of
    // the label string being the same as last time.
    #[allow(clippy::type_complexity)]
    progressor: Arc<dyn Fn(f32, &str) + Send + Sync>,
}

/// Piggyback on the `Arc` we need to store the `dyn Fn` anyway to also store some state.
struct Yielding<F: ?Sized> {
    state: Mutex<YieldState>,

    yielder: F,
}

#[derive(Clone)]
struct YieldState {
    /// The most recent instant at which `yielder`'s future completed.
    /// Used to detect overlong time periods between yields.
    last_finished_yielding: Instant,
    last_yield_location: &'static Location<'static>,
    last_yield_label: Option<Arc<str>>,
}

impl fmt::Debug for YieldProgress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("YieldProgress")
            .field("start", &self.start)
            .field("end", &self.end)
            .field("label", &self.label)
            .finish_non_exhaustive()
    }
}

impl YieldProgress {
    /// Construct a new [`YieldProgress`], which will call `yielder` to yield and
    /// `progressor` to report progress.
    ///
    /// Note that it measures time intervals between yields starting from when this
    /// function is called, as if this is the first yield.
    #[track_caller]
    pub fn new<Y, YFut, P>(yielder: Y, progressor: P) -> Self
    where
        Y: Fn() -> YFut + Send + Sync + 'static,
        YFut: Future<Output = ()> + Send + 'static,
        P: Fn(f32, &str) + Send + Sync + 'static,
    {
        let yielding: Arc<Yielding<_>> = Arc::new(Yielding {
            state: Mutex::new(YieldState {
                last_finished_yielding: Instant::now(),
                last_yield_location: Location::caller(),
                last_yield_label: None,
            }),
            yielder: move || -> BoxFuture<'static, ()> { Box::pin(yielder()) },
        });

        Self {
            start: 0.0,
            end: 1.0,
            label: None,
            yielding,
            progressor: Arc::new(progressor),
        }
    }

    /// Returns a [`YieldProgress`] that does no progress reporting and no yielding.
    pub fn noop() -> Self {
        Self::new(|| std::future::ready(()), |_, _| {})
    }

    /// Add a name for the portion of work this [`YieldProgress`] covers.
    ///
    /// If there is already a label, it will be overwritten.
    pub fn set_label(&mut self, label: impl fmt::Display) {
        self.label = Some(Arc::from(label.to_string()))
    }

    /// Map a `0..=1` value to `self.start..=self.end`.
    #[track_caller]
    fn point_in_range(&self, mut x: f32) -> f32 {
        x = x.clamp(0.0, 1.0);
        if !x.is_finite() {
            if cfg!(debug_assertions) {
                panic!("NaN progress value");
            } else {
                x = 0.5;
            }
        }
        self.start + (x * (self.end - self.start))
    }

    /// Report the current amount of progress (a number from 0 to 1) and yield.
    #[track_caller] // This is not an `async fn` because `track_caller` is not compatible
    pub fn progress(&self, progress_fraction: f32) -> impl Future<Output = ()> + Send + 'static {
        let location = Location::caller();
        let label = self.label.clone();
        (self.progressor)(
            self.point_in_range(progress_fraction),
            self.label
                .as_ref()
                .map_or("", |arc_str_ref| -> &str { arc_str_ref }),
        );

        self.yielding.clone().yield_only(location, label)
    }

    /// Report that 100% of progress has been made.
    ///
    /// This is identical to `.progress(1.0)` but consumes the `YieldProgress` object.
    #[track_caller] // This is not an `async fn` because `track_caller` is not compatible
    pub fn finish(self) -> impl Future<Output = ()> + Send + 'static {
        self.progress(1.0)
    }

    /// Report that the given amount of progress has been made, then return
    /// a [`YieldProgress`] covering the remaining range.
    #[track_caller] // This is not an `async fn` because `track_caller` is not compatible
    pub fn finish_and_cut(
        self,
        progress_fraction: f32,
    ) -> impl Future<Output = Self> + Send + 'static {
        let [a, b] = self.split(progress_fraction);
        let progress_future = a.finish();
        async move {
            progress_future.await;
            b
        }
    }

    fn with_new_range(&self, start: f32, end: f32) -> Self {
        Self {
            start,
            end,
            label: self.label.clone(),
            yielding: Arc::clone(&self.yielding),
            progressor: Arc::clone(&self.progressor),
        }
    }

    /// Construct two new [`YieldProgress`] which divide the progress value into two
    /// subranges.
    ///
    /// The returned instances should be used in sequence, but this is not enforced.
    pub fn split(self, cut: f32) -> [Self; 2] {
        let cut_abs = self.point_in_range(cut);
        [
            self.with_new_range(self.start, cut_abs),
            self.with_new_range(cut_abs, self.end),
        ]
    }

    /// Split into even subdivisions.
    pub fn split_evenly(self, count: usize) -> impl Iterator<Item = YieldProgress> {
        assert!(count < usize::MAX);
        (0..count).map(move |index| {
            self.with_new_range(
                self.point_in_range(index as f32 / count as f32),
                self.point_in_range((index + 1) as f32 / count as f32),
            )
        })
    }
}

impl<F: ?Sized + Fn() -> BoxFuture<'static, ()> + Send + Sync> Yielding<F> {
    async fn yield_only(
        self: Arc<Self>,
        location: &'static Location<'static>,
        label: Option<Arc<str>>,
    ) {
        // Note that we avoid holding the lock while calling yielder().
        // The worst outcome of an inconsistency is that we will output a meaningless
        // "between {location} and {location} message", but none should occur because
        // [`YieldProgress`] is intended to be used in a sequential manner.
        let previous_state: YieldState = { self.state.lock().unwrap().clone() };

        let delta = Instant::now().duration_since(previous_state.last_finished_yielding);
        if delta > Duration::from_millis(100) {
            let last_label = previous_state.last_yield_label;
            log::trace!(
                "Yielding after {delta} ms between {old_location} and {new_location} {rel}",
                delta = delta.as_millis(),
                old_location = previous_state.last_yield_location,
                new_location = location,
                rel = if label == last_label {
                    format!("during {label:?}")
                } else {
                    format!("between {last_label:?} and {label:?}")
                }
            );
        }

        // TODO: Since we're tracking time, we might as well decide whether to not bother
        // yielding if it has been a short time ... except that different yielders might
        // want different granularities/policies.
        (self.yielder)().await;

        {
            let mut state = self.state.lock().unwrap();
            state.last_finished_yielding = Instant::now();
            state.last_yield_location = location;
            state.last_yield_label = label;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::assert_send_sync;
    use tokio::sync::mpsc::{self, error::TryRecvError};

    /// Something that the [`YieldProgress`] under test did.
    #[derive(Debug, Clone, PartialEq)]
    enum Entry {
        Yielded,
        Progress(f32, String),
        /// The receiver was dropped
        Dropped,
    }
    use Entry::*;

    struct YpLog(mpsc::UnboundedReceiver<Entry>);

    fn logging_yield_progress() -> (YieldProgress, YpLog) {
        let (sender, receiver) = mpsc::unbounded_channel();
        let yp = YieldProgress::new(
            {
                let sender = sender.clone();
                move || {
                    let _ = sender.send(Entry::Yielded);
                    std::future::ready(())
                }
            },
            move |progress, label| drop(sender.send(Entry::Progress(progress, label.to_owned()))),
        );
        (yp, YpLog(receiver))
    }

    impl YpLog {
        fn drain(&mut self) -> Vec<Entry> {
            let mut entries = Vec::new();
            loop {
                match self.0.try_recv() {
                    Ok(entry) => entries.push(entry),
                    Err(TryRecvError::Empty) => return entries,
                    Err(TryRecvError::Disconnected) => {
                        entries.push(Dropped);
                        return entries;
                    }
                }
            }
        }
    }

    #[test]
    fn yield_progress_is_sync() {
        assert_send_sync::<YieldProgress>()
    }

    #[tokio::test]
    async fn basic_progress() {
        // Construct instance. Nothing happens immediately.
        let (p, mut r) = logging_yield_progress();
        assert_eq!(r.drain(), vec![]);

        // Simple progress.
        let progress_future = p.progress(0.25);
        assert_eq!(r.drain(), vec![Progress(0.25, "".into())]);
        progress_future.await;
        assert_eq!(r.drain(), vec![Yielded]);
    }

    #[tokio::test]
    async fn set_label() {
        let (mut p, mut r) = logging_yield_progress();
        p.set_label("hello");
        assert_eq!(r.drain(), vec![]); // TODO: labels should take effect sooner, or should they?
        p.progress(0.25).await;
        assert_eq!(r.drain(), vec![Progress(0.25, "hello".into()), Yielded]);
    }

    #[tokio::test]
    async fn finish() {
        let (p, mut r) = logging_yield_progress();
        p.finish().await;
        assert_eq!(r.drain(), vec![Progress(1.0, "".into()), Yielded, Dropped]);
    }

    #[tokio::test]
    async fn finish_and_cut() {
        let (p, mut r) = logging_yield_progress();
        let p2 = p.finish_and_cut(0.5).await;
        assert_eq!(r.drain(), vec![Progress(0.5, "".into()), Yielded]);
        p2.progress(0.5).await;
        assert_eq!(r.drain(), vec![Progress(0.75, "".into()), Yielded]);
    }

    // TODO: test split() and split_evenly()
}
