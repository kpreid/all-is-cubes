use std::fmt;
use std::sync::Arc;

use futures_core::future::{BoxFuture, Future};

/// Allows a long-running async task to report its progress, and to yield to the
/// scheduler if appropriate in the current environment (i.e. web).
///
/// * TODO: This might end up being converted to a trait to allow `Sync` depending
///   on the executor's choice.
/// * TODO: We should probably have a non-async interface for progress reporting
///   once that's built.
pub struct YieldProgress {
    start: f32,
    end: f32,
    yielder: Arc<dyn Fn() -> BoxFuture<'static, ()> + Send + Sync>,
    progressor: Arc<dyn Fn(f32) + Send + Sync>,
}

impl fmt::Debug for YieldProgress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("YieldProgress")
            .field("start", &self.start)
            .field("end", &self.end)
            .finish_non_exhaustive()
    }
}

impl YieldProgress {
    pub fn new<Y, YFut, P>(yielder: Y, progressor: P) -> Self
    where
        Y: Fn() -> YFut + Send + Sync + 'static,
        YFut: Future<Output = ()> + Send + 'static,
        P: Fn(f32) + Send + Sync + 'static,
    {
        Self {
            start: 0.0,
            end: 1.0,
            yielder: Arc::new(move || Box::pin(yielder())),
            progressor: Arc::new(progressor),
        }
    }

    /// Returns a [`YieldProgress`] that does no progress reporting and no yielding.
    pub fn noop() -> Self {
        Self::new(|| std::future::ready(()), |_| {})
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
    pub async fn progress(&self, progress_fraction: f32) {
        (self.progressor)(self.point_in_range(progress_fraction));
        (self.yielder)().await;
    }

    /// Report that the given amount of progress has been made, then return
    /// a [`YieldProgress`] covering the remaining range.
    pub async fn finish_and_cut(self, progress_fraction: f32) -> Self {
        let [a, b] = self.split(progress_fraction);
        a.progress(1.0).await;
        b
    }

    fn with_new_range(&self, start: f32, end: f32) -> Self {
        Self {
            start,
            end,
            yielder: Arc::clone(&self.yielder),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::assert_send_sync;

    #[test]
    fn yield_progress_is_sync() {
        assert_send_sync::<YieldProgress>()
    }
}
