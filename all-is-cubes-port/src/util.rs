use all_is_cubes::util::YieldProgress;

/// Basic not-optimized version of running a blocking operation from an async context.
/// Used for filesystem operations.
#[allow(
    dead_code,
    reason = "only used if at least one export format is enabled"
)]
pub(crate) async fn spawn_blocking<R: Send + 'static>(f: impl FnOnce() -> R + Send + 'static) -> R {
    let (tx, rx) = futures_channel::oneshot::channel();
    std::thread::spawn(move || {
        let (Ok(()) | Err(_)) = tx.send(f());
    });
    rx.await.unwrap()
}

/// Execute `function` on all of `items`, in parallel if the `"auto-threads"` feature is enabled,
/// and with yield points if not.
///
/// As [the scoped task trilemma][st] demands, the items and the function may not borrow any input
/// data.
///
/// [st]: https://without.boats/blog/the-scoped-task-trilemma/
pub(crate) async fn maybe_parallelize<T: Send, U: Send + 'static>(
    progress: YieldProgress,
    items: impl IntoIterator<Item = T, IntoIter: ExactSizeIterator + Send + 'static>,
    label_function: impl Fn(&T) -> String + Send + Sync + 'static,
    work_function: impl Fn(T) -> U + Send + Sync + 'static,
) -> Vec<U> {
    let items = items.into_iter();

    #[cfg(not(feature = "auto-threads"))]
    {
        let mut outputs: Vec<U> = Vec::new();
        let split_p = progress.split_evenly(items.len());
        for (item, mut p) in items.zip(split_p) {
            p.set_label(label_function(&item));
            p.progress(0.0).await; // make label visible
            outputs.push(work_function(item));
            p.finish().await;
        }
        outputs
    }

    #[cfg(feature = "auto-threads")]
    {
        use rayon::prelude::{IntoParallelIterator as _, ParallelIterator as _};

        spawn_blocking(move || {
            let split_p = progress.split_evenly_concurrent(items.len());
            let items_and_progresses = items
                .into_iter()
                .zip(split_p)
                .map(|(item, mut p)| {
                    p.set_label(label_function(&item));
                    p.progress_without_yield(0.0); // cause label to be published
                    (item, p)
                })
                .collect::<Vec<_>>();
            items_and_progresses
                .into_par_iter()
                .map(move |(item, p)| {
                    rayon::yield_now(); // import tasks are possibly long, so allow more interleaving with more realtime tasks -- TODO: have a better plan, like multiple thread pools
                    let output = work_function(item);
                    p.progress_without_yield(1.0); // would be .finish() but we are in a blocking, not async, context
                    output
                })
                .collect()
        })
        .await
    }
}
