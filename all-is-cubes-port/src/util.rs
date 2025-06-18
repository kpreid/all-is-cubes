/// Basic not-optimized version of running a blocking operation from an async context.
/// Used for filesystem operations.
pub(crate) async fn spawn_blocking<R: Send + 'static>(f: impl FnOnce() -> R + Send + 'static) -> R {
    let (tx, rx) = futures_channel::oneshot::channel();
    std::thread::spawn(move || {
        let (Ok(()) | Err(_)) = tx.send(f());
    });
    rx.await.unwrap()
}
