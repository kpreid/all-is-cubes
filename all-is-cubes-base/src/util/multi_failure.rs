use alloc::boxed::Box;
use alloc::vec::Vec;
use std::panic;

/// Guard object which collects multiple panics or other reports of failure,
/// then panics when it is dropped if any occurred (if its collection is nonempty).
///
/// This allows [`#[test]`](macro@test) tests to find multiple failures before exiting,
/// which can be useful for getting more diagnostic information out of a test failure.
#[derive(Debug, Default)]
pub struct MultiFailure {
    panics: Vec<Box<dyn core::any::Any>>,
}

impl MultiFailure {
    /// Constructs an empty [`MultiFailure`].
    ///
    /// If nothing else is done to it, it will have no effect.
    #[inline(never)]
    #[must_use = "this is useless if never invoked to collect failures"]
    pub fn new() -> Self {
        Self::default()
    }

    /// Call `f`, and if it unwinds, count that as a failure and defer it
    /// until this [`MultiFailure`] is dropped.
    #[inline(never)]
    pub fn catch<O, F>(&mut self, f: F) -> Option<O>
    where
        F: FnOnce() -> O,
        F: panic::UnwindSafe,
    {
        match panic::catch_unwind(f) {
            Ok(output) => Some(output),
            Err(panic_payload) => {
                // At this point, the panic hook has *already* been invoked to print a report,
                // so we don't need to do anything but remember that a panic happened.
                self.panics.push(panic_payload);
                None
            }
        }
    }

    // TODO: add a way to log a failure w/o panicking, which should be printed in a summary
}

impl Drop for MultiFailure {
    #[inline(never)]
    fn drop(&mut self) {
        let Self { panics } = self;

        #[cfg(feature = "std")]
        if std::thread::panicking() {
            // For cleaner outcomes, don’t double-panic.
            // Either the test is already failing so we don't need to make it worse,
            // or our own unwind would be caught anyway, which means our usage is futile.
            return;
        }

        if panics.is_empty() {
            // No failures occurred, so continue.
            return;
        }

        let count = panics.len();

        // By using `resume_unwind()`, we don't invoke the panic hook, avoiding printing
        // another full panic message.
        let summary_message = format!(
            "{count} panic{s_were} found",
            s_were = if count == 1 { " was" } else { "s were" }
        );
        std::eprintln!("\n{summary_message}");
        panic::resume_unwind(Box::new(summary_message));
    }
}

#[cfg(test)]
mod tests {
    use super::{panic, *}; // resolve `ambiguous_panic_imports` <https://github.com/rust-lang/rust/issues/147319>

    #[test]
    fn expected_success() {
        // This should not panic on drop, since no failures were logged.
        let _f = MultiFailure::new();
    }

    #[test]
    #[should_panic = "3 panics were found"]
    fn expected_failure() {
        let mut f = MultiFailure::new();

        for i in 0..3 {
            f.catch(|| {
                panic!("test panic {i}");
            });
        }
    }
}
