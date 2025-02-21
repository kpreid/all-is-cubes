//! Test harness for test cases that exercise [`all_is_cubes`]’ various renderers.
//!
//! Test targets use this library by calling the [`harness_main()`] function.

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![allow(
    exported_private_dependencies,
    missing_docs,
    clippy::module_name_repetitions,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::missing_panics_doc,
    reason = "library for internal use only"
)]
#![forbid(unsafe_code)]
#![warn(
    unused_crate_dependencies,
    reason = "should be no false positives for this crate"
)]

use std::io;

use test_renderers_types::TestId;

// -------------------------------------------------------------------------------------------------

mod harness;
pub use harness::*;
mod report;
pub use report::*;

// -------------------------------------------------------------------------------------------------

pub fn initialize_logging(args: &HarnessArgs) {
    struct RenderHarnessLogger {
        level_filter: log::LevelFilter,
    }
    impl log::Log for RenderHarnessLogger {
        fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
            let t = metadata.target();
            metadata.level() <= self.level_filter
                && (!t.starts_with("wgpu") || t == "wgpu_render")
                && !t.starts_with("naga")
        }
        fn log(&self, record: &log::Record<'_>) {
            use io::Write as _;

            if self.enabled(record.metadata()) {
                let test_id: Option<TestId> = TEST_ID.try_with(|id| id.clone()).ok();
                let mut lock = io::stderr().lock();
                _ = time::OffsetDateTime::now_utc().to_offset(time::UtcOffset::UTC).format_into(
                    &mut lock,
                    time::macros::format_description!("[hour]:[minute]:[second]"),
                );
                _ = write!(lock, " [{level}] (", level = record.level());
                _ = match test_id {
                    Some(id) => write!(lock, "{id}"),
                    None => write!(lock, "?"),
                };
                _ = writeln!(lock, ") {}", record.args());
            }
        }
        fn flush(&self) {}
    }

    let level_filter = if args.verbose {
        log::LevelFilter::Trace
    } else {
        log::LevelFilter::Warn
    };

    log::set_logger(Box::leak(Box::new(RenderHarnessLogger { level_filter }))).unwrap();
    log::set_max_level(level_filter);
}
