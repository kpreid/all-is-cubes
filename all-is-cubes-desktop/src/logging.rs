//! Logging. And terminal progress bars. And their cooperation.

use anyhow::Context as _;
use once_cell::sync::Lazy;

use crate::command_options::LoggingArgs;

pub(crate) fn install(options: LoggingArgs) -> Result<(), anyhow::Error> {
    let LoggingArgs {
        verbose,
        simplify_log_format,
    } = options;
    // Note: Something like this log configuration also appears in other binaries.
    // Unclear how to deduplicate since we don't want to have a library-level dep on
    // simplelog. For now, just remember to consider updating other instances.
    use simplelog::LevelFilter::{Debug, Error, Off, Trace};
    let logger = *simplelog::TermLogger::new(
        match verbose {
            // TODO: When we're closer to 1.0, change the default level to `Info`
            false => Debug,
            true => Trace,
        },
        simplelog::ConfigBuilder::new()
            .set_target_level(Off)
            .set_location_level(Off)
            .set_time_level(if simplify_log_format { Off } else { Error })
            .add_filter_ignore_str("wgpu") // noisy
            .add_filter_ignore_str("naga") // noisy
            .add_filter_ignore_str("winit") // noisy at Trace level only
            .build(),
        simplelog::TerminalMode::Stderr,
        if simplify_log_format {
            simplelog::ColorChoice::Never
        } else {
            simplelog::ColorChoice::Auto
        },
    );

    // Install the logger with our wrapper around it.
    let max_level = simplelog::SharedLogger::level(&logger);
    log::set_boxed_logger(Box::new(AicLogger(logger))).context("failed to initialize logging")?;
    log::set_max_level(max_level);

    Ok(())
}

struct AicLogger(simplelog::TermLogger);

impl log::Log for AicLogger {
    fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
        self.0.enabled(metadata)
    }

    fn log(&self, record: &log::Record<'_>) {
        suspend_indicatif_in(|| self.0.log(record))
    }

    fn flush(&self) {
        suspend_indicatif_in(|| self.0.flush())
    }
}

fn suspend_indicatif_in<R>(f: impl FnOnce() -> R) -> R {
    COOPERATIVE_PROGRESS.suspend(f)
}

pub(crate) static COOPERATIVE_PROGRESS: Lazy<indicatif::MultiProgress> =
    Lazy::new(indicatif::MultiProgress::new);

/// Constructs a progress bar which cooperates with logging to use stderr cleanly.
pub(crate) fn new_progress_bar(len: u64) -> indicatif::ProgressBar {
    let pb = indicatif::ProgressBar::new(len).with_style(common_progress_style());
    COOPERATIVE_PROGRESS.add(pb)
}

/// [`ProgressStyle`] for progress bars we display.
pub(crate) fn common_progress_style() -> indicatif::ProgressStyle {
    indicatif::ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6} {msg:30}")
        .unwrap()
}
