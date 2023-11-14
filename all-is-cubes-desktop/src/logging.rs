//! Logging. And terminal progress bars.
//!
//!

use anyhow::Context as _;

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
    simplelog::TermLogger::init(
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
    )
    .context("failed to initialize logging")?;

    Ok(())
}

/// [`ProgressStyle`] for progress bars we display.
pub(crate) fn common_progress_style() -> indicatif::ProgressStyle {
    indicatif::ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6} {msg:30}")
        .unwrap()
}
