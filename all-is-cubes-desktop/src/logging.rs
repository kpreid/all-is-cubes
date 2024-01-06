//! Logging. And terminal progress bars. And their cooperation.

use anyhow::Context as _;
use once_cell::sync::Lazy;

/// A [`clap::Args`] struct for options controlling log output to stderr and Rerun.
#[derive(Clone, Debug, clap::Args)]
pub struct LoggingArgs {
    /// Additional logging to stderr.
    #[arg(long = "verbose", short = 'v')]
    pub verbose: bool,

    /// Remove timestamps from logs so that they are closer to deterministic.
    /// (Note that some logs will still contain timing data.)
    ///
    /// This option is intended for internal tests only.
    #[arg(long = "simplify-log-format", hide = true)]
    pub(crate) simplify_log_format: bool,

    /// Activate logging to Rerun (connecting to the default viewer address) and
    /// log the specified kinds of data.
    #[arg(hide = true, long = "rerun", value_enum, value_delimiter=',', action = clap::ArgAction::Append)]
    pub(crate) rerun: Vec<RerunDataKind>,
}

/// Install a [`log`] global logger based on user-provided `options`.
pub fn install(options: &LoggingArgs, suppress_unless_explicit: bool) -> Result<(), anyhow::Error> {
    use log::LevelFilter::{Debug, Error, Off, Trace};

    let &LoggingArgs {
        verbose,
        simplify_log_format,
        rerun: _,
    } = options;

    let (max_level, stderr_logger): (log::LevelFilter, Option<simplelog::TermLogger>) =
        if options.verbose || !suppress_unless_explicit {
            // Note: Something like this log configuration also appears in other binaries.
            // Unclear how to deduplicate since we don't want to have a library-level dep on
            // simplelog. For now, just remember to consider updating other instances.
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

            (simplelog::SharedLogger::level(&logger), Some(logger))
        } else {
            (Off, None)
        };

    // Install the logger with our wrapper around it.
    log::set_boxed_logger(Box::new(AicLogger { stderr_logger }))
        .context("failed to initialize logging")?;
    log::set_max_level(max_level);

    Ok(())
}

struct AicLogger {
    stderr_logger: Option<simplelog::TermLogger>,
}

impl log::Log for AicLogger {
    fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
        self.stderr_logger
            .as_ref()
            .is_some_and(|l| l.enabled(metadata))
    }

    fn log(&self, record: &log::Record<'_>) {
        if let Some(stderr_logger) = &self.stderr_logger {
            suspend_indicatif_in(|| stderr_logger.log(record))
        }
    }

    fn flush(&self) {
        if let Some(stderr_logger) = &self.stderr_logger {
            suspend_indicatif_in(|| stderr_logger.flush())
        }
    }
}

fn suspend_indicatif_in<R>(f: impl FnOnce() -> R) -> R {
    COOPERATIVE_PROGRESS.suspend(f)
}

pub(crate) static COOPERATIVE_PROGRESS: Lazy<indicatif::MultiProgress> =
    Lazy::new(indicatif::MultiProgress::new);

/// Constructs a progress bar which cooperates with logging to share stderr cleanly.
///
/// As opposed to the defaults, it:
///
/// * will have been added to a shared [`indicatif::MultiProgress`], and
/// * has the style [`common_progress_style()`].
pub fn new_progress_bar(len: u64) -> indicatif::ProgressBar {
    let pb = indicatif::ProgressBar::new(len).with_style(common_progress_style());
    COOPERATIVE_PROGRESS.add(pb)
}

/// [`indicatif::ProgressStyle`] for progress bars we display.
pub fn common_progress_style() -> indicatif::ProgressStyle {
    indicatif::ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6} {msg:30}")
        .unwrap()
}

/// Types of data that command line options can request be written to Rerun.
#[derive(Debug, Clone, Eq, Hash, PartialEq, clap::ValueEnum)]
pub(crate) enum RerunDataKind {
    World,
    RenderPerf,
    RenderImage,
}

#[cfg(feature = "rerun")]
pub(crate) fn connect_rerun<Ren: crate::glue::Renderer, Win>(
    kinds: &std::collections::HashSet<RerunDataKind>,
    universe: &mut all_is_cubes::universe::Universe,
    dsession: &mut crate::DesktopSession<Ren, Win>,
) {
    use all_is_cubes::rerun_glue as rg;
    use all_is_cubes_gpu::RerunFilter;

    let stream = re_sdk::RecordingStreamBuilder::new("all-is-cubes")
        .default_enabled(true)
        .connect()
        .unwrap();
    let destination = rg::Destination {
        stream,
        // Note: This must not be empty for ViewCoordinates to work
        // https://github.com/rerun-io/rerun/issues/3538
        path: rg::entity_path!["dt"],
    };

    // Log timeless configuration
    destination.log_initialization();

    // Attach to universe elements
    // TODO: We need a solution for worlds loaded after app start
    if kinds.contains(&RerunDataKind::World) {
        universe.log_to_rerun(destination.clone());
        if let Some(c) = universe.get_default_character() {
            c.try_modify(|c| c.log_to_rerun(destination.child(&rg::entity_path!["character"])))
                .unwrap();
        }
    }

    // Attach to renderer
    let mut render_filter = RerunFilter::default();
    if kinds.contains(&RerunDataKind::RenderPerf) {
        render_filter.performance = true;
    }
    if kinds.contains(&RerunDataKind::RenderImage) {
        render_filter.image = true;
    }
    if render_filter != RerunFilter::default() {
        dsession
            .renderer
            .log_to_rerun(destination.clone(), render_filter);
    }
}
