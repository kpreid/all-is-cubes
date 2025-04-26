//! Logging. And terminal progress bars. And their cooperation.

use std::collections::HashSet;
use std::fmt;
use std::sync::LazyLock;

use anyhow::Context as _;

#[cfg(feature = "rerun")]
use all_is_cubes::rerun_glue as rg;

/// A [`clap::Args`] struct for options controlling log output to stderr and Rerun.
#[derive(Clone, Debug, clap::Args)]
#[expect(clippy::module_name_repetitions)]
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
pub fn install(
    options: &LoggingArgs,
    suppress_unless_explicit: bool,
) -> Result<LateLogging, anyhow::Error> {
    use log::LevelFilter::{Debug, Error, Off, Trace};

    let &LoggingArgs {
        verbose,
        simplify_log_format,
        rerun: ref kinds,
    } = options;
    let kinds: HashSet<RerunDataKind> = HashSet::from_iter(kinds.iter().cloned());

    let (max_level, stderr_logger): (
        log::LevelFilter,
        Option<simplelog::WriteLogger<std::io::Stderr>>,
    ) = if options.verbose || !suppress_unless_explicit {
        // Note: Something like this log configuration also appears in other binaries.
        // Unclear how to deduplicate since we don't want to have a library-level dep on
        // simplelog. For now, just remember to consider updating other instances.
        let logger = *simplelog::WriteLogger::new(
            match verbose {
                // TODO: When we're closer to 1.0, change the default level to `Info`
                false => Debug,
                true => Trace,
            },
            simplelog::ConfigBuilder::new()
                .set_target_level(Off)
                .set_location_level(Off)
                .set_time_level(if simplify_log_format { Off } else { Error })
                .add_filter_ignore_str("tracing::span") // intentionally very verbose
                .add_filter_ignore_str("wgpu") // noisy
                .add_filter_ignore_str("naga") // noisy
                .add_filter_ignore_str("winit") // noisy at Debug level since 0.30
                .add_filter_ignore_str("h2") // used by Rerun and noisy at Debug level since 0.23
                .build(),
            std::io::stderr(),
        );

        (simplelog::SharedLogger::level(&logger), Some(logger))
    } else {
        (Off, None)
    };

    // If we're going to construct a Rerun recording stream, because the user passed at least
    // one `--rerun=` arg, do so now.
    #[cfg(feature = "rerun")]
    let (rerun_destination_general, rerun_destination_logging) = if !kinds.is_empty() {
        let stream = re_sdk::RecordingStreamBuilder::new("all-is-cubes")
            .default_enabled(true)
            .connect_grpc()
            .unwrap();
        let destination = rg::Destination {
            stream,
            // Note: This must not be empty for ViewCoordinates to work
            // https://github.com/rerun-io/rerun/issues/3538
            path: rg::entity_path!["dt"],
        };

        // Log timeless configuration
        destination.log_initialization();

        // Hook up [`log`] logging if requested.
        let dl = if kinds.contains(&RerunDataKind::Log) {
            destination.child(&rg::entity_path!["log"])
        } else {
            rg::Destination::default()
        };

        (destination, dl)
    } else {
        (rg::Destination::default(), rg::Destination::default())
    };

    let our_combined_logger = AicLogger {
        stderr_logger,
        #[cfg(feature = "rerun")]
        rerun_destination: rerun_destination_logging,
    };

    // Install the logger.
    log::set_boxed_logger(Box::new(our_combined_logger)).context("failed to initialize logging")?;
    log::set_max_level(max_level);

    Ok(LateLogging {
        kinds,
        #[cfg(feature = "rerun")]
        rerun_destination: rerun_destination_general,
    })
}

/// [`log::Log`] implementation that [`install()`] registers globally.
struct AicLogger {
    stderr_logger: Option<simplelog::WriteLogger<std::io::Stderr>>,
    #[cfg(feature = "rerun")]
    rerun_destination: rg::Destination,
}

impl log::Log for AicLogger {
    fn enabled(&self, metadata: &log::Metadata<'_>) -> bool {
        #[cfg(feature = "rerun")]
        let rr = self.rerun_destination.is_enabled();
        #[cfg(not(feature = "rerun"))]
        let rr = false;

        self.stderr_logger
            .as_ref()
            .is_some_and(|l| l.enabled(metadata))
            || rr
    }

    fn log(&self, record: &log::Record<'_>) {
        if let Some(stderr_logger) = &self.stderr_logger {
            suspend_indicatif_in(|| stderr_logger.log(record));
        }
        #[cfg(feature = "rerun")]
        if self.rerun_destination.is_enabled() {
            use rg::components::TextLogLevel;

            // TODO: Need to implement our own log filtering here since simplelog doesn't let us
            // borrow the filter rules.

            self.rerun_destination.log(
                &rg::entity_path![],
                &rg::archetypes::TextLog::new(record.args().to_string()).with_level(TextLogLevel(
                    match record.level() {
                        log::Level::Error => TextLogLevel::ERROR,
                        log::Level::Warn => TextLogLevel::WARN,
                        log::Level::Info => TextLogLevel::INFO,
                        log::Level::Debug => TextLogLevel::DEBUG,
                        log::Level::Trace => TextLogLevel::TRACE,
                    }
                    .into(),
                )),
            );
        }
    }

    fn flush(&self) {
        if let Some(stderr_logger) = &self.stderr_logger {
            suspend_indicatif_in(|| stderr_logger.flush())
        }
        #[cfg(feature = "rerun")]
        if self.rerun_destination.is_enabled() {
            self.rerun_destination.stream.flush_async();
        }
    }
}

fn suspend_indicatif_in<R>(f: impl FnOnce() -> R) -> R {
    COOPERATIVE_PROGRESS.suspend(f)
}

pub(crate) static COOPERATIVE_PROGRESS: LazyLock<indicatif::MultiProgress> =
    LazyLock::new(indicatif::MultiProgress::new);

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
    #![allow(clippy::literal_string_with_formatting_args)]
    indicatif::ProgressStyle::default_bar()
        .template("{prefix:8} [{elapsed}] {wide_bar} {pos:>6}/{len:6} {msg:30}")
        .unwrap()
}

/// Types of data that command line options can request be written to Rerun.
#[derive(Debug, Clone, Eq, Hash, PartialEq, clap::ValueEnum)]
pub(crate) enum RerunDataKind {
    /// Send log messages.
    Log,
    /// Send data about the game world (particularly collisions).
    World,
    /// Send rendering performance data, for plotting.
    RenderPerf,
    /// Send the rendered image (including depth).
    RenderImage,
    /// Send the mesh used for rendering.
    RenderMesh,
    /// Send the texture atlases used for rendering.
    RenderTextures,
}

/// Input for logging-like initialization that needs to happen later when we have more information.
/// Produced by [`install()`] and used by calling [`inner_main()`](crate::inner_main).
#[derive(Debug)]
#[must_use]
#[expect(clippy::module_name_repetitions)]
pub struct LateLogging {
    kinds: HashSet<RerunDataKind>,
    #[cfg(feature = "rerun")]
    rerun_destination: rg::Destination,
}

#[allow(clippy::unnecessary_wraps)]
impl LateLogging {
    /// Hook up to the renderer.
    pub(crate) fn attach_to_renderer<Ren: crate::glue::Renderer>(
        &self,
        renderer: &mut Ren,
    ) -> Result<(), NoRerunSupportError> {
        cfg_if::cfg_if! {
            if #[cfg(feature = "rerun")] {
                // split out into a function so rustfmt isn't interfered with by the macro
                log_renderer_to_rerun(self, renderer);
            } else {
                // suppress warning
                let _ = renderer;

                if !self.kinds.is_empty() {
                    return Err(NoRerunSupportError);
                }
            }
        }
        Ok(())
    }

    /// Hook up to the universe.
    pub(crate) fn finish(
        self: LateLogging,
        universe: &mut all_is_cubes::universe::Universe,
    ) -> Result<(), NoRerunSupportError> {
        cfg_if::cfg_if! {
            if #[cfg(feature = "rerun")] {
                // split out into a function so rustfmt isn't interfered with by the macro
                log_universe_to_rerun(&self, universe);
            } else {
                // suppress warning
                let _ = universe;

                if !self.kinds.is_empty() {
                    return Err(NoRerunSupportError);
                }
            }
        }
        Ok(())
    }
}

#[cfg(feature = "rerun")]
fn log_renderer_to_rerun<Ren: crate::glue::Renderer>(this: &LateLogging, renderer: &mut Ren) {
    use all_is_cubes_gpu::RerunFilter;

    let LateLogging {
        kinds,
        #[cfg(feature = "rerun")]
            rerun_destination: destination,
    } = this;

    let render_filter = RerunFilter {
        performance: kinds.contains(&RerunDataKind::RenderPerf),
        image: kinds.contains(&RerunDataKind::RenderImage),
        textures: kinds.contains(&RerunDataKind::RenderTextures),
    };

    if render_filter != RerunFilter::default() {
        renderer.log_to_rerun(destination.clone(), render_filter);
    }
}

#[cfg(feature = "rerun")]
fn log_universe_to_rerun(this: &LateLogging, universe: &mut all_is_cubes::universe::Universe) {
    use all_is_cubes_render::camera::{Camera, GraphicsOptions, Viewport};

    let LateLogging {
        kinds,
        #[cfg(feature = "rerun")]
            rerun_destination: destination,
    } = this;

    // Attach to universe elements
    // Note that this starts the `session_step_time` timeline, so it should happen
    // before other initial logging.
    // TODO: We need a solution for worlds loaded after app start
    if kinds.contains(&RerunDataKind::World) {
        universe.log_to_rerun(destination.clone());
        if let Some(c) = universe.get_default_character() {
            c.try_modify(|c| {
                c.log_to_rerun(destination.child(&rg::entity_path!["character"]));
            })
            .unwrap();
        }
    }

    if kinds.contains(&RerunDataKind::RenderMesh) {
        if let Some(c) = universe.get_default_character() {
            c.try_modify(|c| {
                // TODO: implement live updates -- need to permanently attach this to the session
                // as another renderer, sort of.
                let mut rm = crate::glue::rerun_mesh::RerunMesher::new(
                    destination.child(&rg::entity_path!("world-mesh")),
                    c.space.clone(),
                );
                rm.update(
                    universe.read_ticket(),
                    &Camera::new(
                        GraphicsOptions::default(),
                        Viewport::with_scale(1.0, [1, 1]),
                    ),
                );
                core::mem::forget(rm);
            })
            .unwrap();
        } else {
            panic!("no character to render mesh from");
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct NoRerunSupportError;
impl fmt::Display for NoRerunSupportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("the --rerun option is not available because Rerun support was not compiled in")
    }
}
impl std::error::Error for NoRerunSupportError {}
