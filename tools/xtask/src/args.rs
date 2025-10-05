//! Command-line parser.

use std::path::Path;

#[derive(Debug, clap::Parser)]
pub(crate) struct XtaskArgs {
    #[clap(subcommand)]
    pub command: XtaskCommand,

    /// Control which workspaces and target triples are built.
    #[arg(long = "scope", default_value = "all")]
    pub scope: Scope,

    /// Pass the `--timings` flag to all `cargo` build invocations, producing
    /// HTML files reporting the time taken.
    ///
    /// Note that a single `xtask` command may end up invoking `cargo` multiple times.
    #[arg(long, global = true)]
    pub timings: bool,

    /// Pass the `--quiet` flag to all `cargo` build/test invocations.
    /// This hides build progress and also switches the test harness to a more concise output.
    #[arg(long, global = true)]
    pub quiet: bool,
}

#[derive(Debug, clap::Subcommand)]
pub(crate) enum XtaskCommand {
    /// Create items that are either necessary for builds to succeed, or trivial.
    /// Currently this means:
    ///
    /// * wasm build output
    /// * `all-is-cubes.desktop` file
    Init,

    /// Run all tests (and some builds without tests) with default features.
    Test {
        /// Build test executables, but don't run them.
        #[arg(long)]
        no_run: bool,
    },

    /// Run tests exercising more combinations of features.
    TestMore {
        #[arg(long)]
        no_run: bool,
    },

    /// Check for lint and generate documentation (to lint the doc markdown), but do not test.
    ///
    /// Caution: If there are rustc or clippy warnings only, the exit code is still zero.
    /// <https://github.com/rust-lang/rust-clippy/issues/1209>
    Lint,

    /// Check every feature combination builds (but do not test them).
    CheckFeatures,

    /// Build documentation.
    ///
    /// This is approximately the same as `cargo doc` but uses the same options as `xtask lint`
    /// does in order to avoid spurious rebuilds.
    Doc,

    /// Format code (as `cargo fmt` but covering all packages)
    Fmt,

    /// Remove build files (as `cargo clean` but covering all packages)
    Clean,

    /// Fuzz: run all fuzz targets, with a chosen duration for each.
    Fuzz {
        duration: f64,
    },

    /// Build binaries with the release profile and report their size on disk.
    BinSize,

    /// Run the `all-is-cubes-server` HTTP server.
    ///
    /// To run this command with automatic rebuilding, use `bacon serve`
    /// after installing `bacon` from <https://crates.io/crates/bacon>.
    RunGameServer {
        server_args: Vec<String>,
    },

    BuildWebRelease,

    /// Update dependency versions.
    Update {
        #[arg(default_value = "latest")]
        to: UpdateTo,

        /// Passes `--dry-run` to `cargo update`, causing it not to actually change the lock files.
        #[arg(long)]
        dry_run: bool,

        /// Additional arguments to pass unchanged to each of the `cargo update` commands.
        additional_args: Vec<String>,
    },

    /// Set the version number of all packages and their dependencies on each other.
    SetVersion {
        version: String,
    },

    /// Publish all of the crates in this workspace that are intended to be published.
    PublishAll {
        /// Actually publish crates rather than dry run.
        #[arg(long = "for-real")]
        for_real: bool,
    },
}

/// Mode for [`XtaskCommand::Update`]
#[derive(Clone, Copy, Debug, clap::ValueEnum)]
pub(crate) enum UpdateTo {
    /// Don't actually update.
    Locked,
    /// Regular `cargo update`.
    Latest,
    /// `cargo update -Z direct-minimal-versions`.
    Minimal,
}

/// Which (workspace × target) combination(s) to build/check/test.
///
/// This is used to allow splitting the work to different CI jobs, and not having them
/// duplicate each other's work and cache data.
///
/// TODO: Add wasm workspace as a separate scope (and break it out in CI).
#[derive(Clone, Copy, Debug, PartialEq, clap::ValueEnum)]
pub(crate) enum Scope {
    /// Default for interactive use — build/check/test everything.
    All,
    /// Build only the main workspace, not the fuzz workspace.
    OnlyNormal,
    /// Build only the fuzz workspace.
    OnlyFuzz,
}
impl Scope {
    pub fn includes_main_workspace(self) -> bool {
        match self {
            Scope::All => true,
            Scope::OnlyNormal => true,
            Scope::OnlyFuzz => false,
        }
    }

    pub fn includes_fuzz_workspace(self) -> bool {
        match self {
            Scope::All => true,
            Scope::OnlyNormal => false,
            Scope::OnlyFuzz => true,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, strum::IntoStaticStr, strum::Display)]
#[strum(serialize_all = "kebab-case")]
pub(crate) enum Profile {
    Dev,
    Release,
}
impl Profile {
    /// Name of the subdirectory in `target/` where Cargo puts builds with this profile.
    pub fn target_subdirectory_name(self) -> &'static Path {
        // The dev profile is a special case:
        // <https://doc.rust-lang.org/cargo/guide/build-cache.html?highlight=debug%20directory#build-cache>
        #[expect(
            clippy::match_wildcard_for_single_variants,
            reason = "this is the correct general form"
        )]
        match self {
            Profile::Dev => Path::new("debug"),
            other => Path::new(other.into()),
        }
    }
}
