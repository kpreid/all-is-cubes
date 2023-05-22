//! Build/test operations for All is Cubes.
//!
//! This is an instance of the `cargo-xtask` pattern as described by
//! <https://github.com/matklad/cargo-xtask>.

// TODO: See if we can abstract the logic to "do <action> to all packages with <features/targets>"
// action can be "build", "test", or "lint"
// features and targets partially overlap e.g. wasm is mutually exclusive with rayon
//
// We might or might not want to reduce this to "compute all the primitive combinations, then find the minimal set of cargo commands to produce this effect".
// That might be overkill or it might be straightforward.

// TODO: Make this independent of the current directory so that it will never do surprising
// things to the current directory.

// Basic lint settings, which should be identical across all all-is-cubes project crates.
// This list is sorted.
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::needless_update)]
#![allow(clippy::single_match)]
#![deny(rust_2018_idioms)]
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::doc_markdown)]
#![warn(clippy::exhaustive_enums)]
#![warn(clippy::exhaustive_structs)]
#![warn(clippy::modulo_arithmetic)]
#![warn(clippy::return_self_not_must_use)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(clippy::unnecessary_self_imports)]
#![warn(clippy::wrong_self_convention)]
#![warn(explicit_outlives_requirements)]
#![warn(missing_debug_implementations)]
#![warn(noop_method_call)]
#![warn(trivial_numeric_casts)]
#![warn(unused_extern_crates)]
#![warn(unused_lifetimes)]
// Lenience for tests.
#![cfg_attr(test,
    allow(clippy::float_cmp), // deterministic tests
    allow(clippy::redundant_clone), // prefer regularity over efficiency
)]
// #![warn(unused_crate_dependencies)]  // noisy for dev-dependencies; enable sometimes for review

// Crate-specific lint settings.
#![forbid(unsafe_code)]

use std::collections::BTreeSet;
use std::ffi::OsStr;
use std::fmt;
use std::fs;
use std::io::Write as _;
use std::mem;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use anyhow::Error as ActionError;
use cargo_metadata::PackageId;
use once_cell::sync::Lazy;
use xshell::{cmd, pushd, Cmd, Pushd};

mod fs_ops;
use fs_ops::{directory_tree_contents, newer_than};

use crate::fs_ops::copy_file_with_context;

#[derive(Debug, clap::Parser)]
struct XtaskArgs {
    #[clap(subcommand)]
    command: XtaskCommand,

    /// Pass the `--timings` flag to all `cargo` build invocations, producing
    /// HTML files reporting the time taken.
    ///
    /// Note that a single `xtask` command may end up invoking `cargo` multiple times.
    #[arg(long, global = true)]
    timings: bool,
}

#[derive(Debug, clap::Subcommand)]
enum XtaskCommand {
    /// Create items that are either necessary for builds to succeed, or trivial.
    /// Currently this means:
    ///
    /// * wasm build output
    /// * `all-is-cubes.desktop` file
    Init,

    /// Run all tests (and some builds without tests) with default features.
    Test {
        #[arg(long)]
        no_run: bool,
    },

    /// Run tests exercising more combinations of features.
    TestMore,

    /// Compile and report warnings without testing.
    Lint,

    /// Format code (as `cargo fmt` but covering all packages)
    Fmt,

    /// Fuzz: run all fuzz targets, with a chosen duration for each.
    Fuzz {
        duration: f64,
    },

    /// Run the game server inside of `cargo watch` (must be installed) and with options
    /// suitable for development.
    RunDev,

    RunGameServer {
        server_args: Vec<String>,
    },

    /// Update dependency versions.
    Update {
        #[arg(default_value = "latest")]
        to: UpdateTo,
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

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
enum UpdateTo {
    /// Don't actually update.
    Locked,
    /// Regular `cargo update`.
    Latest,
    /// `cargo update -Z direct-minimal-versions`.
    Minimal,
}

fn main() -> Result<(), ActionError> {
    let (config, command) = {
        let XtaskArgs { command, timings } = <XtaskArgs as clap::Parser>::parse();
        let config = Config {
            cargo_timings: timings,
        };
        (config, command)
    };

    // TODO: fold time_log into the Config to reduce number of pieces passed around
    let mut time_log: Vec<Timing> = Vec::new();

    match command {
        XtaskCommand::Init => {
            write_development_files()?;
            update_server_static(&mut time_log)?; // includes installing wasm tools
        }
        XtaskCommand::Test { no_run } => {
            do_for_all_packages(
                &config,
                if no_run {
                    TestOrCheck::BuildTests
                } else {
                    TestOrCheck::Test
                },
                Features::Default,
                &mut time_log,
            )?;
        }
        XtaskCommand::TestMore => {
            exhaustive_test(&config, &mut time_log)?;
        }
        XtaskCommand::Lint => {
            do_for_all_packages(&config, TestOrCheck::Lint, Features::Default, &mut time_log)?;

            // Build docs to verify that there are no broken doc links.
            {
                let _t = CaptureTime::new(&mut time_log, "doc");
                // `--no-deps` skips building our dependencies' docs, which saves some time.
                // If their docs have already been built, the old copies will still be linked,
                // so running this does not make local development worse.
                cargo().arg("doc").arg("--no-deps").run()?;
            }
        }
        XtaskCommand::Fmt => {
            do_for_all_workspaces(|| {
                cargo().arg("fmt").run()?;
                Ok(())
            })?;
        }
        XtaskCommand::Fuzz { duration } => {
            let metadata = cargo_metadata::MetadataCommand::new()
                .manifest_path("fuzz/Cargo.toml")
                .exec()
                .unwrap();
            let [fuzzpkg]: [PackageId; 1] = (metadata.workspace_members)
                .clone()
                .try_into()
                .expect("Didn't find only one package");
            for target in metadata[&fuzzpkg].targets.iter() {
                // Note that a timeout is specified because if any one fuzz task takes
                // more than a fraction of a second, that's a sign that something has
                // gone wrong, because all of the things we are fuzzing are supposed to
                // be fast.
                let timeout_seconds = match target.name.as_str() {
                    // TODO: we don't really want to spend this much time on a
                    // block evaluation, but right now, we don't have good strategies
                    // to cap it without interfering with intended uses.
                    // Eventually we should implement some sort of evaluation cost model
                    // that deterministically limits how much time is spent on one
                    // (just like the recursion limit currently does).
                    "fuzz_block_eval" => 15,
                    _ => 5,
                };

                cmd!("cargo +nightly fuzz run")
                    .env("RUST_BACKTRACE", "1")
                    .arg(&target.name)
                    .arg("--")
                    .arg(format!("-timeout={timeout_seconds}"))
                    .arg(format!("-max_total_time={duration}"))
                    .run()?;
            }
        }
        XtaskCommand::RunDev => {
            // TODO: Replace cargo-watch with our own file watching built into the server
            // so that we can avoid restarting it.
            cargo()
                .arg("watch")
                .arg("-s")
                .arg("cargo xtask run-game-server -- --client-source workspace --port 8080")
                .run()?;
        }
        XtaskCommand::RunGameServer { server_args } => {
            update_server_static(&mut time_log)?;

            cargo()
                .arg("run")
                .args(config.cargo_build_args())
                .arg("--bin=aic-server")
                .arg("--features=embed")
                .arg("--")
                .args(server_args)
                .run()?;
        }
        XtaskCommand::Update { to } => match to {
            UpdateTo::Locked => {
                eprintln!("Doing nothing because update type is {to:?}.");
            }
            UpdateTo::Latest => {
                do_for_all_workspaces(|| {
                    // Note: The `fuzz` workspace lock file is ignored in version control.
                    // But we do want to occasionally update it anyway.
                    cargo().arg("update").run()?;
                    Ok(())
                })?;
            }
            UpdateTo::Minimal => {
                do_for_all_workspaces(|| {
                    cmd!("cargo +nightly update -Z direct-minimal-versions").run()?;
                    Ok(())
                })?;
            }
        },
        XtaskCommand::SetVersion { version } => {
            let version_value = toml_edit::value(version.as_str());
            for package in ALL_NONTEST_PACKAGES {
                let manifest_path = format!("{package}/Cargo.toml");
                eprint!("Editing {manifest_path}...");
                let _ = std::io::stderr().flush();
                let mut manifest: toml_edit::Document =
                    fs::read_to_string(&manifest_path)?.parse()?;
                assert_eq!(manifest["package"]["name"].as_str(), Some(package));

                // Update version of the package itself
                manifest["package"]["version"] = version_value.clone();

                // Update versions of dependencies
                let mut count_deps = 0;
                for (_dep_key, dep_item) in manifest["dependencies"]
                    .as_table_mut()
                    .expect("dependencies not a table")
                    .iter_mut()
                    .filter(|(dep_key, _)| ALL_NONTEST_PACKAGES.contains(&dep_key.get()))
                {
                    dep_item["version"] = version_value.clone();
                    count_deps += 1;
                }

                fs::write(&manifest_path, manifest.to_string())?;
                eprintln!("wrote version and {count_deps} deps.");
            }
            eprint!(
                "Versions updated. Manual updates are still needed for:\n\
                • Changelog\n\
                • Documentation links in all-is-cubes/README.md\n\
                "
            );
        }
        XtaskCommand::PublishAll { for_real } => {
            exhaustive_test(&config, &mut time_log)?;

            let maybe_dry = if for_real { vec![] } else { vec!["--dry-run"] };
            for package in ALL_NONTEST_PACKAGES {
                if package == "all-is-cubes-wasm" {
                    // Not published to crates.io; built and packaged as a part of all-is-cubes-server.
                    continue;
                }

                let _pushd: Pushd = pushd(package)?;
                if for_real {
                    // Let crates.io pick up the new all-is-cubes version,
                    // or publishing dependents will fail.
                    // TODO: This will stop being necessary in Rust 1.66:
                    // <https://github.com/rust-lang/cargo/issues/9507>
                    std::thread::sleep(Duration::from_secs(10));
                }
                cargo()
                    .arg("publish")
                    .args(maybe_dry.iter().copied())
                    .run()?;
            }
        }
    }

    for t in time_log {
        eprintln!("{t}");
    }

    Ok(())
}

#[derive(Debug)]
struct Config {
    cargo_timings: bool,
}

impl Config {
    /// Arguments that should be passed to any Cargo command that runs a build
    /// (`build`, `test`, `run`)
    fn cargo_build_args(&self) -> Vec<&str> {
        let mut args = Vec::with_capacity(1);
        if self.cargo_timings {
            args.push("--timings")
        }
        args
    }
}

/// TODO: fetch this list (or at least cross-check it) using `cargo metadata`.
///
/// See also [`do_for_all_workspaces`].
const ALL_NONTEST_PACKAGES: [&str; 9] = [
    "all-is-cubes",
    "all-is-cubes-ui",
    "all-is-cubes-mesh",
    "all-is-cubes-gpu",
    "all-is-cubes-content",
    "all-is-cubes-port",
    "all-is-cubes-desktop",
    "all-is-cubes-wasm",
    "all-is-cubes-server",
];

const CHECK_SUBCMD: &str = "clippy";
const TARGET_WASM: &str = "--target=wasm32-unknown-unknown";

// Test all combinations of situations (that we've bothered to program test
// setup for).
fn exhaustive_test(config: &Config, time_log: &mut Vec<Timing>) -> Result<(), ActionError> {
    // building server with `--feature embed` requires static files
    update_server_static(time_log)?;

    do_for_all_packages(config, TestOrCheck::Test, Features::AllAndNothing, time_log)?;
    Ok(())
}

/// Build the WASM and other 'client' files that the web server might need.
/// Needed for build whenever `all-is-cubes-server` is being tested/run with
/// the `embed` feature; needed for run regardless.
fn update_server_static(time_log: &mut Vec<Timing>) -> Result<(), ActionError> {
    ensure_wasm_tools_installed(time_log)?;

    // Run the compilation if needed, which ensures that the wasm binary is fresh.
    // Note: This must use the same profile as the wasm-pack command is! (Both are dev for now)
    cargo()
        .arg("build")
        .arg("--package=all-is-cubes-wasm")
        .arg(TARGET_WASM)
        .run()?;

    // Run wasm-pack if and only if we need to.
    // This is because it unconditionally runs `wasm-opt` which is slow and also means
    // the files will be touched unnecessarily.
    if newer_than(
        ["target/wasm32-unknown-unknown/debug/all_is_cubes_wasm.wasm"],
        ["all-is-cubes-wasm/pkg/all_is_cubes_wasm.js"],
    ) {
        let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
        cmd!("wasm-pack build --dev --target web").run()?;
    }

    // Combine the static files and build results in the same way that webpack used to
    // (This will need replacement if we get subdirectories)
    let pkg_path = Path::new("all-is-cubes-wasm/pkg");
    let pkg_files: BTreeSet<PathBuf> = {
        let mut set = BTreeSet::from([
            // There are lots of other files in pkg which we do not need
            PathBuf::from("all_is_cubes_wasm_bg.wasm"),
            PathBuf::from("all_is_cubes_wasm.js"),
            PathBuf::from("snippets"), // note this gets only the dir but not the contents
        ]);
        let snippets = &pkg_path.join("snippets/");
        set.extend(directory_tree_contents(pkg_path, snippets)?);
        set
    };
    let static_path = &Path::new("all-is-cubes-wasm/static");
    let static_files = directory_tree_contents(static_path, static_path)?;
    let dest_dir: &'static Path = Path::new("all-is-cubes-wasm/dist/");
    let client_dest_dir = dest_dir.join("client/");
    fs::create_dir_all(dest_dir)?;
    for src_file in &static_files {
        copy_file_with_context(&static_path.join(src_file), &dest_dir.join(src_file))?;
    }
    for src_file in &pkg_files {
        copy_file_with_context(&pkg_path.join(src_file), &client_dest_dir.join(src_file))?;
    }

    // Warn of unexpected files.
    // (In the future with more confidence, perhaps we should delete them.)
    let extra_dest_files = directory_tree_contents(dest_dir, dest_dir)?
        .difference(
            &pkg_files
                .into_iter()
                .map(|p| Path::new("client/").join(p))
                .collect(),
        )
        .cloned()
        .collect::<BTreeSet<_>>()
        .difference(&static_files)
        .cloned()
        .collect::<BTreeSet<_>>();
    if !extra_dest_files.is_empty() {
        eprintln!(
            "Warning: possibly stale files in {dest_dir}: {extra_dest_files:#?}",
            dest_dir = dest_dir.display()
        );
    }

    Ok(())
}

/// Run check or tests for all targets.
fn do_for_all_packages(
    config: &Config,
    op: TestOrCheck,
    features: Features,
    time_log: &mut Vec<Timing>,
) -> Result<(), ActionError> {
    ensure_wasm_tools_installed(time_log)?;

    // Ensure all-is-cubes-server build that might be looking for the files will succeed.
    // Note that this is only an “exists” check not a “up-to-date” check, on the assumption
    // that running server tests will not depend on the specific file contents.
    // TODO: That's a fragile assumption.
    if !Path::new("all-is-cubes-wasm/dist/").exists() {
        update_server_static(time_log)?;
    }

    // Test everything we can with default features and target.
    // But if we're linting, then the below --all-targets run will handle that.
    if op != TestOrCheck::Lint {
        match features {
            Features::Default => {
                let _t = CaptureTime::new(time_log, format!("{op:?}"));
                op.cargo_cmd(config).run()?;
            }

            Features::AllAndNothing => {
                {
                    let _t = CaptureTime::new(time_log, format!("{op:?} --all-features"));
                    op.cargo_cmd(config).arg("--all-features").run()?;
                }

                // To test with limited features, we need to run commands separately for each
                // package, as otherwise they will enable dependencies' features.
                for package_name in ALL_NONTEST_PACKAGES {
                    let _t = CaptureTime::new(time_log, format!("{op:?} --package {package_name}"));
                    op.cargo_cmd(config)
                        .args(["--package", package_name, "--no-default-features"])
                        .run()?;
                }
            }
        }
    }

    // Run wasm tests.
    {
        let _t = CaptureTime::new(time_log, format!("{op:?} all-is-cubes-wasm"));
        let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
        match op {
            TestOrCheck::Test => {
                // TODO: control over choice of browser
                cmd!("wasm-pack test --headless --firefox").run()?;
            }
            TestOrCheck::BuildTests | TestOrCheck::Lint => {
                op.cargo_cmd(config).arg(TARGET_WASM).run()?;
            }
        }
    }

    // Build everything else in the workspace, so non-test targets are checked for compile errors.
    {
        let _t = CaptureTime::new(time_log, "check --all-targets");
        cargo()
            .arg(CHECK_SUBCMD)
            .args(config.cargo_build_args())
            .arg("--all-targets")
            .run()?;
    }

    // Build fuzz targets that are not in the workspace
    {
        let _t = CaptureTime::new(time_log, "check fuzz");
        let _pushd: Pushd = pushd("fuzz")?;
        cargo()
            .arg(CHECK_SUBCMD)
            .args(config.cargo_build_args())
            .run()?;
    }

    Ok(())
}

/// Create files which may be useful for development in the workspace but which cannot
/// simply have constant contents.
fn write_development_files() -> Result<(), ActionError> {
    // .desktop file, used by Linux desktop application launchers to describe how to run
    // our executable. It needs to have an absolute path to the file.
    // The file is also required to be encoded in UTF-8, so non-UTF-8 paths cannot be
    // supported.
    if let Some(dir) = PROJECT_DIR.to_str() {
        let desktop_path = format!("{dir}/all-is-cubes-desktop/all-is-cubes.desktop");
        fs::write(
            &desktop_path,
            format!(
                "\
[Desktop Entry]
Type=Application
Version=1.0
Name=All is Cubes (dev)
Path={dir}
Exec=cargo run --bin all-is-cubes
Terminal=false
Categories=Game;
PrefersNonDefaultGPU=true
SingleMainWindow=true
"
            )
            .as_bytes(),
        )
        .with_context(|| format!("failed to write {desktop_path}"))?;

        // Desktops may decline to use the file unless it has the execute bit set.
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt as _;
            let mut permissions = fs::metadata(&desktop_path)
                .context("read metadata for desktop file")?
                .permissions();
            // Set execute bit
            permissions.set_mode(permissions.mode() | 0o111);
            fs::set_permissions(&desktop_path, permissions)
                .context("write metadata for desktop file")?;
        }
    } else {
        eprintln!("Skipping creation of .desktop file because path is not UTF-8");
    }

    Ok(())
}

/// cd into each workspace and do something.
///
/// [`do_for_all_packages`] doesn't use this because it has more specialized handling
fn do_for_all_workspaces<F>(mut f: F) -> Result<(), ActionError>
where
    F: FnMut() -> Result<(), ActionError>,
{
    // main workspace
    f()?;

    {
        let _pushd: Pushd = pushd("fuzz")?;
        f()?;
    }
    Ok(())
}

fn ensure_wasm_tools_installed(time_log: &mut Vec<Timing>) -> Result<(), ActionError> {
    // TODO: check that wasm-pack is installed

    // Generate combined license file.
    let license_html_path = PROJECT_DIR.join("all-is-cubes-wasm/static/third-party-licenses.html");
    let license_template_path = PROJECT_DIR.join("about.hbs");
    if newer_than(
        [&PROJECT_DIR.join("Cargo.lock"), &license_template_path],
        [&license_html_path],
    ) {
        let _t = CaptureTime::new(time_log, "cargo about generate");
        // TODO: also ensure cargo-about is installed and has at least the expected version
        cargo()
            .args([
                "about",
                "generate",
                "--fail",
                license_template_path.to_str().unwrap(),
                "-o",
                license_html_path.to_str().unwrap(),
            ])
            .run()?;
    }

    Ok(())
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum TestOrCheck {
    Test,
    BuildTests,
    Lint,
}

impl TestOrCheck {
    fn cargo_cmd(self, config: &Config) -> Cmd {
        cargo()
            .args(match self {
                Self::Test => vec!["test"],
                Self::BuildTests => vec!["test", "--no-run"],
                Self::Lint => vec![CHECK_SUBCMD],
            })
            .args(config.cargo_build_args())
    }
}

/// Which features we want to test building with.
enum Features {
    /// Test with default features only
    Default,

    /// Test each package with all features enabled and with all features disabled.
    AllAndNothing,
}

/// Path to the main All is Cubes project/repository directory.
///
/// (In the typical case, this will be equal to the current directory.)
static PROJECT_DIR: Lazy<PathBuf> = Lazy::new(|| {
    let mut path = PathBuf::from(
        std::env::var_os("CARGO_MANIFEST_DIR")
            .expect("CARGO_MANIFEST_DIR environment variable not set"),
    );
    // Sanity check
    assert!(path.is_absolute());
    // Since we are the xtask binary, we'll be given the path to the xtask package.
    assert_eq!(
        path.components().last(),
        Some(Component::Normal(OsStr::new("xtask")))
    );
    // Pop the xtask directory to become the path to the main project directory.
    path.pop();
    path
});

/// Start a [`Cmd`] with the cargo command we should use.
fn cargo() -> Cmd {
    Cmd::new(std::env::var("CARGO").expect("CARGO environment variable not set"))
}

/// Describe how long a sub-task took.
struct Timing {
    label: String,
    time: Duration,
}

impl fmt::Display for Timing {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { label, time } = self;
        write!(f, "{time:5.1?} s  {label}", time = time.as_secs_f64())
    }
}

/// Measure the time from creation to drop.
struct CaptureTime<'a> {
    label: String,
    start: Instant,
    output: &'a mut Vec<Timing>,
}

impl<'a> CaptureTime<'a> {
    fn new(time_log: &'a mut Vec<Timing>, label: impl Into<String>) -> Self {
        Self {
            label: label.into(),
            start: Instant::now(),
            output: time_log,
        }
    }
}

impl Drop for CaptureTime<'_> {
    fn drop(&mut self) {
        self.output.push(Timing {
            label: mem::take(&mut self.label),
            time: Instant::now().duration_since(self.start),
        });
    }
}
