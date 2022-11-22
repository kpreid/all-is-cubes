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
use std::time::SystemTime;

use anyhow::Context as _;
use anyhow::Error as ActionError;
use cargo_metadata::PackageId;
use once_cell::sync::Lazy;
use xshell::{cmd, pushd, Cmd, Pushd};

#[derive(Debug, clap::Parser)]
struct XtaskArgs {
    #[clap(subcommand)]
    command: XtaskCommand,
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
    Test,

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

    /// Run webpack dev server (for testing `all-is-cubes-wasm`).
    RunDev,

    RunGameServer,

    /// Update dependency versions.
    Update,

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

fn main() -> Result<(), ActionError> {
    let XtaskArgs { command } = <XtaskArgs as clap::Parser>::parse();

    let mut time_log: Vec<Timing> = Vec::new();

    match command {
        XtaskCommand::Init => {
            write_development_files()?;
            update_server_static(&mut time_log)?; // includes installing wasm tools
        }
        XtaskCommand::Test => {
            do_for_all_packages(TestOrCheck::Test, Features::Default, &mut time_log)?;
        }
        XtaskCommand::TestMore => {
            exhaustive_test(&mut time_log)?;
        }
        XtaskCommand::Lint => {
            do_for_all_packages(TestOrCheck::Lint, Features::Default, &mut time_log)?;

            // Build docs to verify that there are no broken doc links.
            {
                let _t = CaptureTime::new(&mut time_log, "doc");
                cargo().arg("doc").run()?;
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
                // be fast. (TODO: Tune this per-target.)
                let timeout = 5; // seconds

                cmd!("cargo +nightly fuzz run")
                    .env("RUST_BACKTRACE", "1")
                    .arg(&target.name)
                    .arg("--")
                    .arg(format!("-timeout={timeout}"))
                    .arg(format!("-max_total_time={duration}"))
                    .run()?;
            }
        }
        XtaskCommand::RunDev => {
            let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
            cmd!("npm start").run()?;
        }
        XtaskCommand::RunGameServer => {
            cargo().arg("run").arg("--bin").arg("aic-server").run()?;
        }
        XtaskCommand::Update => {
            cargo().arg("update").run()?;
            let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
            cmd!("npm update").run()?;
            cmd!("npm install").run()?;
        }
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
                Documentation links in the main README\n\
                npm package\n\
                "
            );
        }
        XtaskCommand::PublishAll { for_real } => {
            exhaustive_test(&mut time_log)?;

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

/// TODO: fetch this list (or at least cross-check it) using `cargo metadata`.
///
/// See also [`do_for_all_workspaces`].
const ALL_NONTEST_PACKAGES: [&str; 7] = [
    "all-is-cubes",
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
fn exhaustive_test(time_log: &mut Vec<Timing>) -> Result<(), ActionError> {
    // building server with `--feature embed` requires static files
    update_server_static(time_log)?;

    do_for_all_packages(TestOrCheck::Test, Features::AllAndNothing, time_log)?;
    Ok(())
}

/// Build the WASM and other 'client' files that the web server might need.
/// Needed for build whenever `all-is-cubes-server` is being tested/run with
/// the `embed` feature; needed for run regardless.
fn update_server_static(time_log: &mut Vec<Timing>) -> Result<(), ActionError> {
    ensure_wasm_tools_installed(time_log)?;
    {
        let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
        cmd!("npm run-script build").run()?;
    }

    Ok(())
}

/// Run check or tests for all targets.
fn do_for_all_packages(
    op: TestOrCheck,
    features: Features,
    time_log: &mut Vec<Timing>,
) -> Result<(), ActionError> {
    {
        // Install npm-based tools for all-is-cubes-wasm build.
        ensure_wasm_tools_installed(time_log)?;
    }

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
                op.cargo_cmd().run()?;
            }

            Features::AllAndNothing => {
                {
                    let _t = CaptureTime::new(time_log, format!("{op:?} --all-features"));
                    op.cargo_cmd().arg("--all-features").run()?;
                }

                // To test with limited features, we need to run commands separately for each
                // package, as otherwise they will enable dependencies' features.
                for package_name in ALL_NONTEST_PACKAGES {
                    let _t = CaptureTime::new(time_log, format!("{op:?} --package {package_name}"));
                    op.cargo_cmd()
                        .args(["--package", package_name, "--no-default-features"])
                        .run()?;
                }
            }
        }
    }

    // Check wasm-only code.
    // (Supposedly, running `npm test` should run tests inside JS, but that seems
    // to do nothing for me, so we're limited to confirming it compiles.)
    {
        let _t = CaptureTime::new(time_log, format!("{op:?} all-is-cubes-wasm"));
        let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
        cargo().arg(CHECK_SUBCMD).arg(TARGET_WASM).run()?;
    }

    // Build everything else in the workspace, so non-test targets are checked for compile errors.
    {
        let _t = CaptureTime::new(time_log, "check --all-targets");
        cargo().arg(CHECK_SUBCMD).arg("--all-targets").run()?;
    }

    // Build fuzz targets that are not in the workspace
    {
        let _t = CaptureTime::new(time_log, "check fuzz");
        let _pushd: Pushd = pushd("fuzz")?;
        cargo().arg(CHECK_SUBCMD).run()?;
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
    if !Path::new("all-is-cubes-wasm/node_modules/.bin/webpack").exists() {
        let _t = CaptureTime::new(time_log, "npm install");
        let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
        cmd!("npm install").run()?;
    }

    // Generate combined license file.
    let license_html_path = PROJECT_DIR.join("all-is-cubes-wasm/static/third-party-licenses.html");
    let license_template_path = PROJECT_DIR.join("about.hbs");
    if newer_than(
        [&PROJECT_DIR.join("Cargo.lock"), &license_template_path],
        [&license_html_path],
    ) {
        // TODO: also ensure cargo-about is installed and has at least the expected version
        cargo()
            .args([
                "about",
                "generate",
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
    Lint,
}

impl TestOrCheck {
    fn cargo_cmd(self) -> Cmd {
        cargo().arg(match self {
            Self::Test => "test",
            Self::Lint => CHECK_SUBCMD,
        })
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

/// Test if some input files are newer than some output files, or if the output files don't exist.
#[track_caller]
fn newer_than<P1, P2>(inputs: impl AsRef<[P1]>, outputs: impl AsRef<[P2]>) -> bool
where
    P1: AsRef<Path>,
    P2: AsRef<Path>,
{
    let newest_input: SystemTime = inputs
        .as_ref()
        .iter()
        .map(|path| {
            fs::metadata(path)
                .unwrap_or_else(|e| {
                    panic!(
                        "IO error while checking timestamp of {p}: {e}",
                        p = path.as_ref().display()
                    )
                })
                .modified()
                .expect("mtime support required")
        })
        .max()
        .expect("Must have at least one input file");
    let oldest_output: Option<SystemTime> = outputs
        .as_ref()
        .iter()
        .map(|path| {
            let path: &Path = path.as_ref();
            match fs::metadata(path) {
                Ok(metadata) => Some(metadata.modified().unwrap()),
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
                Err(e) => panic!(
                    "IO error while checking timestamp of {p}: {e}",
                    p = path.display()
                ),
            }
        })
        .min()
        .expect("Must have at least one output file");

    match oldest_output {
        None => true, // some file did not exist
        Some(t) => newest_input >= t,
    }
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
