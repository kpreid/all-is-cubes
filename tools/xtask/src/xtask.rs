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

// Crate-specific lint settings. (General settings can be found in the workspace manifest.)
#![forbid(unsafe_code)]

use std::collections::BTreeSet;
use std::fmt;
use std::fs;
use std::io::Write as _;
use std::mem;
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

    /// Control which workspaces and target triples are built.
    #[arg(long = "scope", default_value = "all")]
    scope: Scope,

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

    /// Remove build files (as `cargo clean` but covering all packages)
    Clean,

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

        #[arg(long)]
        dry_run: bool,
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
enum UpdateTo {
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
enum Scope {
    /// Default for interactive use — build/check/test everything.
    All,
    /// Build only the main workspace, not the fuzz workspace.
    OnlyNormal,
    /// Build only the fuzz workspace.
    OnlyFuzz,
}

fn main() -> Result<(), ActionError> {
    let (config, command) = {
        let XtaskArgs {
            command,
            scope,
            timings,
        } = <XtaskArgs as clap::Parser>::parse();
        let config = Config {
            cargo_timings: timings,
            scope,
        };
        (config, command)
    };

    // TODO: fold time_log into the Config to reduce number of pieces passed around
    let mut time_log: Vec<Timing> = Vec::new();

    match command {
        XtaskCommand::Init => {
            write_development_files(&config)?;
            if config.scope.includes_main_workspace() {
                update_server_static(&config, &mut time_log)?; // includes installing wasm tools
            }
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
            // This applies to the main workspace & target only, because there are no
            // libraries with docs elsewhere.
            if config.scope.includes_main_workspace() {
                let _t = CaptureTime::new(&mut time_log, "doc");
                // TODO: Temporarily using --no-deps due to a bug in Rust 1.75
                // <https://github.com/rust-lang/rust/issues/114891>.
                // Revert the commit that introduced this when Rust 1.76 is released.
                cargo().arg("doc").arg("--no-deps").run()?;
            }
        }
        XtaskCommand::Fmt => {
            config.do_for_all_workspaces(|| {
                cargo().arg("fmt").run()?;
                Ok(())
            })?;
        }
        XtaskCommand::Clean => {
            config.do_for_all_workspaces(|| {
                cargo().arg("clean").run()?;
                Ok(())
            })?;
            // TODO: also remove all-is-cubes-wasm/{dist,pkg}, but do it with more sanity checks
        }
        XtaskCommand::Fuzz { duration } => {
            assert!(config.scope.includes_fuzz_workspace());

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
                    // also does a bunch of block eval stuff
                    "fuzz_mesh" => 30,
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
            update_server_static(&config, &mut time_log)?;

            cargo()
                .arg("run")
                .args(config.cargo_build_args())
                .arg("--bin=aic-server")
                .arg("--features=embed")
                .arg("--")
                .args(server_args)
                .run()?;
        }
        XtaskCommand::Update { to, dry_run } => {
            let mut options: Vec<&str> = Vec::new();
            if dry_run {
                options.push("--dry-run");
            }

            match to {
                UpdateTo::Locked => {
                    eprintln!("Doing nothing because update type is {to:?}.");
                }
                UpdateTo::Latest => {
                    config.do_for_all_workspaces(|| {
                        // Note: The `fuzz` workspace lock file is ignored in version control.
                        // But we do want to occasionally update it anyway.
                        cargo().arg("update").args(&options).run()?;
                        Ok(())
                    })?;
                }
                UpdateTo::Minimal => {
                    config.do_for_all_workspaces(|| {
                        // can't use cargo() to invoke rustup
                        cmd!("cargo +nightly")
                            .args(["update", "-Zdirect-minimal-versions"])
                            .args(&options)
                            .run()?;
                        Ok(())
                    })?;
                }
            }
        }
        XtaskCommand::SetVersion { version } => {
            assert_eq!(config.scope, Scope::All);

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
            assert_eq!(config.scope, Scope::All);

            exhaustive_test(&config, &mut time_log)?;

            let maybe_dry = if for_real { vec![] } else { vec!["--dry-run"] };
            for package in ALL_NONTEST_PACKAGES {
                if package == "all-is-cubes-wasm" {
                    // Not published to crates.io; built and packaged as a part of all-is-cubes-server.
                    continue;
                }

                let _pushd: Pushd = pushd(package)?;
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

/// Configuration which is passed down through everything.
#[derive(Debug)]
struct Config {
    cargo_timings: bool,
    scope: Scope,
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

    /// cd into each in-scope workspace and do something.
    ///
    /// [`do_for_all_packages`] doesn't use this because it has more specialized handling
    fn do_for_all_workspaces<F>(&self, mut f: F) -> Result<(), ActionError>
    where
        F: FnMut() -> Result<(), ActionError>,
    {
        // main workspace
        if self.scope.includes_main_workspace() {
            f()?;

            // TODO: split out wasm as a Scope
            {
                let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
                f()?;
            }
        }

        if self.scope.includes_fuzz_workspace() {
            let _pushd: Pushd = pushd("fuzz")?;
            f()?;
        }
        Ok(())
    }
}

impl Scope {
    fn includes_main_workspace(&self) -> bool {
        match self {
            Scope::All => true,
            Scope::OnlyNormal => true,
            Scope::OnlyFuzz => false,
        }
    }

    fn includes_fuzz_workspace(&self) -> bool {
        match self {
            Scope::All => true,
            Scope::OnlyNormal => false,
            Scope::OnlyFuzz => true,
        }
    }
}

/// TODO: fetch this list (or at least cross-check it) using `cargo metadata`.
///
/// See also [`Config::do_for_all_workspaces`].
const ALL_NONTEST_PACKAGES: [&str; 9] = [
    "all-is-cubes",
    "all-is-cubes-ui",
    "all-is-cubes-mesh",
    "all-is-cubes-gpu",
    "all-is-cubes-content",
    "all-is-cubes-port",
    "all-is-cubes-desktop",
    "all-is-cubes-server",
    "all-is-cubes-wasm",
];

const CHECK_SUBCMD: &str = "clippy";
const TARGET_WASM: &str = "--target=wasm32-unknown-unknown";

// Test all combinations of situations (that we've bothered to program test
// setup for).
fn exhaustive_test(config: &Config, time_log: &mut Vec<Timing>) -> Result<(), ActionError> {
    assert!(config.scope.includes_main_workspace());

    update_server_static(config, time_log)?;

    do_for_all_packages(config, TestOrCheck::Test, Features::AllAndNothing, time_log)?;
    Ok(())
}

/// Build the WASM and other 'client' files that the web server might need.
/// Needed for build whenever `all-is-cubes-server` is being tested/run with
/// the `embed` feature; needed for run regardless.
fn update_server_static(config: &Config, time_log: &mut Vec<Timing>) -> Result<(), ActionError> {
    assert!(config.scope.includes_main_workspace());

    ensure_wasm_tools_installed(config, time_log)?;

    // Run the compilation if needed, which ensures that the wasm binary is fresh.
    // Note: This must use the same profile as thfe wasm-pack command is! (Both are dev for now)
    {
        let _t = CaptureTime::new(time_log, "wasm cargo build");
        cargo()
            .arg("build")
            .arg("--manifest-path=all-is-cubes-wasm/Cargo.toml")
            .arg(TARGET_WASM)
            .args(config.cargo_build_args())
            .run()?;
    }

    // Run wasm-pack if and only if we need to.
    // This is because it unconditionally runs `wasm-opt` which is slow and also means
    // the files will be touched unnecessarily.
    if newer_than(
        ["all-is-cubes-wasm/target/wasm32-unknown-unknown/debug/all_is_cubes_wasm.wasm"],
        ["all-is-cubes-wasm/pkg/all_is_cubes_wasm.js"],
    ) {
        let _t = CaptureTime::new(time_log, "wasm-pack build");
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
    if config.scope.includes_main_workspace() {
        ensure_wasm_tools_installed(config, time_log)?;
    }

    // Ensure all-is-cubes-server build that might be looking for the files will succeed.
    // Note that this is only an “exists” check not a “up-to-date” check, on the assumption
    // that running server tests will not depend on the specific file contents.
    // TODO: That's a fragile assumption.
    if config.scope.includes_main_workspace() && !Path::new("all-is-cubes-wasm/dist/").exists() {
        update_server_static(config, time_log)?;
    }

    // Test everything we can with default features and target.
    // But if we're linting, then the below --all-targets run will handle that.
    if op != TestOrCheck::Lint && config.scope.includes_main_workspace() {
        match features {
            Features::Default => {
                {
                    let _t = CaptureTime::new(time_log, format!("{op:?}"));
                    op.cargo_cmd(config).run()?;
                }

                {
                    // Check `all-is-cubes` with default features disabled, because that's
                    // more easily broken by accident (such as by introducing an unintended
                    // `Send` bound) then our other features.
                    let _t = CaptureTime::new(time_log, "check aic no_std");
                    cargo()
                        .arg(op.non_build_check_subcmd())
                        .args(["--package=all-is-cubes", "--no-default-features"])
                        .run()?;
                }
            }

            Features::AllAndNothing => {
                {
                    let _t = CaptureTime::new(time_log, format!("{op:?} --all-features"));
                    op.cargo_cmd(config).arg("--all-features").run()?;
                }

                // To test with limited features, we need to run commands separately for each
                // package, as otherwise they will enable dependencies' features.
                for package_name in ALL_NONTEST_PACKAGES {
                    if package_name == "all-is-cubes-wasm" {
                        // not in main workspace, and incidentally also has no features to test
                        // TODO: make this exemption data-driven
                        continue;
                    }

                    let _t = CaptureTime::new(time_log, format!("{op:?} --package {package_name}"));
                    op.cargo_cmd(config)
                        .args(["--package", package_name, "--no-default-features"])
                        .run()?;
                }
            }
        }
    }

    // Run wasm tests.
    if config.scope.includes_main_workspace() {
        let _t = CaptureTime::new(time_log, format!("{op:?} all-is-cubes-wasm"));

        match op {
            TestOrCheck::Test => {
                // Run host-side tests (which exist because they're cheaper and because I made them
                // first).
                cmd!("cargo test --manifest-path=all-is-cubes-wasm/Cargo.toml").run()?;

                // TODO: control over choice of browser
                cmd!("wasm-pack test --headless --firefox all-is-cubes-wasm/").run()?;
            }
            TestOrCheck::BuildTests | TestOrCheck::Lint => {
                let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
                op.cargo_cmd(config).arg(TARGET_WASM).run()?;
            }
        }
    }

    // Check everything else in the workspace, so non-test targets are checked for compile errors.
    if config.scope.includes_main_workspace() {
        let _t = CaptureTime::new(time_log, "check --all-targets");
        cargo()
            .arg(op.non_build_check_subcmd())
            .args(config.cargo_build_args())
            .arg("--all-targets")
            .run()?;
    }

    // Check fuzz targets that are not in the main workspace
    if config.scope.includes_fuzz_workspace() {
        let _t = CaptureTime::new(time_log, "check fuzz");
        let _pushd: Pushd = pushd("fuzz")?;
        cargo()
            .arg(op.non_build_check_subcmd())
            .args(config.cargo_build_args())
            .run()?;
    }

    Ok(())
}

/// Create files which may be useful for development in the workspace but which cannot
/// simply have constant contents.
fn write_development_files(_config: &Config) -> Result<(), ActionError> {
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

fn ensure_wasm_tools_installed(
    config: &Config,
    time_log: &mut Vec<Timing>,
) -> Result<(), ActionError> {
    assert!(config.scope.includes_main_workspace());

    // TODO: check that wasm-pack is installed

    // Generate combined license file for the wasm build.
    let web_ws_path = PROJECT_DIR.join("all-is-cubes-wasm");
    let license_html_path = PROJECT_DIR.join("all-is-cubes-wasm/static/third-party-licenses.html");
    let license_template_path = PROJECT_DIR.join("tools/about.hbs");
    let config_path = PROJECT_DIR.join("tools/about.toml");
    if newer_than(
        [&web_ws_path.join("Cargo.lock"), &license_template_path],
        [&license_html_path],
    ) {
        let _t = CaptureTime::new(time_log, "cargo about generate");
        // TODO: also ensure cargo-about is installed and has at least the expected version
        cargo()
            .args([
                "about",
                "generate",
                "--fail",
                "--config",
                config_path.to_str().unwrap(),
                license_template_path.to_str().unwrap(),
                "-o",
                license_html_path.to_str().unwrap(),
                "--manifest-path",
                web_ws_path.join("Cargo.toml").to_str().unwrap(),
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

    /// Return the cargo subcommand to use for the targets that we are *not* planning or able
    /// to run.
    fn non_build_check_subcmd(&self) -> &'static str {
        match self {
            // In place of testing, we use check instead of clippy.
            // This is so that in CI, when a rustc beta release has a broken clippy lint,
            // it doesn't block us running our tests.
            // It also aligns with the behavior of actual testing — a `cargo build` or
            // `cargo test` doesn't run clippy.
            TestOrCheck::Test => "check",
            TestOrCheck::BuildTests => "check",
            TestOrCheck::Lint => CHECK_SUBCMD,
        }
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
    assert!(path.ends_with("tools/xtask"), "{path:?}");
    // Since we are the xtask binary, we'll be given the path to the xtask package.
    // Pop `tools/xtask` to become the path to the main project directory.
    path.pop();
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

/// Measure the time from creation to drop. Also prints text to mark these spans.
struct CaptureTime<'a> {
    label: String,
    start: Instant,
    output: &'a mut Vec<Timing>,
}

impl<'a> CaptureTime<'a> {
    fn new(time_log: &'a mut Vec<Timing>, label: impl Into<String>) -> Self {
        let label = label.into();
        if std::env::var("GITHUB_ACTIONS").is_ok() {
            eprintln!("::group::{label}");
        } else {
            eprintln!("------ [xtask] START: {label} ------");
        }
        Self {
            label,
            start: Instant::now(),
            output: time_log,
        }
    }
}

impl Drop for CaptureTime<'_> {
    fn drop(&mut self) {
        let label = mem::take(&mut self.label);
        let time = Instant::now().duration_since(self.start);
        if std::env::var("GITHUB_ACTIONS").is_ok() {
            eprintln!("::endgroup::");
        } else {
            eprintln!(
                "------ [xtask] END: {label} ({time:.1?} s) ------",
                time = time.as_secs_f64()
            );
        }
        self.output.push(Timing { label, time });
    }
}
