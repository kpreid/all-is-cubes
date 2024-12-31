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
use std::sync::LazyLock;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context as _;
use anyhow::Error as ActionError;
use cargo_metadata::PackageId;
use xshell::{cmd, Cmd, Shell};

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

    /// Pass the `--quiet` flag to all `cargo` build/test invocations.
    /// This hides build progress and also switches the test harness to a more concise output.
    #[arg(long, global = true)]
    quiet: bool,
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
        /// Build test executables, but don't run them.
        #[arg(long)]
        no_run: bool,
    },

    /// Run tests exercising more combinations of features.
    TestMore {
        #[arg(long)]
        no_run: bool,
    },

    /// Check for lint and generate documentation, but do not test.
    ///
    /// Caution: If there are rustc or clippy warnings only, the exit code is still zero.
    /// <https://github.com/rust-lang/rust-clippy/issues/1209>
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

    BuildWebRelease,

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

#[derive(Clone, Copy, Debug, PartialEq, strum::IntoStaticStr, strum::Display)]
#[strum(serialize_all = "kebab-case")]
enum Profile {
    Dev,
    Release,
}
impl Profile {
    /// Name of the subdirectory in `target/` where Cargo puts builds with this profile.
    fn target_subdirectory_name(self) -> &'static Path {
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

fn main() -> Result<(), ActionError> {
    let sh = &Shell::new()?;

    let (config, command) = {
        let XtaskArgs {
            command,
            scope,
            timings,
            quiet,
        } = <XtaskArgs as clap::Parser>::parse();
        let config = Config {
            sh,
            cargo_timings: timings,
            cargo_quiet: quiet,
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
                build_web(&config, &mut time_log, Profile::Dev)?; // includes installing wasm tools
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
        XtaskCommand::TestMore { no_run } => {
            exhaustive_test(
                &config,
                if no_run {
                    TestOrCheck::BuildTests
                } else {
                    TestOrCheck::Test
                },
                &mut time_log,
            )?;
        }
        XtaskCommand::Lint => {
            do_for_all_packages(&config, TestOrCheck::Lint, Features::Default, &mut time_log)?;

            // Build docs to verify that there are no broken doc links.
            // This applies to the main workspace & target only, because there are no
            // libraries with docs elsewhere.
            if config.scope.includes_main_workspace() {
                let _t = CaptureTime::new(&mut time_log, "doc");
                config
                    .cargo()
                    .env("RUSTDOCFLAGS", "-Dwarnings")
                    .arg("doc")
                    .args(config.cargo_build_args())
                    .run()?;
            }
        }
        XtaskCommand::Fmt => {
            config.do_for_all_workspaces(|| {
                config.cargo().arg("fmt").run()?;
                Ok(())
            })?;
        }
        XtaskCommand::Clean => {
            config.do_for_all_workspaces(|| {
                config.cargo().arg("clean").run()?;
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

                cmd!(config.sh, "cargo +nightly fuzz run")
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
            config
                .cargo()
                .arg("watch")
                .arg("-s")
                .arg("cargo xtask run-game-server -- --client-source workspace --port 8080")
                .run()?;
        }
        XtaskCommand::RunGameServer { server_args } => {
            build_web(&config, &mut time_log, Profile::Dev)?;

            config
                .cargo()
                .arg("run")
                .args(config.cargo_build_args())
                .arg("--bin=aic-server")
                .arg("--features=embed")
                .arg("--")
                .args(server_args)
                .run()?;
        }
        XtaskCommand::BuildWebRelease => {
            // We only generate the license file in release builds, to save time.
            generate_wasm_licenses_file(&config, &mut time_log)?;

            build_web(&config, &mut time_log, Profile::Release)?;
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
                        config.cargo().arg("update").args(&options).run()?;
                        Ok(())
                    })?;
                }
                UpdateTo::Minimal => {
                    config.do_for_all_workspaces(|| {
                        // can't use cargo() to invoke rustup
                        cmd!(config.sh, "cargo +nightly")
                            .args(["update", "-Zdirect-minimal-versions"])
                            .args(&options)
                            .run()?;
                        Ok(())
                    })?;
                }
            }
        }
        XtaskCommand::SetVersion { version } => {
            /// Given a dependency table entry like `foo = "1.2.3"` or
            /// `foo = { version = "1.2.3" }`, find the version field.
            /// Returns None if there is no such field.
            fn find_version_field(input: &mut toml_edit::Item) -> Option<&mut toml_edit::Item> {
                match input {
                    toml_edit::Item::None => None,
                    toml_edit::Item::Value(toml_edit::Value::String(_)) => Some(input),
                    table_like => match &mut table_like["version"] {
                        // Don't add a version that didn't exist.
                        toml_edit::Item::None => None,
                        value @ toml_edit::Item::Value(_) => Some(value),
                        v => panic!("unexpected structure {v:?}"),
                    },
                }
            }

            assert_eq!(config.scope, Scope::All);

            let new_version_value: toml_edit::Item = toml_edit::value(version.as_str());
            for manifest_dir in ALL_NONTEST_PACKAGES.into_iter().chain(["."]) {
                let manifest_path = format!("{manifest_dir}/Cargo.toml");
                eprint!("Editing {manifest_path}...");
                let _ = std::io::stderr().flush();

                let mut manifest: toml_edit::DocumentMut =
                    fs::read_to_string(&manifest_path)?.parse()?;

                // For packages only
                let package_version_text = if manifest_dir != "." {
                    assert_eq!(manifest["package"]["name"].as_str(), Some(manifest_dir));

                    // Update version of the package itself
                    manifest["package"]["version"] = new_version_value.clone();
                    "package version and "
                } else {
                    ""
                };

                // Update versions of dependencies in workspace or packages
                let deps_table = if manifest_dir == "." {
                    manifest["workspace"]["dependencies"].as_table_mut()
                } else {
                    manifest["dependencies"].as_table_mut()
                };
                let mut count_deps = 0;
                for version_field in deps_table
                    .expect("dependencies not a table")
                    .iter_mut()
                    .filter(|(dep_key, _)| ALL_NONTEST_PACKAGES.contains(&dep_key.get()))
                    .filter_map(|(_, dep_value)| find_version_field(dep_value))
                {
                    *version_field = new_version_value.clone();
                    count_deps += 1;
                }

                fs::write(&manifest_path, manifest.to_string())?;
                eprintln!("wrote {package_version_text}{count_deps} deps.");
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

            exhaustive_test(&config, TestOrCheck::Test, &mut time_log)?;

            let maybe_dry = if for_real { vec![] } else { vec!["--dry-run"] };
            for package in ALL_NONTEST_PACKAGES {
                if package == "all-is-cubes-wasm" {
                    // Not published to crates.io; built and packaged as a part of all-is-cubes-server.
                    continue;
                }

                let _pushd = sh.push_dir(package);
                config
                    .cargo()
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
struct Config<'a> {
    sh: &'a Shell,
    cargo_timings: bool,
    cargo_quiet: bool,
    scope: Scope,
}

impl Config<'_> {
    /// Start a [`Cmd`] with the cargo command we should use.
    /// Currently, this doesn’t actually depend on the configuration, just the Shell
    fn cargo(&self) -> Cmd<'_> {
        self.sh
            .cmd(std::env::var("CARGO").expect("CARGO environment variable not set"))
    }

    /// Arguments that should be passed to any Cargo command that runs a build
    /// (`build`, `test`, `run`)
    fn cargo_build_args(&self) -> Vec<&str> {
        let mut args = Vec::with_capacity(1);
        if self.cargo_timings {
            args.push("--timings")
        }
        if self.cargo_quiet {
            args.push("--quiet")
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
                let _pushd = self.sh.push_dir("all-is-cubes-wasm");
                f()?;
            }
        }

        if self.scope.includes_fuzz_workspace() {
            let _pushd = self.sh.push_dir("fuzz");
            f()?;
        }
        Ok(())
    }
}

impl Scope {
    fn includes_main_workspace(self) -> bool {
        match self {
            Scope::All => true,
            Scope::OnlyNormal => true,
            Scope::OnlyFuzz => false,
        }
    }

    fn includes_fuzz_workspace(self) -> bool {
        match self {
            Scope::All => true,
            Scope::OnlyNormal => false,
            Scope::OnlyFuzz => true,
        }
    }
}

/// List of all packges that are part of the application and not development tools.
///
/// * This is more than all *published* packages, because `all-is-cubes-wasm` is not
///   currently published.
/// * It must be ordered with dependencies before dependents, to allow `publish-all`
///   to succeed.
///
/// TODO: fetch this list (or at least cross-check it) using `cargo metadata`.
///
/// See also [`Config::do_for_all_workspaces`].
const ALL_NONTEST_PACKAGES: [&str; 11] = [
    "all-is-cubes-base",
    "all-is-cubes",
    "all-is-cubes-render",
    "all-is-cubes-ui",
    "all-is-cubes-content",
    "all-is-cubes-mesh",
    "all-is-cubes-gpu",
    "all-is-cubes-port",
    "all-is-cubes-desktop",
    "all-is-cubes-wasm",
    "all-is-cubes-server",
];

const CHECK_SUBCMD: &str = "clippy";
const TARGET_WASM: &str = "--target=wasm32-unknown-unknown";

// Test all combinations of situations (that we've bothered to program test
// setup for).
fn exhaustive_test(
    config: &Config<'_>,
    op: TestOrCheck,
    time_log: &mut Vec<Timing>,
) -> Result<(), ActionError> {
    assert!(config.scope.includes_main_workspace());

    build_web(config, time_log, Profile::Dev)?;

    do_for_all_packages(config, op, Features::AllAndNothing, time_log)?;
    Ok(())
}

fn static_web_app_out_dir(profile: Profile) -> PathBuf {
    PROJECT_DIR.join(format!("all-is-cubes-wasm/target/web-app-{profile}"))
}

/// Build the WASM and other 'client' files that the web server might need.
/// Needed for build whenever `all-is-cubes-server` is being tested/run with
/// the `embed` feature; needed to run the server regardless.
fn build_web(
    config: &Config<'_>,
    time_log: &mut Vec<Timing>,
    profile: Profile,
) -> Result<(), ActionError> {
    // Currently, wasm is considered part of the main workspace scope,
    // even though it is actually a separate workspace. This is a bug.
    // <https://github.com/kpreid/all-is-cubes/issues/270>
    // <https://github.com/kpreid/all-is-cubes/issues/410>
    assert!(config.scope.includes_main_workspace());

    let wasm_package_dir: &Path = &PROJECT_DIR.join("all-is-cubes-wasm");

    ensure_wasm_tools_installed(config, time_log)?;

    // Run the compilation if needed, which ensures that the wasm binary is fresh.
    // We do this explicitly because wasm-pack release builds run `wasm-opt` unconditionally,
    // and we want to do modification time checks instead.
    {
        let _t = CaptureTime::new(time_log, format!("wasm cargo build --{profile}"));
        config
            .cargo()
            .arg("build")
            .arg("--manifest-path")
            .arg(wasm_package_dir.join("Cargo.toml"))
            .arg("--profile")
            .arg(<&str>::from(profile))
            .arg(TARGET_WASM)
            .args(config.cargo_build_args())
            .run()?;
    }

    // Directory we ask wasm-pack to write its output (modified wasm and generated JS) to.
    let wasm_pack_out_dir = wasm_package_dir.join(format!("target/wasm-pack-{profile}"));
    fs::create_dir_all(&wasm_pack_out_dir)?;
    // Directory where we write the files that make up the all-static web version of All is Cubes,
    // which are made from wasm_pack_out_dir and our static files.
    let static_web_app_out_dir = &static_web_app_out_dir(profile);
    fs::create_dir_all(static_web_app_out_dir)?;

    // Run wasm-pack if and only if we need to because the `cargo build` output is newer than
    // the wasm-pack generated JS.
    if newer_than(
        [PathBuf::from_iter([
            wasm_package_dir,
            Path::new("target/wasm32-unknown-unknown"),
            profile.target_subdirectory_name(),
            Path::new("all_is_cubes_wasm.wasm"),
        ])],
        [wasm_pack_out_dir.join("all_is_cubes_wasm.js")],
    ) {
        let _t = CaptureTime::new(time_log, format!("wasm-pack build --{profile}"));
        cmd!(config.sh, "wasm-pack build --target web")
            .arg("--out-dir")
            .arg(
                wasm_pack_out_dir
                    .canonicalize()
                    .with_context(|| wasm_pack_out_dir.display().to_string())?,
            )
            .arg(format!("--{profile}"))
            .arg(wasm_package_dir)
            .run()?;
    }

    // Combine the static files and build results in the same way that webpack used to
    // (This will need replacement if we get subdirectories)
    let wasm_pack_output_files: BTreeSet<PathBuf> = {
        let mut set = BTreeSet::from([
            // There are lots of other files in pkg which we do not need
            PathBuf::from("all_is_cubes_wasm_bg.wasm"),
            PathBuf::from("all_is_cubes_wasm.js"),
            PathBuf::from("snippets"), // note this gets only the dir but not the contents
        ]);
        set.extend(directory_tree_contents(
            &wasm_pack_out_dir,
            &wasm_pack_out_dir.join("snippets/"),
        )?);
        set
    };
    // Path for our non-compiled static files that should be included.
    let static_input_files_path = &wasm_package_dir.join("static");
    let static_files = directory_tree_contents(static_input_files_path, static_input_files_path)?;
    // Path where the wasm and js files are put.
    let web_client_dir = static_web_app_out_dir.join("client/");
    fs::create_dir_all(static_web_app_out_dir)?;
    for src_file in &static_files {
        copy_file_with_context(
            &static_input_files_path.join(src_file),
            &static_web_app_out_dir.join(src_file),
        )?;
    }
    for src_file in &wasm_pack_output_files {
        copy_file_with_context(
            &wasm_pack_out_dir.join(src_file),
            &web_client_dir.join(src_file),
        )?;
    }

    // Warn of unexpected files.
    // (In the future with more confidence, perhaps we should delete them.)
    let extra_dest_files = directory_tree_contents(static_web_app_out_dir, static_web_app_out_dir)?
        .difference(
            &wasm_pack_output_files
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
            "Warning: possibly stale files in {static_web_app_out_dir}: {extra_dest_files:#?}",
            static_web_app_out_dir = static_web_app_out_dir.display()
        );
    }

    Ok(())
}

/// Run check or tests for all targets.
fn do_for_all_packages(
    config: &Config<'_>,
    op: TestOrCheck,
    features: Features,
    time_log: &mut Vec<Timing>,
) -> Result<(), ActionError> {
    if config.scope.includes_main_workspace() {
        ensure_wasm_tools_installed(config, time_log)?;

        // Ensure all-is-cubes-server build that might be looking for the web client files will succeed.
        if !static_web_app_out_dir(Profile::Dev).exists() {
            build_web(config, time_log, Profile::Dev)?;
        }
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
                    // Check no_std compatiblepackages with default features disabled, because
                    // that's more easily broken by accident (such as by introducing an unintended
                    // `Send` bound) then our other features.
                    //
                    // TODO: Replace this one-off list of packages with something more centralized,
                    // and shared with the CI that actually builds a no_std target.
                    let _t = CaptureTime::new(time_log, "check aic no_std");
                    config
                        .cargo()
                        .arg(op.non_build_check_subcmd())
                        .args([
                            "--package=all-is-cubes",
                            "--package=all-is-cubes-mesh",
                            "--package=all-is-cubes-render",
                            "--package=all-is-cubes-ui",
                            "--no-default-features",
                        ])
                        .run()?;
                }
            }

            Features::AllAndNothing => {
                {
                    let _t = CaptureTime::new(time_log, format!("{op:?} --all-features"));
                    op.cargo_cmd(config)
                        .args(["--all-targets", "--all-features"])
                        .run()?;
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
                        .args([
                            "--package",
                            package_name,
                            "--all-targets",
                            "--no-default-features",
                        ])
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
                cmd!(
                    config.sh,
                    "cargo test --manifest-path=all-is-cubes-wasm/Cargo.toml"
                )
                .run()?;

                // TODO: more general control over choice of browser / autodetection, and
                // run tests on *all* available browsers.

                let browser_arg = if option_env!("CI").is_some() && cfg!(target_os = "macos") {
                    // 2024-04: GitHub's new Apple Silicon macOS CI images don't have Firefox,
                    // only Chrome and Safari.
                    "--chrome"
                } else {
                    "--firefox"
                };

                cmd!(config.sh, "wasm-pack test --headless")
                    .arg(browser_arg)
                    .arg("all-is-cubes-wasm/")
                    .run()?;
            }
            TestOrCheck::BuildTests | TestOrCheck::Lint => {
                let _pushd = config.sh.push_dir("all-is-cubes-wasm");
                op.cargo_cmd(config).arg(TARGET_WASM).run()?;
            }
        }
    }

    // Check everything else in the workspace, so non-test targets are checked for compile errors.
    if config.scope.includes_main_workspace() {
        let _t = CaptureTime::new(time_log, "check --all-targets");
        config
            .cargo()
            .arg(op.non_build_check_subcmd())
            .args(config.cargo_build_args())
            .arg("--all-targets")
            .run()?;
    }

    // Check fuzz targets that are not in the main workspace
    if config.scope.includes_fuzz_workspace() {
        let _t = CaptureTime::new(time_log, "check fuzz");
        let _pushd = config.sh.push_dir("fuzz");
        config
            .cargo()
            .arg(op.non_build_check_subcmd())
            .args(config.cargo_build_args())
            .run()?;
    }

    Ok(())
}

/// Create files which may be useful for development in the workspace but which cannot
/// simply have constant contents.
fn write_development_files(_config: &Config<'_>) -> Result<(), ActionError> {
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

#[allow(clippy::unnecessary_wraps)]
fn ensure_wasm_tools_installed(
    config: &Config<'_>,
    _: &mut Vec<Timing>,
) -> Result<(), ActionError> {
    assert!(config.scope.includes_main_workspace());

    // TODO: check that wasm-pack is installed

    Ok(())
}

fn generate_wasm_licenses_file(
    config: &Config<'_>,
    time_log: &mut Vec<Timing>,
) -> Result<(), ActionError> {
    let web_ws_path = PROJECT_DIR.join("all-is-cubes-wasm");
    let license_html_path = PROJECT_DIR.join("all-is-cubes-wasm/static/third-party-licenses.html");
    let license_template_path = PROJECT_DIR.join("tools/about.hbs");
    if newer_than(
        [&web_ws_path.join("Cargo.lock"), &license_template_path],
        [&license_html_path],
    ) {
        let _t = CaptureTime::new(time_log, "cargo about generate");
        // TODO: also ensure cargo-about is installed and has at least the expected version
        config
            .cargo()
            .args([
                "about",
                "generate",
                "--fail",
                "--config",
                PROJECT_DIR.join("tools/about.toml").to_str().unwrap(),
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
    fn cargo_cmd<'a>(self, config: &'a Config<'_>) -> Cmd<'a> {
        config
            .cargo()
            .args(match self {
                Self::Test => vec!["test"],
                Self::BuildTests => vec!["test", "--no-run"],
                Self::Lint => vec![CHECK_SUBCMD],
            })
            .args(config.cargo_build_args())
    }

    /// Return the cargo subcommand to use for the targets that we are *not* planning or able
    /// to run.
    fn non_build_check_subcmd(self) -> &'static str {
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
#[derive(Clone, Copy, Debug)]
enum Features {
    /// Test with default features only
    Default,

    /// Test each package with all features enabled and with all features disabled.
    AllAndNothing,
}

/// Path to the main All is Cubes project/repository directory.
///
/// (In the typical case, this will be equal to the current directory.)
static PROJECT_DIR: LazyLock<PathBuf> = LazyLock::new(|| {
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
