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

use std::io::Write as _;
use std::path::Path;
use std::time::Duration;

use anyhow::Error as ActionError;
use cargo_metadata::PackageId;
use xshell::{cmd, pushd, Cmd, Pushd};

#[derive(Debug, clap::Parser)]
struct XtaskArgs {
    #[clap(subcommand)]
    command: XtaskCommand,
}

#[derive(Debug, clap::Subcommand)]
enum XtaskCommand {
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
        #[clap(long = "for-real")]
        for_real: bool,
    },
}

fn main() -> Result<(), ActionError> {
    let XtaskArgs { command } = <XtaskArgs as clap::Parser>::parse();

    match command {
        XtaskCommand::Test => {
            do_for_all_packages(TestOrCheck::Test, Features::Default)?;
        }
        XtaskCommand::TestMore => {
            exhaustive_test()?;
        }
        XtaskCommand::Lint => {
            do_for_all_packages(TestOrCheck::Lint, Features::Default)?;
            // Build docs to verify that there are no broken doc links.
            cargo().arg("doc").run()?;
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
            update_server_static()?;
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
                    std::fs::read_to_string(&manifest_path)?.parse()?;
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

                std::fs::write(&manifest_path, manifest.to_string())?;
                eprintln!("wrote version and {count_deps} deps.");
            }
            eprint!(
                "Versions updated. Manual updates are still needed for:\n\
                Documentation links\n\
                npm package\n\
                "
            );
        }
        XtaskCommand::PublishAll { for_real } => {
            update_server_static()?;
            exhaustive_test()?;

            let maybe_dry = if for_real { vec![] } else { vec!["--dry-run"] };
            for package in ALL_NONTEST_PACKAGES {
                if package == "all-is-cubes-wasm" {
                    // Not published to crates.io; built and packaged as a part of of all-is-cubes-server.
                    continue;
                }

                let _pushd: Pushd = pushd(package)?;
                let mut cmd = cargo().arg("publish").args(maybe_dry.iter().copied());
                if package == "all-is-cubes-server" {
                    // static-all-is-cubes-wasm counts as dirty despite .gitignore so we must use --allow-dirty
                    cmd = cmd.arg("--allow-dirty");
                }
                if for_real {
                    // Let crates.io pick up the new all-is-cubes version or publishing dependents will fail
                    std::thread::sleep(Duration::from_secs(10));
                }
                cmd.run()?;
            }
        }
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
fn exhaustive_test() -> Result<(), ActionError> {
    do_for_all_packages(TestOrCheck::Test, Features::AllAndNothing)?;
    Ok(())
}

/// Build the WASM and other static files for the web/game server.
/// Needed whenever `all-is-cubes-server` is being tested/run/published.
fn update_server_static() -> Result<(), ActionError> {
    ensure_wasm_tools_installed()?;
    {
        let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
        cmd!("npm run-script build").run()?;
    }

    // TODO: Copy files ourselves instead of involving rsync.
    cmd!("rsync --archive all-is-cubes-wasm/dist/ all-is-cubes-server/static-all-is-cubes-wasm/")
        .run()?;

    Ok(())
}

/// Run check or tests for all targets.
///
/// TODO: run tests with and without relevant features, like rayon
fn do_for_all_packages(op: TestOrCheck, features: Features) -> Result<(), ActionError> {
    // Install npm-based tools for all-is-cubes-wasm build.
    ensure_wasm_tools_installed()?;

    // Ensure all-is-cubes-server build will succeed.
    // Note that this is only an “exists” check not a “up-to-date” check, on the assumption
    // that running server tests will not depend on the specific file contents.
    // TODO: That's a fragile assumption.
    if !Path::new("all-is-cubes-server/static-all-is-cubes-wasm/").exists() {
        update_server_static()?;
    }

    // Test everything we can with default features and target.
    // But if we're linting, then the below --all-targets run will handle that.
    if op != TestOrCheck::Lint {
        match features {
            Features::Default => {
                op.cargo_cmd().run()?;
            }

            Features::AllAndNothing => {
                op.cargo_cmd().arg("--all-features").run()?;

                // To test with limited features, we need to run commands separately for each
                // package, as otherwise they will enable dependencies' features.
                for package_name in ALL_NONTEST_PACKAGES {
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
        let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
        cargo().arg(CHECK_SUBCMD).arg(TARGET_WASM).run()?;
    }

    // Build everything else in the workspace, so non-test targets are checked for compile errors.
    cargo().arg(CHECK_SUBCMD).arg("--all-targets").run()?;

    // Build fuzz targets that are not in the workspace
    {
        let _pushd: Pushd = pushd("fuzz")?;
        cargo().arg(CHECK_SUBCMD).run()?;
    }

    Ok(())
}

/// cd into each workspace and do something.
///
/// do_for_all_packages doesn't use this because it has more specialized handling
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

fn ensure_wasm_tools_installed() -> Result<(), ActionError> {
    if !Path::new("all-is-cubes-wasm/node_modules/.bin/webpack").exists() {
        let _pushd: Pushd = pushd("all-is-cubes-wasm")?;
        cmd!("npm install").run()?;
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

/// Start a [`Cmd`] with the cargo command we should use.
fn cargo() -> Cmd {
    Cmd::new(std::env::var("CARGO").expect("CARGO environment variable not set"))
}
