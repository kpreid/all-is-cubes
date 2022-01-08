// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

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

use std::path::Path;
use std::time::Duration;

use clap::{App, AppSettings, Arg, SubCommand};
use xaction::{cmd, Cmd};

fn main() -> Result<(), xaction::Error> {
    let matches = App::new("xtask")
        .setting(AppSettings::SubcommandRequired)
        .arg(
            Arg::with_name("without-luminance")
                .long("without-luminance")
                .global(true)
                .help("Avoid depending on the luminance library."),
        )
        .subcommand(SubCommand::with_name("test"))
        .subcommand(
            SubCommand::with_name("lint").help("Compile and report warnings without testing."),
        )
        .subcommand(SubCommand::with_name("run-dev"))
        .subcommand(SubCommand::with_name("run-game-server"))
        .subcommand(SubCommand::with_name("update").help("Update dependency versions."))
        .subcommand(
            SubCommand::with_name("publish-all").arg(
                Arg::with_name("for-real")
                    .long("for-real")
                    .help("Actually publish crates rather than dry run"),
            ),
        )
        .get_matches();

    let features = if matches.is_present("without-luminance") {
        Features::WithoutLuminance
    } else {
        Features::Default
    };

    match matches.subcommand() {
        ("test", Some(_matches)) => {
            do_for_all_packages(TestOrCheck::Test, features)?;
        }
        ("lint", Some(_matches)) => {
            do_for_all_packages(TestOrCheck::Lint, features)?;
            // Build docs to verify that there are no broken doc links.
            cargo().arg("doc").run()?;
        }
        ("run-dev", Some(_matches)) => {
            let _pushd = xaction::pushd("all-is-cubes-wasm");
            cmd!("npm start").run()?;
        }
        ("run-game-server", Some(_matches)) => {
            update_server_static()?;
            cargo().arg("run").arg("--bin").arg("aic-server").run()?;
        }
        ("update", Some(_matches)) => {
            cargo().arg("update").run()?;
            let _pushd = xaction::pushd("all-is-cubes-wasm");
            cmd!("npm update").run()?;
            cmd!("npm install").run()?;
        }
        ("publish-all", Some(publish_matches)) => {
            update_server_static()?;
            exhaustive_test()?;

            let for_real = publish_matches.is_present("for-real");
            let maybe_dry = if for_real { vec![] } else { vec!["--dry-run"] };
            for package in [
                "all-is-cubes",
                "all-is-cubes-content",
                "all-is-cubes-desktop",
                "all-is-cubes-server",
            ] {
                let _pushd = xaction::pushd(package);
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
        _ => panic!("shouldn't happen: command not matched"),
    }
    Ok(())
}

const CHECK_SUBCMD: &str = "clippy";
const TARGET_WASM: &str = "--target=wasm32-unknown-unknown";

fn exhaustive_test() -> Result<(), xaction::Error> {
    // TODO: This should be a more exhaustive test and lint procedure
    do_for_all_packages(TestOrCheck::Test, Features::Default)?;
    Ok(())
}

/// Build the WASM and other static files for the web/game server.
/// Needed whenever `all-is-cubes-server` is being tested/run/published.
fn update_server_static() -> Result<(), xaction::Error> {
    ensure_wasm_tools_installed()?;
    {
        let _pushd = xaction::pushd("all-is-cubes-wasm");
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
fn do_for_all_packages(op: TestOrCheck, features: Features) -> Result<(), xaction::Error> {
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
        op.cargo_cmd().args(features.cargo_flags()).run()?;
    }

    // Check wasm-only code.
    // (Supposedly, running `npm test` should run tests inside JS, but that seems
    // to do nothing for me, so we're limited to confirming it compiles.)
    {
        let _pushd = xaction::pushd("all-is-cubes-wasm");
        cargo().arg(CHECK_SUBCMD).arg(TARGET_WASM).run()?;
    }

    // Build everything else in the workspace, so non-test targets are checked for compile errors.
    cargo().arg(CHECK_SUBCMD).arg("--all-targets").run()?;

    // Build fuzz targets that are not in the workspace
    {
        let _pushd = xaction::pushd("all-is-cubes/fuzz");
        cargo().arg(CHECK_SUBCMD).run()?;
    }

    Ok(())
}

fn ensure_wasm_tools_installed() -> Result<(), xaction::Error> {
    if !Path::new("all-is-cubes-wasm/node_modules/.bin/webpack").exists() {
        let _pushd = xaction::pushd("all-is-cubes-wasm");
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
/// This will need to become more combinatorial.
enum Features {
    Default,
    WithoutLuminance,
}

impl Features {
    // TODO: this needs to be package-specific
    fn cargo_flags(self) -> impl IntoIterator<Item = &'static str> {
        match self {
            Self::Default => vec![],
            Self::WithoutLuminance => vec!["--no-default-features"],
        }
    }
}

/// Start a [`Cmd`] with the cargo command we should use.
fn cargo() -> Cmd {
    Cmd::new(std::env::var("CARGO").expect("CARGO environment variable not set"))
}
