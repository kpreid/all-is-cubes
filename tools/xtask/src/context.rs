//! Configuration/context used by xtask operations.

use anyhow::Error as ActionError;
use xshell::{Cmd, Shell};

use crate::args::Scope;

// -------------------------------------------------------------------------------------------------

/// Configuration which is passed down through everything.
#[derive(Debug)]
pub struct Config<'a> {
    pub sh: &'a Shell,
    pub cargo_timings: bool,
    pub cargo_quiet: bool,
    pub scope: Scope,
}

impl Config<'_> {
    /// Start a [`Cmd`] with the cargo command we should use.
    /// Currently, this doesn’t actually depend on the configuration, just the Shell
    pub fn cargo(&self) -> Cmd<'_> {
        self.sh
            .cmd(std::env::var("CARGO").expect("CARGO environment variable not set"))
    }

    /// Arguments that should be passed to any Cargo command that runs a build
    /// (`build`, `test`, `run`)
    pub fn cargo_build_args(&self) -> Vec<&str> {
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
    /// [`crate::do_for_all_packages`] doesn't use this because it has more specialized handling
    pub fn do_for_all_workspaces<F>(&self, mut f: F) -> Result<(), ActionError>
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

// -------------------------------------------------------------------------------------------------

const CHECK_SUBCMD: &str = "clippy";

/// What to do to a set of packages.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) enum TestOrCheck {
    Test,
    BuildTests,
    Lint,
}

impl TestOrCheck {
    pub fn cargo_cmd<'a>(self, config: &'a Config<'_>) -> Cmd<'a> {
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
    pub fn non_build_check_subcmd(self) -> &'static str {
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

// -------------------------------------------------------------------------------------------------

/// Which features we want to test building with.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Features {
    /// Test with default features only
    Default,

    /// Test each package with all features enabled and with all features disabled.
    AllAndNothing,
}
