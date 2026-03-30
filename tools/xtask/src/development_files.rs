use std::collections::BTreeSet;
use std::fmt;
use std::fs;
use std::io;
use std::path::Path;

use anyhow::Context as _;
use serde_json::{Value, json};

use crate::context::Config;
use crate::{ActionError, PROJECT_DIR};

// -------------------------------------------------------------------------------------------------

/// Create files which may be useful for development in the workspace but for which
/// constant, version controlled contents are not adequate.
pub(crate) fn write_development_files(
    config: &Config<'_>,
    overwrite: bool,
) -> Result<(), ActionError> {
    // .desktop file, used by Linux desktop application launchers to describe how to run
    // our executable. It needs to have an absolute path to the file.
    // The file is also required to be encoded in UTF-8, so non-UTF-8 paths cannot be
    // supported.
    if let Some(dir) = PROJECT_DIR.to_str() {
        check_or_overwrite_file(
            &PROJECT_DIR.join("all-is-cubes-desktop/all-is-cubes.desktop"),
            generate_desktop_file(dir).as_bytes(),
            overwrite,
            true,
        )
        .context("refreshing desktop file")?;
    } else {
        eprintln!("Skipping creation of .desktop file because path is not UTF-8");
    }

    // VS Code tasks file.
    //
    // TODO: It would be useful to have some support for merging generated tasks with existing ones,
    // or even copying them back into the fixed part of this generator.
    {
        let tasks_data = format!("{:#}", generate_vscode_tasks(&config.main_metadata));
        check_or_overwrite_file(
            &PROJECT_DIR.join(".vscode/tasks.json"),
            tasks_data.as_bytes(),
            overwrite,
            false,
        )
        .context("refreshing tasks file")?;
    }

    // Note: When adding a new file, also update the documentation of `crate::XtaskCommand::Init`.

    Ok(())
}

/// Compare the contents of a file against expected contents, and overwrite it if directed to.
///
/// TODO: This function is complex and could use unit tests.
fn check_or_overwrite_file(
    path: &Path,
    expected_contents: &[u8],
    overwrite: bool,
    set_executable: bool,
) -> Result<(), ActionError> {
    let current_contents_result = fs::read(path);

    let is_not_found = current_contents_result
        .as_ref()
        .is_err_and(|e| e.kind() == io::ErrorKind::NotFound);
    if current_contents_result
        .as_ref()
        .is_ok_and(|contents| contents == expected_contents)
    {
        // File is up to date; do nothing.
    } else if !is_not_found && let Err(error) = current_contents_result {
        // Unexpected error
        return Err(ActionError::from(error)
            .context(format!("checking current contents of “{}”", path.display())));
    } else if overwrite || is_not_found {
        eprintln!(
            "{action} “{path}” with new configuration.",
            action = if is_not_found {
                "Creating"
            } else {
                "Overwriting"
            },
            path = path.display()
        );
        fs::write(path, expected_contents)
            .with_context(|| format!("overwriting development file “{}”", path.display()))?;

        #[cfg(unix)]
        if set_executable {
            use std::os::unix::fs::PermissionsExt as _;
            let mut permissions = fs::metadata(path)
                .with_context(|| format!("read metadata for file “{}”", path.display()))?
                .permissions();
            // Set execute bit
            permissions.set_mode(permissions.mode() | 0o111);
            fs::set_permissions(path, permissions)
                .with_context(|| format!("write permissions for file “{}”", path.display()))?;
        }
    } else {
        eprintln!(
            "⚠️ “{}” is not up to date. Run “cargo xtask init --overwrite” to fix.",
            path.display()
        );
    }
    Ok(())
}

fn generate_desktop_file(project_directory_string: &str) -> String {
    assert!(
        !project_directory_string.contains('\n'),
        "path must not contain newline"
    );
    format!(
        "\
[Desktop Entry]
Type=Application
Version=1.0
Name=All is Cubes (dev)
Path={project_directory_string}
Exec=cargo run --bin all-is-cubes
Terminal=false
Categories=Game;
PrefersNonDefaultGPU=true
SingleMainWindow=true
"
    )
}

/// Generate content for the file `.vscode/tasks.json`.
///
/// This function has no side effects and only returns updated JSON;
/// other functions will write it or check whether it is up to date.
///
/// Format as per <https://go.microsoft.com/fwlink/?LinkId=733558>
fn generate_vscode_tasks(metadata: &cargo_metadata::Metadata) -> Value {
    let mut tasks = Vec::new();

    // These fixed tasks are largely a subset of the subcommands from `XtaskCommand`
    tasks.extend([
        json!({
            "label": "all-is-cubes: lint all code",
            "type": "cargo",
            "command": "xtask",
            "args": ["lint"],
            "problemMatcher": ["$rustc"],
            "group": {"kind": "build"},
        }),
        json!({
            "label": "all-is-cubes: lint dependency graph",
            "type": "cargo",
            "command": "xtask",
            "args": ["check-deps"],
            "problemMatcher": ["$rustc"],
            "group": {"kind": "build"},
        }),
        json!({
            "label": "all-is-cubes: test all",
            "type": "cargo",
            "command": "xtask",
            "args": ["test"],
            "problemMatcher": ["$rustc"],
            "group": {
                "kind": "test",
                "isDefault": true
            }
        }),
        json!({
            "label": "all-is-cubes: benchmark all",
            "type": "cargo",
            "command": "bench",
            // TODO: Ideally we would do something to set up a useful baseline automatically,
            // but that’ll require stateful logic checking git state and switching branches.
            "args": [],
            "problemMatcher": ["$rustc"],
            "group": {
                "kind": "test",
                "isDefault": true
            }
        }),
        json!({
            "label": "all-is-cubes: check feature flag combinations",
            "type": "cargo",
            "command": "xtask",
            "args": ["check-features"],
            "problemMatcher": ["$rustc"],
            "group": {"kind": "build"},
        }),
        json!({
            "label": "all-is-cubes: run desktop",
            "type": "cargo",
            "command": "run",
            "args": ["--bin", "all-is-cubes", "--"],
            "problemMatcher": ["$rustc"],
            "group": "test"
        }),
        json!({
            "label": "all-is-cubes: run dev-server",
            "type": "cargo",
            "command": "xtask",
            "args": ["run-dev"],
            "problemMatcher": ["$rustc"],
            "group": "test"
        }),
        json!({
            "label": "all-is-cubes: run all fuzzers",
            "type": "cargo",
            "command": "xtask",
            "args": ["fuzz", "60"],
            "problemMatcher": ["$rustc"],
            "group": "test"
        }),
        json!({
            "label": "all-is-cubes: build documentation",
            "type": "cargo",
            "command": "xtask",
            "args": ["doc"],
            "problemMatcher": ["$rustc"],
            "group": {"kind": "build"}
        }),
        json!({
            "label": "all-is-cubes: check release binary sizes",
            "type": "cargo",
            "command": "xtask",
            "args": ["bin-size"],
            "problemMatcher": ["$rustc"],
            "group": {"kind": "test"}
        }),
        json!({
            "label": "all-is-cubes: reinitialize development files",
            "type": "cargo",
            "command": "xtask",
            "args": ["init", "--overwrite"],
            "problemMatcher": ["$rustc"],
            "group": {"kind": "build"}
        }),
    ]);

    for package_to_test in crate::ALL_NONTEST_PACKAGES {
        if package_to_test == "all-is-cubes-wasm" {
            // special testing requirements because it is web code and because it is not in the main
            // workspace; not supported for now
            continue;
        }

        // Find the topic-specific part of the package name, e.g. `foo` in `all-is-cubes-foo`.
        let topic = if package_to_test == "all-is-cubes" {
            "aic"
        } else if let Some(suffix) = package_to_test.strip_prefix("all-is-cubes-") {
            suffix
        } else {
            panic!("package name {package_to_test:?} doesn't follow a recognized scheme")
        };

        // List of packages that may contain relevant tests and benches
        let mut relevant_packages: BTreeSet<String> = BTreeSet::from([package_to_test.into()]);

        // Add the associated test package if it exists.
        let test_package_name = format!("test-{topic}");
        if metadata.packages.iter().find(|p| *p.name == test_package_name).is_some() {
            relevant_packages.insert(test_package_name);
        }

        // Add test-renderers if the package to test implements renderering.
        if [
            "all-is-cubes-render",
            "all-is-cubes-mesh",
            "all-is-cubes-gpu",
        ]
        .contains(&package_to_test)
        {
            relevant_packages.insert("test-renderers".into());
        }

        // Determine which packages, out of the relevant packages, contain benchmarks
        let benchmark_packages: Vec<String> = relevant_packages
            .iter()
            .filter(|package_name| {
                metadata
                    .packages
                    .iter()
                    .find(|p| p.name == package_name)
                    .unwrap_or_else(|| panic!("failed to find package {package_name:?}"))
                    .targets
                    .iter()
                    .any(|t| t.is_bench())
            })
            .cloned()
            .collect();

        // Some benchmarks only run if the _special_testing feature is enabled.
        // But we can't just always specify it, because Cargo will error if none of the selected
        // packages has it.
        let needs_special_testing_feature = metadata.packages.iter().any(|p| {
            benchmark_packages.contains(&p.name) && p.features.contains_key("_special_testing")
        });
        let benchmark_features_arg = if needs_special_testing_feature {
            "--features=_special_testing"
        } else {
            "--features=" // noop placeholder
        };

        tasks.push(json!({
            "label": if relevant_packages.len() == 1 {
                format!("all-is-cubes: test {package_to_test}")
            } else {
                format!("all-is-cubes: test {package_to_test} and related")
            },
            "type": "cargo",
            "command": "test",
            "args": Vec::from_iter(relevant_packages.iter().map(cargo_package_arg)),
            "problemMatcher": ["$rustc"],
            "group": {
                "kind": "test"
            }
        }));

        if !benchmark_packages.is_empty() {
            tasks.push(json!({
                "label": if benchmark_packages.len() == 1 {
                    format!("all-is-cubes: benchmark {package_to_test}")
                } else {
                    format!("all-is-cubes: benchmark {package_to_test} and related")
                },
                "type": "cargo",
                "command": "bench",
                "args": Vec::from_iter(benchmark_packages.iter().map(cargo_package_arg).chain([String::from(benchmark_features_arg)])),
                "problemMatcher": ["$rustc"],
                "group": {
                    "kind": "test"
                }
            }));
        }
    }

    json!({
        "version": "2.0.0",
        "tasks": tasks,
    })
}

/// Construct a `--package=...` argument value.
fn cargo_package_arg(package: impl fmt::Display) -> String {
    format!("--package={package}")
}
