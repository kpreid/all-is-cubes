use std::fs;
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
fn check_or_overwrite_file(
    path: &Path,
    expected_contents: &[u8],
    overwrite: bool,
    set_executable: bool,
) -> Result<(), ActionError> {
    let current_contents = fs::read(path)
        .with_context(|| format!("checking current contents of “{}”", path.display()))?;
    if current_contents == expected_contents {
        // File is up to date; do nothing.
    } else if overwrite {
        eprintln!("Overwriting “{}” with new configuration.", path.display());
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

    tasks.extend([
        json!({
            "label": "all-is-cubes: lint all",
            "type": "cargo",
            "command": "xtask",
            "args": [
                "lint"
            ],
            "problemMatcher": ["$rustc"],
            "group": {
                "kind": "build",
                "isDefault": true
            }
        }),
        json!({
            "label": "all-is-cubes: test all",
            "type": "cargo",
            "command": "xtask",
            "args": [
                "test"
            ],
            "problemMatcher": ["$rustc"],
            "group": {
                "kind": "test",
                "isDefault": true
            }
        }),
        json!({
            "label": "all-is-cubes: run desktop",
            "type": "cargo",
            "command": "run",
            "args": [
                "--bin",
                "all-is-cubes",
                "--",
            ],
            "problemMatcher": ["$rustc"],
            "group": "test"
        }),
        json!({
            "label": "all-is-cubes: run dev-server",
            "type": "cargo",
            "command": "xtask",
            "args": [
                "run-dev",
            ],
            "problemMatcher": ["$rustc"],
            "group": "test"
        }),
    ]);

    for package_to_test in crate::ALL_NONTEST_PACKAGES {
        let mut test_package_args = vec![format!("--package={package_to_test}")];

        // Find the topic-specific part of the package name, e.g. `foo` in `all-is-cubes-foo`.
        let topic = if package_to_test == "all-is-cubes" {
            "aic"
        } else if let Some(suffix) = package_to_test.strip_prefix("all-is-cubes-") {
            suffix
        } else {
            panic!("package name {package_to_test:?} doesn't follow a recognized scheme")
        };

        // Add the associated test package if it exists.
        let test_package_name = format!("test-{topic}");
        if metadata
            .workspace_packages()
            .iter()
            .find(|p| *p.name == test_package_name)
            .is_some()
        {
            test_package_args.push(format!("--package={test_package_name}"));
        }

        // Add test-renderers if the package to test implements renderering.
        if [
            "all-is-cubes-render",
            "all-is-cubes-mesh",
            "all-is-cubes-gpu",
        ]
        .contains(&package_to_test)
        {
            test_package_args.push("--package=test-renderers".into());
        }

        let test_task_label = if test_package_args.len() == 1 {
            format!("all-is-cubes: test {package_to_test}")
        } else {
            format!("all-is-cubes: test {package_to_test} and related")
        };

        tasks.push(json!({
            "label": test_task_label,
            "type": "cargo",
            "command": "test",
            "args": test_package_args,
            "problemMatcher": ["$rustc"],
            "group": {
                "kind": "test"
            }
        }));
    }

    json!({
        "version": "2.0.0",
        "tasks": tasks,
    })
}
