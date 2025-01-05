//! Test glTF output.
//!
//! TODO: This should really be a test in [`all_is_cubes_port`], but right now, some of
//! the top-level glTF output logic is in this package so we need to test it here.

use std::process;

fn gltf_round_trip_test(args: &[&str]) {
    let temp_dir = tempfile::tempdir().unwrap();
    let gltf_path = temp_dir.path().join("output.gltf");

    let output = process::Command::new(env!("CARGO_BIN_EXE_all-is-cubes"))
        .env("AIC_DO_NOT_USE_CONFIG_FILES_IN_TESTS", "1")
        .arg("--no-config-files")
        .arg("--graphics=record")
        .arg("--output")
        .arg(&*gltf_path)
        .args(args) // to specify template, duration, etc
        .output()
        .expect("Failed to start all-is-cubes process");

    print_output(&output);
    assert!(output.status.success());

    // Run validation via import (but don't make any custom assertions, yet...)
    match gltf::import(&gltf_path) {
        Ok(_) => {
            // No further custom validation for now.
        }
        Err(e) => {
            // Keep the file output alive so it can be examined.
            let _ = temp_dir.into_path();

            panic!(
                "glTF import failed.\nFiles: {p}\nError: {e}\n",
                p = gltf_path.display()
            );
        }
    }
}

fn print_output(output: &process::Output) {
    println!(
        "[command stderr]\n{stderr}\n[command stdout]\n{stdout}\n[end of output]",
        stderr = String::from_utf8_lossy(&output.stderr),
        stdout = String::from_utf8_lossy(&output.stdout),
    );
}

#[test]
fn non_animated() {
    gltf_round_trip_test(&["--template=lighting-bench"])
}

#[test]
fn animated() {
    gltf_round_trip_test(&["--template=lighting-bench", "--duration=0.2"])
}
