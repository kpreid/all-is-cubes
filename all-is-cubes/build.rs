#![allow(missing_docs)]

fn main() {
    // We must emit at least one rerun-if-changed directive to avoid superfluous rebuilds.
    // <https://doc.rust-lang.org/cargo/reference/build-scripts.html#rerun-if-changed>
    println!("cargo::rerun-if-changed=build.rs");

    // TODO(ecs) TODO(no_std): horrible kludge: pretend the 'std' feature is always enabled,
    // because disabling it is not currently functional.
    println!("cargo::rustc-cfg=feature=\"std\"");
}
