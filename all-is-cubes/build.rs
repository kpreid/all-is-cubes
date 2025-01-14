#![allow(missing_docs)]

fn main() {
    // TODO(ecs) TODO(no_std): horrible kludge: pretend the 'std' feature is always enabled,
    // because disabling it is not currently functional.
    println!("cargo::rustc-cfg=feature=\"std\"");
}
