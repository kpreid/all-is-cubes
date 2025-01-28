#![allow(missing_docs)]

fn main() {
    // Make the build profile visible so that we can
    // embed/read a client built with the same profile.
    // We use cfg rather than env because `include_dir!()` forces us to do that anyway.
    // TODO: Replace `include_dir!()` with a build script.
    // Also, replace requiring the build to already exist with having this build script do it.
    println!(
        "cargo::rustc-env=AIC_CLIENT_BUILD_DIR={manifest}/../all-is-cubes-wasm/target/web-app-{profile_dir_name}",
        manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap(),
        profile_dir_name = match &*std::env::var("PROFILE").unwrap() {
            "debug" => "dev",
            "release" => "release",
            other => panic!("unexpected PROFILE={other}"),
        },
    );
}
