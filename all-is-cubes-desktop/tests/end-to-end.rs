//! Tests which execute the `all-is-cubes` binary.

#[test]
fn trycmd_tests() {
    let tc = trycmd::TestCases::new();

    // Extra protection against depending on config now or in the future.
    // This will cause a test to fail if it has a dependence on a system/user config file.
    tc.env("AIC_DO_NOT_USE_CONFIG_FILES_IN_TESTS", "1");

    // Prevent `anyhow` from printing backtraces if it otherwise would due to env having
    // "RUST_BACKTRACE=1", because they'd cause our error output tests to fail.
    // Docs: https://docs.rs/anyhow/1.0.75/anyhow/index.html#details
    tc.env("RUST_LIB_BACKTRACE", "0");

    tc.insert_var("[DESKTOPVERSION]", env!("CARGO_PKG_VERSION")).unwrap();
    tc.case("tests/end-to-end/*.toml");

    // TODO: doesn't work, for reasons that are not currently a priority
    #[cfg(windows)]
    {
        tc.skip("tests/end-to-end/output-path-with-graphics.toml");
    }

    tc.run();
}
