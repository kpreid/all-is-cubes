//! Tests which execute the `all-is-cubes` binary.

#[test]
fn trycmd_tests() {
    let tc = trycmd::TestCases::new();

    // Prevent `anyhow` from printing backtraces if it otherwise would due to env having
    // "RUST_BACKTRACE=1", because they'd cause our error output tests to fail.
    // Docs: https://docs.rs/anyhow/1.0.75/anyhow/index.html#details
    tc.env("RUST_LIB_BACKTRACE", "0");

    tc.insert_var("[DESKTOPVERSION]", env!("CARGO_PKG_VERSION"))
        .unwrap();
    tc.case("tests/end-to-end/*.toml");

    // TODO: doesn't work, for reasons that are not currently a priority
    #[cfg(windows)]
    {
        tc.skip("tests/end-to-end/output-path-with-graphics.toml");
    }

    tc.run();
}
