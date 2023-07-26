#[test]
fn trycmd_tests() {
    let tc = trycmd::TestCases::new();

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
