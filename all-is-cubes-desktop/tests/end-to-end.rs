#[test]
fn trycmd_tests() {
    trycmd::TestCases::new()
        .insert_var("[DESKTOPVERSION]", env!("CARGO_PKG_VERSION"))
        .unwrap()
        .case("tests/end-to-end/*.toml");
}
