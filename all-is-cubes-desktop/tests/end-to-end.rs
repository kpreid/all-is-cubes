#[test]
fn trycmd_tests() {
    trycmd::TestCases::new().case("tests/end-to-end/*.toml");
}
