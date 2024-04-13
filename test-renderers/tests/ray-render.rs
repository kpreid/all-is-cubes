//! Runs [`test_renderers::harness_main`] against [`all_is_cubes::raytracer`].

use clap::Parser as _;

#[tokio::main]
async fn main() -> test_renderers::HarnessResult {
    test_renderers::initialize_logging();

    test_renderers::harness_main(
        test_renderers::HarnessArgs::parse(),
        test_renderers::RendererId::Raytracer,
        test_renderers::test_cases::all_tests,
        || std::future::ready(test_renderers::RtFactory),
        None,
    )
    .await
}
