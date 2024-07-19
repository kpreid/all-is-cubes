//! Runs [`test_renderers::harness_main`] against [`all_is_cubes_render::raytracer`].

use clap::Parser as _;

#[tokio::main]
async fn main() -> test_renderers::HarnessResult {
    let args = test_renderers::HarnessArgs::parse();
    test_renderers::initialize_logging(&args);

    test_renderers::harness_main(
        &args,
        test_renderers::RendererId::Raytracer,
        test_renderers::SuiteId::Renderers,
        test_renderers::test_cases::all_tests,
        || std::future::ready(test_renderers::RtFactory),
        None,
    )
    .await
}
