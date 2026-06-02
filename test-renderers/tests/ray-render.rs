//! Runs [`test_renderers_runner::harness_main`] against [`all_is_cubes_render::raytracer`].

use test_renderers_dylib::{
    clap::Parser as _, test_renderers_cases, test_renderers_runner, test_renderers_types, tokio,
};

#[tokio::main]
async fn main() -> test_renderers_runner::HarnessResult {
    let args = test_renderers_runner::HarnessArgs::parse();
    test_renderers_runner::initialize_logging(&args);

    test_renderers_runner::harness_main(
        &args,
        test_renderers_types::RendererId::Raytracer,
        test_renderers_types::SuiteId::Renderers,
        test_renderers_cases::all_tests,
        |_| std::future::ready(test_renderers_types::RtFactory),
        None,
    )
    .await
}
