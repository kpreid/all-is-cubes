// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use clap::Parser as _;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::camera::HeadlessRenderer;
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::raytracer::RtRenderer;
use test_renderers::{RendererFactory, RendererId};

#[tokio::main]
pub async fn main() -> test_renderers::HarnessResult {
    test_renderers::harness_main(
        test_renderers::HarnessArgs::parse(),
        RendererId::Raytracer,
        test_renderers::test_cases::all_tests,
        || std::future::ready(RtFactory),
    )
    .await
}

#[derive(Clone, Debug)]
struct RtFactory;

impl RendererFactory for RtFactory {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send> {
        Box::new(RtRenderer::new(
            cameras,
            Box::new(|v| v),
            ListenableSource::constant(()),
        ))
    }

    fn id(&self) -> RendererId {
        RendererId::Raytracer
    }
}
