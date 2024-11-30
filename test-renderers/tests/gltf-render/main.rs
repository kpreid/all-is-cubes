//! Runs [`test_renderers::harness_main`] against glTF files produced by [`all_is_cubes_port`].
//!
//! Uses [`bevy`] as the glTF loader and renderer.
//!
//! Note that in order to run this test, the `gltf` feature must be enabled.

#![allow(elided_lifetimes_in_paths, reason = "Bevy")]

use core::fmt;

use clap::Parser as _;

use all_is_cubes::util::StatusText;

use test_renderers::{KnownIncorrectness, RendererId};

// -------------------------------------------------------------------------------------------------

mod bevy_app;
mod message;
mod renderer_impl;

// -------------------------------------------------------------------------------------------------

#[tokio::main]
async fn main() -> test_renderers::HarnessResult {
    let args = test_renderers::HarnessArgs::parse();
    test_renderers::initialize_logging(&args);

    test_renderers::harness_main(
        &args,
        RendererId::Gltf,
        test_renderers::SuiteId::Renderers,
        test_renderers::test_cases::all_tests,
        get_factory,
        None,
    )
    .await
}

async fn get_factory(label: String) -> GltfFactory {
    GltfFactory {
        label,
        handle: tokio::runtime::Handle::current(),
    }
}

#[derive(Clone, Debug)]
struct GltfFactory {
    label: String,
    handle: tokio::runtime::Handle,
}

impl test_renderers::RendererFactory for GltfFactory {
    fn renderer_from_cameras(
        &self,
        cameras: all_is_cubes_render::camera::StandardCameras,
    ) -> Box<dyn all_is_cubes_render::HeadlessRenderer + Send> {
        Box::new(renderer_impl::GltfBevyRenderer::new(self, cameras))
    }
    fn id(&self) -> RendererId {
        RendererId::Gltf
    }
    fn info(&self) -> String {
        // TODO: ask bevy some questions?
        String::new()
    }
    fn known_incorrect(&self) -> KnownIncorrectness {
        // TODO: handle startup failure and report it here
        KnownIncorrectness::NONE
    }
}

// [`Rendering::info`] implementation for gltf-render.
#[derive(Debug)]
pub(crate) struct RenderInfo {}

impl manyfmt::Fmt<StatusText> for RenderInfo {
    fn fmt(&self, _fmt: &mut fmt::Formatter<'_>, _: &StatusText) -> fmt::Result {
        // no info yet
        Ok(())
    }
}
