// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Abstraction over renderers (for test purposes only).

use std::fmt::Debug;

use futures_core::future::BoxFuture;
use image::RgbaImage;

use all_is_cubes::apps::StandardCameras;
use all_is_cubes::camera::{GraphicsOptions, Viewport};
use all_is_cubes::cgmath::Vector2;
use all_is_cubes::universe::Universe;

use crate::RendererId;

/// Things that can be passed from a test case to the renderer.
/// This trait allows concise specification of input regardless of how much detail
/// the test case needs.
pub trait Scene {
    fn into_cameras(self) -> StandardCameras;
}

impl Scene for StandardCameras {
    fn into_cameras(self) -> StandardCameras {
        self
    }
}

impl Scene for &Universe {
    fn into_cameras(self) -> StandardCameras {
        StandardCameras::from_constant_for_test(GraphicsOptions::default(), COMMON_VIEWPORT, self)
    }
}

pub trait RendererFactory: Send + Sync + Debug {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send>;

    fn renderer_from_scene(&self, scene: impl Scene) -> Box<dyn HeadlessRenderer + Send>
    where
        Self: Sized,
    {
        self.renderer_from_cameras(scene.into_cameras())
    }

    fn id(&self) -> RendererId;
}

/// Trait for renderers which remember a scene to draw (via `URef`s) and can draw
/// fresh instances of it to an in-memory image.
pub trait HeadlessRenderer {
    /// Render an image of the current state of the previously given scene.
    fn render(&mut self) -> BoxFuture<'_, RgbaImage>;
}

/// Viewport to use for tests not needing a specific other size.
///
/// This size is chosen to be small enough that CPU raytracing tests run in a reasonable
/// time and output images are small, without being so small as to be unintelligible.
/// It also needs to match wgpu::COPY_BYTES_PER_ROW_ALIGNMENT.
pub const COMMON_VIEWPORT: Viewport = Viewport {
    nominal_size: Vector2::new(128., 96.),
    framebuffer_size: Vector2::new(128, 96),
};
