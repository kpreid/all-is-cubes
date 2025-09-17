//! Abstraction over renderers (for test purposes only).

use std::fmt::Debug;

use all_is_cubes::character::Cursor;
use all_is_cubes::euclid::size2;
use all_is_cubes::listen;
use all_is_cubes::universe::Universe;
use all_is_cubes_render::HeadlessRenderer;
use all_is_cubes_render::camera::{GraphicsOptions, StandardCameras, Viewport};

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
        StandardCameras::from_constant_for_test(
            GraphicsOptions::UNALTERED_COLORS,
            COMMON_VIEWPORT,
            self,
        )
    }
}

/// Dynamic overlay content passed to the [`HeadlessRenderer`] per-frame.
/// This is a combination of the parameters of `update()` and `draw()`.
#[derive(Clone, Debug, PartialEq)]
#[expect(
    clippy::exhaustive_structs,
    reason = "will very likely go through incompatible changes anyway"
)]
pub struct Overlays<'a> {
    /// The player's cursor, or [`None`] to draw nothing.
    pub cursor: Option<&'a Cursor>,
    /// Text to draw on top of the scene, in a style that is some compromise between
    /// readability and not obscuring the content, or [`None`] to draw nothing.
    pub info_text: Option<&'a str>,
}

impl Overlays<'static> {
    pub const NONE: Self = Overlays {
        cursor: None,
        info_text: None,
    };
}

/// Test-configuration-specific source of new [`HeadlessRenderer`]s.
pub trait RendererFactory: Send + Sync + Debug {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send>;

    fn renderer_from_scene(&self, scene: impl Scene) -> Box<dyn HeadlessRenderer + Send>
    where
        Self: Sized,
    {
        self.renderer_from_cameras(scene.into_cameras())
    }

    fn id(&self) -> RendererId;

    /// Returns whether all images from this renderer are known to be incorrect due to inability
    /// to obtain a GPU or other necessary resources. Those images should also report
    /// `Flaws::OTHER` (TODO: assign a dedicated flag bit?).
    fn known_incorrect(&self) -> bool;
}

/// [`RendererFactory`] implementor which produces [`all_is_cubes_render::raytracer::RtRenderer`]s.
#[derive(Clone, Debug)]
#[expect(clippy::exhaustive_structs)]
pub struct RtFactory;

impl RendererFactory for RtFactory {
    fn renderer_from_cameras(&self, cameras: StandardCameras) -> Box<dyn HeadlessRenderer + Send> {
        Box::new(all_is_cubes_render::raytracer::RtRenderer::new(
            cameras,
            Box::new(core::convert::identity),
            listen::constant(Default::default()),
        ))
    }

    fn id(&self) -> RendererId {
        RendererId::Raytracer
    }

    fn known_incorrect(&self) -> bool {
        false
    }
}

/// Viewport to use for tests not needing a specific other size.
///
/// This size is chosen to be small enough that CPU raytracing tests run in a reasonable
/// time and output images are small, without being so small as to be unintelligible.
/// It also needs to match `wgpu::COPY_BYTES_PER_ROW_ALIGNMENT`.
pub const COMMON_VIEWPORT: Viewport = Viewport {
    nominal_size: size2(128., 96.),
    framebuffer_size: size2(128, 96),
};
