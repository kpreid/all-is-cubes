//! Runs the software raytracer and writes the results into a texture.

use half::f16;
use rand::prelude::SliceRandom as _;
use rand::SeedableRng as _;
#[cfg(feature = "threads")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use web_time::{Duration, Instant};

use all_is_cubes::camera::{Camera, GraphicsOptions, RenderError};
use all_is_cubes::camera::{StandardCameras, Viewport};
use all_is_cubes::character::Cursor;
use all_is_cubes::drawing::embedded_graphics::pixelcolor::PixelColor;
use all_is_cubes::drawing::embedded_graphics::{draw_target::DrawTarget, prelude::Point, Pixel};
use all_is_cubes::euclid::{point2, vec2, Box2D};
use all_is_cubes::listen::{ListenableCellWithLocal, ListenableSource};
use all_is_cubes::math::{Rgb, Rgba, VectorOps as _};
use all_is_cubes::raytracer::{ColorBuf, RtRenderer};

use crate::in_wgpu::frame_texture::DrawableTexture;
use crate::in_wgpu::pipelines::Pipelines;
use crate::{GraphicsResourceError, Memo, ToTexel};

#[derive(Debug)]
pub(crate) struct RaytraceToTexture {
    graphics_options: ListenableCellWithLocal<GraphicsOptions>,
    rtr: RtRenderer,
    render_target: DrawableTexture<Rgbf16, [f16; 4]>,
    frame_copy_bind_group: Memo<wgpu::Id<wgpu::TextureView>, wgpu::BindGroup>,
    pixel_picker: PixelPicker,
    rays_per_frame: usize,
}

impl RaytraceToTexture {
    pub fn new(cameras: StandardCameras) -> Self {
        Self {
            graphics_options: ListenableCellWithLocal::new(GraphicsOptions::default()),
            rtr: RtRenderer::new(
                cameras,
                Box::new(raytracer_size_policy),
                ListenableSource::constant(()),
            ),
            render_target: DrawableTexture::new(wgpu::TextureFormat::Rgba16Float),
            frame_copy_bind_group: Memo::new(),
            pixel_picker: PixelPicker::new(Viewport::with_scale(1.0, vec2(1, 1)), false),
            rays_per_frame: 50000,
        }
    }

    /// Copy [`Space`] data from the camera spaces.
    pub fn update(&mut self, cursor: Option<&Cursor>) -> Result<(), RenderError> {
        self.rtr.update(cursor)
    }

    /// Trace a frame's worth of rays (which may be less than the scene) and update the texture.
    pub fn prepare_frame(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        pipelines: &Pipelines,
        camera: &Camera,
    ) -> Result<(), GraphicsResourceError> {
        if camera.options() != self.graphics_options.borrow() {
            self.graphics_options.set(camera.options().clone());
        }
        let render_viewport = raytracer_size_policy(camera.viewport());
        self.render_target.resize(
            device,
            Some("RaytraceToTexture::render_target"),
            render_viewport.framebuffer_size,
        );
        self.pixel_picker.resize(render_viewport);
        let rt_texture_view = self.render_target.view().unwrap(); // guaranteed
        self.frame_copy_bind_group
            .get_or_insert(rt_texture_view.global_id(), || {
                device.create_bind_group(&wgpu::BindGroupDescriptor {
                    label: Some("rt to scene copy"),
                    layout: &pipelines.frame_copy_layout,
                    entries: &[
                        wgpu::BindGroupEntry {
                            binding: 0,
                            resource: wgpu::BindingResource::TextureView(rt_texture_view),
                        },
                        wgpu::BindGroupEntry {
                            binding: 1,
                            resource: wgpu::BindingResource::Sampler(&pipelines.linear_sampler),
                        },
                    ],
                })
            });

        // TODO: Instead of the whole size policy business, maybe we should just expect
        // camera.viewport() to have a "reasonable" framebuffer size. Or, just construct
        // a copy of the Camera with an adjusted viewport.

        #[allow(clippy::needless_collect)] // needed with rayon and not without
        let this_frame_pixels: Vec<Point> = (0..self.rays_per_frame)
            .map(|_i| self.pixel_picker.next().unwrap())
            .collect();

        let start_time = Instant::now();
        let scene = self.rtr.scene::<ColorBuf>();
        let trace = |point: Point| {
            let x = point.x as usize;
            let y = point.y as usize;
            let (color_buf, _info) = scene.trace_patch(Box2D {
                min: point2(
                    render_viewport.normalize_fb_x_edge(x),
                    render_viewport.normalize_fb_y_edge(y),
                ),
                max: point2(
                    render_viewport.normalize_fb_x_edge(x + 1),
                    render_viewport.normalize_fb_y_edge(y + 1),
                ),
            });
            // Note: these are *not* postprocessed colors, because we let the GPU do that.
            let color = Rgbf16::from(Rgba::from(color_buf).to_rgb());
            Pixel(point, color)
        };

        #[cfg(feature = "threads")]
        let traces: Vec<Pixel<Rgbf16>> = this_frame_pixels.into_par_iter().map(trace).collect();
        #[cfg(not(feature = "threads"))]
        let traces: Vec<Pixel<Rgbf16>> = this_frame_pixels.into_iter().map(trace).collect();

        let tracing_duration = Instant::now().duration_since(start_time);

        match tracing_duration.cmp(&Duration::from_millis(10)) {
            std::cmp::Ordering::Greater => {
                self.rays_per_frame = (self.rays_per_frame.saturating_sub(5000)).max(100);
            }
            std::cmp::Ordering::Equal => {}
            std::cmp::Ordering::Less => {
                let fbs = render_viewport.framebuffer_size;
                self.rays_per_frame =
                    (self.rays_per_frame + 5000).min(fbs.x as usize * fbs.y as usize);
            }
        }

        self.render_target.draw_target().draw_iter(traces).unwrap();
        self.render_target.upload(queue);

        Ok(())
    }

    pub fn frame_copy_bind_group(&self) -> Option<&wgpu::BindGroup> {
        self.frame_copy_bind_group.get()
    }
}

#[derive(Clone, Debug)]
struct PixelPicker {
    iter: std::iter::Cycle<std::ops::Range<usize>>,
    viewport: Viewport,
    /// If None, don't shuffle.
    shuffled_pixels: Option<Box<[usize]>>,
}

impl PixelPicker {
    fn new(viewport: Viewport, shuffle: bool) -> Self {
        let pixel_count = viewport.pixel_count().unwrap();
        let shuffled_pixels = shuffle.then(|| {
            // Generate a pseudorandom order to evenly distributed update rays about the
            // screen. Note that this is deterministic since we don't reuse the RNG.
            let mut shuffled_pixels: Box<[usize]> = (0..pixel_count).collect();
            shuffled_pixels.shuffle(&mut rand_xoshiro::Xoshiro256Plus::seed_from_u64(
                0x9aa8bc4be2112757,
            ));
            shuffled_pixels
        });

        PixelPicker {
            iter: (0..pixel_count).cycle(),
            viewport,
            shuffled_pixels,
        }
    }

    fn resize(&mut self, viewport: Viewport) {
        if self.viewport != viewport {
            *self = Self::new(viewport, self.shuffled_pixels.is_some());
        }
    }
}

impl Iterator for PixelPicker {
    type Item = Point;

    fn next(&mut self) -> Option<Self::Item> {
        // `as usize` is safe because we would have failed earlier if it doesn't fit in usize.
        let size = self.viewport.framebuffer_size.map(|s| s as usize);
        let linear_index = self.iter.next().unwrap();
        let index = match &self.shuffled_pixels {
            Some(lookup) => lookup[linear_index],
            None => linear_index,
        };
        Some(Point::new(
            index.rem_euclid(size.x) as i32,
            index.div_euclid(size.x).rem_euclid(size.y) as i32,
        ))
    }
}

fn raytracer_size_policy(mut viewport: Viewport) -> Viewport {
    // use 2x2 nominal pixels
    viewport.framebuffer_size = viewport
        .nominal_size
        .map(|c| (c / 2.0).round() as u32)
        .cast_unit();
    viewport
}

/// embedded-graphics pixel type for f16 colors
#[derive(Clone, Copy, Default, PartialEq, bytemuck::Zeroable, bytemuck::Pod)]
#[repr(transparent)]
pub(crate) struct Rgbf16(pub [f16; 3]);

impl From<Rgb> for Rgbf16 {
    fn from(value: Rgb) -> Self {
        Self([
            f16::from_f32(value.red().into_inner()),
            f16::from_f32(value.green().into_inner()),
            f16::from_f32(value.blue().into_inner()),
        ])
    }
}

impl PixelColor for Rgbf16 {
    type Raw = ();
}

impl ToTexel<[f16; 4]> for Rgbf16 {
    fn to_texel(self) -> [f16; 4] {
        let Self([r, g, b]) = self;
        [r, g, b, f16::ONE]
    }
}

#[cfg(test)]
mod tests {
    //use super::*;
    // ...
    // TODO: Test PixelPicker and Srgb8Adapter since they are independent of GraphicsContext
}
