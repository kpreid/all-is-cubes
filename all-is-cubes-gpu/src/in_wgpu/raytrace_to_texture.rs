//! Runs the software raytracer and writes the results into a texture.

use std::sync::{Arc, Mutex};
// TODO: if not using threads, don't even use a Mutex as it's entirely wasted
#[cfg(feature = "auto-threads")]
use std::sync::Weak;

use half::f16;
use rand::prelude::SliceRandom as _;
use rand::SeedableRng as _;
#[cfg(feature = "auto-threads")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use web_time::{Duration, Instant};

use all_is_cubes::character::Cursor;
use all_is_cubes::drawing::embedded_graphics::pixelcolor::PixelColor;
use all_is_cubes::drawing::embedded_graphics::{draw_target::DrawTarget, prelude::Point, Pixel};
use all_is_cubes::euclid::{point2, vec2, Box2D};
use all_is_cubes::listen::ListenableSource;
use all_is_cubes::math::{Rgb, Rgba, VectorOps as _};
use all_is_cubes_render::camera::{area_usize, Camera, StandardCameras, Viewport};
use all_is_cubes_render::raytracer::{ColorBuf, RtRenderer};
use all_is_cubes_render::RenderError;

use crate::in_wgpu::frame_texture::DrawableTexture;
use crate::in_wgpu::pipelines::Pipelines;
use crate::{Identified, Memo, ToTexel};

#[derive(Debug)]
pub(crate) struct RaytraceToTexture {
    inner: Arc<Mutex<Inner>>,
    frame_copy_bind_group: Memo<crate::Id<wgpu::TextureView>, wgpu::BindGroup>,
}

/// State for the possibly-asynchronous tracing job.
#[derive(Debug)]
struct Inner {
    rtr: RtRenderer,
    render_viewport: Viewport,
    pixel_picker: PixelPicker,
    dirty_pixels: usize,
    rays_per_frame: usize,
    render_target: DrawableTexture<Rgbf16, [f16; 4]>,
}

impl RaytraceToTexture {
    pub fn new(cameras: StandardCameras) -> Self {
        let initial_viewport = Viewport::with_scale(1.0, vec2(1, 1));
        let inner = Arc::new(Mutex::new(Inner {
            render_viewport: initial_viewport,
            rtr: RtRenderer::new(
                cameras,
                Box::new(raytracer_size_policy),
                ListenableSource::constant(()),
            ),
            pixel_picker: PixelPicker::new(initial_viewport, false),
            dirty_pixels: initial_viewport.pixel_count().unwrap(),
            rays_per_frame: 50000,
            render_target: DrawableTexture::new(wgpu::TextureFormat::Rgba16Float),
        }));

        #[cfg(feature = "auto-threads")]
        {
            let weak_inner = Arc::downgrade(&inner);
            match std::thread::Builder::new()
                .name("RaytraceToTexture".into())
                .spawn(move || background_tracing_task(weak_inner))
            {
                Ok(_) => {
                    // The thread will stop itself when its weak reference breaks.
                }
                Err(e) => {
                    log::error!(
                        "RaytraceToTexture failed to create background tracing thread: {e}. \
                            Tracing will proceed synchronously."
                    )
                }
            };
        }

        Self {
            frame_copy_bind_group: Memo::new(),
            inner,
        }
    }

    /// Copy [`Space`] data from the camera spaces, and camera state from the [`StandardCameras`].
    pub fn update(&mut self, cursor: Option<&Cursor>) -> Result<(), RenderError> {
        self.inner.lock().unwrap().update_inputs(cursor)
    }

    /// Trace a frame's worth of rays (which may be less than the scene) and update the texture.
    pub fn prepare_frame(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        pipelines: &Pipelines,
        camera: &Camera,
    ) {
        let inner = &mut *self.inner.lock().unwrap(); // not handling poisoning, just fail

        inner.set_viewport(device, raytracer_size_policy(camera.viewport()));

        // Update bind group if needed
        let rt_texture_view: &Identified<wgpu::TextureView> = inner.render_target.view().unwrap();
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

        inner.do_some_tracing();
        inner.render_target.upload(queue);
    }

    pub fn frame_copy_bind_group(&self) -> Option<&wgpu::BindGroup> {
        self.frame_copy_bind_group.get()
    }
}

impl Inner {
    fn update_inputs(&mut self, cursor: Option<&Cursor>) -> Result<(), RenderError> {
        self.rtr.update(cursor)?;
        self.dirty();
        Ok(())
    }

    fn set_viewport(&mut self, device: &wgpu::Device, render_viewport: Viewport) {
        self.render_viewport = render_viewport;
        self.render_target.resize(
            device,
            Some("RaytraceToTexture::render_target"),
            render_viewport.framebuffer_size,
        );
        self.pixel_picker.resize(render_viewport);
        self.dirty();
    }

    fn dirty(&mut self) {
        self.dirty_pixels = self.render_viewport.pixel_count().unwrap_or(usize::MAX);
    }

    fn do_some_tracing(&mut self) {
        if self.dirty_pixels == 0 {
            // We've traced the entire frame buffer area, and no changes have occurred,
            // so we have no need to trace again.
            return;
        }

        let render_viewport = self.render_viewport;

        #[allow(clippy::needless_collect, reason = "needed with rayon and not without")]
        let this_frame_pixels: Vec<Point> = (0..self.rays_per_frame)
            .map(|_i| self.pixel_picker.next().unwrap())
            .collect();

        self.dirty_pixels = self.dirty_pixels.saturating_sub(this_frame_pixels.len());

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

        #[cfg(feature = "auto-threads")]
        let traces: Vec<Pixel<Rgbf16>> = this_frame_pixels.into_par_iter().map(trace).collect();
        #[cfg(not(feature = "auto-threads"))]
        let traces: Vec<Pixel<Rgbf16>> = this_frame_pixels.into_iter().map(trace).collect();

        let tracing_duration = Instant::now().duration_since(start_time);

        match tracing_duration.cmp(&Duration::from_millis(2)) {
            std::cmp::Ordering::Greater => {
                self.rays_per_frame = (self.rays_per_frame.saturating_sub(5000)).max(100);
            }
            std::cmp::Ordering::Equal => {}
            std::cmp::Ordering::Less => {
                self.rays_per_frame = (self.rays_per_frame + 5000)
                    .min(area_usize(render_viewport.framebuffer_size).unwrap());
            }
        }

        self.render_target.draw_target().draw_iter(traces).unwrap();
    }
}

#[cfg(feature = "auto-threads")]
/// Runs raytracing, periodically releasing the lock to allow updating input and retrieving output.
#[allow(
    clippy::needless_pass_by_value,
    reason = "let thread function own its state"
)]
fn background_tracing_task(weak_inner: Weak<Mutex<Inner>>) {
    // By using a weak reference, we arrange for this task to stop itself when it is no longer
    // relevant.
    while let Some(strong_inner) = weak_inner.upgrade() {
        strong_inner.lock().unwrap().do_some_tracing();
        std::thread::yield_now();
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

    #[allow(clippy::cast_possible_wrap)]
    fn next(&mut self) -> Option<Self::Item> {
        // `as usize` is safe because we would have failed earlier if it doesn't fit in usize.
        let size = self.viewport.framebuffer_size.map(|s| s as usize);
        let linear_index = self.iter.next().unwrap();
        let index = match &self.shuffled_pixels {
            Some(lookup) => lookup[linear_index],
            None => linear_index,
        };
        Some(Point::new(
            index.rem_euclid(size.width) as i32,
            index.div_euclid(size.width).rem_euclid(size.height) as i32,
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
