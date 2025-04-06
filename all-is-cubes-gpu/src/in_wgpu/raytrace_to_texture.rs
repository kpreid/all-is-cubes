//! Runs the software raytracer and writes the results into a texture.

use alloc::sync::Arc;
// TODO: if not using threads, don't even use a Mutex as it's entirely wasted
use alloc::boxed::Box;
#[cfg(feature = "auto-threads")]
use alloc::sync::Weak;
use alloc::vec::Vec;
use std::sync::Mutex;

use half::f16;
use itertools::Itertools;
#[cfg(feature = "auto-threads")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use web_time::{Duration, Instant};

use all_is_cubes::character::Cursor;
use all_is_cubes::euclid::{Box2D, point2, vec2};
use all_is_cubes::listen;
use all_is_cubes::math::{Rgba, VectorOps as _};
use all_is_cubes_render::RenderError;
use all_is_cubes_render::camera::{Camera, ImagePixel, StandardCameras, Viewport, area_usize};
use all_is_cubes_render::raytracer::{ColorBuf, RtRenderer};

use crate::in_wgpu::frame_texture::DrawableTexture;
use crate::in_wgpu::pipelines::Pipelines;
use crate::{Identified, Memo};

// TODO: this type definition makes more sense in `draw_to_texture.rs` once we are not partly using embedded_graphics
type Point = all_is_cubes::euclid::Point2D<u32, ImagePixel>;

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
    render_target: DrawableTexture<[f16; 4], [f16; 4]>,
}

impl RaytraceToTexture {
    pub fn new(cameras: StandardCameras) -> Self {
        let initial_viewport = Viewport::with_scale(1.0, vec2(1, 1));
        let inner = Arc::new(Mutex::new(Inner {
            render_viewport: initial_viewport,
            rtr: RtRenderer::new(
                cameras,
                Box::new(raytracer_size_policy),
                listen::constant(Arc::new(())),
            ),
            pixel_picker: PixelPicker::new(initial_viewport),
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
            }
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
        if self.rtr.update(cursor)? {
            self.dirty();
        }
        Ok(())
    }

    fn set_viewport(&mut self, device: &wgpu::Device, render_viewport: Viewport) {
        if self.render_viewport == render_viewport {
            // don't reset/dirty anything
            return;
        }
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
        self.dirty_pixels = self.pixel_picker.cycle_length;
    }

    fn do_some_tracing(&mut self) {
        if self.dirty_pixels == 0 {
            // We've traced the entire frame buffer area, and no changes have occurred,
            // so we have no need to trace again.
            return;
        }

        let render_viewport = self.render_viewport;

        #[allow(clippy::needless_collect, reason = "needed with rayon and not without")]
        let this_frame_pixels: Vec<Point> =
            (&mut self.pixel_picker).take(self.rays_per_frame).collect();

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
            let color = Rgba::from(color_buf);
            let color: [f16; 4] = [
                f16::from_f32(color.red().into_inner()),
                f16::from_f32(color.green().into_inner()),
                f16::from_f32(color.blue().into_inner()),
                f16::from_f32(color.alpha().into_inner()),
            ];
            (point, color)
        };

        #[cfg(feature = "auto-threads")]
        let traces: Vec<(Point, [f16; 4])> = this_frame_pixels.into_par_iter().map(trace).collect();
        #[cfg(not(feature = "auto-threads"))]
        let traces: Vec<(Point, [f16; 4])> = this_frame_pixels.into_iter().map(trace).collect();

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

        let target = self.render_target.draw_target();
        for (point, color) in traces {
            target.set_pixel(point, color);
        }
    }
}

#[cfg(feature = "auto-threads")]
/// Runs raytracing, periodically releasing the lock to allow updating input and retrieving output.
#[expect(
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
    viewport: Viewport,

    /// Iterator that generates indices into `sorted_pixels` to tell us what to render next.
    iter: itertools::Interleave<
        core::iter::Cycle<core::ops::Range<usize>>,
        core::iter::Cycle<core::ops::Range<usize>>,
    >,

    /// Ordering of pixels such that earlier pixels are higher priorities
    /// (currently, this means closer to the center).
    sorted_pixels: Box<[usize]>,

    /// After at least this many pixels have been picked, the entire image has been covered.
    cycle_length: usize,
}

impl PixelPicker {
    fn new(viewport: Viewport) -> Self {
        let pixel_count = viewport.pixel_count().unwrap();
        let width = viewport.framebuffer_size.width as usize;
        // Subtracting 0.5 here means that when we use this later,
        // it'll give us pixel-center coordinates
        let image_center = viewport.framebuffer_size.to_vector().to_f64() / 2.0 - vec2(0.5, 0.5);

        // Precompute an ordering of the pixels based on distance from center with some dithering.
        let mut sorted_pixels: Box<[usize]> = (0..pixel_count).collect();
        sorted_pixels.sort_by_key(|index| {
            let image_point = vec2(index % width, index / width);
            let from_center_point = image_point.to_f64() - image_center;
            let blend = ((image_point.x ^ image_point.y).rem_euclid(4) * 2) as f64;
            //let square_radius = from_center_point.length();
            let square_radius = from_center_point.x.abs().max(from_center_point.y.abs());
            (square_radius + blend) as i64
        });

        // We prioritize tracing these pixels, tracing them more often than the rest.
        // One could call this a form of fixed foveated rendering, to be fancy about it.
        let central_pixel_count: usize = 60000.min(pixel_count / 4);

        let inner_range = 0..central_pixel_count;
        let outer_range = central_pixel_count..pixel_count;

        // There are two cyclic iterators interleaved, so the overall cycle length is
        // twice the longest individual cycle length.
        let cycle_length = inner_range.len().max(outer_range.len()) * 2;

        PixelPicker {
            iter: ((inner_range).cycle()).interleave(outer_range.cycle()),
            viewport,
            sorted_pixels,
            cycle_length,
        }
    }

    fn resize(&mut self, viewport: Viewport) {
        if self.viewport != viewport {
            *self = Self::new(viewport);
        }
    }
}

impl Iterator for PixelPicker {
    type Item = Point;

    fn next(&mut self) -> Option<Self::Item> {
        // `as usize` is safe because we would have failed earlier if it doesn't fit in usize.
        let size = self.viewport.framebuffer_size.map(|s| s as usize);
        let linear_index = self.iter.next().unwrap();
        let index = self.sorted_pixels[linear_index];
        Some(Point::new(
            index.rem_euclid(size.width) as u32,
            index.div_euclid(size.width).rem_euclid(size.height) as u32,
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

#[cfg(test)]
mod tests {
    //use super::*;
    // ...
    // TODO: Test PixelPicker since it is independent of the GPU
}
