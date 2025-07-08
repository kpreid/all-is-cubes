//! Runs the software raytracer and writes the results into a texture.

use alloc::sync::Arc;
// TODO: if not using threads, don't even use a Mutex as it's entirely wasted
use alloc::boxed::Box;
#[cfg(feature = "auto-threads")]
use alloc::sync::Weak;
#[cfg(feature = "auto-threads")]
use alloc::vec::Vec;
use std::sync::Mutex;

use half::f16;
use itertools::Itertools;
#[cfg(feature = "auto-threads")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use web_time::{Duration, Instant};

use all_is_cubes::character::Cursor;
use all_is_cubes::euclid::{Box2D, point2, point3, vec2, vec3};
use all_is_cubes::listen;
use all_is_cubes::math::{Rgba, VectorOps as _};
use all_is_cubes::universe::ReadTicket;
use all_is_cubes_render::camera::{
    Camera, ImagePixel, Layers, StandardCameras, Viewport, area_usize,
};
use all_is_cubes_render::raytracer::{ColorBuf, DepthBuf, RtRenderer};
use all_is_cubes_render::{Flaws, RenderError};

use crate::in_wgpu::frame_texture::DrawableTexture;
use crate::in_wgpu::pipelines::Pipelines;
use crate::{Identified, Memo};

// -------------------------------------------------------------------------------------------------

// TODO: this type definition makes more sense in `draw_to_texture.rs` once we are not partly using embedded_graphics
type Point = all_is_cubes::euclid::Point2D<u32, ImagePixel>;

#[derive(Debug)]
pub(crate) struct RaytraceToTexture {
    inner: Arc<Mutex<Inner>>,
    rt_frame_copy_bind_group:
        Memo<(crate::Id<wgpu::TextureView>, crate::Id<wgpu::TextureView>), wgpu::BindGroup>,

    /// Whether it is allowed for an `update()` to actually change camera and the scene data.
    /// This only has an effect when `update_strategy` is `Consistent`.
    may_start_next_frame: bool,
}

/// State for the possibly-asynchronous tracing job.
#[derive(Debug)]
struct Inner {
    rtr: RtRenderer,
    update_strategy: UpdateStrategy,
    dirty_pixels: usize,
    rays_per_frame: usize,
    color_render_target: DrawableTexture<[f16; 4], [f16; 4]>,
    depth_render_target: DrawableTexture<f32, f32>,
}

/// Schedule with which a [`RaytraceToTexture`] re-traces its pixels.
///
/// Also always knows the rendering (reduced resolution) viewport.
#[derive(Debug)]
enum UpdateStrategy {
    /// Allow frames to be “torn” (allow camera and scene data to change midway through rendering),
    /// to provide lower average latency. The central area of the image will be traced more often.
    Incremental(PixelPicker),

    /// Don't update anything; produce wholly consistent frames only.
    /// This does less work than waiting for the same frames to appear in incremental mode,
    /// but has higher latency.
    Consistent {
        render_viewport: Viewport,
        /// Index of the next pixel to render.
        next: usize,
    },
}

// -------------------------------------------------------------------------------------------------

impl RaytraceToTexture {
    pub fn new(cameras: StandardCameras) -> Self {
        let initial_viewport = Viewport::with_scale(1.0, vec2(1, 1));

        // TODO: allow choice of incremental mode.
        let update_strategy = if true {
            UpdateStrategy::Incremental(PixelPicker::new(initial_viewport))
        } else {
            UpdateStrategy::Consistent {
                render_viewport: initial_viewport,
                next: 0,
            }
        };

        let inner = Arc::new(Mutex::new(Inner {
            rays_per_frame: 5000
                * match cameras.graphics_options().lighting_display {
                    all_is_cubes_render::camera::LightingOption::Bounce => 1,
                    _ => 10,
                },
            rtr: RtRenderer::new(
                cameras,
                Box::new(raytracer_size_policy),
                listen::constant(Default::default()),
            ),
            dirty_pixels: update_strategy.cycle_length(),
            update_strategy,
            color_render_target: DrawableTexture::new(wgpu::TextureFormat::Rgba16Float),
            // Not using a depth texture format because float depth textures cannot be copied to.
            depth_render_target: DrawableTexture::new(wgpu::TextureFormat::R32Float),
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
            rt_frame_copy_bind_group: Memo::new(),
            inner,
            may_start_next_frame: true,
        }
    }

    /// Copy [`Space`] data from the camera spaces, and camera state from the [`StandardCameras`].
    pub fn update(
        &mut self,
        read_tickets: Layers<ReadTicket<'_>>,
        cursor: Option<&Cursor>,
    ) -> Result<(), RenderError> {
        let inner = &mut *self.inner.lock().unwrap();
        if inner.update_strategy.incremental() || self.may_start_next_frame {
            self.may_start_next_frame = false;
            inner.update_inputs(read_tickets, cursor)?;
        }
        Ok(())
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
        let color_view: &Identified<wgpu::TextureView> = inner.color_render_target.view().unwrap();
        let depth_view: &Identified<wgpu::TextureView> = inner.depth_render_target.view().unwrap();
        self.rt_frame_copy_bind_group.get_or_insert(
            (color_view.global_id(), depth_view.global_id()),
            || {
                device.create_bind_group(&wgpu::BindGroupDescriptor {
                    label: Some("rt to scene copy"),
                    layout: &pipelines.rt_frame_copy_layout,
                    entries: &[
                        wgpu::BindGroupEntry {
                            binding: 0,
                            resource: wgpu::BindingResource::TextureView(color_view),
                        },
                        wgpu::BindGroupEntry {
                            binding: 1,
                            resource: wgpu::BindingResource::TextureView(depth_view),
                        },
                        wgpu::BindGroupEntry {
                            binding: 2,
                            resource: wgpu::BindingResource::Sampler(&pipelines.linear_sampler),
                        },
                    ],
                })
            },
        );

        inner.do_some_tracing();
        // Copy to GPU texture if we are in incremental mode or the frame is finished.
        if inner.update_strategy.incremental() || inner.dirty_pixels == 0 {
            inner.color_render_target.upload(queue);
            inner.depth_render_target.upload(queue);
            self.may_start_next_frame = true;
        }
    }

    /// Draw a copy of the raytraced scene (as of the last [`Self::prepare_frame()`]).
    pub(crate) fn draw(
        &self,
        pipelines: &Pipelines,
        render_pass: &mut wgpu::RenderPass<'_>,
    ) -> Flaws {
        let Some(rt_bind_group) = self.rt_frame_copy_bind_group.get() else {
            return Flaws::UNFINISHED;
        };

        render_pass.set_bind_group(0, rt_bind_group, &[]);
        render_pass.set_pipeline(&pipelines.rt_frame_copy_pipeline);
        render_pass.draw(0..3, 0..1);

        Flaws::empty()
    }
}

impl Inner {
    fn update_inputs(
        &mut self,
        read_tickets: Layers<ReadTicket<'_>>,
        cursor: Option<&Cursor>,
    ) -> Result<(), RenderError> {
        if self.rtr.update(read_tickets, cursor)? {
            self.dirty();
        }
        Ok(())
    }

    fn set_viewport(&mut self, device: &wgpu::Device, render_viewport: Viewport) {
        if self.update_strategy.render_viewport() == render_viewport {
            // don't reset/dirty anything
            return;
        }
        self.color_render_target.resize(
            device,
            Some("RaytraceToTexture::color_render_target"),
            render_viewport.framebuffer_size,
        );
        self.depth_render_target.resize(
            device,
            Some("RaytraceToTexture::depth_render_target"),
            render_viewport.framebuffer_size,
        );
        self.update_strategy.resize(render_viewport);
        self.dirty();
    }

    fn dirty(&mut self) {
        self.dirty_pixels = self.update_strategy.cycle_length();
    }

    fn do_some_tracing(&mut self) {
        type Trace = (Point, [f16; 4], f32);

        if self.dirty_pixels == 0 {
            // We've traced the entire frame buffer area, and no changes have occurred,
            // so we have no need to trace again.
            return;
        }

        let render_viewport = self.update_strategy.render_viewport();
        let scene = self.rtr.scene::<(ColorBuf, DepthBuf)>();
        let camera = &scene.cameras().world;

        // Compute the transformation which maps ray distance back to world-space distance.
        //
        // Note that this depends on the ray lengths that `RtScene`, and thus
        // `Camera::project_ndc_into_world()`, use.
        //
        let depth_scale =
            -(camera.view_distance().into_inner() - camera.near_plane_distance().into_inner());
        let depth_bias = -camera.near_plane_distance().into_inner();
        let depth_transform = camera
            .projection_matrix()
            .pre_translate(vec3(0., 0., depth_bias))
            .pre_scale(0., 0., depth_scale);

        let start_time = Instant::now();

        // Function to trace one ray, independent of strategy.
        let trace_one = |point: Point| -> Trace {
            let x = point.x as usize;
            let y = point.y as usize;
            let ((color_buf, depth_buf), _info) = scene.trace_patch(Box2D {
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

            let linear_depth = depth_buf.depth().clamp(0.0, 1.0);
            // Note that this assumes that depth doesn't interact with position in the image.
            // If it did, we'd need to compute the NDC X and Y and pass them in here.
            let projected_depth_homogeneous =
                depth_transform.transform_point3d_homogeneous(point3(0., 0., linear_depth));
            let projected_depth = projected_depth_homogeneous.z / projected_depth_homogeneous.w;

            (point, color, projected_depth as f32)
        };

        // Function to write the trace results to storage.
        let color_target = self.color_render_target.draw_target();
        let depth_target = self.depth_render_target.draw_target();
        let mut store_one = |(point, color, depth): Trace| {
            color_target.set_pixel(point, color);
            depth_target.set_pixel(point, depth);
        };

        match self.update_strategy {
            UpdateStrategy::Incremental(ref mut pixel_picker) => {
                cfg_if::cfg_if! {
                    if #[cfg(feature = "auto-threads")] {
                        let this_frame_pixels: Vec<Point> =
                            pixel_picker.take(self.rays_per_frame).collect();
                        // Note: I tried making these steps execute in parallel using a channel
                        // instead of a `Vec`, and it was slower.
                        let traces: Vec<Trace> =
                            this_frame_pixels.into_par_iter().map(trace_one).collect();
                        for trace in traces {
                            store_one(trace);
                        }
                    } else {
                        for pixel in pixel_picker.take(self.rays_per_frame) {
                            store_one(trace_one(pixel));
                        }
                    }
                }
            }
            UpdateStrategy::Consistent {
                render_viewport: _,
                ref mut next,
            } => {
                let pixel_iter = (0..self.rays_per_frame)
                    .map(|i| point_from_pixel_index(render_viewport, i + *next));
                cfg_if::cfg_if! {
                    if #[cfg(feature = "auto-threads")] {
                        let this_frame_pixels: Vec<Point> =
                            pixel_iter.take(self.rays_per_frame).collect();
                        let traces: Vec<Trace> =
                            this_frame_pixels.into_par_iter().map(trace_one).collect();
                        for trace in traces {
                            store_one(trace);
                        }
                    } else {
                        for pixel in pixel_iter {
                            store_one(trace_one(pixel));
                        }
                    }
                }

                *next += self.rays_per_frame;
            }
        }

        // Every strategy updates exactly this many pixels.
        self.dirty_pixels = self.dirty_pixels.saturating_sub(self.rays_per_frame);

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

// -------------------------------------------------------------------------------------------------

impl UpdateStrategy {
    fn render_viewport(&self) -> Viewport {
        match *self {
            UpdateStrategy::Incremental(ref pixel_picker) => pixel_picker.viewport,
            UpdateStrategy::Consistent {
                render_viewport,
                next: _,
            } => render_viewport,
        }
    }

    /// After at least this many pixels have been traced according to this strategy,
    /// the entire image has been covered.
    fn cycle_length(&self) -> usize {
        match self {
            UpdateStrategy::Incremental(picker) => picker.cycle_length,
            UpdateStrategy::Consistent {
                render_viewport,
                next: _,
            } => render_viewport.pixel_count().unwrap_or(usize::MAX),
        }
    }

    fn incremental(&self) -> bool {
        match self {
            UpdateStrategy::Incremental(_) => true,
            UpdateStrategy::Consistent { .. } => false,
        }
    }

    fn resize(&mut self, new_render_viewport: Viewport) {
        match self {
            UpdateStrategy::Incremental(pixel_picker) => {
                pixel_picker.resize(new_render_viewport);
            }
            UpdateStrategy::Consistent {
                render_viewport,
                next: _,
            } => {
                *render_viewport = new_render_viewport;
            }
        }
    }
}

// -------------------------------------------------------------------------------------------------

/// Sorts pixels into an update order that produces more quickly useful results for interactive
/// viewing than a linear top-to-bottom sweep would.
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
        let linear_index = self.iter.next().unwrap();
        let index = self.sorted_pixels[linear_index];
        Some(point_from_pixel_index(self.viewport, index))
    }
}

/// Convert an index in the viewport to a pixel position. If out of range, wraps around.
#[inline(always)]
fn point_from_pixel_index(viewport: Viewport, index: usize) -> Point {
    // `as usize` is valid because we would have failed earlier if it doesn't fit in usize.
    let size = viewport.framebuffer_size.map(|s| s as usize);
    Point::new(
        index.rem_euclid(size.width) as u32,
        index.div_euclid(size.width).rem_euclid(size.height) as u32,
    )
}

// -------------------------------------------------------------------------------------------------

fn raytracer_size_policy(mut viewport: Viewport) -> Viewport {
    // use 2x2 nominal pixels
    viewport.framebuffer_size = viewport
        .nominal_size
        .map(|c| (c / 2.0).round() as u32)
        .cast_unit();
    viewport
}

// -------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    //use super::*;
    // ...
    // TODO: Test PixelPicker since it is independent of the GPU
}
