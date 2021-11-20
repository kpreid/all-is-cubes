// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Runs the software raytracer and writes the results into a texture.

// TODO: This is not yet used. Finish hooking it up or delete it.
#![allow(dead_code)]

use cgmath::{Point2, Vector2};
use embedded_graphics::draw_target::DrawTarget;
use embedded_graphics::pixelcolor::Rgb888;
use embedded_graphics::prelude::Point;
use embedded_graphics::Pixel;
use instant::{Duration, Instant};
use luminance::backend::shader::Uniformable;
use luminance::context::GraphicsContext;
use luminance::pipeline::TextureBinding;
use luminance::pixel::NormUnsigned;
use luminance::texture::Dim2;
use rand::prelude::SliceRandom as _;
use rand::SeedableRng as _;
#[cfg(feature = "rayon")]
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::rc::Rc;

use crate::camera::Viewport;
use crate::camera::{Camera, GraphicsOptions};
use crate::listen::ListenableSource;
use crate::lum::frame_texture::{FullFramePainter, FullFrameTexture};
use crate::lum::types::AicLumBackend;
use crate::lum::GraphicsResourceError;
use crate::raytracer::{ColorBuf, PixelBuf, UpdatingSpaceRaytracer};
use crate::space::{Space, SpaceBlockData};
use crate::universe::URef;

pub(crate) struct RaytraceToTexture<Backend: AicLumBackend> {
    raytracer: Option<UpdatingSpaceRaytracer<Srgb8Adapter>>,
    // TODO: should not be public but we want an easy way to grab it for drawing
    pub(crate) render_target: FullFrameTexture<Backend>,
    pixel_picker: PixelPicker,
    rays_per_frame: usize,
}

impl<Backend> RaytraceToTexture<Backend>
where
    Backend: AicLumBackend,
    TextureBinding<Dim2, NormUnsigned>: Uniformable<Backend>,
{
    pub fn new(fp: Rc<FullFramePainter<Backend>>) -> Result<Self, GraphicsResourceError> {
        Ok(Self {
            raytracer: None,
            render_target: fp.new_texture(),
            pixel_picker: PixelPicker::new(Viewport {
                nominal_size: Vector2::new(1., 1.),
                framebuffer_size: Vector2::new(1, 1),
            }),
            rays_per_frame: 50000,
        })
    }

    pub fn set_space(&mut self, space: Option<URef<Space>>) {
        // TODO: hook up options
        self.raytracer = space.map(|s| {
            UpdatingSpaceRaytracer::new(s, ListenableSource::constant(GraphicsOptions::default()))
        });
    }

    /// Trace a frame's worth of rays and update the texture.
    pub fn prepare_frame<'a, C>(
        &'a mut self,
        context: &mut C,
        camera: &Camera,
    ) -> Result<(), GraphicsResourceError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        self.render_target
            .resize(context, camera.viewport(), raytracer_size_policy)?;
        let render_viewport = self.render_target.scaled_viewport().unwrap();
        self.pixel_picker.resize(render_viewport);

        // TODO: Instead of the whole size policy business, maybe we should just expect
        // camera.viewport() to have a "reasonable" framebuffer size. Or, just construct
        // a copy of the Camera with an adjusted viewport.

        if let Some(urt) = &mut self.raytracer {
            urt.update().unwrap(/* TODO */);
            let tracer = urt.get();

            #[allow(clippy::needless_collect)] // needed with rayon and not without
            let this_frame_pixels: Vec<Point> = (0..self.rays_per_frame)
                .map(|_i| self.pixel_picker.next().unwrap())
                .collect();

            let start_time = Instant::now();
            let trace = |p| {
                Pixel(
                    p,
                    tracer
                        .trace_ray(camera.project_ndc_into_world(Point2::new(
                            render_viewport.normalize_fb_x(p.x as usize),
                            render_viewport.normalize_fb_y(p.y as usize),
                        )))
                        .0,
                )
            };
            #[cfg(feature = "rayon")]
            let traces: Vec<Pixel<Rgb888>> = this_frame_pixels.into_par_iter().map(trace).collect();
            #[cfg(not(feature = "rayon"))]
            let traces: Vec<Pixel<Rgb888>> = this_frame_pixels.into_iter().map(trace).collect();
            let tracing_duration = Instant::now().duration_since(start_time);

            match tracing_duration.cmp(&Duration::from_millis(40)) {
                std::cmp::Ordering::Greater => {
                    self.rays_per_frame = (self.rays_per_frame - 100).max(100);
                }
                std::cmp::Ordering::Equal => {}
                std::cmp::Ordering::Less => {
                    let fbs = render_viewport.framebuffer_size;
                    self.rays_per_frame =
                        (self.rays_per_frame + 100).min(fbs.x as usize * fbs.y as usize);
                }
            }

            self.render_target.draw_iter(traces).unwrap();
            self.render_target.upload()?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
struct PixelPicker {
    iter: std::iter::Cycle<std::ops::Range<usize>>,
    viewport: Viewport,
    shuffled_pixels: Box<[usize]>,
}

impl PixelPicker {
    fn new(viewport: Viewport) -> Self {
        // Generate a pseudorandom order to evenly distributed update rays about the
        // screen. Note that this is deterministic since we don't reuse the RNG.
        let pixel_count = viewport.pixel_count().unwrap();
        let mut shuffled_pixels: Box<[usize]> = (0..pixel_count).collect();
        shuffled_pixels.shuffle(&mut rand_xoshiro::Xoshiro256Plus::seed_from_u64(
            0x9aa8bc4be2112757,
        ));

        PixelPicker {
            iter: (0..pixel_count).cycle(),
            viewport,
            shuffled_pixels,
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
        let index = self.shuffled_pixels[linear_index];
        Some(Point::new(
            index.rem_euclid(size.x) as i32,
            index.div_euclid(size.x).rem_euclid(size.y) as i32,
        ))
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
struct Srgb8Adapter(ColorBuf);

impl PixelBuf for Srgb8Adapter {
    //type Pixel = [u8; 4];
    type Pixel = Rgb888;
    type BlockData = <ColorBuf as PixelBuf>::BlockData;

    fn compute_block_data(block: &SpaceBlockData) -> Self::BlockData {
        ColorBuf::compute_block_data(block)
    }

    fn error_block_data() -> Self::BlockData {
        ColorBuf::error_block_data()
    }

    fn sky_block_data() -> Self::BlockData {
        ColorBuf::sky_block_data()
    }

    fn opaque(&self) -> bool {
        self.0.opaque()
    }

    fn result(self) -> Self::Pixel {
        let [r, g, b, _] = self.0.result().to_srgb_32bit();
        Rgb888::new(r, g, b)
    }

    fn add(&mut self, surface_color: crate::math::Rgba, block_data: &Self::BlockData) {
        self.0.add(surface_color, block_data)
    }
}

fn raytracer_size_policy(mut viewport: Viewport) -> Viewport {
    // use 2x2 nominal pixels
    viewport.framebuffer_size = viewport.nominal_size.map(|c| (c / 2.0).round() as u32);
    viewport
}

#[cfg(test)]
mod tests {
    //use super::*;
    // ...
    // TODO: Test PixelPicker and Srgb8Adapter since they are independent of GraphicsContext
}
