//! Axis-aligned orthographic raytracing as a special case.

#[cfg(feature = "auto-threads")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use all_is_cubes::block::Resolution;
use all_is_cubes::euclid::{Point2D, Scale, Transform3D, point2, vec2, vec3};
use all_is_cubes::math::{
    Axis, Cube, Face6, FreeVector, GridAab, GridRotation, GridSizeCoord, Gridgid, Rgba,
};
use all_is_cubes::raycast;
use all_is_cubes::space::Space;
use all_is_cubes::universe::{Handle, ReadTicket};

use crate::camera::{self, GraphicsOptions, ImagePixel};
use crate::raytracer;
use crate::{Flaws, Rendering};

/// Special-purpose renderer which uses a pixel-perfect orthographic projection and adapts to the
/// size of the space input.
//---
// TODO: This renderer has no tests of its output other than the implicit testing where
// it is used in UI tests.
pub fn render_orthographic(read_ticket: ReadTicket<'_>, space: &Handle<Space>) -> Rendering {
    // TODO: Figure out how to make this less of a from-scratch reimplementation and share
    // more components with the regular raytracer.
    // To do this we will need to add ortho support to `Camera` or some other special case.

    // TODO: Given this special orthographic pixel-aligned projection, and an accumulator
    // for "what's the highest resolution that was tested along this ray", we can do adaptive
    // sampling so we can trace whole blocks at once when they're simple, and also detect if
    // the image scale is too low to accurately capture the scene.
    let space = space.read(read_ticket).expect("failed to read space to render");
    let camera = &MultiOrthoCamera::new(Resolution::R32, space.bounds());
    let rt = &raytracer::SpaceRaytracer::new(&space, GraphicsOptions::UNALTERED_COLORS, ());

    // TODO: The caching logic here saves time by skipping tracing pixels that are
    // predictably equal to previous ones. However, the optimal solution would be 2D rather
    // than 1D: breaking up the image into block-sized tiles, then casting one ray per tile
    // (to start) to determine the resolution needed. That'll probably mean redesigning
    // `OrthoCamera` to be more aware of its alignment with the block grid.

    #[cfg(feature = "auto-threads")]
    let data = (0..camera.image_size.height)
        .into_par_iter()
        .flat_map(|y| {
            (0..camera.image_size.width)
                .into_par_iter()
                .map_with(Cache::default(), move |cache: &mut Cache, x| {
                    trace_one_pixel_with_cache(camera, rt, cache, x, y)
                })
        })
        .collect();

    #[cfg(not(feature = "auto-threads"))]
    let data = (0..camera.image_size.height)
        .flat_map(|y| {
            let mut cache = Cache::default();

            (0..camera.image_size.width)
                .map(move |x| trace_one_pixel_with_cache(camera, rt, &mut cache, x, y))
        })
        .collect();

    Rendering {
        size: camera.image_size,
        data,
        flaws: Flaws::empty(), // TODO: wrong
    }
}

fn trace_one_pixel_with_cache(
    camera: &MultiOrthoCamera,
    rt: &raytracer::SpaceRaytracer<Resolution>,
    cache: &mut Cache,
    x: u32,
    y: u32,
) -> [u8; 4] {
    match camera.project_pixel_into_world(point2(x, y)) {
        Some(ray) => {
            // Note that this cache key may be nonsense (using the resolution from the wrong cube),
            // but in that case it will always not match the cube, so we’ll then create
            // a new key with the correct resolution.
            let cache_key: CacheKey = (
                ray.origin_cube(),
                cache.resolution,
                ray.zoom_in(ray.origin_cube(), cache.resolution).origin_cube(),
            );
            if let Some((_, v)) = cache.pixel.filter(|&(k, _)| k == cache_key) {
                v
            } else {
                let mut pixel = OrthoBuf::default();
                rt.trace_axis_aligned_ray(ray, &mut pixel, true);

                let output = Rgba::from(pixel.color);

                let new_cache_key = (
                    ray.origin_cube(),
                    pixel.max_resolution,
                    ray.zoom_in(ray.origin_cube(), pixel.max_resolution).origin_cube(),
                );
                *cache = Cache {
                    resolution: pixel.max_resolution,
                    pixel: Some((new_cache_key, output)),
                };
                output
            }
        }
        None => Rgba::TRANSPARENT,
    }
    .to_srgb8()
}

type CacheKey = (Cube, Resolution, Cube);

#[derive(Clone, Copy)]
struct Cache {
    resolution: Resolution,
    pixel: Option<(CacheKey, Rgba)>,
}
impl Default for Cache {
    fn default() -> Self {
        Self {
            resolution: Resolution::R1,
            pixel: None,
        }
    }
}

/// A view of a `Space` from multiple directions at a chosen pixel-perfect resolution.
#[derive(Debug)]
pub struct MultiOrthoCamera {
    pub image_size: camera::ImageSize,
    views: [(OrthoCamera, Point2D<u32, ImagePixel>); 5],
}

impl MultiOrthoCamera {
    pub fn new(resolution: Resolution, bounds: GridAab) -> Self {
        let top = OrthoCamera::new(resolution, bounds, Face6::PY);
        let left = OrthoCamera::new(resolution, bounds, Face6::NX);
        let front = OrthoCamera::new(resolution, bounds, Face6::PZ);
        let right = OrthoCamera::new(resolution, bounds, Face6::PX);
        let bottom = OrthoCamera::new(resolution, bounds, Face6::NY);
        let views = [
            (top, point2(left.image_size.width + 1, 0)),
            (left, point2(0, top.image_size.height + 1)),
            (
                front,
                point2(left.image_size.width + 1, top.image_size.height + 1),
            ),
            (
                right,
                point2(
                    left.image_size.width + front.image_size.width + 2,
                    top.image_size.height + 1,
                ),
            ),
            (
                bottom,
                point2(
                    left.image_size.width + 1,
                    top.image_size.height + front.image_size.height + 2,
                ),
            ),
        ];

        let mut bottom_corner = point2(0, 0);
        for &(ref cam, origin) in views.iter() {
            bottom_corner = bottom_corner.max(origin + cam.image_size.to_vector());
        }

        Self {
            image_size: bottom_corner.to_vector().to_size(),
            views,
        }
    }

    pub fn project_pixel_into_world(
        &self,
        point: Point2D<u32, ImagePixel>,
    ) -> Option<raycast::AaRay> {
        // Find which camera's rectangle contains the point
        for &(ref cam, origin) in self.views.iter() {
            if let (Some(x), Some(y)) =
                (point.x.checked_sub(origin.x), point.y.checked_sub(origin.y))
            {
                if x < cam.image_size.width && y < cam.image_size.height {
                    return cam.project_pixel_into_world(point2(x, y));
                }
            }
        }
        None
    }
}

/// A view of a `Space` from an axis-aligned direction at a chosen pixel-perfect resolution.
#[derive(Clone, Copy, Debug)]
#[expect(clippy::module_name_repetitions)]
pub struct OrthoCamera {
    image_size: camera::ImageSize,
    transform: Transform3D<f64, ImagePixel, Cube>,
    ray_direction: FreeVector,
}

impl OrthoCamera {
    pub fn new(resolution: Resolution, bounds: GridAab, viewed_face: Face6) -> Self {
        let cube_to_pixel_scale: Scale<GridSizeCoord, Cube, ImagePixel> =
            Scale::new(resolution.into());
        let pixel_to_cube_scale: Scale<f64, ImagePixel, Cube> =
            cube_to_pixel_scale.cast::<f64>().inverse();

        let image_size = cube_to_pixel_scale
            .transform_size(
                {
                    let sizevec = bounds.size().to_vector();
                    match viewed_face.axis() {
                        Axis::X => vec2(sizevec.z, sizevec.y),
                        Axis::Y => sizevec.xz(),
                        Axis::Z => sizevec.xy(),
                    }
                }
                .to_size(),
            )
            .to_u32();
        let origin_translation: FreeVector = {
            let lb = bounds.lower_bounds();
            let ub = bounds.upper_bounds();
            match viewed_face {
                // note Y flip — this is the world point that should be the top left corner of each view
                Face6::NX => vec3(lb.x, ub.y, lb.z),
                Face6::NY => vec3(lb.x, lb.y, ub.z),
                Face6::NZ => vec3(ub.x, ub.y, lb.z),
                Face6::PX => vec3(ub.x, ub.y, ub.z),
                Face6::PY => vec3(lb.x, ub.y, lb.z),
                Face6::PZ => vec3(lb.x, ub.y, ub.z),
            }
        }
        .to_f64();
        let rotation = Gridgid::from_rotation_about_origin(match viewed_face {
            Face6::NX => Face6::PY.clockwise(),
            Face6::NY => Face6::PX.clockwise(),
            Face6::NZ => Face6::PY.clockwise() * Face6::PY.clockwise(), // arbitrary 180°
            Face6::PX => Face6::PY.counterclockwise(),
            Face6::PY => Face6::PX.counterclockwise(),
            Face6::PZ => GridRotation::IDENTITY,
        })
        .to_matrix()
        .to_free();

        let transform = Transform3D::translation(0.5, 0.5, 0.0) // pixel centers
            .then_scale(1., -1., 1.) // Y flip
            .then(&Transform3D::from_scale(pixel_to_cube_scale)) // overall scale
            .then(&rotation)
            .then_translate(origin_translation); // image origin to world origin

        // TODO: Add side and top/bottom orthographic views.

        Self {
            image_size,
            transform,
            ray_direction: transform.transform_vector3d(vec3(0., 0., -1.)),
        }
    }

    pub fn project_pixel_into_world(
        &self,
        point: Point2D<u32, ImagePixel>,
    ) -> Option<raycast::AaRay> {
        raycast::Ray {
            origin: self.transform.transform_point3d(point.to_f64().to_3d()).unwrap(),
            direction: self.ray_direction,
        }
        .try_into()
        .ok()
    }
}

struct OrthoBuf {
    color: raytracer::ColorBuf,
    max_resolution: Resolution,
}

impl Default for OrthoBuf {
    fn default() -> Self {
        Self {
            color: Default::default(),
            max_resolution: Resolution::R1,
        }
    }
}

impl raytracer::Accumulate for OrthoBuf {
    type BlockData = Resolution;

    fn opaque(&self) -> bool {
        self.color.opaque()
    }

    fn add(&mut self, hit: raytracer::Hit<'_, Self::BlockData>) {
        self.color.add(hit.map_block_data(|_| &()));
        self.max_resolution = self.max_resolution.max(*hit.block);
    }

    // This ensures we check the resolution of blocks the ray doesn't hit any voxels of.
    fn enter_block(&mut self, &resolution: &Self::BlockData) {
        self.max_resolution = self.max_resolution.max(resolution);
    }

    fn mean<const N: usize>(bufs: [Self; N]) -> Self {
        Self {
            color: raytracer::ColorBuf::mean(bufs.each_ref().map(|buf| buf.color)),
            max_resolution: bufs.iter().map(|buf| buf.max_resolution).max().unwrap(),
        }
    }
}
