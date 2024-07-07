//! Axis-aligned orthographic raytracing as a special case.

#[cfg(feature = "auto-threads")]
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use all_is_cubes::euclid::{point2, vec2, vec3, Point2D, Scale, Transform3D};
use all_is_cubes::math::{
    Axis, Cube, Face6, FreeVector, GridAab, GridRotation, GridSizeCoord, Gridgid, Rgba,
};
use all_is_cubes::raycast;
use all_is_cubes::space::Space;
use all_is_cubes::universe::Handle;
use all_is_cubes::{block, raytracer};

use crate::camera::{self, GraphicsOptions, ImagePixel};
use crate::{Flaws, Rendering};

/// Special-purpose renderer which uses a pixel-perfect orthographic projection and adapts to the
/// size of the space input.
pub fn render_orthographic(space: &Handle<Space>) -> Rendering {
    // TODO: Figure out how to make this less of a from-scratch reimplementation and share
    // more components with the regular raytracer.
    // To do this we will need to add ortho support to `Camera` or some other special case.

    // TODO: Given this special orthographic pixel-aligned projection, and an accumulator
    // for "what's the highest resolution that was tested along this ray", we can do adaptive
    // sampling so we can trace whole blocks at once when they're simple, and also detect if
    // the image scale is too low to accurately capture the scene.
    let space = &*space.read().expect("failed to read space to render");
    let camera = &MultiOrthoCamera::new(block::Resolution::R32, space.bounds());
    let rt = &raytracer::SpaceRaytracer::new(space, GraphicsOptions::UNALTERED_COLORS, ());

    #[cfg(feature = "auto-threads")]
    let data = (0..camera.image_size.height)
        .into_par_iter()
        .flat_map(|y| {
            (0..camera.image_size.width).into_par_iter().map(move |x| {
                match camera.project_pixel_into_world(point2(x, y)) {
                    Some(ray) => {
                        let (pixel, _): (raytracer::ColorBuf, _) =
                            rt.trace_axis_aligned_ray(ray, true);
                        Rgba::from(pixel)
                    }
                    None => Rgba::TRANSPARENT,
                }
                .to_srgb8()
            })
        })
        .collect();

    #[cfg(not(feature = "auto-threads"))]
    let data = (0..camera.image_size.height)
        .flat_map(|y| {
            (0..camera.image_size.width).map(move |x| {
                match camera.project_pixel_into_world(point2(x, y)) {
                    Some(ray) => {
                        let (pixel, _): (raytracer::ColorBuf, _) =
                            rt.trace_axis_aligned_ray(ray, true);
                        Rgba::from(pixel)
                    }
                    None => Rgba::TRANSPARENT,
                }
                .to_srgb8()
            })
        })
        .collect();

    Rendering {
        size: camera.image_size,
        data,
        flaws: Flaws::empty(), // TODO: wrong
    }
}

/// A view of a `Space` from multiple directions at a chosen pixel-perfect resolution.
#[derive(Debug)]
pub struct MultiOrthoCamera {
    pub image_size: camera::ImageSize,
    views: [(OrthoCamera, Point2D<u32, ImagePixel>); 5],
}

impl MultiOrthoCamera {
    pub fn new(resolution: block::Resolution, bounds: GridAab) -> Self {
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
pub struct OrthoCamera {
    image_size: camera::ImageSize,
    transform: Transform3D<f64, ImagePixel, Cube>,
    ray_direction: FreeVector,
}

impl OrthoCamera {
    pub fn new(resolution: block::Resolution, bounds: GridAab, viewed_face: Face6) -> Self {
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
            Face6::NX => GridRotation::CLOCKWISE,
            Face6::NY => GridRotation::RXZy,
            Face6::NZ => GridRotation::CLOCKWISE * GridRotation::CLOCKWISE,
            Face6::PX => GridRotation::COUNTERCLOCKWISE,
            Face6::PY => GridRotation::RXzY,
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
            origin: self
                .transform
                .transform_point3d(point.to_f64().to_3d())
                .unwrap(),
            direction: self.ray_direction,
        }
        .try_into()
        .ok()
    }
}
