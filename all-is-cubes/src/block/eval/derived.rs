use core::ops;

use euclid::Vector3D;
use ordered_float::NotNan;

/// Acts as polyfill for float methods
#[cfg(not(feature = "std"))]
#[allow(unused_imports)]
use num_traits::float::FloatCore as _;

use crate::block::{self, EvaluatedBlock};
use crate::math::{Cube, Face6, FaceMap, GridAab, Intensity, Rgb, Rgba, Vol};
use crate::raytracer;

/// Compute the derived properties of block voxels to create a full [`EvaluatedBlock`].
#[inline(never)] // neither cheap nor going to benefit from per-call optimizations
pub(crate) fn voxels_to_evaluated_block(
    attributes: block::BlockAttributes,
    voxels: block::Evoxels,
    cost: block::Cost,
) -> EvaluatedBlock {
    // Optimization for single voxels:
    // don't allocate any `Vol`s or perform any generalized scans.
    if let Some(block::Evoxel {
        color,
        emission,
        selectable: _,
        collision,
    }) = voxels.single_voxel()
    {
        let visible = !color.fully_transparent();
        return EvaluatedBlock {
            attributes,
            color,
            face_colors: FaceMap::repeat(color),
            light_emission: emission,
            voxels,
            opaque: FaceMap::repeat(color.fully_opaque()),
            visible,
            uniform_collision: Some(collision),
            // Note an edge case shenanigan:
            // `AIR_EVALUATED` cannot allocate a mask, and we want this to match the
            // output of that so that `EvaluatedBlock::consistency_check()` will agree.)
            // It's also useful to skip the mask when the block is invisible, but
            // that's not the motivation of doing this this way.
            voxel_opacity_mask: if !visible {
                None
            } else {
                Some(Vol::from_element(color.opacity_category()))
            },
            cost,
        };
    }

    let resolution = voxels.resolution();
    let full_block_bounds = GridAab::for_block(resolution);
    let less_than_full = full_block_bounds != voxels.bounds();

    // Compute color sum from voxels.
    // This is actually a sort of mini-raytracer, in that it computes the appearance
    // of all six faces by tracing in from the edges, and then averages them.
    let (color, face_colors, emission): (Rgba, FaceMap<Rgba>, Rgb) = {
        let mut all_faces_sum = VoxSum::default();
        let mut face_colors = FaceMap::repeat(Rgba::TRANSPARENT);

        // Loop over all face voxels.
        // (This is a similar structure to the algorithm we use for mesh generation.)
        for face in Face6::ALL {
            let mut face_sum = VoxSum::default();
            let transform = face.face_transform(resolution.into());
            let rotated_voxel_range = voxels.bounds().transform(transform.inverse()).unwrap();

            for v in rotated_voxel_range.y_range() {
                for u in rotated_voxel_range.x_range() {
                    let cube: Cube = transform.transform_cube(Cube::new(
                        u,
                        v,
                        rotated_voxel_range.z_range().start,
                    ));
                    debug_assert!(voxels.bounds().contains_cube(cube));

                    face_sum +=
                        raytracer::trace_for_eval(&voxels, cube, face.opposite(), resolution);
                }
            }
            all_faces_sum += face_sum;
            face_colors[face] = face_sum.color(f32::from(resolution).powi(2))
        }
        let surface_area = full_block_bounds.surface_area_f64() as f32;
        (
            all_faces_sum.color(surface_area),
            face_colors,
            all_faces_sum.emission(surface_area),
        )
    };

    // Compute if the collision is uniform in all voxels.
    let uniform_collision = {
        let mut collision: Option<block::BlockCollision> = if less_than_full {
            Some(block::BlockCollision::None)
        } else {
            None
        };
        let mut collision_unequal = false;
        for voxel in voxels.as_vol_ref().as_linear().iter() {
            match (collision, collision_unequal) {
                // Already unequal
                (_, true) => {}
                // First voxel
                (None, false) => collision = Some(voxel.collision),
                // Matching voxel
                (Some(prev), false) if prev == voxel.collision => {}
                // Non-matching voxel
                (Some(_), false) => {
                    collision = None;
                    collision_unequal = true;
                }
            }
        }

        collision
    };

    let visible = voxels.as_vol_ref().as_linear().iter().any(
        #[inline(always)]
        |voxel| !voxel.color.fully_transparent(),
    );

    // Generate mask only if the block is not invisible, because it will never be
    // useful for invisible blocks. (The purpose of the mask is to allow re-texturing
    // a mesh of the appropriate shape, and invisible blocks have no mesh.)
    let voxel_opacity_mask = if !visible {
        None
    } else {
        Some(voxels.as_vol_ref().map_container(|voxels| {
            voxels
                .iter()
                .map(|voxel| voxel.color.opacity_category())
                .collect()
        }))
    };

    EvaluatedBlock {
        attributes,
        color,
        face_colors,
        light_emission: emission,
        opaque: FaceMap::from_fn(|face| {
            // TODO: This test should be refined by flood-filling in from the face,
            // so that we can also consider a face opaque if it has hollows/engravings.
            // Merge this with the raytracer above.
            let surface_volume = full_block_bounds.abut(face, -1).unwrap();
            if surface_volume.intersection_cubes(voxels.bounds()) == Some(surface_volume) {
                surface_volume.interior_iter().all(
                    #[inline(always)]
                    |p| voxels[p].color.fully_opaque(),
                )
            } else {
                false
            }
        }),
        visible,
        uniform_collision,
        voxel_opacity_mask,
        voxels,
        cost,
    }
}

/// Accumulator of surface properties of faces of a cube.
/// Used internally by evaluation to produce average colors.
#[derive(Clone, Copy, Default)]
struct VoxSum {
    /// Color multiplied by its alpha (i.e. "premultiplied")
    color_sum: Vector3D<f32, Intensity>,
    alpha_sum: f32,
    emission_sum: Vector3D<f32, Intensity>,
    count: usize,
}
impl VoxSum {
    /// Retures the reflectance color.
    ///
    /// `surface_area` should be the area in pixels (voxel faces) of the full block/face, not the
    /// area which had actual data.
    fn color(&self, surface_area: f32) -> Rgba {
        // Dividing by alpha_sum to un-"premultiply" the weighted data from when it was added.
        // This also has the effect of scaling the value appropriately to make it an average.
        let color_scale = self.alpha_sum;
        if color_scale.partial_cmp(&0.0) != Some(core::cmp::Ordering::Greater) {
            // If there are zero things, the result should be transparent (not divide-by-zero)
            Rgba::TRANSPARENT
        } else {
            Rgb::try_from(self.color_sum / color_scale)
                .expect("Recursive block color computation produced NaN")
                .with_alpha(
                    // Note that by dividing the alpha by the full surface area, not the count,
                    // we handle the case where the voxel data doesn't cover the full block and
                    // uncounted pixels should act as if they are transparent.
                    NotNan::new(self.alpha_sum / (surface_area))
                        .expect("Recursive block alpha computation produced NaN"),
                )
        }
    }

    /// Returns the aggregate light emission.
    ///
    /// `surface_area` should be the area in pixels (voxel faces) of the full block/face, not the
    /// area which had actual data.
    fn emission(&self, surface_area: f32) -> Rgb {
        if self.count == 0 {
            Rgb::ZERO
        } else {
            Rgb::try_from(self.emission_sum / surface_area)
                .expect("Recursive block emission computation produced NaN")
        }
    }
}
impl ops::AddAssign<raytracer::EvalTrace> for VoxSum {
    fn add_assign(&mut self, rhs: raytracer::EvalTrace) {
        let raytracer::EvalTrace { color, emission } = rhs;
        let alpha = color.alpha().into_inner();
        // Multiply by alpha to produce an appropriately weighted sum
        self.color_sum += Vector3D::from(color.to_rgb()) * alpha;
        self.alpha_sum += alpha;
        self.emission_sum += emission;
        self.count += 1;
    }
}
impl ops::AddAssign for VoxSum {
    fn add_assign(&mut self, rhs: VoxSum) {
        let Self {
            color_sum,
            alpha_sum,
            emission_sum,
            count,
        } = rhs;
        self.color_sum += color_sum;
        self.alpha_sum += alpha_sum;
        self.emission_sum += emission_sum;
        self.count += count;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::raytracer::EvalTrace;
    use euclid::vec3;

    /// Unit tests for `VoxSum`'s math. `VoxSum` is an internal helper type, so if there is reason
    /// to change it, these tests should be freely discarded; the point of these tests is to help
    /// directly test the arithmetic without the complication of setting up a voxel block scenario.

    #[test]
    fn voxsum_simple_opaque() {
        let mut v = VoxSum::default();
        v += EvalTrace {
            color: Rgba::new(1., 0., 0., 1.),
            emission: vec3(0., 0., 1.),
        };
        v += EvalTrace {
            color: Rgba::new(0., 1., 0., 1.),
            emission: vec3(0., 0., 1.),
        };
        assert_eq!(v.color(2.), Rgba::new(0.5, 0.5, 0., 1.));
        assert_eq!(v.emission(2.), Rgb::new(0., 0., 1.));
    }

    #[test]
    fn voxsum_weighted_transparency() {
        let mut v = VoxSum::default();
        v += EvalTrace {
            color: Rgba::new(1., 0., 0., 0.25),
            emission: vec3(0., 0., 1.),
        };
        v += EvalTrace {
            color: Rgba::new(0., 1., 0., 0.75),
            emission: vec3(0., 0., 1.),
        };
        assert_eq!(v.color(2.), Rgba::new(0.25, 0.75, 0., 0.5));
        assert_eq!(v.emission(2.), Rgb::new(0., 0., 1.));
    }
}
