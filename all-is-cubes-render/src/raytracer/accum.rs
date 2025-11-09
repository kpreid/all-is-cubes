//! [`Accumulate`] implementations.
//!
//! Note that this module builds on `all_is_cubes::raytracer::accum`.

use super::{Accumulate, Hit};

/// Implements [`Accumulate`] to collect depth data.
///
/// It returns the depth of the nearest surface, even if that surface is transparent.
#[derive(Clone, Copy, Debug)]
#[allow(clippy::exhaustive_structs)]
pub struct DepthBuf {
    depth: f64,
}

impl Default for DepthBuf {
    fn default() -> Self {
        Self {
            depth: f64::INFINITY,
        }
    }
}

impl Accumulate for DepthBuf {
    type BlockData = ();

    fn opaque(&self) -> bool {
        // Note that if we get a NaN, this returns false, which is correct because the min()
        // in add() will overwrite the NaN.
        self.depth < f64::INFINITY
    }

    fn add(&mut self, hit: Hit<'_, Self::BlockData>) {
        if let Some(d) = hit.t_distance {
            // Note: If the caller respects self.opaque(), then this could just be
            //     self.depth = d;
            // but we don't make that part of the contract.
            self.depth = self.depth.min(d);
        }
    }

    fn mean<const N: usize>(items: [Self; N]) -> Self {
        // In general, it doesn’t make sense to literally take the mean of depth values,
        // since it could create a position floating in space between two objects.
        // Use the minimum as a less-bad choice.
        //
        // More broadly, this method is used to do antialiasing via multiple samples,
        // and depth value processing doesn't fit well with that.
        //
        // TODO: Choosing the approximate mode might make more sense than choosing the minimum.
        // Also,
        Self {
            depth: items.into_iter().map(|db| db.depth).reduce(f64::min).unwrap_or(f64::INFINITY),
        }
    }
}

impl DepthBuf {
    /// The depth value, or [`f64::INFINITY`] if no surface was hit.
    /// See [`Hit::t_distance`] for more information on the interpretation of this number.
    ///
    /// Currently, it is not possible to distinguish different kinds of nothing hit.
    /// If that is needed, compose this with another `Accumulate` implementation.
    //---
    // Design note: This is a method so that we have the option of changing the representation.
    pub fn depth(self) -> f64 {
        self.depth
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::camera::GraphicsOptions;
    use crate::raytracer::SpaceRaytracer;
    use all_is_cubes::block::Resolution::*;
    use all_is_cubes::content;
    use all_is_cubes::math::GridAab;
    use all_is_cubes::raycast::Ray;
    use all_is_cubes::space::Space;
    use all_is_cubes::universe::Universe;

    #[test]
    fn depth_buf() {
        let universe = &mut Universe::new();
        let slab = content::make_slab(universe, 1, R2);
        let space = Space::builder(GridAab::from_lower_size([0, 0, 0], [1, 1, 1]))
            .read_ticket(universe.read_ticket())
            .filled_with(slab)
            .build();
        let rt = &SpaceRaytracer::<()>::new(&space.read(), GraphicsOptions::default(), ());

        #[track_caller]
        fn assert_depth(rt: &SpaceRaytracer<()>, expected_depth: f64, ray: Ray, label: &str) {
            let mut depth_buf = DepthBuf::default();
            rt.trace_ray(ray, &mut depth_buf, false);
            assert_eq!(depth_buf.depth, expected_depth, "{label}");
        }

        assert_depth(
            rt,
            0.0,
            Ray::new([0.25, 0.25, 0.], [0., 0., 1.]),
            "immediate collision",
        );

        let d = 0.25;
        assert_depth(
            rt,
            d,
            Ray::new([0.25, 0.25, -d], [0., 0., 1.]),
            "some distance back",
        );

        let scale = 4.0;
        assert_depth(
            rt,
            d / scale,
            Ray::new([0.25, 0.25, -d], [0., 0., scale]),
            "ray length scales results",
        );

        let starting_y = 5.25;
        assert_depth(
            rt,
            starting_y - 0.5,
            Ray::new([0.5, starting_y, 0.5], [0.0, -1.0, 0.0]),
            "ray onto top of slab",
        );

        assert_depth(
            rt,
            f64::INFINITY,
            Ray::new([0.5, 0.75, -0.5], [0.0, 0.0, 1.0]),
            "miss slab over the top",
        );
    }
}
