use alloc::boxed::Box;

use rand::{RngExt, SeedableRng as _};

use all_is_cubes::block::Resolution;
use all_is_cubes::math::{Cube, GridAab, GridPoint, Vol};

/// Generate a [`Block`]-shape of random values.
//---
// (Strictly speaking, this isn't noise, but it is used in places where we used to abuse
// value noise.)
#[inline(never)]
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn array_of_random<T, U, R>(
    resolution: Resolution,
    seed: u64,
    range: R,
    mut postprocess: impl FnMut(T) -> U,
) -> Vol<Box<[U]>>
where
    T: rand::distr::uniform::SampleUniform,
    R: rand::distr::uniform::SampleRange<T> + Clone,
{
    let mut rng = rand_xoshiro::Xoshiro256Plus::seed_from_u64(seed);
    Vol::from_fn(GridAab::for_block(resolution), |_| {
        postprocess(rng.random_range(range.clone()))
    })
}

/// Extension trait for [`noise_functions::Noise`] which makes it usable with our [`Cube`]s.
pub(crate) trait NoiseExt: noise_functions::Noise {
    /// Sample the noise at the center of the given cube. That is, convert the integer
    /// vector to `f64`, add 0.5 to all coordinates, and call [`NoiseFn::get`].
    ///
    /// This offset is appropriate for the most resolution-independent sampling, or
    /// symmetric shapes with even-numbered widths.
    fn at_cube(&self, cube: Cube) -> f32;

    /// As [`NoiseFn::get`], but converting from integer. Unlike [`NoiseFnExt::at_cube`],
    /// does not apply any offset.
    fn at_grid(&self, point: GridPoint) -> f32;
}
impl<T> NoiseExt for T
where
    T: noise_functions::Noise + noise_functions::Sample<3>,
{
    fn at_cube(&self, cube: Cube) -> f32 {
        let point = cube.center().to_f32().to_array();
        noise_functions::Noise::sample3(self, point)
    }

    fn at_grid(&self, point: GridPoint) -> f32 {
        let point = point.to_f32().to_array();
        noise_functions::Noise::sample3(self, point)
    }
}
