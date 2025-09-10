use alloc::boxed::Box;

use noise::NoiseFn;

use all_is_cubes::block::Resolution;
use all_is_cubes::math::{Cube, GridAab, GridPoint, Vol};

/// Generates a [`Block`]-shape of noise values from a [`NoiseFn`].
///
/// As a convenience, it also accepts a postprocessing function that is allowed to return
/// any type.
///
/// TODO: The previous rationale, which made this function an improvement on passing
/// around `NoiseFn`s, is obsolete. Review how many of its uses are actually valuable.
pub(crate) fn array_of_noise<O>(
    resolution: Resolution,
    noise_fn: &impl NoiseFn<f64, 3>,
    mut postprocess: impl FnMut(f64) -> O,
) -> Vol<Box<[O]>> {
    Vol::from_fn(GridAab::for_block(resolution), |cube| {
        postprocess(noise_fn.get(cube.center().into()))
    })
}

/// Extension trait for [`noise::NoiseFn`] which makes it usable with our [`Cube`]s.
pub(crate) trait NoiseFnExt: NoiseFn<f64, 3> {
    /// Sample the noise at the center of the given cube. That is, convert the integer
    /// vector to `f64`, add 0.5 to all coordinates, and call [`NoiseFn::get`].
    ///
    /// This offset is appropriate for the most resolution-independent sampling, or
    /// symmetric shapes with even-numbered widths.
    fn at_cube(&self, cube: Cube) -> f64;

    /// As [`NoiseFn::get`], but converting from integer. Unlike [`NoiseFnExt::at_cube`],
    /// does not apply any offset.
    fn at_grid(&self, point: GridPoint) -> f64;
}
impl<T> NoiseFnExt for T
where
    T: NoiseFn<f64, 3> + Sized,
{
    fn at_cube(&self, cube: Cube) -> f64
    where
        Self: Sized,
    {
        let point = cube.center();
        NoiseFn::get(&self, point.into())
    }

    fn at_grid(&self, point: GridPoint) -> f64
    where
        Self: Sized,
    {
        let point = point.map(f64::from);
        NoiseFn::get(&self, point.into())
    }
}
