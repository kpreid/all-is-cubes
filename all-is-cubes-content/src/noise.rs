// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

use noise::NoiseFn;

use all_is_cubes::block::Resolution;
use all_is_cubes::math::cube_to_midpoint;
use all_is_cubes::space::{Grid, GridArray};

/// Generates a [`Block`]-shape of noise values from a [`NoiseFn`].
///
/// As a convenience, it also accepts a postprocessing function that is allowed to return
/// any type.
///
/// The [`noise`] library currently uses `&dyn NoiseFn` ubiquitously in all combiners and
/// modifiers, which does not implement [`Send`] and so cannot be used inside futures that
/// need to be [`Send`]. As a stopgap before a release that fixes this (or switching to a
/// different noise library), we use this utility function to pre-compute all the noise we
/// want for patterns in a [`Block`].
pub(crate) fn array_of_noise<O>(
    resolution: Resolution,
    noise_fn: &impl NoiseFn<[f64; 3]>,
    mut postprocess: impl FnMut(f64) -> O,
) -> GridArray<O> {
    GridArray::from_fn(Grid::for_block(resolution), |cube| {
        postprocess(noise_fn.get(cube_to_midpoint(cube).into()))
    })
}
