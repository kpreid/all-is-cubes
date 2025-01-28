//! Algorithms used in content generation.
//!
//! They are gathered in this module to encourage noting similarities rather than writing
//! near-duplicate code.

use alloc::boxed::Box;

use hashbrown::HashSet;

use all_is_cubes::block::{AIR, Atom, Block, Primitive, Resolution};
use all_is_cubes::euclid::vec3;
use all_is_cubes::math::{
    Cube, CubeFace, Face6, FaceMap, FreeCoordinate, FreePoint, GridAab, GridCoordinate, GridPoint,
    GridSizeCoord, GridVector, Gridgid, PositiveSign, Vol,
};
use all_is_cubes::space::{CubeTransaction, SetCubeError, Space, SpaceTransaction};

mod noise;
pub(crate) use noise::*;

/// Create a function to define texture in a block, based on a set of points
/// to form a 3D Voronoi diagram, optionally wrapping around the boundaries for
/// seamless tiling.
///
/// The points' coordinates should be in the range 0 to 1.
///
/// TODO: Once we have better composable tools than `impl Fn(Cube)`, allow
/// each point to refer to a pattern of its own to delegate to.
pub(crate) fn voronoi_pattern<'a>(
    resolution: Resolution,
    wrapping: bool,
    // TODO: not a well-founded choice of iterator type, just convenient
    points: impl IntoIterator<Item = &'a (FreePoint, Block)> + Clone,
) -> impl Fn(Cube) -> &'a Block {
    // We use the strategy of flood-filling each point up front, because for
    // large numbers of points that's much cheaper than evaluating every cube
    // against every point. (An alternative would be to build a spatial index
    // of the points, but the only benefit that would give would be supporting
    // using the same precomputation with different resolutions.)
    //
    // Note: This is not strictly correct if we view it as a discrete rendering
    // of continuous space, because some nearly-parallel boundary planes could
    // produce "Moiré patterns" of non-contiguous blocks, which a flood-fill
    // will not. However, this should be good enough for the procedural-generation
    // goals of this function.

    let mut pattern: Vol<Box<[(FreeCoordinate, &Block)]>> =
        Vol::from_fn(GridAab::for_block(resolution), |_| (f64::INFINITY, &AIR));
    let mut flood_fill_todo = HashSet::<Cube>::new();
    for &(region_point, ref block) in points {
        let region_point = region_point * FreeCoordinate::from(resolution);
        let starting_cube: Cube = match Cube::containing(region_point) {
            Some(p) => p,
            None => continue, // TODO: panic? this can only happen when the inputs are not in 0 to 1
        };
        flood_fill_todo.insert(starting_cube);
        while let Some(cube) = flood_fill_todo.iter().next().copied() {
            flood_fill_todo.remove(&cube);
            let cube_wrapped = cube.map(|component| component.rem_euclid(resolution.into()));

            if !wrapping && cube_wrapped != cube {
                // If we are not wrapping, then stop filling when out of bounds.
                continue;
            }

            let test_point: FreePoint = cube.midpoint();

            let offset = test_point - region_point;
            // TODO: add ability to muck with the distance metric in custom ways
            // instead of this hardcoded one.
            let offset = offset.component_mul(vec3(1.0, 2.0, 1.0));
            let distance_squared = offset.square_length();

            if distance_squared < pattern[cube_wrapped].0 {
                pattern[cube_wrapped] = (distance_squared, block);
                for direction in Face6::ALL {
                    // TODO: I tried filtering to
                    //    direction.normal_vector().dot(offset) >= 0.0
                    // which should be an optimization but it changed the results.
                    // Investigate with actual well-defined test cases.
                    let adjacent = cube + direction.normal_vector();
                    // The flood fill can escape the cube bounds here,
                    // but is wrapped around at lookup time (so the distance stays true).
                    flood_fill_todo.insert(adjacent);
                }
            }
        }
    }

    move |cube| pattern[cube.map(|component| component.rem_euclid(resolution.into()))].1
}

/// Given a room's exterior bounding box, act on its four walls.
///
/// The function is given the bottom-left (from an exterior perspective) corner cube
/// of each wall, the rightward direction of the wall, its length (counted such
/// that each wall overlaps its neighbor at the corner), and its bounding box (assuming
/// the wall is one block thick, but _excluding_ the corners).
///
/// TODO: There is probably other worldgen code that should be using this now that we've invented it.
///
/// TODO: Change the callback value to a struct
pub(crate) fn four_walls<F, E>(bounding_box: GridAab, mut f: F) -> Result<(), E>
where
    F: FnMut(GridPoint, Face6, GridSizeCoord, GridAab) -> Result<(), E>,
{
    let interior = bounding_box.shrink(FaceMap::symmetric([1, 0, 1])).unwrap();
    let low = bounding_box.lower_bounds();
    let high = bounding_box.upper_bounds() - GridVector::new(1, 1, 1);
    let size = bounding_box.size();
    f(
        low,
        Face6::PZ,
        size.depth,
        interior.abut(Face6::NX, 1).unwrap(),
    )?;
    f(
        GridPoint::new(low.x, low.y, high.z),
        Face6::PX,
        size.width,
        interior.abut(Face6::PZ, 1).unwrap(),
    )?;
    f(
        GridPoint::new(high.x, low.y, high.z),
        Face6::NZ,
        size.depth,
        interior.abut(Face6::PX, 1).unwrap(),
    )?;
    f(
        GridPoint::new(high.x, low.y, low.z),
        Face6::NX,
        size.width,
        interior.abut(Face6::NZ, 1).unwrap(),
    )?;
    Ok(())
}

// TODO: this should probably be in main all-is-cubes crate
pub(crate) fn space_to_space_copy(
    src: &Space,
    src_bounds: GridAab,
    dst: &mut Space,
    src_to_dst_transform: Gridgid,
) -> Result<(), SetCubeError> {
    let dst_to_src_transform = src_to_dst_transform.inverse();
    let block_rotation = src_to_dst_transform.rotation;
    dst.fill(src_bounds.transform(src_to_dst_transform).unwrap(), |p| {
        Some(
            src[dst_to_src_transform.transform_cube(p)]
                .clone()
                .rotate(block_rotation),
        )
    })
}

/// As [`space_to_space_copy`], but producing a transaction.
#[expect(dead_code)] // TODO: currently unused but will probably come up again...?
pub(crate) fn space_to_transaction_copy(
    src: &Space,
    src_bounds: GridAab,
    src_to_dst_transform: Gridgid,
) -> SpaceTransaction {
    let dst_to_src_transform = src_to_dst_transform.inverse();
    let block_rotation = src_to_dst_transform.rotation;
    SpaceTransaction::filling(
        src_bounds.transform(src_to_dst_transform).unwrap(),
        |cube| {
            CubeTransaction::replacing(
                None, // TODO: provide control over what the old-values are
                Some(
                    src[dst_to_src_transform.transform_cube(cube)]
                        .clone()
                        .rotate(block_rotation),
                ),
            )
        },
    )
}

/// Generate a copy of a [`Primitive::Atom`] block with its color scaled by the given scalar.
///
/// The scalar is rounded to steps of `quantization`, to reduce the number of distinct
/// block types generated.
///
/// If the computation is NaN or the block is not an atom, it is returned unchanged.
pub(crate) fn scale_color(mut block: Block, scalar: f64, quantization: f64) -> Block {
    let scalar = (scalar / quantization).round() * quantization;
    match (
        block.primitive_mut(),
        PositiveSign::<f32>::try_from(scalar as f32),
    ) {
        (Primitive::Atom(Atom { color, .. }), Ok(scalar)) => {
            *color = (color.to_rgb() * scalar).with_alpha(color.alpha());
        }
        _ => {}
    }
    block
}

/// Subdivide the range 0.0 to 1.0 into `gradient.len()` parts and return the [`Block`]
/// which the value falls into.
///
/// Panics if `gradient.len() == 0`.
pub(crate) fn gradient_lookup(gradient: &[Block], value: f32) -> &Block {
    &gradient[((value * gradient.len() as f32) as usize).clamp(0, gradient.len() - 1)]
}

/// Compute the cube's distance from the midpoint of the Y axis of the block volume.
///
/// The centermost 4 cubes that exist in every resolution above 1 all have a distance of 1.
/// (No cube ever has a distance of 0, so 0 can be used in a comparison for “never”.)
///
/// The first returned number is the "radius" value and the second is the distance
/// on the lesser axis, which may be used for distance from the center or corner along
/// the surface.
pub(crate) fn square_radius(resolution: Resolution, cube: Cube) -> [GridCoordinate; 2] {
    let distances_vec = cube
        .lower_bounds()
        .map(|c| (c * 2 + 1 - GridCoordinate::from(resolution)).abs() / 2 + 1);
    if distances_vec.x > distances_vec.z {
        [distances_vec.x, distances_vec.z]
    } else {
        [distances_vec.z, distances_vec.x]
    }
}

/// Returns a path of single-cube steps from `start` to `end`.
///
/// TODO: If useful, allow specifying style of traversal — which axis first, or longest
/// axis first, or evenly distributed (Bresenham).
pub(crate) fn walk(start: Cube, end: Cube) -> impl Iterator<Item = CubeFace> + Clone {
    use Face6::*;
    use itertools::repeat_n;
    let delta = end - start;
    let dists = delta.abs().cast::<usize>();

    let x_steps = repeat_n(if delta.x > 0 { PX } else { NX }, dists.x);
    let y_steps = repeat_n(if delta.y > 0 { PY } else { NY }, dists.y);
    let z_steps = repeat_n(if delta.z > 0 { PZ } else { NZ }, dists.z);
    let steps = y_steps.chain(x_steps).chain(z_steps);

    #[derive(Clone, Debug)]
    struct CubeIter<I> {
        start: Cube,
        // end: Cube,
        inner: I,
    }
    impl<I: Iterator<Item = Face6>> Iterator for CubeIter<I> {
        type Item = CubeFace;

        fn next(&mut self) -> Option<Self::Item> {
            let face = self.inner.next()?;
            let step = CubeFace {
                cube: self.start,
                face: face.into(),
            };
            self.start += face.normal_vector();
            Some(step)
        }
    }

    CubeIter {
        start,
        //  end,
        inner: steps,
    }
}

/// Place a series of blocks on top of each other, starting at the specified point.
///
/// TODO: think about whether this should be instead returning a `VoxelBrush` or a `SpaceTransaction` or something, for the future of composable worldgen
pub(crate) fn stack<'b, B>(
    space: &mut Space,
    origin: impl Into<Cube>,
    blocks: impl IntoIterator<Item = B>,
) -> Result<(), SetCubeError>
where
    B: Into<alloc::borrow::Cow<'b, Block>>,
{
    let origin = origin.into();
    for (y, block) in (0..).zip(blocks) {
        space.set(origin + GridVector::new(0, y, 0), block)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::make_some_blocks;
    use Resolution::*;
    use all_is_cubes::math::Face7;
    use itertools::Itertools as _;

    #[test]
    fn gradient_lookup_cases() {
        let blocks = make_some_blocks::<4>();
        let inputs_and_output_indices = [
            (-f32::INFINITY, 0),
            (-10.0, 0),
            (-0.1, 0),
            (0.0, 0),
            (0.24, 0),
            (0.25, 1),
            (0.26, 1),
            (0.49, 1),
            (0.50, 2),
            (0.51, 2),
            (0.9, 3),
            (1.0, 3),
            (1.1, 3),
            (10.0, 3),
            (f32::INFINITY, 3),
            (f32::NAN, 0),
        ];
        assert_eq!(
            inputs_and_output_indices.map(|(i, _)| gradient_lookup(&blocks, i)),
            inputs_and_output_indices.map(|(_, o)| &blocks[o]),
        );
    }

    #[test]
    fn square_radius_cases() {
        assert_eq!(
            [6, 7, 8, 9].map(|x| square_radius(R16, Cube::new(x, 2, 8))[0]),
            [2, 1, 1, 2]
        );
    }

    #[test]
    fn walk_cases() {
        use Face7::*;
        let start = Cube::new(1, 2, 3);

        assert_eq!(walk(start, start).collect_vec(), vec![]);
        assert_eq!(
            walk(start, Cube::new(1, 3, 3)).collect_vec(),
            vec![CubeFace {
                cube: start,
                face: PY
            }]
        );
        assert_eq!(
            walk(start, Cube::new(1, 1, 3)).collect_vec(),
            vec![CubeFace {
                cube: start,
                face: NY
            }]
        );
        assert_eq!(
            walk(start, Cube::new(0, 4, 0)).collect_vec(),
            vec![
                CubeFace {
                    cube: start,
                    face: PY
                },
                CubeFace {
                    cube: Cube::new(1, 3, 3),
                    face: PY
                },
                CubeFace {
                    cube: Cube::new(1, 4, 3),
                    face: NX
                },
                CubeFace {
                    cube: Cube::new(0, 4, 3),
                    face: NZ
                },
                CubeFace {
                    cube: Cube::new(0, 4, 2),
                    face: NZ
                },
                CubeFace {
                    cube: Cube::new(0, 4, 1),
                    face: NZ
                },
            ]
        );
    }
}
