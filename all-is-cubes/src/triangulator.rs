// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Algorithms for converting blocks/voxels to triangle-based rendering
//! (as opposed to raytracing, voxel display hardware, or whatever else).
//!
//! All of the algorithms here are independent of graphics API but may presume that
//! one exists and has specific data types to specialize in.

use cgmath::{EuclideanSpace as _, Point3, Transform as _, Vector3};

use crate::block::{Block};
use crate::math::{Face, FaceMap, FreeCoordinate, RGBA};
use crate::space::{Space};

/// Generic structure of output from triangulator. Implement `GfxVertex`
/// to provide a specialized version.
#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct BlockVertex {
    pub position: Point3<FreeCoordinate>,
    pub normal: Vector3<FreeCoordinate>,
    // TODO: Eventually color will be replaced with texture coordinates.
    pub color: RGBA,
}

pub trait GfxVertex: From<BlockVertex> + Clone + Sized {
    fn translate(&mut self, offset: Vector3<FreeCoordinate>);
}

impl GfxVertex for BlockVertex {
    fn translate(&mut self, offset: Vector3<FreeCoordinate>) {
        self.position += offset;
    }
}

/// Describes how to draw one `Face` of a `Block`.
#[derive(Clone, Debug)]
struct FaceRenderData<V: GfxVertex> {
    /// Vertices of triangles (i.e. length is a multiple of 3) in counterclockwise order.
    vertices: Box<[V]>,
    fully_opaque: bool,
}

impl<V: GfxVertex> Default for FaceRenderData<V> {
    fn default() -> Self {
        FaceRenderData {
            vertices: Box::new([]),
            fully_opaque: false,
        }
    }
}

/// Describes how to draw a block, as broken down by face.
pub struct BlockRenderData<V: GfxVertex> {
    ///The value for `Face::WITHIN` is the parts which do not touch the cube faces.
    faces: FaceMap<FaceRenderData<V>>,
}

impl<V: GfxVertex> Default for BlockRenderData<V> {
    fn default() -> Self {
        Self {
            faces: FaceMap::generate(|_| FaceRenderData::default())
        }
    }
}

/// Collection of `BlockRenderData` indexed by a `Space`'s block indices.
pub type BlocksRenderData<V> = Box<[BlockRenderData<V>]>;

/// Generate `BlockRenderData` for a block.
fn triangulate_block<V: GfxVertex>(block :&Block) -> BlockRenderData<V> {
    let faces = FaceMap::generate(|face| {
        if face == Face::WITHIN {
            // Until we have blocks with interior detail, nothing to do here.
            return FaceRenderData::default();
        }

        let mut face_vertices: Vec<V> = Vec::new();
        let transform = face.matrix();
        let color = block.color();
        let fully_opaque = color.binary_opaque();

        // TODO: Port over pseudo-transparency mechanism, then change this to a
        // within-epsilon-of-zero test. ...conditional on `GfxVertex` specifying support.
        if fully_opaque {
            let mut push_1 = |p: Point3<FreeCoordinate>| {
                face_vertices.push(V::from(BlockVertex {
                    position: transform.transform_point(p),
                    normal: face.normal_vector(),
                    color,
                }));
            };

            // Two-triangle quad.
            // Note that looked at from a X-right Y-up view, these triangles are
            // clockwise, but they're properly counterclockwise from the perspective
            // that we're drawing the face _facing towards negative Z_ (into the screen).
            //
            // TODO: We can save CPU/memory/bandwidth by using a tessellation shader
            // to generate all six vertices from just one, right?
            push_1(Point3::new(0.0, 0.0, 0.0));
            push_1(Point3::new(0.0, 1.0, 0.0));
            push_1(Point3::new(1.0, 0.0, 0.0));
            push_1(Point3::new(1.0, 0.0, 0.0));
            push_1(Point3::new(0.0, 1.0, 0.0));
            push_1(Point3::new(1.0, 1.0, 0.0));
        }

        FaceRenderData {
            vertices: face_vertices.into_boxed_slice(),
            fully_opaque,
        }
    });

    BlockRenderData {
        faces
    }
}

/// Precomputes vertices for blocks present in a space.
///
/// The resulting `Vec` is indexed by the `Space`'s internal unstable IDs.
pub fn triangulate_blocks<V: GfxVertex>(space: &Space) -> BlocksRenderData<V> {
    space.distinct_blocks_unfiltered().iter().map(triangulate_block).collect()
}

/// Computes a triangle-based representation of a `Space` for rasterization.
///
/// `blocks_render_data` should be provided by `triangulate_blocks` and must be up to
/// date (TODO: provide a means to ensure it is up to date).
pub fn triangulate_space<V: GfxVertex>(space: &Space, blocks_render_data: &BlocksRenderData<V>) -> Vec<V> {
    // TODO: take a Grid parameter for chunked rendering

    let empty_render = BlockRenderData::<V>::default();
    let lookup = |cube| {
        match space.get_block_index(cube) {
            // TODO: On out-of-range, draw an obviously invalid block instead of an invisible one.
            Some(index) => &blocks_render_data.get(index as usize).unwrap_or(&empty_render),
            None => &empty_render,
        }
    };

    let mut space_vertices: Vec<V> = Vec::new();
    for cube in space.grid().interior_iter() {
        let precomputed = lookup(cube);
        let low_corner = cube.cast::<FreeCoordinate>().unwrap();
        // TODO: Tidy this up by implementing an Iterator for FaceMap.
        for &face in Face::all_six() {
            if lookup(cube + face.normal_vector()).faces[face.opposite()].fully_opaque {
                // Don't draw obscured faces
                continue;
            }

            // Copy vertices, offset to the block position.
            for vertex in precomputed.faces[face].vertices.iter() {
                space_vertices.push({
                    let mut v = (*vertex).clone();
                    v.translate(low_corner.to_vec());
                    v
                });
            }
        }
    }
    space_vertices
}

#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::{MetricSpace as _};
    use crate::worldgen::make_some_blocks;

    #[test]
    fn excludes_interior_faces() {
        let block = make_some_blocks(1).swap_remove(0);
        let mut space = Space::empty_positive(2, 2, 2);
        for cube in space.grid().interior_iter() {
            space.set(cube, &block);
        }

        let rendering :Vec<BlockVertex> = triangulate_space(&space, &triangulate_blocks(&space));
        assert_eq!(
            Vec::<&BlockVertex>::new(),
            rendering.iter()
                .filter(|vertex|
                        vertex.position.distance2(Point3::new(1.0, 1.0, 1.0)) < 0.99)
                .collect::<Vec<&BlockVertex>>(),
            "found an interior point");
        assert_eq!(rendering.len(),
            6 /* vertices per face */
            * 4 /* block faces per exterior side of space */
            * 6 /* sides of space */,
            "wrong number of faces");
    }

    #[test]
    fn no_panic_on_missing_blocks() {
        let block = make_some_blocks(1).swap_remove(0);
        let mut space = Space::empty_positive(2, 1, 1);
        let blocks_render_data = triangulate_blocks(&space);
        assert_eq!(blocks_render_data.len(), 1);  // check our assumption

        // This should not panic; visual glitches are preferable to failure.
        space.set((0, 0, 0), &block);  // render data does not know about this
        let _ :Vec<BlockVertex> = triangulate_space(&space, &blocks_render_data);
    }

    // TODO: more tests
}
