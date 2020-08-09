// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Algorithms for converting blocks/voxels to triangle-based rendering
//! (as opposed to raytracing, voxel display hardware, or whatever else).
//!
//! All of the algorithms here are independent of graphics API but may presume that
//! one exists and has specific data types to specialize in.

use cgmath::{EuclideanSpace, Point3, Transform as _, Vector3};

use crate::block::{Block, Color};
use crate::math::{Face, FaceMap, FreeCoordinate};
use crate::space::{Space};

/// Trait allowing construction of specific vertex types from input data.
pub trait BlockVertex: Clone + Sized {
    // TODO: Eventually color will be replaced with texture coordinates.
    fn from_block_vertex_parts(position: Vector3<FreeCoordinate>, color: Color) -> Self;
    
    fn translate(&mut self, offset: Vector3<FreeCoordinate>);
}

/// Describes how to draw one `Face` of a `Block`.
#[derive(Clone, Debug)]
struct FaceRenderData<V: BlockVertex> {
    vertices: Box<[V]>,
    fully_opaque: bool,
}

impl<V: BlockVertex> Default for FaceRenderData<V> {
    fn default() -> Self {
        FaceRenderData {
            vertices: Box::new([]),
            fully_opaque: false,
        }
    }
}

/// Describes how to draw a block, as broken down by face.
pub(crate) struct BlockRenderData<V: BlockVertex> {
    ///The value for `Face::WITHIN` is the parts which do not touch the cube faces.
    faces: FaceMap<FaceRenderData<V>>,
}

impl<V: BlockVertex> Default for BlockRenderData<V> {
    fn default() -> Self {
        Self {
            faces: FaceMap::generate(|_| FaceRenderData::default())
        }
    }
}

/// Collection of `BlockRenderData` indexed by a `Space`'s block indices.
pub(crate) type BlocksRenderData<V> = Vec<BlockRenderData<V>>;

/// Generate `BlockRenderData` for a block.
fn triangulate_block<V: BlockVertex>(block :&Block) -> BlockRenderData<V> {
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
        // within-epsilon-of-zero test. ...conditional on `BlockVertex` specifying support.
        if fully_opaque {
            let mut push_1 = |p: Point3<FreeCoordinate>| {
                face_vertices.push(V::from_block_vertex_parts(
                    transform.transform_point(p).to_vec(),
                    color));
            };

            // Two-triangle quad.
            // TODO: We can save CPU/memory/bandwidth by using a tessellation shader
            // to generate all six vertices from just one, right?
            push_1(Point3::new(0.0, 0.0, 0.0));
            push_1(Point3::new(1.0, 0.0, 0.0));
            push_1(Point3::new(0.0, 1.0, 0.0));
            push_1(Point3::new(1.0, 0.0, 0.0));
            push_1(Point3::new(0.0, 1.0, 0.0));
            push_1(Point3::new(1.0, 1.0, 0.0));
        }

        FaceRenderData {
            vertices: face_vertices.into_boxed_slice(),
            fully_opaque: fully_opaque,
        }
    });
    
    BlockRenderData {
        faces
    }
}

/// Precomputes vertices for blocks present in a space.
///
/// The resulting `Vec` is indexed by the `Space`'s internal unstable IDs.
pub(crate) fn triangulate_blocks<V: BlockVertex>(space: &Space) -> BlocksRenderData<V> {
    let mut results: BlocksRenderData<V> = Vec::new();
    for block in space.distinct_blocks_unfiltered() {
        results.push(triangulate_block(block));
    }
    results
}

/// Computes a triangle-based representation of a `Space` for rasterization.
///
/// `blocks_render_data` should be provided by `triangulate_blocks` and must be up to
/// date (TODO: provide a means to ensure it is up to date).
pub(crate) fn triangulate_space<V: BlockVertex>(space: &Space, blocks_render_data: &BlocksRenderData<V>) -> Vec<V> {
    // TODO: take a Grid parameter for chunked rendering

    let empty_render = BlockRenderData::<V>::default();
    let lookup = |cube| {
        match space.get_block_index(cube) {
            Some(index) => &blocks_render_data[index as usize],
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

            let face_vertices = &precomputed.faces[face].vertices;
            // Copy vertices, offset to the block position.
            for vertex in face_vertices.into_iter() {
                let mut positioned_vertex: V = (*vertex).clone();
                positioned_vertex.translate(low_corner.to_vec());
                space_vertices.push(positioned_vertex);
            }
        }
    }
    space_vertices
}


#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Copy, Debug, PartialEq)]
    struct TestVertex {
        position: Point3<FreeCoordinate>,
        color: Color,
    }
    
    // TODO: write tests
}