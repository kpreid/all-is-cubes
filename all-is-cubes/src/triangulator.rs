// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Algorithms for converting blocks/voxels to triangle-based rendering
//! (as opposed to raytracing, voxel display hardware, or whatever else).
//!
//! All of the algorithms here are independent of graphics API but may presume that
//! one exists and has specific data types to specialize in.
//!
//! Note: Some sources say that “tesselation” would be a better name for this
//! operation than “triangulation”. However, “tesselation” means a specific
//! other operation in OpenGL graphics programming, and “triangulation” seems to
//! be the more commonly used terms.

use cgmath::{EuclideanSpace as _, Point3, Transform as _, Vector3};

use crate::block::{Block};
use crate::math::{Face, FaceMap, FreeCoordinate, RGBA};
use crate::lighting::PackedLight;
use crate::space::{Space};
use crate::util::{ConciseDebug as _};

pub type TextureCoordinate = f32;

/// Generic structure of output from triangulator. Implement `GfxVertex`
/// to provide a specialized version.
#[derive(Clone, Copy, PartialEq)]
#[non_exhaustive]
pub struct BlockVertex {
    pub position: Point3<FreeCoordinate>,
    pub normal: Vector3<FreeCoordinate>,  // TODO: Use a smaller number type? Storage vs convenience?
    // TODO: Eventually color will be fully replaced with texture coordinates.
    pub color: RGBA,
    pub tex: Vector3<TextureCoordinate>,
}

impl std::fmt::Debug for BlockVertex {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        // Print compactly on single line even if the formatter is in prettyprint mode.
        write!(fmt, "{{ p: {:?} n: {:?} c: {:?} t: {:?} }}",
            self.position.as_concise_debug(),
            self.normal.cast::<i8>().unwrap().as_concise_debug(),  // no decimals!
            self.color,
            self.tex.as_concise_debug())
    }
}

/// Implement this trait along with `From<BlockVertex>` to provide a representation
/// of `BlockVertex` suitable for the target graphics system.
pub trait ToGfxVertex<GV>: From<BlockVertex> + Sized {
    fn instantiate(&self, offset: Vector3<FreeCoordinate>, lighting: PackedLight) -> GV;
}

/// Trivial implementation for testing purposes. Discards lighting.
impl ToGfxVertex<BlockVertex> for BlockVertex {
    fn instantiate(&self, offset: Vector3<FreeCoordinate>, _lighting: PackedLight) -> Self {
        Self {
            position: self.position + offset,
            ..*self
        }
    }
}

/// Describes how to draw one `Face` of a `Block`.
#[derive(Clone, Debug, PartialEq, Eq)]
struct FaceRenderData<V: From<BlockVertex>> {
    /// Vertices of triangles (i.e. length is a multiple of 3) in counterclockwise order.
    vertices: Box<[V]>,
    /// Whether the block entirely fills its cube, such that nothing can be seen through
    /// it and faces of adjacent blocks may be removed.
    fully_opaque: bool,
}

impl<V: From<BlockVertex>> Default for FaceRenderData<V> {
    fn default() -> Self {
        FaceRenderData {
            vertices: Box::new([]),
            fully_opaque: false,
        }
    }
}

/// Describes how to draw a block. Pass it to `triangulate_space` to use it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockRenderData<V: From<BlockVertex>> {
    /// Vertices grouped by the face they belong to.
    /// 
    /// All triangles which are on the surface of the cube (such that they may be omitted
    /// when a `fully_opaque` block is adjacent) are grouped under the corresponding
    /// face, and all other triangles are grouped under `Face::WITHIN`.
    faces: FaceMap<FaceRenderData<V>>,
}

impl<V: From<BlockVertex>> Default for BlockRenderData<V> {
    fn default() -> Self {
        Self {
            faces: FaceMap::generate(|_| FaceRenderData::default())
        }
    }
}

/// Collection of `BlockRenderData` indexed by a `Space`'s block indices.
/// Pass it to `triangulate_space` to use it.
pub type BlocksRenderData<V> = Box<[BlockRenderData<V>]>;

fn push_quad<V: From<BlockVertex>>(vertices: &mut Vec<V>, face: Face, color: RGBA) {
    let transform = face.matrix();
    let mut push_1 = |p: Point3<FreeCoordinate>| {
        vertices.push(V::from(BlockVertex {
            position: transform.transform_point(p),
            normal: face.normal_vector(),
            color: color,
            tex: Vector3::new(p.x as TextureCoordinate, p.y as TextureCoordinate, 0.0),
        }));
    };
    
    // Two-triangle quad.
    // Note that looked at from a X-right Y-up view, these triangles are
    // clockwise, but they're properly counterclockwise from the perspective
    // that we're drawing the face _facing towards negative Z_ (into the screen),
    // which is how cube faces as implicitly defined by Face::matrix work.
    push_1(Point3::new(0.0, 0.0, 0.0));
    push_1(Point3::new(0.0, 1.0, 0.0));
    push_1(Point3::new(1.0, 0.0, 0.0));
    push_1(Point3::new(1.0, 0.0, 0.0));
    push_1(Point3::new(0.0, 1.0, 0.0));
    push_1(Point3::new(1.0, 1.0, 0.0));
}

/// Generate `BlockRenderData` for a block.
fn triangulate_block<V: From<BlockVertex>>(block :&Block) -> BlockRenderData<V> {
    match block {
        Block::Atom(_attributes, color) => {
            let faces = FaceMap::generate(|face| {
                if face == Face::WITHIN {
                    // No interior detail for atom blocks.
                    return FaceRenderData::default();
                }

                let mut face_vertices: Vec<V> = Vec::new();
                let fully_opaque = color.binary_opaque();

                // TODO: Port over pseudo-transparency mechanism, then change this to a
                // within-epsilon-of-zero test. ...conditional on `GfxVertex` specifying support.
                if fully_opaque {
                    push_quad(&mut face_vertices, face, *color);
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
        Block::Recur(_attributes, space_ref) => {
            // TODO: Recursive triangulation is a bad strategy; use texturing instead, at least
            // for rectangular areas.
            let mut space_vertices: FaceMap<Vec<BlockVertex>> = FaceMap::generate(|_| Vec::new());
            let space = &*space_ref.borrow();
            triangulate_space(space, &triangulate_blocks::<BlockVertex>(space), &mut space_vertices);

            // TODO: do error-checking on the transformation (what if the space is not cubical, or
            // not in the positive octant).
            let scale = 1.0 / (space.grid().size().x as FreeCoordinate);

            // Transform vertices.
            let faces = FaceMap::generate(|face| {
                FaceRenderData {
                    vertices: space_vertices[face].iter().map(|v| V::from(BlockVertex {
                        position: v.position * scale,
                        ..*v
                    })).collect(),
                    fully_opaque: true,  // TODO: actually calculate this
                }
            });
            BlockRenderData { faces }
        }
    }
}

/// Precomputes vertices for blocks present in a space.
///
/// The resulting `Vec` is indexed by the `Space`'s internal unstable IDs.
pub fn triangulate_blocks<V: From<BlockVertex>>(space: &Space) -> BlocksRenderData<V> {
    space.distinct_blocks_unfiltered().iter().map(triangulate_block).collect()
}

/// Allocate an output buffer for `triangulate_space`.
pub fn new_space_buffer<V>() -> FaceMap<Vec<V>> {
    FaceMap::generate(|_| Vec::new())
}

/// Computes a triangle-based representation of a `Space` for rasterization.
///
/// `blocks_render_data` should be provided by `triangulate_blocks` and must be up to
/// date (TODO: provide a means to ensure it is up to date).
///
/// The triangles will be written into `output_vertices`, replacing the existing
/// contents. This is intended to avoid memory reallocation in the common case of
/// new geometry being similar to old geometry.
///
/// `output_vertices` is a `FaceMap` dividing the faces according to their normal
/// vectors.
pub fn triangulate_space<BV, GV>(
    space: &Space,
    blocks_render_data: &BlocksRenderData<BV>,
    output_vertices: &mut FaceMap<Vec<GV>>,
) where
    BV: ToGfxVertex<GV>
{
    // TODO: take a Grid parameter for chunked rendering

    let empty_render = BlockRenderData::<BV>::default();
    let lookup = |cube| {
        match space.get_block_index(cube) {
            // TODO: On out-of-range, draw an obviously invalid block instead of an invisible one.
            Some(index) => &blocks_render_data.get(index as usize).unwrap_or(&empty_render),
            None => &empty_render,
        }
    };

    for &face in Face::ALL_SEVEN.iter() {
        // use the buffer but not the existing data
        output_vertices[face].clear();
    }
    for cube in space.grid().interior_iter() {
        let precomputed = lookup(cube);
        let low_corner = cube.cast::<FreeCoordinate>().unwrap();
        for &face in Face::ALL_SEVEN {
            let adjacent_cube = cube + face.normal_vector();
            if lookup(adjacent_cube).faces[face.opposite()].fully_opaque {
                // Don't draw obscured faces
                continue;
            }

            let lighting = space.get_lighting(adjacent_cube);

            // Copy vertices, offset to the block position and with lighting
            for vertex in precomputed.faces[face].vertices.iter() {
                output_vertices[face].push(
                    vertex.instantiate(low_corner.to_vec(), lighting));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cgmath::{MetricSpace as _};
    use crate::block::BlockAttributes;
    use crate::blockgen::make_some_blocks;
    use crate::universe::{Universe};

    #[test]
    fn excludes_interior_faces() {
        let block = make_some_blocks(1).swap_remove(0);
        let mut space = Space::empty_positive(2, 2, 2);
        for cube in space.grid().interior_iter() {
            space.set(cube, &block);
        }

        let mut rendering = new_space_buffer();
        triangulate_space::<BlockVertex, BlockVertex>(&space, &triangulate_blocks(&space), &mut rendering);
        let rendering_flattened: Vec<BlockVertex> = rendering.values().iter().flat_map(|r| (*r).clone()).collect();
        assert_eq!(
            Vec::<&BlockVertex>::new(),
            rendering_flattened.iter()
                .filter(|vertex|
                        vertex.position.distance2(Point3::new(1.0, 1.0, 1.0)) < 0.99)
                .collect::<Vec<&BlockVertex>>(),
            "found an interior point");
        assert_eq!(rendering_flattened.len(),
            6 /* vertices per face */
            * 4 /* block faces per exterior side of space */
            * 6 /* sides of space */,
            "wrong number of faces");
    }

    #[test]
    fn no_panic_on_missing_blocks() {
        let block = make_some_blocks(1).swap_remove(0);
        let mut space = Space::empty_positive(2, 1, 1);
        let blocks_render_data: BlocksRenderData<BlockVertex> = triangulate_blocks(&space);
        assert_eq!(blocks_render_data.len(), 1);  // check our assumption

        // This should not panic; visual glitches are preferable to failure.
        space.set((0, 0, 0), &block);  // render data does not know about this
        triangulate_space(&space, &blocks_render_data, &mut new_space_buffer());
    }

    #[test]
    fn trivial_subcube_rendering() {
        let mut u = Universe::new();
        let mut inner_block_space = Space::empty_positive(1, 1, 1);
        inner_block_space.set((0, 0, 0), &make_some_blocks(1)[0]);
        let inner_block = Block::Recur(BlockAttributes::default(), u.insert_anonymous(inner_block_space));
        let mut outer_space = Space::empty_positive(1, 1, 1);
        outer_space.set((0, 0, 0), &inner_block);

        let blocks_render_data: BlocksRenderData<BlockVertex> = triangulate_blocks(&outer_space);
        let block_render_data: BlockRenderData<BlockVertex> = blocks_render_data[0].clone();
        
        eprintln!("{:#?}", blocks_render_data);
        let mut space_rendered = new_space_buffer();
        triangulate_space(&outer_space, &blocks_render_data, &mut space_rendered);
        eprintln!("{:#?}", space_rendered);

        assert_eq!(space_rendered, block_render_data.faces.map(|_, frd| frd.vertices.to_vec()));
    }

    // TODO: more tests
}
