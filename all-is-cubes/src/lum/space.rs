// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Get from [`Space`] to [`Tess`].

use cgmath::{EuclideanSpace as _, Matrix as _, Matrix4, Point3, SquareMatrix as _, Vector3};
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::{Pipeline, PipelineError};
use luminance_front::tess::{Interleaved, Mode, Tess, VerticesMut};
use luminance_front::tess_gate::TessGate;
use luminance_front::Backend;
use std::cell::RefCell;
use std::collections::{hash_map::Entry::Vacant, HashMap, HashSet};
use std::rc::{Rc, Weak};

use crate::chunking::{
    cube_to_chunk, point_to_chunk, ChunkChart, ChunkPos, CHUNK_SIZE, CHUNK_SIZE_FREE,
};
use crate::lum::block_texture::{BlockGLTexture, BlockTexture, BoundBlockTexture, GLTile};
use crate::lum::types::{GLBlockVertex, Vertex};
use crate::math::{Face, FaceMap, FreeCoordinate, GridPoint, RGB};
use crate::space::{BlockIndex, Grid, Space, SpaceChange};
use crate::triangulator::{
    triangulate_block, triangulate_blocks, triangulate_space, BlockTriangulation,
};
use crate::universe::{Listener, URef};

/// Manages cached data and GPU resources for drawing a single [`Space`].
pub struct SpaceRenderer {
    space: URef<Space>,
    todo: Rc<RefCell<SpaceRendererTodo>>,
    block_triangulations: Vec<BlockTriangulation<GLBlockVertex, GLTile>>,
    block_texture: Option<BlockGLTexture>,
    chunks: HashMap<ChunkPos, Chunk>,
    chunk_chart: ChunkChart,
}

impl SpaceRenderer {
    /// Constructs a [`SpaceRenderer`] for the given [`Space`].
    ///
    /// Note that the actual geometry for the [`Space`] will be computed over several
    /// frames after construction. There is not currently a specific way to wait for
    /// completion.
    pub fn new(space: URef<Space>) -> Self {
        let space_borrowed = space.borrow_mut();

        let todo = SpaceRendererTodo::default();
        let todo_rc = Rc::new(RefCell::new(todo));
        space_borrowed.listen(TodoListener(Rc::downgrade(&todo_rc)));

        Self {
            space,
            todo: todo_rc,
            block_triangulations: Vec::new(),
            block_texture: None,
            chunks: HashMap::new(),
            // TODO: Use the actual draw distance!
            chunk_chart: ChunkChart::new(200.),
        }
    }

    /// Get the reference to the [`Space`] this draws.
    pub fn space(&self) -> &URef<Space> {
        &self.space
    }

    /// Prepare to draw a frame, performing the steps that must be done while holding a
    /// `&mut C`; the returned [`SpaceRendererOutput`] is then for use within the
    /// luminance pipeline.
    pub fn prepare_frame<'a, C>(
        &'a mut self,
        context: &mut C,
        view_matrix: Matrix4<FreeCoordinate>,
    ) -> SpaceRendererOutput<'a>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let space = &*self
            .space
            .try_borrow()
            .expect("TODO: return a trivial result instead of panic.");

        let block_texture_allocator = self.block_texture.get_or_insert_with(|| {
            // TODO: friendlier error
            BlockGLTexture::new(context).expect("texture setup failure")
        });

        let mut todo = self.todo.borrow_mut();
        
        let mut block_update_count = 0;
        if self.block_triangulations.is_empty() {
            // Must be first run and we haven't prepared the blocks at all.
            // (Or somehow the space has zero blocks in which case this is trivial anyway.)
            self.block_triangulations =
                Vec::from(triangulate_blocks(space, block_texture_allocator));
            block_update_count = self.block_triangulations.len();
        } else if !todo.blocks.is_empty() {
            // Partial update.
            let block_data = space.block_data();

            // Update the vector length to match the space.
            let new_length = block_data.len();
            // (Correctness: cannot overflow because the largest array size in Rust is isize::MAX.
            // And our block indices don't get that big anyway.)
            let delta_length = (new_length as isize) - (self.block_triangulations.len() as isize);
            if delta_length < 0 {
                self.block_triangulations.truncate(new_length);
            } else if delta_length > 0 {
                self.block_triangulations.extend((0..delta_length).map(|_| BlockTriangulation::default()));
            }

            for index in todo.blocks.drain() {
                let index: usize = index.into();
                self.block_triangulations[index] =
                    triangulate_block(block_data[index].evaluated(), block_texture_allocator);
                block_update_count += 1;
            }
        }

        if block_update_count > 0 {
            // TODO: recoverable error
            // TODO: We wouldn't need to flush conditionally if the allocator was smart enough
            // to not copy unmodified tiles.
            let _flush_info = block_texture_allocator
                .flush()
                .expect("texture write failure");
            // TODO propagate flush_info

            // Mark all chunks as needing redrawing.
            // TODO: This is a crude approximation of the precise approach of
            // "recompute the chunks that contain the affected blocks".
            // We would also like to support merely modifying the chunk geometry
            // in-place when the geometry is compatible, but that's a whole
            // new algorithmic challenge.
            todo.chunks.extend(self.chunks.keys());
        }

        // TODO: tested function for this matrix op mess
        // TODO: replace unwrap()s with falling back to drawing nothing or drawing the origin
        let view_point =
            Point3::from_vec(view_matrix.invert().unwrap().transpose().row(3).truncate());
        // TODO: This coordinate tweak should be in ChunkChart since odd/even sizes are its business
        let view_chunk = point_to_chunk(view_point + Vector3::new(0.5, 0.5, 0.5) * CHUNK_SIZE_FREE);

        // Update some chunk geometry.
        let chunk_grid = space.grid().divide(CHUNK_SIZE);
        let mut chunk_update_count = 0;
        for p in self.chunk_chart.chunks(view_chunk) {
            if !chunk_grid.contains_cube(p.0) {
                // Chunk not in the Space
                continue;
            }

            // TODO: tune max update count dynamically?
            if chunk_update_count >= 4 {
                break;
            }

            let chunk_entry = self.chunks.entry(p);
            // If the chunk needs updating or never existed, update it.
            if todo.chunks.remove(&p) || matches!(chunk_entry, Vacant(_)) {
                chunk_entry.or_insert_with(|| Chunk::new(p)).update(
                    context,
                    &space,
                    &self.block_triangulations,
                );
                chunk_update_count += 1;
            }
        }

        // TODO: flush todo.chunks and self.chunks of out-of-range chunks.

        SpaceRendererOutput {
            sky_color: space.sky_color(),
            block_texture: &mut block_texture_allocator.texture,
            view_matrix,
            chunks: &self.chunks, // TODO visibility culling, and don't allocate every frame
            chunk_chart: &self.chunk_chart,
            view_chunk,
            info: SpaceRenderInfo {
                chunk_queue_count: todo.chunks.len(),
                chunk_update_count,
                block_update_count,
                chunks_drawn: 0,
                squares_drawn: 0, // filled later
            },
        }
    }
}

/// Ingredients to actually draw the [`Space`] inside a luminance pipeline, produced by
/// [`SpaceRenderer::prepare_frame`].
pub struct SpaceRendererOutput<'a> {
    /// Space's sky color, to be used as clear color when drawing a frame (if applicable).
    pub sky_color: RGB,

    block_texture: &'a mut BlockTexture,
    view_matrix: Matrix4<FreeCoordinate>,
    /// Chunks are handy wrappers around some Tesses
    chunks: &'a HashMap<ChunkPos, Chunk>,
    chunk_chart: &'a ChunkChart,
    view_chunk: ChunkPos,
    info: SpaceRenderInfo,
}
/// As [`SpaceRendererOutput`], but past the texture-binding stage of the pipeline.
pub struct SpaceRendererBound<'a> {
    /// Block texture to pass to the shader.
    pub bound_block_texture: BoundBlockTexture<'a>,
    /// View matrix to pass to the shader.
    pub view_matrix: Matrix4<FreeCoordinate>,
    chunks: &'a HashMap<ChunkPos, Chunk>,
    chunk_chart: &'a ChunkChart,
    view_chunk: ChunkPos,
    info: SpaceRenderInfo,
}

impl<'a> SpaceRendererOutput<'a> {
    /// Bind texture, in preparation for using the
    /// [`ShadingGate`](luminance_front::shading_gate::ShadingGate).
    pub fn bind(self, pipeline: &'a Pipeline<'a>) -> Result<SpaceRendererBound<'a>, PipelineError> {
        Ok(SpaceRendererBound {
            bound_block_texture: pipeline.bind_texture(self.block_texture)?,
            view_matrix: self.view_matrix,
            chunks: self.chunks,
            chunk_chart: self.chunk_chart,
            view_chunk: self.view_chunk,
            info: self.info,
        })
    }
}
impl<'a> SpaceRendererBound<'a> {
    /// Use a [`TessGate`] to actually draw the space.
    pub fn render<E>(&self, tess_gate: &mut TessGate) -> Result<SpaceRenderInfo, E> {
        let mut chunks_drawn = 0;
        let mut squares_drawn = 0;
        for p in self.chunk_chart.chunks(self.view_chunk) {
            if let Some(chunk) = self.chunks.get(&p) {
                chunks_drawn += 1;
                squares_drawn += chunk.render(tess_gate)?;
            }
            // TODO: If the chunk is missing, draw a blocking shape, possibly?
        }
        Ok(SpaceRenderInfo {
            chunks_drawn,
            squares_drawn,
            ..self.info
        })
    }
}

/// Performance info from a [`SpaceRenderer`] drawing one frame.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct SpaceRenderInfo {
    /// How many chunks need to be recomputed (redrawn) but are still waiting in queue.
    pub chunk_queue_count: usize,
    /// How many chunks were recomputed this time.
    pub chunk_update_count: usize,
    /// How many block triangulations were recomputed this time.
    pub block_update_count: usize,
    pub chunks_drawn: usize,
    /// How many squares (quadrilaterals; sets of 2 triangles = 6 vertices) were used
    /// to draw this frame.
    pub squares_drawn: usize,
}

/// Storage for rendering of part of a [`Space`].
pub struct Chunk {
    bounds: Grid,
    /// Vertices grouped by the direction they face
    vertices: FaceMap<Vec<Vertex>>,
    tesses: FaceMap<Option<Tess<Vertex>>>,
    tiles: Vec<GLTile>,
}

impl Chunk {
    fn new(chunk_pos: ChunkPos) -> Self {
        Chunk {
            bounds: chunk_pos.grid(),
            vertices: FaceMap::default(),
            tesses: FaceMap::default(),
            tiles: Vec::new(),
        }
    }

    fn update<C: GraphicsContext<Backend = Backend>>(
        &mut self,
        context: &mut C,
        space: &Space,
        block_triangulations: &[BlockTriangulation<GLBlockVertex, GLTile>],
    ) {
        triangulate_space(space, self.bounds, block_triangulations, &mut self.vertices);

        // Stash all the texture tiles so they aren't deallocated out from under us.
        // TODO: Maybe we should have something more like a Vec<Rc<BlockTriangulation>>
        self.tiles = block_triangulations
            .iter()
            .flat_map(|bt| bt.textures().iter())
            .cloned()
            .collect();

        for &face in Face::ALL_SEVEN {
            let tess_option = &mut self.tesses[face];
            let new_vertices: &[Vertex] = self.vertices[face].as_ref();

            if tess_option.as_ref().map(|tess| tess.vert_nb()) != Some(new_vertices.len()) {
                // Existing buffer, if any, is not the right length. Discard it.
                *tess_option = None;
            }

            // TODO: replace unwrap()s with an error logging/flagging mechanism
            if new_vertices.is_empty() {
                // Render zero vertices by not rendering anything.
                *tess_option = None;
            } else if let Some(tess) = tess_option.as_mut() {
                // We already have a buffer, and it is a matching length.
                // TODO: Generalize this to be able to shrink buffers via degenerate triangles.
                let mut buffer_slice: VerticesMut<Vertex, (), (), Interleaved, Vertex> = tess
                    .vertices_mut()
                    .expect("failed to map vertices for copying");
                buffer_slice.copy_from_slice(new_vertices);
            } else {
                // Allocate and populate new buffer.
                *tess_option = Some(
                    context
                        .new_tess()
                        .set_vertices(self.vertices[face].clone())
                        .set_mode(Mode::Triangle)
                        .build()
                        .unwrap(),
                );
            }
        }
    }

    fn render<E>(&self, tess_gate: &mut TessGate) -> Result<usize, E> {
        let mut count = 0;
        for &face in Face::ALL_SEVEN {
            if let Some(tess) = self.tesses[face].as_ref() {
                count += tess.vert_nb() / 6;
                tess_gate.render(tess)?;
            }
        }
        Ok(count)
    }
}

/// [`SpaceRenderer`]'s set of things that need recomputing.
#[derive(Default)]
struct SpaceRendererTodo {
    blocks: HashSet<BlockIndex>,
    chunks: HashSet<ChunkPos>,
}

impl SpaceRendererTodo {
    fn insert_block_and_adjacent(&mut self, cube: GridPoint) {
        // Mark adjacent blocks to account for opaque faces hiding adjacent
        // blocks' faces. We don't need to bother with the current block since
        // the adjacent chunks will always include it (presuming that the chunk
        // size is greater than 1).
        for axis in 0..3 {
            for offset in &[-1, 1] {
                let mut adjacent = cube;
                adjacent[axis] += offset;
                self.chunks.insert(cube_to_chunk(adjacent));
            }
        }
    }
}

/// [`Listener`] adapter for [`SpaceRendererTodo`].
struct TodoListener(Weak<RefCell<SpaceRendererTodo>>);

impl Listener<SpaceChange> for TodoListener {
    fn receive(&self, message: SpaceChange) {
        if let Some(cell) = self.0.upgrade() {
            let mut todo = cell.borrow_mut();
            match message {
                SpaceChange::Block(p) => {
                    todo.insert_block_and_adjacent(p);
                }
                SpaceChange::Lighting(p) => {
                    todo.insert_block_and_adjacent(p);
                }
                SpaceChange::Number(index) => {
                    todo.blocks.insert(index);
                }
                SpaceChange::BlockValue(index) => {
                    todo.blocks.insert(index);
                }
            }
        }
    }

    fn alive(&self) -> bool {
        self.0.strong_count() > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: Arrange, somehow, to test the parts that need a GraphicsContext

    #[test]
    fn update_adjacent_chunk_positive() {
        let todo: Rc<RefCell<SpaceRendererTodo>> = Default::default();
        let listener = TodoListener(Rc::downgrade(&todo));
        listener.receive(SpaceChange::Block(GridPoint::new(
            CHUNK_SIZE - 1,
            CHUNK_SIZE / 2,
            CHUNK_SIZE / 2,
        )));
        assert_eq!(
            todo.borrow().chunks,
            vec![ChunkPos::new(0, 0, 0), ChunkPos::new(1, 0, 0)]
                .into_iter()
                .collect::<HashSet<_>>(),
        );
    }

    #[test]
    fn update_adjacent_chunk_negative() {
        let todo: Rc<RefCell<SpaceRendererTodo>> = Default::default();
        let listener = TodoListener(Rc::downgrade(&todo));
        listener.receive(SpaceChange::Block(GridPoint::new(
            0,
            CHUNK_SIZE / 2,
            CHUNK_SIZE / 2,
        )));
        assert_eq!(
            todo.borrow().chunks,
            vec![ChunkPos::new(0, 0, 0), ChunkPos::new(-1, 0, 0)]
                .into_iter()
                .collect::<HashSet<_>>(),
        );
    }
}
