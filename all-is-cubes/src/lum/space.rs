// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Get from `Space` to `luminance::tess::Tess`.

use cgmath::Matrix4;
use indexmap::IndexSet;
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::{Pipeline, PipelineError};
use luminance_front::tess::{Interleaved, Mode, Tess, VerticesMut};
use luminance_front::tess_gate::TessGate;
use luminance_front::Backend;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::lum::block_texture::{
    BlockGLRenderData, BlockGLTexture, BlockTexture, BoundBlockTexture,
};
use crate::lum::types::{GLBlockVertex, Vertex};
use crate::math::{Face, FaceMap, FreeCoordinate, GridCoordinate, GridPoint};
use crate::space::{Grid, Space, SpaceChange};
use crate::triangulator::{triangulate_space, BlocksRenderData};
use crate::universe::{Listener, URef};

const CHUNK_SIZE: GridCoordinate = 16;

/// Manages cached data and GPU resources for drawing a single `Space`.
pub struct SpaceRenderer {
    space: URef<Space>,
    todo: Rc<RefCell<SpaceRendererTodo>>,
    block_data_cache: Option<BlockGLRenderData>, // TODO: quick hack, needs an invalidation strategy
    /// Indexed by coordinate divided by CHUNK_SIZE
    chunks: HashMap<GridPoint, Chunk>,
}

impl SpaceRenderer {
    pub fn new(space: URef<Space>) -> Self {
        let mut space_borrowed = space.borrow_mut();

        let mut todo = SpaceRendererTodo::default();
        // TODO: Eventually we will want to draw only in-view-range chunks
        for chunk_pos in space_borrowed.grid().divide(CHUNK_SIZE).interior_iter() {
            todo.chunks.insert(chunk_pos);
        }
        let todo_rc = Rc::new(RefCell::new(todo));
        space_borrowed.listen(TodoListener(Rc::downgrade(&todo_rc)));

        Self {
            space,
            todo: todo_rc,
            block_data_cache: None,
            chunks: HashMap::new(),
        }
    }

    pub fn space(&self) -> &URef<Space> {
        &self.space
    }

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

        let mut todo = self.todo.borrow_mut();

        if todo.blocks {
            todo.blocks = false;
            self.block_data_cache = None;
        }
        let block_data = self.block_data_cache.get_or_insert_with(|| {
            let (block_data, _info) =
                BlockGLRenderData::prepare(context, space).expect("texture failure");
            // TODO get a logging strategy and use it for _info
            block_data
        });

        // Update some chunk geometry.
        // TODO: tune max update count
        let mut chunk_update_count = 0;
        while chunk_update_count < 10 {
            // TODO: We want to add chunks if they are _in the view range_, not unconditionally
            if let Some(p) = todo.chunks.pop() {
                self.chunks
                    .entry(p)
                    .or_insert_with(|| Chunk::new(p))
                    .update(context, &space, &block_data.block_render_data);
                chunk_update_count += 1;
            } else {
                break;
            }
        }

        SpaceRendererOutput {
            block_texture: block_data.texture(),
            chunks: self.chunks.values().collect(), // TODO visibility culling, and don't allocate every frame
            view_matrix,
            info: SpaceRenderInfo {
                chunk_queue_count: todo.chunks.len(),
                chunk_update_count,
                square_count: 0, // filled later
            },
        }
    }
}

/// Ingredients to actually draw the Space inside a luminance pipeline, produced by
/// `prepare_frame`.
pub struct SpaceRendererOutput<'a> {
    block_texture: &'a mut BlockTexture,
    /// Chunks are handy wrappers around some Tesses
    chunks: Vec<&'a Chunk>,
    view_matrix: Matrix4<FreeCoordinate>,
    info: SpaceRenderInfo,
}
/// As `SpaceRendererBound`, but past the texture-binding stage of the pipeline.
pub struct SpaceRendererBound<'a> {
    pub bound_block_texture: BoundBlockTexture<'a>,
    pub chunks: Vec<&'a Chunk>,
    pub view_matrix: Matrix4<FreeCoordinate>,
    info: SpaceRenderInfo,
}

impl<'a> SpaceRendererOutput<'a> {
    pub fn bind(self, pipeline: &'a Pipeline<'a>) -> Result<SpaceRendererBound<'a>, PipelineError> {
        Ok(SpaceRendererBound {
            bound_block_texture: pipeline.bind_texture(self.block_texture)?,
            chunks: self.chunks,
            view_matrix: self.view_matrix,
            info: self.info,
        })
    }
}
impl<'a> SpaceRendererBound<'a> {
    pub fn render<E>(&self, tess_gate: &mut TessGate) -> Result<SpaceRenderInfo, E> {
        let mut square_count = 0;
        for chunk in &self.chunks {
            square_count += chunk.render(tess_gate)?;
        }
        Ok(SpaceRenderInfo {
            square_count,
            ..self.info
        })
    }
}

/// Performance info from a `SpaceRenderer`.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct SpaceRenderInfo {
    pub chunk_queue_count: usize,
    pub chunk_update_count: usize,
    pub square_count: usize,
}

/// Divide a cube position to obtain a chunk position.
fn chunkify(cube: GridPoint) -> GridPoint {
    GridPoint::new(
        cube.x.div_euclid(CHUNK_SIZE),
        cube.y.div_euclid(CHUNK_SIZE),
        cube.z.div_euclid(CHUNK_SIZE),
    )
}

/// Storage for rendering of part of a `Space`.
pub struct Chunk {
    bounds: Grid,
    /// Vertices grouped by the direction they face
    vertices: FaceMap<Vec<Vertex>>,
    tesses: FaceMap<Option<Tess<Vertex>>>,
}

impl Chunk {
    fn new(chunk_pos: GridPoint) -> Self {
        Chunk {
            bounds: Grid::new(chunk_pos * CHUNK_SIZE, (CHUNK_SIZE, CHUNK_SIZE, CHUNK_SIZE)),
            vertices: FaceMap::default(),
            tesses: FaceMap::default(),
        }
    }

    fn update<C: GraphicsContext<Backend = Backend>>(
        &mut self,
        context: &mut C,
        space: &Space,
        blocks_render_data: &BlocksRenderData<GLBlockVertex, BlockGLTexture>,
    ) {
        triangulate_space(space, &self.bounds, blocks_render_data, &mut self.vertices);

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

    pub fn render<E>(&self, tess_gate: &mut TessGate) -> Result<usize, E> {
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

/// `SpaceRenderer`'s set of things that need recomputing.
#[derive(Default)]
struct SpaceRendererTodo {
    blocks: bool,
    // TODO: Organize chunks as a priority queue ordered by distance from camera.
    chunks: IndexSet<GridPoint>,
}

impl SpaceRendererTodo {
    fn insert_block_and_adjacent(&mut self, cube: GridPoint) {
        // Mark adjacent blocks to account for opaque faces hiding adjacent
        // blocks' faces. We don't need to bother with the current block since
        // the adjacent chunks will always include it (presuming that the chunk
        // size is greater than 1).
        for axis in 0..3 {
            for offset in &[0, 1] {
                let mut adjacent = cube;
                adjacent[axis] += offset;
                self.chunks.insert(chunkify(adjacent));
            }
        }
    }
}

/// `Listener` adapter for `SpaceRendererTodo`.
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
                SpaceChange::Number(_) => {
                    todo.blocks = true;
                }
            }
        }
    }

    fn alive(&self) -> bool {
        self.0.upgrade().is_some()
    }
}

// TODO: Could really use some tests of this logic, but how to have a GraphicsContext...?
