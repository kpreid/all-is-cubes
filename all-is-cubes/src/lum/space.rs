// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Get from `Space` to `luminance::tess::Tess`.

use cgmath::Matrix4;
use luminance_front::context::GraphicsContext;
use luminance_front::pipeline::{Pipeline, PipelineError};
use luminance_front::tess::{Interleaved, Mode, Tess, VerticesMut};
use luminance_front::tess_gate::TessGate;
use luminance_front::Backend;

use crate::lum::block_texture::{
    BlockGLRenderData, BlockGLTexture, BlockTexture, BoundBlockTexture,
};
use crate::lum::types::{GLBlockVertex, Vertex};
use crate::math::{Face, FaceMap, FreeCoordinate};
use crate::space::{BlockIndex, Space, SpaceChange};
use crate::triangulator::{triangulate_space, BlocksRenderData};
use crate::universe::{ListenerHelper, Sink, URef};

/// Manages cached data and GPU resources for drawing a single `Space`.
pub struct SpaceRenderer {
    space: URef<Space>,
    todo: Sink<SpaceRendererDirty>,
    block_data_cache: Option<BlockGLRenderData>, // TODO: quick hack, needs an invalidation strategy
    chunk: Chunk,
}

impl SpaceRenderer {
    pub fn new(space: URef<Space>) -> Self {
        let todo = Sink::new();
        space.borrow_mut().listen(todo.listener().filter(|m| {
            Some(match m {
                SpaceChange::Block(_) => SpaceRendererDirty::Chunk,
                SpaceChange::Lighting(_) => SpaceRendererDirty::Chunk,
                SpaceChange::Number(n) => SpaceRendererDirty::Block(n),
            })
        }));
        Self {
            space,
            todo,
            block_data_cache: None,
            chunk: Chunk::new(),
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
        let space = &*self.space.borrow();

        let mut dirty_blocks = false;
        let mut dirty_chunk = false;
        while let Some(m) = self.todo.next() {
            match m {
                SpaceRendererDirty::Chunk => {
                    dirty_chunk = true;
                }
                SpaceRendererDirty::Block(_id) => {
                    // TODO: update individual blocks
                    dirty_blocks = true;
                    // Vertices may have changed.
                    // TODO: Implement recognizing texture-only updates, and
                    // (once chunking exists) only updating chunks that need it
                    dirty_chunk = true;
                }
            }
        }

        if dirty_blocks {
            self.block_data_cache = None;
        }
        let block_data = self.block_data_cache.get_or_insert_with(|| {
            let (block_data, _info) =
                BlockGLRenderData::prepare(context, space).expect("texture failure");
            // TODO get a logging strategy and use it for _info
            block_data
        });

        if dirty_chunk {
            self.chunk
                .update(context, &space, &block_data.block_render_data);
        }

        SpaceRendererOutput {
            block_texture: block_data.texture(),
            chunks: vec![&self.chunk],
            view_matrix,
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
}
/// As `SpaceRendererBound`, but past the texture-binding stage of the pipeline.
pub struct SpaceRendererBound<'a> {
    pub bound_block_texture: BoundBlockTexture<'a>,
    pub chunks: Vec<&'a Chunk>,
    pub view_matrix: Matrix4<FreeCoordinate>,
}

impl<'a> SpaceRendererOutput<'a> {
    pub fn bind(self, pipeline: &'a Pipeline<'a>) -> Result<SpaceRendererBound<'a>, PipelineError> {
        Ok(SpaceRendererBound {
            bound_block_texture: pipeline.bind_texture(self.block_texture)?,
            chunks: self.chunks,
            view_matrix: self.view_matrix,
        })
    }
}
impl<'a> SpaceRendererBound<'a> {
    pub fn render<E>(&self, tess_gate: &mut TessGate) -> Result<usize, E> {
        let mut square_count = 0;
        for chunk in &self.chunks {
            square_count += chunk.render(tess_gate)?;
        }
        Ok(square_count)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum SpaceRendererDirty {
    Block(BlockIndex),
    Chunk, // TODO: once we have specific chunks
}

/// Not yet actually chunked rendering, but bundles the data that would be used in one.
pub struct Chunk {
    // bounds: Grid,
    /// Vertices grouped by the direction they face
    vertices: FaceMap<Vec<Vertex>>,
    tesses: FaceMap<Option<Tess<Vertex>>>,
}

impl Chunk {
    fn new() -> Self {
        Chunk {
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
        triangulate_space(space, blocks_render_data, &mut self.vertices);

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

// TODO: Could really use some tests of this logic, but how to have a GraphicsContext...?
