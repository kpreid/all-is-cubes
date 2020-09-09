// Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

//! Get from `Space` to `luminance::tess::Tess`.

use luminance_front::context::GraphicsContext;
use luminance_front::tess::{Interleaved, Mode, Tess, VerticesMut};
use luminance_front::tess_gate::TessGate;
use luminance_front::Backend;

use crate::lum::block_texture::{BlockGLRenderData, BlockGLTexture, BlockTexture};
use crate::lum::types::{GLBlockVertex, Vertex};
use crate::math::{Face, FaceMap};
use crate::space::Space;
use crate::triangulator::{triangulate_space, BlocksRenderData};
use crate::universe::{DirtyFlag, URef};

/// Manages cached data and GPU resources for drawing a single `Space`.
pub struct SpaceRenderer {
    space: URef<Space>,
    dirty_chunks: DirtyFlag,
    block_data_cache: Option<BlockGLRenderData>, // TODO: quick hack, needs an invalidation strategy
    chunk: Chunk,
}

impl SpaceRenderer {
    pub fn new(space: URef<Space>) -> Self {
        let dirty_chunks = DirtyFlag::new();
        space.borrow_mut().listen(dirty_chunks.listener());
        Self {
            space,
            dirty_chunks,
            block_data_cache: None,
            chunk: Chunk::new(),
        }
    }

    pub fn space(&self) -> &URef<Space> {
        &self.space
    }

    pub fn prepare_frame<C>(&mut self, context: &mut C) -> (&mut BlockTexture, Vec<&Chunk>)
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let space = &*self.space.borrow();
        // TODO: quick hack; we need actual invalidation, not memoization
        let block_data = self.block_data_cache.get_or_insert_with(|| {
            let (block_data, _info) =
                BlockGLRenderData::prepare(context, space).expect("texture failure");
            // TODO get a logging strategy and use it for _info
            block_data
        });

        if self.dirty_chunks.get_and_clear() {
            self.chunk
                .update(context, &space, &block_data.block_render_data);
        }

        (block_data.texture(), vec![&self.chunk])
    }
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
