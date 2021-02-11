// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Get from [`Space`] to [`Tess`].

use bitvec::prelude::BitVec;
use cgmath::{EuclideanSpace as _, Matrix as _, Matrix4, Point3, SquareMatrix as _};
use luminance::tess::View as _;
use luminance_front::blending::{Blending, Equation, Factor};
use luminance_front::context::GraphicsContext;
use luminance_front::depth_test::DepthWrite;
use luminance_front::face_culling::{FaceCulling, FaceCullingMode, FaceCullingOrder};
use luminance_front::pipeline::{Pipeline, PipelineError};
use luminance_front::render_gate::RenderGate;
use luminance_front::render_state::RenderState;
use luminance_front::tess::{Mode, Tess};
use luminance_front::tess_gate::TessGate;
use luminance_front::Backend;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{hash_map::Entry::*, HashMap, HashSet};
use std::rc::{Rc, Weak};

use crate::camera::ProjectionHelper;
use crate::chunking::{cube_to_chunk, point_to_chunk, ChunkChart, ChunkPos, CHUNK_SIZE};
use crate::listen::Listener;
use crate::lum::block_texture::{BlockTexture, BoundBlockTexture, LumAtlasAllocator, LumAtlasTile};
use crate::lum::types::{GLBlockVertex, Vertex};
use crate::math::{FreeCoordinate, GridPoint, Rgb};
use crate::space::{BlockIndex, Grid, Space, SpaceChange};
use crate::triangulator::{
    triangulate_block, triangulate_blocks, BlockTriangulation, BlockTriangulationProvider,
    SpaceTriangulation,
};
use crate::universe::URef;

use super::block_texture::AtlasFlushInfo;

/// Manages cached data and GPU resources for drawing a single [`Space`].
pub struct SpaceRenderer {
    space: URef<Space>,
    todo: Rc<RefCell<SpaceRendererTodo>>,
    block_triangulations: Vec<BlockTriangulation<GLBlockVertex, LumAtlasTile>>,
    /// Version IDs used to track whether chunks have stale block triangulations.
    /// Indices are block indices and values are version numbers.
    block_versioning: Vec<u32>,
    block_version_counter: u32,
    block_texture: Option<LumAtlasAllocator>,
    /// Invariant: the set of present chunks (keys here) is the same as the set of keys
    /// in `todo.borrow().chunks`.
    chunks: HashMap<ChunkPos, Chunk>,
    chunk_chart: ChunkChart,
    /// Whether, on the previous frame, some chunks were unavailable.
    /// If so, then we prioritize adding new chunks over updating existing ones.
    chunks_were_missing: bool,
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
            block_versioning: Vec::new(),
            block_version_counter: 0,
            block_texture: None,
            chunks: HashMap::new(),
            chunk_chart: ChunkChart::new(0.0),
            chunks_were_missing: true,
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
        projection: &ProjectionHelper,
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
            LumAtlasAllocator::new(context).expect("texture setup failure")
        });

        let mut todo = self.todo.borrow_mut();

        if todo.everything {
            todo.everything = false;
            self.block_triangulations.clear();
            self.block_version_counter = self.block_version_counter.wrapping_add(1);
            // We don't need to clear self.chunks because they will automatically be considered
            // stale by the new block versioning value.
        }

        let mut block_update_count = 0;
        if self.block_triangulations.is_empty() {
            // One of the following cases:
            // * It's the first run and we haven't prepared the blocks at all.
            // * The space somehow has zero blocks, in which case this is trivial anyway.
            // * The space signaled SpaceChange::EveryBlock.
            todo.everything = false;
            self.block_triangulations =
                Vec::from(triangulate_blocks(space, block_texture_allocator));
            self.block_versioning =
                vec![self.block_version_counter; self.block_triangulations.len()];
            block_update_count = self.block_triangulations.len();
        } else if !todo.blocks.is_empty() {
            // Partial update.
            self.block_version_counter = self.block_version_counter.wrapping_add(1);
            let block_data = space.block_data();

            // Update the vector length to match the space.
            let new_length = block_data.len();
            let old_length = self.block_triangulations.len();
            match new_length.cmp(&old_length) {
                Ordering::Less => {
                    self.block_triangulations.truncate(new_length);
                    self.block_versioning.truncate(new_length);
                }
                Ordering::Greater => {
                    let added = old_length..new_length;
                    self.block_triangulations
                        .extend(added.clone().map(|_| BlockTriangulation::default()));
                    self.block_versioning.extend(added.map(|_| 0));
                }
                Ordering::Equal => {}
            }
            assert_eq!(self.block_triangulations.len(), new_length);

            for index in todo.blocks.drain() {
                let index: usize = index.into();
                self.block_triangulations[index] =
                    triangulate_block(block_data[index].evaluated(), block_texture_allocator);
                self.block_versioning[index] = self.block_version_counter;
                block_update_count += 1;
            }
        }

        let texture_info = block_texture_allocator
            .flush()
            .expect("texture write failure"); // TODO: recover from this error

        // TODO: tested function for this matrix op mess
        // TODO: replace unwrap()s with falling back to drawing nothing or drawing the origin
        let view_point = Point3::from_vec(
            projection
                .view()
                .invert()
                .unwrap()
                .transpose()
                .row(3)
                .truncate(),
        );
        let view_chunk = point_to_chunk(view_point);
        self.chunk_chart
            .resize_if_needed(projection.view_distance());

        // Update some chunk geometry.
        let chunk_grid = space.grid().divide(CHUNK_SIZE);
        let mut chunk_update_count = 0;
        let mut chunks_are_missing = false;
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
            if (todo
                .chunks
                .get(&p)
                .map(|ct| ct.update_triangulation)
                .unwrap_or(false)
                && !self.chunks_were_missing)
                || matches!(chunk_entry, Vacant(_))
                || matches!(chunk_entry, Occupied(ref oe) if oe.get().stale_blocks(&self.block_versioning))
            {
                chunk_entry
                    .or_insert_with(|| {
                        // Chunk is missing. Note this for update planning.
                        chunks_are_missing = true;
                        // Remember that we want to track dirty flags.
                        todo.chunks.insert(p, ChunkTodo::CLEAN);
                        // Generate new chunk.
                        Chunk::new(p)
                    })
                    .update(
                        context,
                        todo.chunks.get_mut(&p).unwrap(), // TODO: can we eliminate the double lookup with a todo entry?
                        &space,
                        &self.block_triangulations,
                        &self.block_versioning,
                    );
                chunk_update_count += 1;
            }
        }
        self.chunks_were_missing = chunks_are_missing;

        // TODO: flush todo.chunks and self.chunks of out-of-range chunks.

        SpaceRendererOutput {
            sky_color: space.sky_color(),
            block_texture: &mut block_texture_allocator.texture,
            view_matrix: projection.view(),
            chunks: &self.chunks, // TODO visibility culling, and don't allocate every frame
            chunk_chart: &self.chunk_chart,
            view_chunk,
            info: SpaceRenderInfo {
                chunk_update_count,
                block_update_count,
                chunks_drawn: 0,
                squares_drawn: 0, // filled later
                texture_info,
            },
        }
    }
}

/// Ingredients to actually draw the [`Space`] inside a luminance pipeline, produced by
/// [`SpaceRenderer::prepare_frame`].
pub struct SpaceRendererOutput<'a> {
    /// Space's sky color, to be used as background color (clear color / fog).
    pub sky_color: Rgb,

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
    /// Space's sky color, to be used as background color (clear color / fog).
    pub sky_color: Rgb,

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
            sky_color: self.sky_color,
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
    /// Use a [`RenderGate`] to actually draw the space.
    pub fn render<E>(&self, render_gate: &mut RenderGate) -> Result<SpaceRenderInfo, E> {
        let mut chunks_drawn = 0;
        let mut squares_drawn = 0;

        // These two blocks are *almost* identical but the iteration order is reversed,
        // and we only count the chunks once.
        // TODO: For performance, also use a shader that doesn't support opacity for the
        // opaque pass. Once block geometry no longer uses alpha discard, we can skip that
        // too.
        {
            let pass = SpaceRendererPass::Opaque;
            render_gate.render(&pass.render_state(), |mut tess_gate| {
                for p in self.chunk_chart.chunks(self.view_chunk) {
                    if let Some(chunk) = self.chunks.get(&p) {
                        chunks_drawn += 1;
                        squares_drawn += chunk.render(&mut tess_gate, pass)?;
                    }
                    // TODO: If the chunk is missing, draw a blocking shape, possibly?
                }
                Ok(())
            })?;
        }
        {
            let pass = SpaceRendererPass::Transparent;
            render_gate.render(&pass.render_state(), |mut tess_gate| {
                for p in self.chunk_chart.chunks(self.view_chunk).rev() {
                    if let Some(chunk) = self.chunks.get(&p) {
                        squares_drawn += chunk.render(&mut tess_gate, pass)?;
                    }
                }
                Ok(())
            })?;
        }

        Ok(SpaceRenderInfo {
            chunks_drawn,
            squares_drawn,
            ..self.info.clone()
        })
    }
}

/// Performance info from a [`SpaceRenderer`] drawing one frame.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SpaceRenderInfo {
    /// How many chunks were recomputed this frame.
    pub chunk_update_count: usize,
    /// How many block triangulations were recomputed this time.
    pub block_update_count: usize,
    pub chunks_drawn: usize,
    /// How many squares (quadrilaterals; sets of 2 triangles = 6 vertices) were used
    /// to draw this frame.
    pub squares_drawn: usize,
    /// Status of the texture atlas.
    pub texture_info: AtlasFlushInfo,
}

// Which drawing pass we're doing.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum SpaceRendererPass {
    Opaque,
    Transparent,
}
impl SpaceRendererPass {
    /// Returns the [`RenderState`] to use for this pass.
    pub fn render_state(self) -> RenderState {
        let base = RenderState::default().set_face_culling(FaceCulling {
            order: FaceCullingOrder::CCW,
            mode: FaceCullingMode::Back,
        });
        match self {
            SpaceRendererPass::Opaque => base,
            SpaceRendererPass::Transparent => {
                base.set_depth_write(DepthWrite::Off) // Unnecessary
                    .set_blending(Some(Blending {
                        // Note that this blending configuration is for premultiplied alpha.
                        // The fragment shaders are responsible for producing premultiplied alpha outputs.
                        equation: Equation::Additive,
                        src: Factor::One,
                        dst: Factor::SrcAlphaComplement,
                    }))
            }
        }
    }
}

/// Storage for rendering of part of a [`Space`].
pub struct Chunk {
    bounds: Grid,
    triangulation: SpaceTriangulation<Vertex>,
    tess: Option<Tess<Vertex, u32>>,
    /// Texture tiles that our vertices' texture coordinates refer to.
    tile_dependencies: Vec<LumAtlasTile>,
    block_dependencies: Vec<(BlockIndex, u32)>,
}

impl Chunk {
    fn new(chunk_pos: ChunkPos) -> Self {
        Chunk {
            bounds: chunk_pos.grid(),
            triangulation: SpaceTriangulation::new(),
            tess: None,
            tile_dependencies: Vec::new(),
            block_dependencies: Vec::new(),
        }
    }

    fn stale_blocks(&self, versions: &[u32]) -> bool {
        self.block_dependencies
            .iter()
            .copied()
            .any(|(index, version)| versions[usize::from(index)] != version)
    }

    fn update<C: GraphicsContext<Backend = Backend>>(
        &mut self,
        context: &mut C,
        chunk_todo: &mut ChunkTodo,
        space: &Space,
        block_triangulations: &[BlockTriangulation<GLBlockVertex, LumAtlasTile>],
        block_versioning: &[u32],
    ) {
        let mut block_provider = TrackingBlockProvider::new(block_triangulations);

        let old_indices_len = self.triangulation.indices().len();

        self.triangulation
            .compute(space, self.bounds, &mut block_provider);

        // Stash all the texture tiles so they aren't deallocated out from under us.
        // TODO: Maybe we should have something more like a Vec<Rc<BlockTriangulation>>
        self.tile_dependencies.clear();
        self.tile_dependencies.extend(
            block_provider
                .seen()
                .flat_map(|index| block_triangulations[index].textures().iter())
                .cloned(),
        );
        // Record the block triangulations we used.
        self.block_dependencies.clear();
        self.block_dependencies.extend(
            block_provider
                .seen()
                .map(|index| (index as BlockIndex, block_versioning[index])),
        );

        let tess_option = &mut self.tess;
        let new_triangulation = &self.triangulation;

        // TODO: Theoretically, we should be able to reuse an existing vertex buffer that's too
        // large, or even an index buffer that's too large via degenerate triangles.
        // In practice, doing so seems to end up drawing some invalid vertices but only under
        // luminance-webgl, and the copy_from_slice _doesn't report a length mismatch_,
        // suggesting there's a subtle bug somewhere in our code (but how?), luminance, or rustc.
        let existing_tess_size_ok = if let Some(tess) = tess_option.as_ref() {
            tess.vert_nb() == new_triangulation.vertices().len()
                && old_indices_len == new_triangulation.indices().len()
        } else {
            false
        };
        if !existing_tess_size_ok {
            // Existing buffer, if any, is not the right length. Discard it.
            *tess_option = None;
        }

        // TODO: replace unwrap()s with an error logging/flagging mechanism
        if new_triangulation.is_empty() {
            // Render zero vertices by not rendering anything.
            *tess_option = None;
        } else if let Some(tess) = tess_option.as_mut() {
            // We already have a buffer, and it is a matching length.
            tess.vertices_mut()
                .expect("failed to map vertices for copying")
                .copy_from_slice(new_triangulation.vertices());
            tess.indices_mut()
                .expect("failed to map indices for copying")
                .copy_from_slice(new_triangulation.indices());
        } else {
            // Allocate and populate new buffer.
            *tess_option = Some(
                context
                    .new_tess()
                    .set_vertices(new_triangulation.vertices())
                    .set_indices(new_triangulation.indices())
                    .set_mode(Mode::Triangle)
                    .build()
                    .unwrap(),
            );
        }

        chunk_todo.update_triangulation = false;
    }

    fn render<E>(&self, tess_gate: &mut TessGate, pass: SpaceRendererPass) -> Result<usize, E> {
        let mut count = 0;
        if let Some(tess) = &self.tess {
            let range = match pass {
                SpaceRendererPass::Opaque => self.triangulation.opaque_range(),
                SpaceRendererPass::Transparent => self.triangulation.transparent_range(),
            };
            if range.is_empty() {
                return Ok(0);
            }
            count += (range.end - range.start) / 6;
            tess_gate.render(
                tess.view(range)
                    .expect("can't happen: inconsistent chunk index range"),
            )?;
        }
        Ok(count)
    }
}

/// Helper for [`Chunk`]'s dependency tracking.
struct TrackingBlockProvider<'a> {
    block_triangulations: &'a [BlockTriangulation<GLBlockVertex, LumAtlasTile>],
    seen: BitVec,
}
impl<'a> TrackingBlockProvider<'a> {
    fn new(block_triangulations: &'a [BlockTriangulation<GLBlockVertex, LumAtlasTile>]) -> Self {
        Self {
            block_triangulations,
            seen: BitVec::with_capacity(256), // TODO: cleverer choice
        }
    }

    /// Return the indices of all the block triangulations that were used.
    ///
    /// Note: In principle, the value type should be [`BlockIndex`], but in practice it
    /// is used as an array index so this avoids writing a double conversion.
    fn seen<'s: 'a>(&'s self) -> impl Iterator<Item = usize> + 's {
        self.seen.iter_ones()
    }
}
impl<'a> BlockTriangulationProvider<'a, GLBlockVertex, LumAtlasTile>
    for &mut TrackingBlockProvider<'a>
{
    fn get(
        &mut self,
        index: BlockIndex,
    ) -> Option<&'a BlockTriangulation<GLBlockVertex, LumAtlasTile>> {
        let index = usize::from(index);
        if index >= self.seen.len() {
            self.seen.resize(index + 1, false);
        }
        self.seen.set(index, true);
        self.block_triangulations.get(index)
    }
}

/// [`SpaceRenderer`]'s set of things that need recomputing.
#[derive(Debug, Default)]
struct SpaceRendererTodo {
    everything: bool,
    blocks: HashSet<BlockIndex>,
    /// Membership in this table indicates that the chunk *exists;* todos for chunks
    /// outside of the view area are not tracked.
    chunks: HashMap<ChunkPos, ChunkTodo>,
}

impl SpaceRendererTodo {
    fn modify_block_and_adjacent<F>(&mut self, cube: GridPoint, mut f: F)
    where
        F: FnMut(&mut ChunkTodo),
    {
        // Mark adjacent blocks to account for opaque faces hiding adjacent
        // blocks' faces. We don't need to bother with the current block since
        // the adjacent chunks will always include it (presuming that the chunk
        // size is greater than 1).
        for axis in 0..3 {
            for offset in &[-1, 1] {
                let mut adjacent = cube;
                adjacent[axis] += offset;
                if let Some(chunk) = self.chunks.get_mut(&cube_to_chunk(adjacent)) {
                    f(chunk);
                }
            }
        }
    }
}

/// What might be dirty about a single chunk.
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct ChunkTodo {
    update_triangulation: bool,
}

impl ChunkTodo {
    const CLEAN: Self = Self {
        update_triangulation: false,
    };
}

/// [`Listener`] adapter for [`SpaceRendererTodo`].
struct TodoListener(Weak<RefCell<SpaceRendererTodo>>);

impl Listener<SpaceChange> for TodoListener {
    fn receive(&self, message: SpaceChange) {
        if let Some(cell) = self.0.upgrade() {
            let mut todo = cell.borrow_mut();
            match message {
                SpaceChange::EveryBlock => {
                    todo.everything = true;
                    todo.blocks.clear();
                    todo.chunks.clear();
                }
                SpaceChange::Block(p) => {
                    todo.modify_block_and_adjacent(p, |chunk_todo| {
                        chunk_todo.update_triangulation = true;
                    });
                }
                SpaceChange::Lighting(p) => {
                    todo.modify_block_and_adjacent(p, |chunk_todo| {
                        chunk_todo.update_triangulation = true;
                    });
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
    use super::ChunkTodo;
    use super::*;
    use crate::math::GridCoordinate;

    // TODO: Arrange, somehow, to test the parts that need a GraphicsContext

    fn read_todo_chunks(todo: &RefCell<SpaceRendererTodo>) -> Vec<(ChunkPos, ChunkTodo)> {
        let mut v = todo
            .borrow()
            .chunks
            .iter()
            .map(|(&p, &ct)| (p, ct))
            .collect::<Vec<_>>();
        v.sort_by_key(|(p, _): &(ChunkPos, _)| <_ as Into<[GridCoordinate; 3]>>::into(p.0));
        v
    }

    #[test]
    fn update_adjacent_chunk_positive() {
        let todo: Rc<RefCell<SpaceRendererTodo>> = Default::default();
        let listener = TodoListener(Rc::downgrade(&todo));
        todo.borrow_mut().chunks.extend(vec![
            (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
            (ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN),
            (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
        ]);
        listener.receive(SpaceChange::Block(GridPoint::new(
            CHUNK_SIZE - 1,
            CHUNK_SIZE / 2,
            CHUNK_SIZE / 2,
        )));
        assert_eq!(
            read_todo_chunks(&todo),
            vec![
                (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
                (
                    ChunkPos::new(0, 0, 0),
                    ChunkTodo {
                        update_triangulation: true,
                        ..ChunkTodo::CLEAN
                    }
                ),
                (
                    ChunkPos::new(1, 0, 0),
                    ChunkTodo {
                        update_triangulation: true,
                        ..ChunkTodo::CLEAN
                    }
                ),
            ],
        );
    }

    #[test]
    fn update_adjacent_chunk_negative() {
        let todo: Rc<RefCell<SpaceRendererTodo>> = Default::default();
        let listener = TodoListener(Rc::downgrade(&todo));
        todo.borrow_mut().chunks.extend(vec![
            (ChunkPos::new(-1, 0, 0), ChunkTodo::CLEAN),
            (ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN),
            (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
        ]);
        listener.receive(SpaceChange::Block(GridPoint::new(
            0,
            CHUNK_SIZE / 2,
            CHUNK_SIZE / 2,
        )));
        assert_eq!(
            read_todo_chunks(&todo),
            vec![
                (
                    ChunkPos::new(-1, 0, 0),
                    ChunkTodo {
                        update_triangulation: true,
                        ..ChunkTodo::CLEAN
                    }
                ),
                (
                    ChunkPos::new(0, 0, 0),
                    ChunkTodo {
                        update_triangulation: true,
                        ..ChunkTodo::CLEAN
                    }
                ),
                (ChunkPos::new(1, 0, 0), ChunkTodo::CLEAN),
            ],
        );
    }

    #[test]
    fn todo_ignores_absent_chunks() {
        let todo: Rc<RefCell<SpaceRendererTodo>> = Default::default();
        let listener = TodoListener(Rc::downgrade(&todo));

        let p = GridPoint::new(1, 1, 1) * (CHUNK_SIZE / 2);
        // Nothing happens...
        listener.receive(SpaceChange::Block(p));
        assert_eq!(read_todo_chunks(&todo), vec![]);
        // until the chunk exists in the table already.
        todo.borrow_mut()
            .chunks
            .insert(ChunkPos::new(0, 0, 0), ChunkTodo::CLEAN);
        listener.receive(SpaceChange::Block(p));
        assert_eq!(
            read_todo_chunks(&todo),
            vec![(
                ChunkPos::new(0, 0, 0),
                ChunkTodo {
                    update_triangulation: true,
                    ..ChunkTodo::CLEAN
                }
            ),],
        );
    }
}
