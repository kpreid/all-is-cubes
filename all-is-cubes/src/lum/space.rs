// Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Get from [`Space`] to [`Tess`].

use bitvec::prelude::BitVec;
use cgmath::{EuclideanSpace as _, Point3, Vector3};
use luminance::tess::View as _;
use luminance_front::blending::{Blending, Equation, Factor};
use luminance_front::context::GraphicsContext;
use luminance_front::depth_test::DepthWrite;
use luminance_front::face_culling::{FaceCulling, FaceCullingMode, FaceCullingOrder};
use luminance_front::pipeline::{BoundTexture, Pipeline, PipelineError};
use luminance_front::pixel::NormRGBA8UI;
use luminance_front::render_state::RenderState;
use luminance_front::shading_gate::ShadingGate;
use luminance_front::tess::{Mode, Tess};
use luminance_front::tess_gate::TessGate;
use luminance_front::texture::{Dim3, GenMipmaps, Sampler, Texture, TextureError};
use luminance_front::Backend;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{hash_map::Entry::*, HashMap, HashSet};
use std::fmt;
use std::rc::{Rc, Weak};

use crate::camera::{Camera, GraphicsOptions};
use crate::chunking::{cube_to_chunk, point_to_chunk, ChunkChart, ChunkPos, CHUNK_SIZE};
use crate::listen::Listener;
use crate::lum::block_texture::{BlockTexture, BoundBlockTexture, LumAtlasAllocator, LumAtlasTile};
use crate::lum::shading::BlockPrograms;
use crate::lum::types::LumBlockVertex;
use crate::math::{FaceMap, FreeCoordinate, GridCoordinate, GridPoint, Rgb};
use crate::space::{BlockIndex, Grid, Space, SpaceChange};
use crate::triangulator::{
    triangulate_block, triangulate_blocks, BlockTriangulation, BlockTriangulationProvider,
    DepthOrdering, SpaceTriangulation,
};
use crate::universe::URef;
use crate::util::{CustomFormat, StatusText};

use super::block_texture::AtlasFlushInfo;

/// Manages cached data and GPU resources for drawing a single [`Space`].
pub struct SpaceRenderer {
    space: URef<Space>,
    todo: Rc<RefCell<SpaceRendererTodo>>,
    block_triangulations: Vec<BlockTriangulation<LumBlockVertex, LumAtlasTile>>,
    /// Version IDs used to track whether chunks have stale block triangulations.
    /// Indices are block indices and values are version numbers.
    block_versioning: Vec<u32>,
    block_version_counter: u32,
    block_texture: Option<LumAtlasAllocator>,
    light_texture: Option<SpaceLightTexture>,
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
            light_texture: None,
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
        camera: &Camera,
    ) -> SpaceRendererOutput<'a>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let graphics_options = camera.options();
        let mut todo = self.todo.borrow_mut();

        let space = &*self
            .space
            .try_borrow()
            .expect("TODO: return a trivial result instead of panic.");

        let block_texture_allocator = self.block_texture.get_or_insert_with(|| {
            // TODO: friendlier error
            LumAtlasAllocator::new(context).expect("texture setup failure")
        });

        let light_texture = self.light_texture.get_or_insert_with(|| {
            todo.light = None; // signal to update everything
            SpaceLightTexture::new(context, space.grid()).expect("texture setup failure")
        });

        if todo.all_blocks_and_chunks {
            todo.all_blocks_and_chunks = false;
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
            todo.all_blocks_and_chunks = false;
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
                let new_triangulation =
                    triangulate_block(block_data[index].evaluated(), block_texture_allocator);

                // Only invalidate the chunks if we actually have different data.
                // Note: This comparison depends on such things as the definition of PartialEq
                // for LumAtlasTile, which compares by texture tile reference rather than contents.
                // TODO: We don't currently make use of this optimally because the triangulator
                // never reuses textures. (If it did, we'd need to consider what we want to do
                // about stale chunks with fresh textures, which might have geometry gaps or
                // otherwise be obviously inconsistent.)
                if new_triangulation != self.block_triangulations[index] {
                    self.block_triangulations[index] = new_triangulation;
                    self.block_versioning[index] = self.block_version_counter;
                } else {
                    // The new triangulation is identical to the old one (which might happen because
                    // interior voxels or non-rendered attributes were changed), so don't invalidate
                    // the chunks.
                }

                block_update_count += 1;
            }
        }

        let texture_info = block_texture_allocator
            .flush()
            .expect("block texture write failure"); // TODO: recover from this error

        // Update light texture
        if let Some(set) = &mut todo.light {
            // TODO: work in larger, ahem, chunks
            for cube in set.drain() {
                light_texture
                    .update(space, Grid::new(cube, [1, 1, 1]))
                    .expect("light texture write failure");
            }
        } else {
            light_texture
                .update_all(space)
                .expect("light texture write failure");
            todo.light = Some(HashSet::new());
        }

        let view_point = camera.view_position();
        let view_chunk = point_to_chunk(view_point);
        self.chunk_chart.resize_if_needed(camera.view_distance());

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
            if chunk_update_count >= graphics_options.chunks_per_frame.into() {
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
                        graphics_options,
                        &self.block_triangulations,
                        &self.block_versioning,
                    );
                chunk_update_count += 1;
            }
        }
        self.chunks_were_missing = chunks_are_missing;

        if let Some(chunk) = self.chunks.get_mut(&view_chunk) {
            chunk.depth_sort_for_view(view_point);
        }

        // TODO: flush todo.chunks and self.chunks of out-of-range chunks.

        SpaceRendererOutput {
            sky_color: space.sky_color(),
            block_texture: &mut block_texture_allocator.texture,
            light_texture,
            camera: camera.clone(),
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
    light_texture: &'a mut SpaceLightTexture,
    camera: Camera,
    /// Chunks are handy wrappers around some Tesses
    chunks: &'a HashMap<ChunkPos, Chunk>,
    chunk_chart: &'a ChunkChart,
    view_chunk: ChunkPos,
    info: SpaceRenderInfo,
}
/// As [`SpaceRendererOutput`], but past the texture-binding stage of the pipeline.
/// Note: This must be public to satisfy luminance derive macros' public requirements.
pub struct SpaceRendererBound<'a> {
    /// Space's sky color, to be used as background color (clear color / fog).
    pub(super) sky_color: Rgb,

    /// Block texture to pass to the shader.
    pub(super) bound_block_texture: BoundBlockTexture<'a>,
    /// Block texture to pass to the shader.
    pub(super) bound_light_texture: SpaceLightTextureBound<'a>,
    pub(super) camera: Camera,
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
            bound_light_texture: self.light_texture.bind(pipeline)?,
            camera: self.camera,
            chunks: self.chunks,
            chunk_chart: self.chunk_chart,
            view_chunk: self.view_chunk,
            info: self.info,
        })
    }
}
impl<'a> SpaceRendererBound<'a> {
    /// Use a [`ShadingGate`] to actually draw the space.
    pub(crate) fn render<E>(
        &self,
        shading_gate: &mut ShadingGate<'_>,
        block_programs: &mut BlockPrograms,
    ) -> Result<SpaceRenderInfo, E> {
        let mut chunks_drawn = 0;
        let mut squares_drawn = 0;

        // These two blocks are *almost* identical but the iteration order is reversed,
        // the shader is different, and we only count the chunks once.
        shading_gate.shade(
            &mut block_programs.opaque,
            |ref mut program_iface, u, mut render_gate| {
                u.initialize(program_iface, self);
                let pass = SpaceRendererPass::Opaque;
                render_gate.render(&pass.render_state(), |mut tess_gate| {
                    for p in self.chunk_chart.chunks(self.view_chunk) {
                        if let Some(chunk) = self.chunks.get(&p) {
                            if self.cull(p) {
                                continue;
                            }
                            chunks_drawn += 1;
                            squares_drawn +=
                                chunk.render(&mut tess_gate, pass, DepthOrdering::Any)?;
                        }
                        // TODO: If the chunk is missing, draw a blocking shape, possibly?
                    }
                    Ok(())
                })
            },
        )?;

        shading_gate.shade(
            &mut block_programs.transparent,
            |ref mut program_iface, u, mut render_gate| {
                u.initialize(program_iface, self);
                let pass = SpaceRendererPass::Transparent;
                render_gate.render(&pass.render_state(), |mut tess_gate| {
                    for p in self.chunk_chart.chunks(self.view_chunk).rev() {
                        if let Some(chunk) = self.chunks.get(&p) {
                            if self.cull(p) {
                                continue;
                            }
                            squares_drawn += chunk.render(
                                &mut tess_gate,
                                pass,
                                // TODO: avoid adding and then subtracting view_chunk
                                DepthOrdering::from_view_direction(p.0 - self.view_chunk.0),
                            )?;
                        }
                    }
                    Ok(())
                })
            },
        )?;

        Ok(SpaceRenderInfo {
            chunks_drawn,
            squares_drawn,
            ..self.info.clone()
        })
    }

    fn cull(&self, chunk: ChunkPos) -> bool {
        self.camera.options().use_frustum_culling && !self.camera.aab_in_view(chunk.grid().into())
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

impl CustomFormat<StatusText> for SpaceRenderInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        writeln!(
            fmt,
            "Chunk updates: {:3} Block updates: {:3}",
            self.chunk_update_count, self.block_update_count,
        )?;
        writeln!(
            fmt,
            "Chunks drawn: {:3} Quads drawn: {:3}",
            self.chunks_drawn, self.squares_drawn,
        )?;
        write!(fmt, "{:#?}", self.texture_info.custom_format(StatusText))?;
        Ok(())
    }
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
struct Chunk {
    bounds: Grid,
    triangulation: SpaceTriangulation<LumBlockVertex>,
    tess: Option<Tess<LumBlockVertex, u32>>,
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
        options: &GraphicsOptions,
        block_triangulations: &[BlockTriangulation<LumBlockVertex, LumAtlasTile>],
        block_versioning: &[u32],
    ) {
        let mut block_provider = TrackingBlockProvider::new(block_triangulations);

        let old_indices_len = self.triangulation.indices().len();

        self.triangulation
            .compute(space, self.bounds, options, &mut block_provider);

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
                // TODO: workaround for https://github.com/phaazon/luminance-rs/issues/483
                && !cfg!(target_arch = "wasm32")
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

    fn depth_sort_for_view(&mut self, view_position: Point3<FreeCoordinate>) {
        // Disable dynamic depth sorting because luminance bug
        // https://github.com/phaazon/luminance-rs/issues/483
        // means indices_mut() can fail and corrupt other buffers.
        // TODO: Reenble this and also in-place chunk updating when bug is fixed
        if !cfg!(target_arch = "wasm32") {
            let range = self.triangulation.transparent_range(DepthOrdering::Within);
            if !range.is_empty() {
                if let Some(tess) = &mut self.tess {
                    self.triangulation
                        .depth_sort_for_view(view_position.map(|s| s as f32));
                    tess.indices_mut()
                        .expect("failed to map indices for depth sorting")[range.clone()]
                    .copy_from_slice(&self.triangulation.indices()[range]);
                }
            }
        }
    }

    fn render<E>(
        &self,
        tess_gate: &mut TessGate<'_>,
        pass: SpaceRendererPass,
        ordering: DepthOrdering,
    ) -> Result<usize, E> {
        let mut count = 0;
        if let Some(tess) = &self.tess {
            let range = match pass {
                SpaceRendererPass::Opaque => self.triangulation.opaque_range(),
                SpaceRendererPass::Transparent => self.triangulation.transparent_range(ordering),
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
    block_triangulations: &'a [BlockTriangulation<LumBlockVertex, LumAtlasTile>],
    seen: BitVec,
}
impl<'a> TrackingBlockProvider<'a> {
    fn new(block_triangulations: &'a [BlockTriangulation<LumBlockVertex, LumAtlasTile>]) -> Self {
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
impl<'a> BlockTriangulationProvider<'a, LumBlockVertex, LumAtlasTile>
    for &mut TrackingBlockProvider<'a>
{
    fn get(
        &mut self,
        index: BlockIndex,
    ) -> Option<&'a BlockTriangulation<LumBlockVertex, LumAtlasTile>> {
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
    all_blocks_and_chunks: bool,
    blocks: HashSet<BlockIndex>,
    /// Membership in this table indicates that the chunk *exists;* todos for chunks
    /// outside of the view area are not tracked.
    chunks: HashMap<ChunkPos, ChunkTodo>,

    /// Blocks whose light texels should be updated.
    /// None means do a full space reupload.
    ///
    /// TODO: experiment with different granularities of light invalidation (chunks, dirty rects, etc.)
    light: Option<HashSet<GridPoint>>,
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
                    todo.all_blocks_and_chunks = true;
                    todo.blocks.clear();
                    todo.chunks.clear();
                    todo.light = None;
                }
                SpaceChange::Block(p) => {
                    todo.modify_block_and_adjacent(p, |chunk_todo| {
                        chunk_todo.update_triangulation = true;
                    });
                }
                SpaceChange::Lighting(p) => {
                    // None means everything
                    if let Some(set) = &mut todo.light {
                        set.insert(p);
                    }
                }
                SpaceChange::Number(index) => {
                    if !todo.all_blocks_and_chunks {
                        todo.blocks.insert(index);
                    }
                }
                SpaceChange::BlockValue(index) => {
                    if !todo.all_blocks_and_chunks {
                        todo.blocks.insert(index);
                    }
                }
            }
        }
    }

    fn alive(&self) -> bool {
        self.0.strong_count() > 0
    }
}

/// Keeps a 3D [`Texture`] up to date with the light data from a [`Space`].
///
/// The texels are in [`PackedLight`] form.
/// The alpha component is unused.
/// TODO: Use alpha component to communicate block opacity.
struct SpaceLightTexture {
    texture: Texture<Dim3, NormRGBA8UI>,
    /// The region of cube coordinates for which there are valid texels.
    texture_grid: Grid,
}

impl SpaceLightTexture {
    /// Construct a new `SpaceLightTexture` for the specified size of [`Space`],
    /// with no data.
    pub fn new<C>(context: &mut C, grid: Grid) -> Result<Self, TextureError>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        // Boundary of 1 extra cube automatically captures sky light.
        let texture_grid = grid.expand(FaceMap {
            px: 1,
            py: 1,
            pz: 1,
            nx: 0,
            ny: 0,
            nz: 0,
            within: 0,
        });
        let texture = Texture::new(
            context,
            texture_grid.unsigned_size().into(),
            /* mipmaps= */ 0,
            Sampler::default(), // sampler options don't matter because we're using texelFetch()
        )?;
        Ok(Self {
            texture,
            texture_grid,
        })
    }

    /// Copy the specified region of light data.
    pub fn update(&mut self, space: &Space, region: Grid) -> Result<(), TextureError> {
        let mut data = Vec::with_capacity(region.volume());
        // TODO: Enable circular operation and eliminate the need for the offset of the
        // coordinates (texture_grid.lower_bounds() and light_offset in the shader)
        // by doing a coordinate wrap-around -- the shader and the Space will agree
        // on coordinates modulo the texture size, and this upload will need to be broken
        // into up to 8 pieces.
        for z in region.z_range() {
            for y in region.y_range() {
                for x in region.x_range() {
                    data.push(space.get_lighting([x, y, z]).as_texel());
                }
            }
        }
        self.texture.upload_part(
            GenMipmaps::No,
            (region.lower_bounds() - self.texture_grid.lower_bounds())
                .map(|s| s as u32)
                .into(),
            region.unsigned_size().into(),
            &data,
        )
    }

    pub fn update_all(&mut self, space: &Space) -> Result<(), TextureError> {
        self.update(space, self.texture_grid)
    }

    fn bind<'a>(
        &'a mut self,
        pipeline: &'a Pipeline<'a>,
    ) -> Result<SpaceLightTextureBound<'a>, PipelineError> {
        Ok(SpaceLightTextureBound {
            texture: pipeline.bind_texture(&mut self.texture)?,
            offset: -self.texture_grid.lower_bounds().to_vec(),
        })
    }
}

pub(crate) struct SpaceLightTextureBound<'a> {
    pub(crate) texture: BoundTexture<'a, Dim3, NormRGBA8UI>,
    pub(crate) offset: Vector3<GridCoordinate>,
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
