// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Block texture atlas management: provides [`LumAtlasAllocator`], the
//! [`TextureAllocator`] implementation for use with [`luminance`].

use luminance::context::GraphicsContext;
use luminance::pipeline::BoundTexture;
use luminance::pixel::SRGBA8UI;
use luminance::tess::{Mode, Tess};
use luminance::texture::{
    Dim3, MagFilter, MinFilter, Sampler, TexelUpload, Texture, TextureError, Wrap,
};
use std::fmt;
use std::sync::{Arc, Mutex, Weak};

use all_is_cubes::cgmath::Vector3;
use all_is_cubes::mesh::{Texel, TextureAllocator, TextureCoordinate, TextureTile};
use all_is_cubes::space::Grid;
use all_is_cubes::util::{CustomFormat, StatusText};

use crate::octree_alloc::{Alloctree, AlloctreeHandle};
use crate::types::{AicLumBackend, LumBlockVertex};

/// Alias for the concrete type of the block texture.
pub type BlockTexture<Backend> = Texture<Backend, Dim3, SRGBA8UI>;
/// Alias for the concrete type of the block texture when bound in a luminance pipeline.
pub type BoundBlockTexture<'a, Backend> = BoundTexture<'a, Backend, Dim3, SRGBA8UI>;

/// Implementation of [`TextureAllocator`] for [`luminance`].
///
/// After any allocations, you must call [`LumAtlasAllocator::flush`] to write the
/// updates to the actual GPU texture for drawing.
pub struct LumAtlasAllocator<Backend>
where
    Backend: AicLumBackend,
{
    pub texture: BlockTexture<Backend>,
    /// Note on lock ordering: Do not attempt to acquire this lock while a tile's lock is held.
    backing: Arc<Mutex<AllocatorBacking>>,
    in_use: Vec<Weak<Mutex<TileBacking>>>,
}
/// Texture tile handle used by [`LumAtlasAllocator`].
///
/// This is public out of necessity but should not generally need to be used.
#[derive(Clone, Debug)]
pub struct LumAtlasTile {
    /// Translation of the requested grid to the actual region within the texture.
    /// (This is always integer but will always be used in a float computation.)
    offset: Vector3<TextureCoordinate>,
    /// Scale factor to convert from texel grid coordinates (`backing.atlas_grid` and
    /// `offset`) to GPU texture coordinates where 0.0 and 1.0 are the final size.
    /// In other words, the reciprocal of the overall texture size. This does not
    /// vary per-tile but is stored here for convenience of implementing [`TextureTile`].
    scale: TextureCoordinate,
    /// Actual storage and metadata about the tile; may be updated as needed by the
    /// allocator to grow the texture.
    ///
    /// Note on lock ordering: Do not attempt to acquire the allocator's lock while this
    /// lock is held.
    backing: Arc<Mutex<TileBacking>>,
}
#[derive(Debug)]
struct TileBacking {
    /// Allocator information, and the region of the atlas texture which this tile owns.
    ///
    /// Property: `self.handle.unwrap().allocation.volume() == self.data.len()`.
    handle: Option<AlloctreeHandle>,
    /// Texture data (that might not be sent to the GPU yet).
    data: Option<Box<[Texel]>>,
    /// Whether the data has changed so that we need to send it to the GPU on next
    /// [`LumAtlasAllocator::flush`].
    dirty: bool,
    /// Reference to the allocator so we can coordinate.
    /// Weak because if the allocator is dropped, nobody cares.
    allocator: Weak<Mutex<AllocatorBacking>>,
}
/// Data shared by [`LumAtlasAllocator`] and all its [`LumAtlasTile`]s.
#[derive(Debug)]
struct AllocatorBacking {
    /// Whether flush needs to do anything.
    dirty: bool,
    alloctree: Alloctree,
}

impl<Backend: AicLumBackend> LumAtlasAllocator<Backend> {
    pub fn new<C>(context: &mut C) -> Result<Self, TextureError>
    where
        C: GraphicsContext<Backend = Backend>,
        Backend: AicLumBackend,
    {
        let alloctree = Alloctree::new(8);

        let texture = context.new_texture(
            alloctree.bounds().unsigned_size().into(),
            Sampler {
                wrap_s: Wrap::ClampToEdge,
                wrap_t: Wrap::ClampToEdge,
                wrap_r: Wrap::ClampToEdge,
                mag_filter: MagFilter::Nearest,
                min_filter: MinFilter::Nearest,
                ..Sampler::default()
            },
            TexelUpload::reserve(0),
        )?;
        // TODO: distinguish between "logic error" errors and "out of texture memory" errors...though it doesn't matter much until we have atlas resizing reallocations.

        Ok(Self {
            texture,
            backing: Arc::new(Mutex::new(AllocatorBacking {
                dirty: false,
                alloctree,
            })),
            in_use: Vec::new(),
        })
    }

    /// Copy the texels of all modified and still-referenced tiles to the GPU's texture.
    ///
    /// If any errors prevent complete flushing, it will be attempted again on the next
    /// call.
    pub fn flush(&mut self) -> Result<AtlasFlushInfo, TextureError> {
        let mut allocator_backing = self.backing.lock().unwrap();
        if !allocator_backing.dirty {
            return Ok(AtlasFlushInfo {
                flushed: 0,
                in_use_tiles: self.in_use.len(),
                in_use_texels: allocator_backing.alloctree.occupied_volume(),
                capacity_texels: allocator_backing.alloctree.bounds().volume(),
            });
        }

        let mut count_written = 0;
        // retain() doesn't let us exit early on error, so we track any upload errors
        // separately.
        let mut error: Option<TextureError> = None;

        let texture = &mut self.texture;
        self.in_use.retain(|weak_backing| {
            // Process the non-dropped weak references
            weak_backing.upgrade().map_or(false, |strong_backing| {
                let backing: &mut TileBacking = &mut strong_backing.lock().unwrap();
                if backing.dirty && error.is_none() {
                    if let Some(data) = backing.data.as_ref() {
                        let region: Grid = backing
                            .handle
                            .as_ref()
                            .expect("can't happen: dead TileBacking")
                            .allocation;
                        match texture.upload_part(
                            region.lower_bounds().map(|c| c as u32).into(),
                            region.size().map(|c| c as u32).into(),
                            TexelUpload::levels(&[data]),
                        ) {
                            Ok(()) => {
                                // Only clear dirty flag if upload was successful.
                                backing.dirty = false;
                            }
                            Err(e) => error = Some(e),
                        }
                        count_written += 1;
                    }
                }
                true // retain in self.in_use
            })
        });

        if let Some(error) = error {
            return Err(error);
        }

        allocator_backing.dirty = false;
        Ok(AtlasFlushInfo {
            flushed: count_written,
            in_use_tiles: self.in_use.len(),
            in_use_texels: allocator_backing.alloctree.occupied_volume(),
            capacity_texels: allocator_backing.alloctree.bounds().volume(),
        })
    }

    #[allow(dead_code)]
    pub(crate) fn debug_atlas_tess<C>(&self, context: &mut C) -> Tess<Backend, LumBlockVertex>
    where
        C: GraphicsContext<Backend = Backend>,
    {
        let mut vertices = Vec::new();
        //for layer in 0..self.layer_count {
        let layer = 0;
        vertices.extend(&*LumBlockVertex::rectangle(
            // position
            Vector3::new(0.0, 0.0, 0.0),
            Vector3::new(1.0, 1.0, 0.0),
            // texture
            Vector3::new(0.0, 0.0, layer as TextureCoordinate),
            Vector3::new(1.0, 1.0, layer as TextureCoordinate),
        ));
        //}
        context
            .new_tess()
            .set_vertices(vertices)
            .set_mode(Mode::Triangle)
            .build()
            .expect("failed to construct debug tess")
    }
}

impl<Backend: AicLumBackend> TextureAllocator for LumAtlasAllocator<Backend> {
    type Tile = LumAtlasTile;

    fn allocate(&mut self, requested_grid: Grid) -> Option<LumAtlasTile> {
        let alloctree = &mut self.backing.lock().unwrap().alloctree;
        let handle = alloctree.allocate(requested_grid)?;
        let result = LumAtlasTile {
            offset: handle.offset.map(|c| c as TextureCoordinate),
            scale: (alloctree.bounds().size().x as TextureCoordinate).recip(),
            backing: Arc::new(Mutex::new(TileBacking {
                handle: Some(handle),
                data: None,
                dirty: false,
                allocator: Arc::downgrade(&self.backing),
            })),
        };
        self.in_use.push(Arc::downgrade(&result.backing));
        Some(result)
    }
}

impl TextureTile for LumAtlasTile {
    fn grid(&self) -> Grid {
        todo!()
    }

    fn grid_to_texcoord(
        &self,
        in_tile_grid: Vector3<TextureCoordinate>,
    ) -> Vector3<TextureCoordinate> {
        (in_tile_grid + self.offset) * self.scale
    }

    fn write(&mut self, data: &[Texel]) {
        // Note: acquiring the two locks separately to avoid possible deadlock
        // with another thread trying to flush() (which acquires allocator and
        // then tile locks). I believe that in all possible interleavings, the
        // worst cases are:
        //
        // * a redundant setting of the AllocatorBacking::dirty flag.
        // * this write() blocking until flush() finishes (this could be fixed with
        //   making the dirty flag a `DirtyFlag` (atomic bool based) instead of being
        //   inside the lock).
        //
        // It should always be the case that a write() then flush() will actually
        // write the data.
        let allocator_backing_ref = {
            let mut backing = self.backing.lock().unwrap();
            backing.data = Some(data.into());
            backing.dirty = true;

            backing.allocator.upgrade()
        };
        if let Some(allocator_backing_ref) = allocator_backing_ref {
            allocator_backing_ref.lock().unwrap().dirty = true;
        }
    }
}

/// Compared by reference. This definition of equality is cheaper and non-panicking
/// vs. the derived behavior of RefCell::eq which is to borrow and compare the contents.
impl PartialEq for LumAtlasTile {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.backing, &other.backing)
    }
}
impl Eq for LumAtlasTile {}

impl Drop for TileBacking {
    fn drop(&mut self) {
        if let Some(ab) = self.allocator.upgrade() {
            if let Some(handle) = self.handle.take() {
                ab.lock().unwrap().alloctree.free(handle);
            }
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct AtlasFlushInfo {
    flushed: usize,
    in_use_tiles: usize,
    in_use_texels: usize,
    capacity_texels: usize,
}

impl CustomFormat<StatusText> for AtlasFlushInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>, _: StatusText) -> fmt::Result {
        write!(
            fmt,
            "Textures: {} tiles, {} texels ({}%) used, {:2} flushed",
            self.in_use_tiles,
            self.in_use_texels,
            (self.in_use_texels as f32 / self.capacity_texels as f32 * 100.0).ceil() as usize,
            self.flushed,
        )
    }
}
