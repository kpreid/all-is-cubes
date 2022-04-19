// Copyright 2020-2022 Kevin Reid under the terms of the MIT License as detailed
// in the accompanying file README.md or <https://opensource.org/licenses/MIT>.

//! Block texture atlas management: provides [`AtlasAllocator`], the
//! [`TextureAllocator`] implementation for use with [`wgpu`].

use std::sync::{Arc, Mutex, Weak};

use instant::Instant;

use all_is_cubes::cgmath::Vector3;
use all_is_cubes::content::palette;
use all_is_cubes::mesh::{Texel, TextureAllocator, TextureCoordinate, TextureTile};
use all_is_cubes::space::Grid;

use crate::in_wgpu::glue::{size_vector_to_extent, write_texture_by_grid};
use crate::octree_alloc::{Alloctree, AlloctreeHandle};
use crate::BlockTextureInfo;
use crate::GraphicsResourceError;

/// Alias for the concrete type of the block texture.
type BlockTexture = wgpu::Texture;

/// Implementation of [`TextureAllocator`] for [`wgpu`].
///
/// After any allocations, you must call [`AtlasAllocator::flush`] to write the
/// updates to the actual GPU texture for drawing.
#[derive(Debug)]
pub struct AtlasAllocator {
    // GPU resources
    texture: BlockTexture,
    pub texture_view: wgpu::TextureView,
    pub sampler: wgpu::Sampler,

    // CPU allocation tracking
    /// Note on lock ordering: Do not attempt to acquire this lock while a tile's lock is held.
    backing: Arc<Mutex<AllocatorBacking>>,
    in_use: Vec<Weak<Mutex<TileBacking>>>,
}

/// Texture tile handle used by [`AtlasAllocator`].
///
/// This is public out of necessity but should not generally need to be used.
#[derive(Clone, Debug)]
pub struct AtlasTile {
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
    /// [`AtlasAllocator::flush`].
    dirty: bool,
    /// Reference to the allocator so we can coordinate.
    /// Weak because if the allocator is dropped, nobody cares.
    allocator: Weak<Mutex<AllocatorBacking>>,
}
/// Data shared by [`AtlasAllocator`] and all its [`AtlasTile`]s.
#[derive(Debug)]
struct AllocatorBacking {
    /// Whether flush needs to do anything.
    dirty: bool,
    alloctree: Alloctree,
}

impl AtlasAllocator {
    pub fn new(
        label_prefix: &str,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> Result<Self, GraphicsResourceError> {
        // TODO: When we have reallocation implemented, be willing to use
        // a smaller texture to start, to save GPU memory.
        let alloctree = Alloctree::new(8);

        // TODO: How do we check for insufficient memory?
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            size: size_vector_to_extent(alloctree.bounds().size()),
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D3,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            label: Some(&format!("{label_prefix} block texture")),
        });

        // Fill texture with a marker color, so it isn't transparent.
        // (If we didn't, wgpu would leave it as [0, 0, 0, 0].)
        // This is mostly useful for debugging since the texture allocation
        // procedure should never actually let unwritten texels appear.
        write_texture_by_grid(
            queue,
            &texture,
            alloctree.bounds(),
            &vec![palette::UNPAINTED_TEXTURE_FALLBACK.to_srgb8(); alloctree.bounds().volume()],
        );

        let texture_view = texture.create_view(&wgpu::TextureViewDescriptor::default());

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some(&format!("{label_prefix} block sampler")),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            mipmap_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        Ok(Self {
            texture,
            texture_view,
            sampler,
            backing: Arc::new(Mutex::new(AllocatorBacking {
                dirty: false,
                alloctree,
            })),
            in_use: Vec::new(),
        })
    }

    /// Copy the texels of all modified and still-referenced tiles to the GPU's texture.
    pub fn flush(&mut self, queue: &wgpu::Queue) -> BlockTextureInfo {
        let start_time = Instant::now();
        let mut allocator_backing = self.backing.lock().unwrap();

        let mut count_written = 0;
        if allocator_backing.dirty {
            self.in_use.retain(|weak_backing| {
                // Process the non-dropped weak references
                weak_backing.upgrade().map_or(false, |strong_backing| {
                    let backing: &mut TileBacking = &mut strong_backing.lock().unwrap();
                    if backing.dirty {
                        if let Some(data) = backing.data.as_ref() {
                            let region: Grid = backing
                                .handle
                                .as_ref()
                                .expect("can't happen: dead TileBacking")
                                .allocation;

                            write_texture_by_grid(queue, &self.texture, region, data);
                            backing.dirty = false;
                            count_written += 1;
                        }
                    }
                    true // retain in self.in_use
                })
            });
        }

        allocator_backing.dirty = false;
        BlockTextureInfo {
            flushed: count_written,
            flush_time: Instant::now().duration_since(start_time),
            in_use_tiles: self.in_use.len(),
            in_use_texels: allocator_backing.alloctree.occupied_volume(),
            capacity_texels: allocator_backing.alloctree.bounds().volume(),
        }
    }
}

impl TextureAllocator for AtlasAllocator {
    type Tile = AtlasTile;

    fn allocate(&mut self, requested_grid: Grid) -> Option<AtlasTile> {
        let alloctree = &mut self.backing.lock().unwrap().alloctree;
        let handle = alloctree.allocate(requested_grid)?;
        let result = AtlasTile {
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

impl TextureTile for AtlasTile {
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
impl PartialEq for AtlasTile {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.backing, &other.backing)
    }
}
impl Eq for AtlasTile {}

impl Drop for TileBacking {
    fn drop(&mut self) {
        if let Some(ab) = self.allocator.upgrade() {
            if let Some(handle) = self.handle.take() {
                ab.lock().unwrap().alloctree.free(handle);
            }
        }
    }
}
