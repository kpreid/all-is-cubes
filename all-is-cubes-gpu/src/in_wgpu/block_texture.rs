//! Block texture atlas management: provides [`AtlasAllocator`], the
//! [`texture::Allocator`] implementation for use with [`wgpu`].

#![allow(clippy::arc_with_non_send_sync)] // wgpu on wasm

use std::sync::{Arc, Mutex, Weak};

use instant::Instant;

use all_is_cubes::cgmath::{Point3, Vector3};
use all_is_cubes::math::GridAab;
use all_is_cubes_mesh::texture;

use crate::in_wgpu::glue::{size_vector_to_extent, write_texture_by_aab};
use crate::in_wgpu::vertex::TexPoint;
use crate::octree_alloc::{Alloctree, AlloctreeHandle};
use crate::BlockTextureInfo;

/// Implementation of [`texture::Allocator`] for [`wgpu`].
///
/// After any allocations, you must call [`AtlasAllocator::flush()`] to write the
/// new texels to the actual GPU texture for drawing. Existing allocations will remain
/// valid regardless.
#[derive(Clone, Debug)]
pub struct AtlasAllocator {
    /// Note on lock ordering: Do not attempt to acquire this lock while a tile's lock is held.
    backing: Arc<Mutex<AllocatorBacking>>,
}

/// Texture tile handle used by [`AtlasAllocator`].
///
/// This is public out of necessity but should not generally need to be used.
#[derive(Clone, Debug)]
pub struct AtlasTile {
    /// Original bounds as requested (not texture coordinates).
    requested_bounds: GridAab,
    /// Translation of the requested bounds to the actual region within the texture.
    offset: Vector3<i32>,
    /// Actual storage and metadata about the tile; may be updated as needed by the
    /// allocator to grow the texture.
    ///
    /// Note on lock ordering: Do not attempt to acquire the allocator's lock while this
    /// lock is held.
    backing: Arc<Mutex<TileBacking>>,
}

/// Texture plane handle used by [`AtlasAllocator`].
///
/// This is public out of necessity but should not generally need to be used.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AtlasPlane {
    tile: AtlasTile,
    requested_bounds: GridAab,
}

#[derive(Debug)]
struct TileBacking {
    /// Allocator information, and the region of the atlas texture which this tile owns.
    ///
    /// Property: `self.handle.unwrap().allocation.volume() == self.data.len()`.
    handle: Option<AlloctreeHandle>,
    /// Texture data (that might not be sent to the GPU yet).
    data: Option<Box<[texture::Texel]>>,
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
    /// Tracks which regions of the texture are free or allocated.
    alloctree: Alloctree,

    /// Whether flush needs to do anything.
    dirty: bool,

    /// Weak references to every tile.
    /// This is used to gather all data that needs to be flushed (written to the GPU
    /// texture).
    in_use: Vec<Weak<Mutex<TileBacking>>>,

    /// Debug label for the GPU texture resource.
    texture_label: String,

    /// GPU texture. [`None`] if no texture has yet been created.
    ///
    /// The texture view is wrapped in [`Arc`] so that it can be used by drawing code
    /// without holding the lock around this.
    texture: Option<(wgpu::Texture, Arc<wgpu::TextureView>)>,
}

impl AtlasAllocator {
    pub fn new(label_prefix: &str) -> Self {
        // TODO: When we have reallocation implemented, be willing to use
        // a smaller size to start, to save GPU memory.
        let alloctree = Alloctree::new(8);

        Self {
            backing: Arc::new(Mutex::new(AllocatorBacking {
                alloctree,
                dirty: false,
                in_use: Vec::new(),
                texture_label: format!("{label_prefix} block texture"),
                texture: None,
            })),
        }
    }

    /// Copy the texels of all modified and still-referenced tiles to the GPU's texture.
    pub fn flush(
        &self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> (Arc<wgpu::TextureView>, BlockTextureInfo) {
        let start_time = Instant::now();
        let backing = &mut *self.backing.lock().unwrap();

        let needed_texture_size = size_vector_to_extent(backing.alloctree.bounds().size());

        // If we have a texture, check if it is the right size.
        let old_texture: Option<wgpu::Texture> = if matches!(
            backing.texture,
            Some((ref texture, _))
            if texture.size() != needed_texture_size
        ) {
            backing.texture.take().map(|(texture, _)| texture)
        } else {
            None
        };

        // Allocate a texture if needed.
        let (texture, texture_view) = backing.texture.get_or_insert_with(|| {
            // TODO: Add an error scope so we can detect and recover from errors,
            // including out-of-memory.
            let texture = device.create_texture(&wgpu::TextureDescriptor {
                size: needed_texture_size,
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D3,
                format: wgpu::TextureFormat::Rgba8UnormSrgb,
                view_formats: &[],
                usage: wgpu::TextureUsages::TEXTURE_BINDING
                    | wgpu::TextureUsages::COPY_SRC
                    | wgpu::TextureUsages::COPY_DST,
                label: Some(&backing.texture_label),
            });

            if let Some(old_texture) = old_texture {
                let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some(&format!(
                        "{} copy old to new texture",
                        backing.texture_label
                    )),
                });
                encoder.copy_texture_to_texture(
                    wgpu::ImageCopyTexture {
                        texture: &old_texture,
                        mip_level: 0,
                        origin: wgpu::Origin3d::ZERO,
                        aspect: wgpu::TextureAspect::default(),
                    },
                    wgpu::ImageCopyTexture {
                        texture: &texture,
                        mip_level: 0,
                        origin: wgpu::Origin3d::ZERO,
                        aspect: wgpu::TextureAspect::default(),
                    },
                    old_texture.size(),
                );
                queue.submit([encoder.finish()]);
            }

            let texture_view =
                Arc::new(texture.create_view(&wgpu::TextureViewDescriptor::default()));

            (texture, texture_view)
        });

        let mut count_written = 0;
        if backing.dirty {
            backing.in_use.retain(|weak_backing| {
                // Process the non-dropped weak references
                weak_backing.upgrade().map_or(false, |strong_backing| {
                    let backing: &mut TileBacking = &mut strong_backing.lock().unwrap();
                    if backing.dirty {
                        if let Some(data) = backing.data.as_ref() {
                            let region: GridAab = backing
                                .handle
                                .as_ref()
                                .expect("can't happen: dead TileBacking")
                                .allocation;

                            write_texture_by_aab(queue, texture, region, data);
                            backing.dirty = false;
                            count_written += 1;
                        }
                    }
                    true // retain in self.in_use
                })
            });
        }

        backing.dirty = false;
        (
            texture_view.clone(),
            BlockTextureInfo {
                flushed: count_written,
                flush_time: Instant::now().duration_since(start_time),
                in_use_tiles: backing.in_use.len(),
                in_use_texels: backing.alloctree.occupied_volume(),
                capacity_texels: backing.alloctree.bounds().volume(),
            },
        )
    }

    /// Returns a `wgpu::TextureView` that is current as of the last `flush()`, or
    /// `None` if `flush()` has not been called.
    pub fn current_texture_view(&self) -> Option<Arc<wgpu::TextureView>> {
        self.backing
            .lock()
            .unwrap()
            .texture
            .as_ref()
            .map(|(_, texture_view)| texture_view.clone())
    }
}

impl texture::Allocator for AtlasAllocator {
    type Tile = AtlasTile;
    type Point = TexPoint;

    fn allocate(&self, requested_bounds: GridAab) -> Option<AtlasTile> {
        let mut allocator_backing = self.backing.lock().unwrap();
        let handle = allocator_backing.alloctree.allocate(requested_bounds)?;
        let result = AtlasTile {
            requested_bounds,
            offset: handle.offset,
            backing: Arc::new(Mutex::new(TileBacking {
                handle: Some(handle),
                data: None,
                dirty: false,
                allocator: Arc::downgrade(&self.backing),
            })),
        };
        allocator_backing
            .in_use
            .push(Arc::downgrade(&result.backing));
        Some(result)
    }
}

impl texture::Tile for AtlasTile {
    type Point = TexPoint;
    type Plane = AtlasPlane;
    const REUSABLE: bool = true;

    fn bounds(&self) -> GridAab {
        self.requested_bounds
    }

    fn slice(&self, requested_bounds: GridAab) -> Self::Plane {
        texture::validate_slice(self.requested_bounds, requested_bounds);
        AtlasPlane {
            tile: self.clone(),
            requested_bounds,
        }
    }

    fn write(&mut self, data: &[texture::Texel]) {
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
/// vs. the derived behavior of [`RefCell::eq`] which is to borrow and compare the contents.
impl PartialEq for AtlasTile {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.backing, &other.backing)
    }
}
impl Eq for AtlasTile {}

impl texture::Plane for AtlasPlane {
    type Point = TexPoint;

    fn grid_to_texcoord(&self, in_tile_grid: Point3<f32>) -> Self::Point {
        // TODO: assert in bounds, just in case
        in_tile_grid + self.tile.offset.map(|c| c as f32)
    }
}

impl Drop for TileBacking {
    fn drop(&mut self) {
        let Some(backing_lock) = self.allocator.upgrade() else {
            // Texture is gone, so nothing to do
            return;
        };

        // Take allocation handle out of self.
        let Some(handle) = self.handle.take() else {
            // Shouldn't happen but if it does, don't panic
            return;
        };

        let Ok(mut backing) = backing_lock.lock() else {
            // Mutex is poisoned, which means the allocator state is corrupt,
            // so give up.
            return;
        };

        backing.alloctree.free(handle);
    }
}
