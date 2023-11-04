//! Block texture atlas management: provides [`AtlasAllocator`], the
//! [`texture::Allocator`] implementation for use with [`wgpu`].

#![allow(clippy::arc_with_non_send_sync)] // wgpu on wasm

use std::sync::{Arc, Mutex, Weak};

use all_is_cubes::block::Evoxel;
use all_is_cubes::content::palette;
use all_is_cubes::euclid::Translation3D;
use all_is_cubes::math::{GridAab, GridCoordinate, VectorOps, Vol};
use all_is_cubes::time;
use all_is_cubes_mesh::texture;

use crate::in_wgpu::glue::{size_vector_to_extent, write_texture_by_aab};
use crate::in_wgpu::vertex::{AtlasTexel, FixTexCoord, TexPoint};
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
    offset: Translation3D<GridCoordinate, texture::TexelUnit, AtlasTexel>,
    /// Actual storage and metadata about the tile; may be updated as needed by the
    /// allocator to grow the texture.
    ///
    /// Note on lock ordering: Do not attempt to acquire the allocator's lock while this
    /// lock is held.
    backing: Arc<Mutex<TileBacking>>,
}

/// Internal, weak-referencing version of [`AtlasTile`].
#[derive(Debug)]
struct WeakTile {
    /// Bounds of the allocation in the atlas.
    allocated_bounds: GridAab,
    backing: Weak<Mutex<TileBacking>>,
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
    /// sRGB texture data (that might not be sent to the GPU yet).
    data: Option<Box<[[u8; 4]]>>,
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
    in_use: Vec<WeakTile>,

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
        // Default size of 2⁵ = 32 holding up to 8 × 16³ block textures.
        let alloctree = Alloctree::new(5);

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
    pub fn flush<I: time::Instant>(
        &self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> (Arc<wgpu::TextureView>, BlockTextureInfo) {
        let start_time = I::now();
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

        // TODO: On WebGL, copying from the old texture silently does nothing. We should
        // report a `wgpu` bug, but for now, avoid it by re-writing everything
        // instead of copying. When the bug is fixed, delete this variable entirely.
        let copy_everything_anyway = cfg!(target_family = "wasm") && old_texture.is_some();

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

            // Copy the old texture into the low corner of the new texture, so existing
            // data is preserved. (Note that this assumes that the new texture is larger,
            // which is currently always true. Shrinking the texture would require also
            // defragmenting it.)
            if let Some(old_texture) = old_texture.filter(|_| !copy_everything_anyway) {
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

        // Process tiles needing updates or deallocation.
        let mut count_written = 0;
        if backing.dirty {
            backing.in_use.retain(|weak_tile| {
                // Process the non-dropped weak references
                match weak_tile.backing.upgrade() {
                    Some(strong_ref) => {
                        let backing: &mut TileBacking = &mut strong_ref.lock().unwrap();
                        if backing.dirty || copy_everything_anyway {
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
                    }
                    None => {
                        // No strong references to the tile remain.
                        // Overwrite it to mark stale data.
                        // TODO: This is inefficient but we want to keep it at least until fixing
                        // <https://github.com/kpreid/all-is-cubes/issues/378>, at which point we
                        // might reasonably disable it.
                        let region = weak_tile.allocated_bounds;
                        write_texture_by_aab(
                            queue,
                            texture,
                            region,
                            // TODO: keep a preallocated GPU buffer instead
                            &vec![palette::UNALLOCATED_TEXELS_ERROR.to_srgb8(); region.volume()],
                        );

                        false // discard from self.in_use
                    }
                }
            });
        }

        backing.dirty = false;
        (
            texture_view.clone(),
            BlockTextureInfo {
                flushed: count_written,
                flush_time: I::now().saturating_duration_since(start_time),
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

        // If alloctree grows, the next flush() will take care of reallocating the texture.
        let handle = allocator_backing
            .alloctree
            .allocate_with_growth(requested_bounds)?;
        let allocated_bounds = handle.allocation;

        let result = AtlasTile {
            requested_bounds,
            offset: Translation3D::from_untyped(&handle.offset),
            backing: Arc::new(Mutex::new(TileBacking {
                handle: Some(handle),
                data: None,
                dirty: false,
                allocator: Arc::downgrade(&self.backing),
            })),
        };
        allocator_backing.in_use.push(WeakTile {
            allocated_bounds,
            backing: Arc::downgrade(&result.backing),
        });
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

    fn write(&mut self, data: Vol<&[Evoxel]>) {
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
            let buffer = backing.data.get_or_insert_with(|| {
                vec![[0, 0, 0, 0]; self.bounds().volume()].into_boxed_slice()
            });
            texture::copy_voxels_into_xmaj_texture(data, buffer);
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

    fn grid_to_texcoord(&self, in_tile_grid: texture::TilePoint) -> Self::Point {
        // TODO: assert in bounds, just in case
        let float_point =
            (in_tile_grid + self.tile.offset.to_vector().map(|c| c as f32)).cast_unit();
        float_point.map(FixTexCoord::from_float)
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
