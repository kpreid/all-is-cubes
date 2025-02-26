//! Block texture atlas management: provides [`AtlasAllocator`], the
//! [`texture::Allocator`] implementation for use with [`wgpu`].

#![allow(clippy::arc_with_non_send_sync, reason = "wgpu on wasm")]

use alloc::boxed::Box;
use alloc::format;
use alloc::string::String;
use alloc::sync::{Arc, Weak};
use alloc::vec;
use alloc::vec::Vec;
use std::sync::{Mutex, MutexGuard};

#[cfg(feature = "rerun")]
use itertools::iproduct;

use all_is_cubes::block::Evoxel;
use all_is_cubes::content::palette;
use all_is_cubes::euclid::{Box3D, Translation3D};
use all_is_cubes::math::{Cube, GridAab, GridCoordinate, VectorOps as _, Vol};
#[cfg(feature = "rerun")]
use all_is_cubes::rerun_glue as rg;
use all_is_cubes::time;
use all_is_cubes_mesh::texture::{self, Channels};

use crate::in_wgpu::glue::{size3d_to_extent, write_texture_by_aab};
use crate::in_wgpu::vertex::{AtlasTexel, FixTexCoord, TexPoint};
use crate::octree_alloc::{Alloctree, AlloctreeHandle};
use crate::{BlockTextureInfo, Identified, Msw};

//------------------------------------------------------------------------------------------------//
// Types

/// Implementation of [`texture::Allocator`] for [`wgpu`].
///
/// After any allocations, you must call [`AtlasAllocator::flush()`] to write the
/// new texels to the actual GPU texture for drawing. Existing allocations will remain
/// valid regardless.
#[derive(Clone, Debug)]
pub struct AtlasAllocator {
    /// Mutable state tracking for texture and all tiles containing reflectance data only.
    ///
    /// Note on lock ordering: Do not attempt to acquire this lock while a tile's lock is held.
    reflectance_backing: Arc<Mutex<AllocatorBacking>>,

    reflectance_and_emission_backing: Arc<Mutex<AllocatorBacking>>,
}

/// Texture tile handle used by [`AtlasAllocator`].
///
/// This is public out of necessity but should not generally need to be used.
#[derive(Clone, Debug)]
pub struct AtlasTile {
    /// Original bounds as requested (not texture coordinates).
    requested_bounds: GridAab,
    /// Identifies both the requested channels and the vertex attribute to use to select the
    /// matching texture.
    channels: Channels,
    /// Translation of the requested bounds to the actual region within the texture.
    offset: Translation3D<GridCoordinate, texture::TexelUnit, AtlasTexel>,
    /// Actual storage and metadata about the tile; may be updated as needed by the
    /// allocator to grow the texture.
    ///
    /// Note on lock ordering: Do not attempt to acquire the allocator's lock while this
    /// lock is held.
    backing: Arc<Mutex<TileBacking>>,
}

pub(crate) struct BlockTextureViews {
    pub g0_reflectance: Arc<Identified<wgpu::TextureView>>,
    pub g1_reflectance: Arc<Identified<wgpu::TextureView>>,
    pub g1_emission: Arc<Identified<wgpu::TextureView>>,
}

/// Internal, weak-referencing version of [`AtlasTile`].
#[derive(Debug)]
struct WeakTile {
    /// Bounds of the allocation in the atlas.
    allocated_bounds: Box3D<u16, AtlasTexel>,
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
    handle: Option<AlloctreeHandle<AtlasTexel>>,

    /// sRGB reflectance data (that might not be sent to the GPU yet, or reused upon resize).
    /// Is `Some` if `write()` has been called.
    reflectance: Option<Box<[[u8; 4]]>>,

    /// sRGB emission data (that might not be sent to the GPU yet, or reused upon resize).
    /// Is `Some` if `write()` has been called and there is an emission channel.
    emission: Option<Box<[[u8; 4]]>>,

    /// Whether the data has changed so that we need to send it to the GPU on next
    /// [`AtlasAllocator::flush`].
    dirty: bool,

    /// Reference to the allocator so we can coordinate.
    /// Weak because if the allocator is dropped, nobody cares.
    allocator: Weak<Mutex<AllocatorBacking>>,

    /// If we are logging atlas content to Rerun, then this is where we log it.
    #[cfg(feature = "rerun")]
    rerun_destination: rg::Destination,
}

/// Data shared by [`AtlasAllocator`] and all its [`AtlasTile`]s.
#[derive(Debug)]
struct AllocatorBacking {
    /// Tracks which regions of the texture are free or allocated.
    alloctree: Alloctree<AtlasTexel>,

    /// log2 of the maximum texture size we may consider growing to.
    maximum_texture_size_exponent: u8,

    /// Whether flush needs to do anything.
    dirty: bool,

    /// Weak references to every tile.
    /// This is used to gather all data that needs to be flushed (written to the GPU
    /// texture).
    in_use: Vec<WeakTile>,

    /// Debug label for the GPU texture resource.
    texture_label: String,

    /// Channels that we should allocate or have allocated textures for.
    channels: Channels,

    /// GPU texture objects and texture views. [`None`] if `flush()` has never been called.
    textures: Option<Msw<Group<GpuTexture>>>,

    /// If we are logging the state of the texture, do it here.
    /// This is not used directly but used to create destinations for child entities
    /// for each tile.
    #[cfg(feature = "rerun")]
    rerun_destination: rg::Destination,
}

#[derive(Debug)]
struct Group<T> {
    reflectance: T,
    emission: Option<T>,
}

#[derive(Debug)]
struct GpuTexture {
    texture: wgpu::Texture,
    /// The texture view is wrapped in [`Arc`] so that it can be used by drawing code
    /// without holding the lock around this.
    texture_view: Arc<Identified<wgpu::TextureView>>,
}

//------------------------------------------------------------------------------------------------//
// Implementations

impl AtlasAllocator {
    pub fn new(label_prefix: &str, limits: &wgpu::Limits) -> Self {
        Self {
            reflectance_backing: AllocatorBacking::new(label_prefix, Channels::Reflectance, limits),
            reflectance_and_emission_backing: AllocatorBacking::new(
                label_prefix,
                Channels::ReflectanceEmission,
                limits,
            ),
        }
    }

    /// Copy the texels of all modified and still-referenced tiles to the GPU's textures.
    ///
    /// Returns `wgpu::TextureView`s for the textures, which will remain unchanged until the next
    /// time `flush()` is called.
    pub(crate) fn flush<I: time::Instant>(
        &self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> (BlockTextureViews, BlockTextureInfo) {
        let start_time = I::now();
        let (views0, info0) = AllocatorBacking::flush(&self.reflectance_backing, device, queue);
        let (views1, info1) =
            AllocatorBacking::flush(&self.reflectance_and_emission_backing, device, queue);

        (
            BlockTextureViews {
                g0_reflectance: views0.reflectance,
                g1_reflectance: views1.reflectance,
                g1_emission: views1.emission.unwrap(),
            },
            BlockTextureInfo {
                flush_time: I::now().saturating_duration_since(start_time),
                ..info0 + info1
            },
        )
    }

    #[cfg(feature = "rerun")]
    pub(crate) fn log_to_rerun(&self, destination: rg::Destination) {
        let Self {
            reflectance_backing,
            reflectance_and_emission_backing,
        } = self;
        reflectance_backing
            .lock()
            .unwrap()
            .log_to_rerun(destination.child(&rg::entity_path!["reflectance"]));
        reflectance_and_emission_backing
            .lock()
            .unwrap()
            .log_to_rerun(destination.into_child(&rg::entity_path!["reflectance_and_emission"]));
    }
}

impl texture::Allocator for AtlasAllocator {
    type Tile = AtlasTile;
    type Point = TexPoint;

    fn allocate(&self, requested_bounds: GridAab, channels: Channels) -> Option<AtlasTile> {
        assert!(!requested_bounds.is_empty());

        let backing_arc = match channels {
            Channels::Reflectance => &self.reflectance_backing,
            Channels::ReflectanceEmission => &self.reflectance_and_emission_backing,
        };
        let backing_guard = &mut *backing_arc.lock().unwrap();

        // If alloctree grows, the next flush() will take care of reallocating the texture.
        let handle = backing_guard.alloctree.allocate_with_growth(
            requested_bounds,
            backing_guard.maximum_texture_size_exponent,
        )?;
        let allocated_bounds = handle.allocation;

        // Turn the tile’s position into a entity path for it, that is unique *as long as it is
        // allocated* but will be reused if the same space is reallocated
        #[cfg(feature = "rerun")]
        let rerun_destination = backing_guard
            .rerun_destination
            .child(&rg::entity_path![format!("{:?}", handle.allocation.min)]);
        #[cfg(feature = "rerun")]
        {
            rerun_destination.log(
                &rg::entity_path!["bbox"],
                &rg::archetypes::Boxes3D::from_mins_and_sizes(
                    [handle.allocation.min.to_f32().to_array()],
                    [handle.allocation.size().to_f32().to_array()],
                )
                .with_fill_mode(rg::components::FillMode::MajorWireframe),
            );
        }

        let result = AtlasTile {
            requested_bounds,
            channels,
            // TODO: generalize Alloctree so it doesn't use Cube here and the units match automatically
            offset: Translation3D::<_, texture::TexelUnit, Cube>::identity() + handle.offset,
            backing: Arc::new(Mutex::new(TileBacking {
                handle: Some(handle),
                reflectance: None,
                emission: None,
                dirty: false,
                allocator: Arc::downgrade(backing_arc),

                #[cfg(feature = "rerun")]
                rerun_destination,
            })),
        };
        backing_guard.in_use.push(WeakTile {
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

    fn channels(&self) -> Channels {
        self.channels
    }

    #[track_caller]
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
        //   making the dirty flag a `Flag` (atomic bool based) instead of being
        //   inside the lock).
        //
        // It should always be the case that a write() then flush() will actually
        // write the data.
        let allocator_backing_ref = {
            let tile_backing = &mut *self.backing.lock().unwrap();
            let volume = self.bounds().volume().unwrap(); // TODO: tile bounds should be `Vol<(), XMaj>` and we won't need this unwrap

            texture::copy_voxels_into_xmaj_texture(
                data,
                tile_backing
                    .reflectance
                    .get_or_insert_with(|| zero_box(volume)),
                self.channels.has_emission().then(|| {
                    &mut tile_backing
                        .emission
                        .get_or_insert_with(|| zero_box(volume))[..]
                }),
            );
            tile_backing.dirty = true;

            #[cfg(feature = "rerun")]
            if tile_backing.rerun_destination.is_enabled() {
                let mut points_for_rerun = Vec::new();
                let mut colors_for_rerun = Vec::new();
                let mut radii_for_rerun = Vec::new();
                let region: Box3D<_, _> = self
                    .offset
                    .transform_box3d(&Box3D::from(self.bounds()).cast_unit());

                points_for_rerun.extend(
                    iproduct!(region.z_range(), region.y_range(), region.x_range()).map(
                        |(z, y, x)| {
                            rg::components::Position3D::new(
                                x as f32 + 0.5,
                                y as f32 + 0.5,
                                z as f32 + 0.5,
                            )
                        },
                    ),
                );
                colors_for_rerun.extend(
                    tile_backing
                        .reflectance
                        .as_ref()
                        .unwrap()
                        .iter()
                        .map(|&[r, g, b, _a]| rg::components::Color::from_rgb(r, g, b)),
                );
                radii_for_rerun.extend(tile_backing.reflectance.as_ref().unwrap().iter().map(
                    |&[_, _, _, alpha]| {
                        rg::components::Radius::new_scene_units(if alpha == 0 { 0.1 } else { 0.6 })
                    },
                ));
                tile_backing.rerun_destination.log(
                    &rg::entity_path![],
                    &rg::archetypes::Points3D::new(points_for_rerun)
                        .with_colors(colors_for_rerun)
                        .with_radii(radii_for_rerun),
                );
            }

            tile_backing.allocator.upgrade()
        };

        if let Some(allocator_backing_ref) = allocator_backing_ref {
            allocator_backing_ref.lock().unwrap().dirty = true;
        }
    }
}

impl AllocatorBacking {
    fn new(label_prefix: &str, channels: Channels, limits: &wgpu::Limits) -> Arc<Mutex<Self>> {
        let maximum_texture_size_exponent = limits
            .max_texture_dimension_3d
            // Kludge: Chrome WebGPU fails if the buffer size is exceeded, as of 129.0.6668.101,
            // even though we’re not making any such buffer, only a texture.
            // Could not reproduce standalone.
            .min(u32::try_from(cube_root_u64(limits.max_buffer_size / 4)).unwrap_or(u32::MAX))
            .ilog2()
            .try_into()
            .unwrap_or(u8::MAX);

        Arc::new(Mutex::new(AllocatorBacking {
            // Default size of 2⁵ = 32 holding up to 8 × 16³ block textures.
            alloctree: Alloctree::new(5),
            maximum_texture_size_exponent,
            dirty: false,
            in_use: Vec::new(),
            channels,
            texture_label: format!("{label_prefix} block {channels:?} texture"),
            textures: None,

            #[cfg(feature = "rerun")]
            rerun_destination: rg::Destination::default(),
        }))
    }

    fn flush(
        backing_mutex: &Mutex<Self>,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> (Group<Arc<Identified<wgpu::TextureView>>>, BlockTextureInfo) {
        // TODO: Consider replacing all the queued texture writes and copies with `StagingBelt`.
        // Will need to teach `StagingBelt` about copying to textures.

        let mut backing_lock_guard = backing_mutex.lock().unwrap();
        let backing = &mut *backing_lock_guard;

        let needed_texture_size = size3d_to_extent(backing.alloctree.bounds().size());
        // Note: We have the Alloctree ensure that it does not exceed the device’s texture size
        // limit, so needed_texture_size will not be too big.
        // However, there is no handling of if texture allocation fails; that would require
        // using an error scope and being able to recover asynchronously from the failed attempt.

        // If we have textures already, check if they are the right size.
        let old_textures: Option<Msw<Group<_>>> = if matches!(
            backing.textures.as_deref(),
            Some(Group { reflectance: GpuTexture { texture, .. }, .. })
            if texture.size() != needed_texture_size
        ) {
            backing.textures.take()
        } else {
            None
        };

        // TODO: On WebGL, copying from the old texture silently does nothing. We should
        // report a `wgpu` bug, but for now, avoid it by re-writing everything
        // instead of copying. When the bug is fixed, delete this variable entirely.
        let copy_everything_anyway = cfg!(target_family = "wasm") && old_textures.is_some();

        // Allocate a texture if needed.
        let textures: &mut Msw<Group<GpuTexture>> = backing.textures.get_or_insert_with(|| {
            // TODO: Add an error scope so we can detect and recover from errors,
            // including out-of-memory.
            let new_textures = Group {
                reflectance: GpuTexture::new(
                    device,
                    needed_texture_size,
                    wgpu::TextureFormat::Rgba8UnormSrgb,
                    &format!("{} reflectance", backing.texture_label),
                ),
                emission: backing.channels.has_emission().then(|| {
                    GpuTexture::new(
                        device,
                        needed_texture_size,
                        wgpu::TextureFormat::Rgba8UnormSrgb, // TODO: use HDR format
                        &format!("{} emission", backing.texture_label),
                    )
                }),
            };

            // Copy the old texture into the low corner of the new texture, so existing
            // data is preserved. (Note that this assumes that the new texture is larger,
            // which is currently always true. Shrinking the texture would require also
            // defragmenting it.)
            if let Some(old_textures) = old_textures.filter(|_| !copy_everything_anyway) {
                let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
                    label: Some(&format!(
                        "{} copy old to new texture",
                        backing.texture_label
                    )),
                });
                fn copy(encoder: &mut wgpu::CommandEncoder, old: &GpuTexture, new: &GpuTexture) {
                    encoder.copy_texture_to_texture(
                        old.texture.as_image_copy(),
                        new.texture.as_image_copy(),
                        old.texture.size(),
                    );
                }
                copy(
                    &mut encoder,
                    &old_textures.reflectance,
                    &new_textures.reflectance,
                );
                if let (Some(oe), Some(ne)) = (&old_textures.emission, &new_textures.emission) {
                    copy(&mut encoder, oe, ne);
                }

                // TODO(efficiency): Eliminate this separated submit, without breaking things
                // by reordering the copy_texture_to_texture() after the following write_texture()s.
                queue.submit([encoder.finish()]);
            }

            Msw::new(new_textures)
        });

        // Process tiles needing updates or deallocation.
        let mut count_written = 0;
        let mut deferred_tile_drops: Vec<Arc<Mutex<TileBacking>>> = Vec::new();
        if backing.dirty {
            backing.in_use.retain(|weak_tile| {
                // Process the non-dropped weak references
                match weak_tile.backing.upgrade() {
                    Some(strong_ref) => {
                        {
                            let tile_backing: &mut TileBacking = &mut strong_ref.lock().unwrap();
                            if tile_backing.dirty || copy_everything_anyway {
                                let region: Box3D<u32, AtlasTexel> = tile_backing
                                    .handle
                                    .as_ref()
                                    .expect("can't happen: dead TileBacking")
                                    .allocation
                                    .map(u32::from);

                                if let Some(data) = tile_backing.reflectance.as_ref() {
                                    write_texture_by_aab(
                                        queue,
                                        &textures.reflectance.texture,
                                        region,
                                        data,
                                    );
                                    // If we don't have reflectance then the tile was never written
                                    // so we have nothing to flush
                                    count_written += 1;
                                }
                                if let (Some(data), Some(gtexture)) =
                                    (tile_backing.emission.as_ref(), &textures.emission)
                                {
                                    write_texture_by_aab(queue, &gtexture.texture, region, data);
                                    // If we don't have reflectance then the tile was never written
                                    // so we have nothing to flush
                                    count_written += 1;
                                }

                                tile_backing.dirty = false;
                            }
                        }

                        // The other tile references might have been dropped by another thread
                        // in the time since we upgraded. Therefore, defer the drop of the tile
                        // until we've released the allocator backing lock, to avoid deadlock.
                        deferred_tile_drops.push(strong_ref);

                        true // retain in self.in_use
                    }
                    None => {
                        // No strong references to the tile remain.
                        // Overwrite it to mark stale data.
                        // TODO: This is inefficient but we want to keep it at least until fixing
                        // <https://github.com/kpreid/all-is-cubes/issues/378>, at which point we
                        // might reasonably disable it.
                        let region = weak_tile.allocated_bounds.map(u32::from);

                        // TODO: keep a preallocated GPU buffer instead
                        let data = vec![
                            palette::UNALLOCATED_TEXELS_ERROR.to_srgb8();
                            region.volume() as usize
                        ];

                        write_texture_by_aab(queue, &textures.reflectance.texture, region, &data);
                        if let Some(t) = &textures.emission {
                            write_texture_by_aab(queue, &t.texture, region, &data);
                        }

                        false // discard from self.in_use
                    }
                }
            });

            backing.dirty = false;
        }

        let output = (
            Group {
                reflectance: textures.reflectance.texture_view.clone(),
                emission: textures.emission.as_ref().map(|t| t.texture_view.clone()),
            },
            BlockTextureInfo {
                flushed: count_written,
                flush_time: time::Duration::ZERO,
                in_use_tiles: backing.in_use.len(),
                texels: backing.alloctree.info(),
            },
        );

        drop::<MutexGuard<'_, _>>(backing_lock_guard);
        // Now that we no longer hold the allocator backing lock, drop the tiles which might
        // want to acquire that lock on drop.
        drop(deferred_tile_drops);

        output
    }

    /// Activate logging of all texels to Rerun as a point cloud.
    /// Does not take effect until the next [`Self::flush()`].
    #[cfg(feature = "rerun")]
    pub(crate) fn log_to_rerun(&mut self, destination: rg::Destination) {
        // Logging an invalid transform signals this should be a disconnected coordinate system.
        // We're supposed to use an explicit blueprint instead, but those aren't available from
        // Rust yet.
        // <https://rerun.io/docs/reference/migration/migration-0-21#disconnectedspace-archetypecomponent-deprecated>
        destination.log_static(
            &rg::entity_path![],
            &rg::archetypes::Transform3D::from_scale(0.0),
        );
        destination.log(&rg::entity_path![], &rg::archetypes::ViewCoordinates::RUF());
        self.rerun_destination = destination;
    }
}

impl GpuTexture {
    fn new(
        device: &wgpu::Device,
        size: wgpu::Extent3d,
        format: wgpu::TextureFormat,
        label: &str,
    ) -> Self {
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            size,
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D3,
            format,
            view_formats: &[],
            usage: wgpu::TextureUsages::TEXTURE_BINDING
                | wgpu::TextureUsages::COPY_SRC
                | wgpu::TextureUsages::COPY_DST,
            label: Some(label),
        });
        let texture_view = Arc::new(Identified::new(
            texture.create_view(&wgpu::TextureViewDescriptor::default()),
        ));
        Self {
            texture,
            texture_view,
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
        TexPoint {
            atlas_id: match self.tile.channels {
                Channels::Reflectance => 0,
                Channels::ReflectanceEmission => 1,
            },
            tc: float_point.map(FixTexCoord::from_float),
        }
    }
}

impl Drop for TileBacking {
    fn drop(&mut self) {
        #[cfg(feature = "rerun")]
        {
            self.rerun_destination.clear_recursive(&rg::entity_path![]);
        }

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

fn zero_box(volume: usize) -> Box<[[u8; 4]]> {
    vec![[0, 0, 0, 0]; volume].into_boxed_slice()
}

/// Compute the cube root of `value`, rounded down.
/// (This algorithm is probably wrong for certain large values, but we only use it to compute
/// a size limit and underapproximating is OK.)
fn cube_root_u64(value: u64) -> u64 {
    let mut root = (value as f64).powf(3f64.recip()) as u64;
    while root.saturating_pow(3) > value {
        root -= 1;
    }
    root
}
