//! Traits for textures used by the meshes this library generates.

use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};

use all_is_cubes::block::{Evoxel, Evoxels};
use all_is_cubes::content::palette;
use all_is_cubes::euclid::Point3D;
use all_is_cubes::math::{Cube, GridAab};
use all_is_cubes::util::{ConciseDebug, CustomFormat};

#[cfg(doc)]
use all_is_cubes::math::GridArray;

/// Numeric type used to calculate texture coordinates and store them in [`BlockVertex`].
///
/// Note that this type is only exposed publicly within [`texture::Tile::grid_to_texcoord()`];
/// dependents’ texture coordinates are not required to be f32.
pub(crate) type TextureCoordinate = f32;

/// Color data accepted by [`Allocator`].
/// The components are sRGB `[R, G, B, A]`.
pub type Texel = [u8; 4];

/// Unit-of-measure identifier used with [`euclid`](all_is_cubes::euclid) for “whole texels”.
/// TODO(euclid migration): Better name, and a type alias for the point
#[allow(clippy::exhaustive_enums)]
#[derive(Debug)]
pub enum TexelUnit {}

/// 3D point type which identifies a point within a specific allocated [`Tile`],
/// in the same coordinate system as the `bounds` passed to [`Allocator::allocate()`].
///
/// Note that this uses float, not integer, coordinates; but fractional values refer to
/// fractions of texels.
pub type TilePoint = Point3D<TextureCoordinate, TexelUnit>;

/// Allocator of 3D regions (“tiles”) in a texture atlas to paint block voxels into.
/// Implement this trait using the target graphics API's 3D texture type.
///
/// Allocations may be deallocated for reuse by dropping the returned [`Tile`]s.
pub trait Allocator {
    /// Allocation handles produced by this allocator.
    type Tile: Tile<Point = Self::Point>;

    /// Type of points within the texture, that vertices store.
    type Point;

    /// Allocate a tile, whose range of texels will be reserved for use as long as the
    /// [`Tile`] value, and its clones, are not dropped.
    ///
    /// The given [`GridAab`] specifies the desired size of the allocation;
    /// its translation does not affect the size but may be used to make the resulting
    /// texture coordinate transformation convenient for the caller.
    ///
    /// Returns [`None`] if no space is available for another region.
    fn allocate(&self, bounds: GridAab) -> Option<Self::Tile>;
}

/// 3D texture volume provided by an [`Allocator`] to paint a block's voxels in.
pub trait Tile: Clone {
    /// Return type of [`Self::slice()`].
    type Plane: Plane<Point = Self::Point>;

    /// Type of points within the texture, that vertices store.
    type Point;

    /// Whether `write()` may be called more than once.
    const REUSABLE: bool;

    /// Returns the [`GridAab`] originally passed to the texture allocator for this tile.
    fn bounds(&self) -> GridAab;

    /// Returns a [`Plane`] instance referring to some 2D slice of this 3D texture volume.
    ///
    /// `bounds` specifies the region to be sliced and must have a size of 1 in at least
    /// one axis. If it is not completely within [`Self::bounds()`], this function may panic.
    ///
    /// Depending on the texture implementation, this may be merely a coordinate system
    /// helper (for 3D texturing) or it may actually allocate a region of 2D texture.
    fn slice(&self, bounds: GridAab) -> Self::Plane;

    /// Write texture data as RGBA color.
    ///
    /// `data` must be of length `self.bounds().volume()`, and ordered “X-major”, that is,
    /// the coordinates of the texels follow the pattern
    /// `[[0, 0, 0], [1, 0, 0], ..., [0, 1, 0], [1, 1, 0], ..., [0, 0, 1], [1, 0, 1], ...]`.
    /// Note that this is not the same as the ordering built into [`GridArray`].
    ///
    /// If `Self::REUSABLE` is false, this must not be called more than once.
    //---
    // TODO: Replace slice with a GridArray (requires changing the ordering of one or the other).
    // TODO: `REUSABLE` is a lousy API because it isn't statically checked
    fn write(&mut self, data: &[Texel]);
}

/// 2D texture slice to use for texturing the surface of a voxel mesh.
///
/// When all clones of this value are dropped, the texture allocation may be released and
/// the texture coordinate region may be reused for different data. (Implementations which
/// are natively 3D may accomplish that by having this type simply contain a [`Tile`] rather
/// than tracking each plane on its own.)
pub trait Plane: Clone {
    /// Type of points within this texture, that are to be used in vertices.
    type Point: Copy;

    /// Transform a point in the coordinate system of, and within, the `bounds` given to
    /// create this plane (that is, 1 unit = 1 texel) into texture coordinates suitable for
    /// the target [`GfxVertex`](super::GfxVertex) type.
    ///
    /// The returned texture coordinates are guaranteed to be valid only as long as
    /// `self` (or a clone of it) has not been dropped.
    fn grid_to_texcoord(&self, in_tile_grid: TilePoint) -> Self::Point;
}

impl<T: Allocator> Allocator for &T {
    type Tile = T::Tile;
    type Point = T::Point;
    #[mutants::skip] // trivial
    fn allocate(&self, bounds: GridAab) -> Option<Self::Tile> {
        <T as Allocator>::allocate(self, bounds)
    }
}
impl<T: Allocator> Allocator for std::sync::Arc<T> {
    type Tile = T::Tile;
    type Point = T::Point;
    #[mutants::skip] // trivial
    fn allocate(&self, bounds: GridAab) -> Option<Self::Tile> {
        <T as Allocator>::allocate(self, bounds)
    }
}
impl<T: Allocator> Allocator for std::rc::Rc<T> {
    type Tile = T::Tile;
    type Point = T::Point;
    #[mutants::skip] // trivial
    fn allocate(&self, bounds: GridAab) -> Option<Self::Tile> {
        <T as Allocator>::allocate(self, bounds)
    }
}

/// Validate that the argument to [`Tile::slice()`] is within bounds, and thickness
/// 1 on some axis.
///
/// * If valid, return the axis (`0`, `1`, or `2`).
///     * If there are multiple axes on which the size is 1, the highest one is returned
///       (i.e. Z is preferred).
/// * If invalid, panic.
///
/// This function may be useful to [`Tile`] implementors.
// --
// TODO: more evidence we need an Axis enum. Or, we should be accepting the slice as a type
// that doesn't need axis validation (but we would still need bounds validation)
#[track_caller]
pub fn validate_slice(tile_bounds: GridAab, slice_bounds: GridAab) -> usize {
    assert!(
        tile_bounds.contains_box(slice_bounds),
        "Tile::slice() bounds {slice_bounds:?} are not within the tile bounds {tile_bounds:?}"
    );
    match Into::<[i32; 3]>::into(slice_bounds.size()) {
        [_, _, 1] => 2,
        [_, 1, _] => 1,
        [1, _, _] => 0,
        _ => panic!("Tile::slice() bounds {slice_bounds:?} are not flat enough"),
    }
}

pub(super) fn copy_voxels_to_texture<A: Allocator>(
    texture_allocator: &A,
    voxels: &Evoxels,
) -> Option<A::Tile> {
    texture_allocator
        .allocate(voxels.bounds())
        .map(|mut texture| {
            copy_voxels_into_existing_texture(voxels, &mut texture);
            texture
        })
}

pub(super) fn copy_voxels_into_existing_texture<T: Tile>(voxels: &Evoxels, texture: &mut T) {
    let bounds = voxels.bounds();
    let mut texels: Vec<Texel> = Vec::with_capacity(bounds.volume());
    // TODO: Teach GridArray about alternate array orderings so that we can express
    // this as a map-and-shuffle operation instead of a special loop.
    for z in bounds.z_range() {
        for y in bounds.y_range() {
            for x in bounds.x_range() {
                texels.push(
                    voxels
                        .get(Cube { x, y, z })
                        .unwrap_or(Evoxel::from_color(palette::MISSING_VOXEL_FALLBACK))
                        .color
                        .to_srgb8(),
                );
            }
        }
    }
    texture.write(&texels);
}

/// Null [`Allocator`]; rejects all allocations.
///
/// Used for generating textureless meshes. TODO: Modify triangulator to actually
/// generate separate triangles when textures are unavailable.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_structs)]
pub struct NoTextures;

impl Allocator for NoTextures {
    type Tile = NoTexture;
    type Point = NoTexture;

    fn allocate(&self, _: GridAab) -> Option<Self::Tile> {
        None
    }
}

/// Uninhabited [`Tile`] type; no instance of this ever exists.
///
/// TODO: this can and should be just ! (never) when that's available in stable Rust
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(clippy::exhaustive_enums)]
pub enum NoTexture {}

impl Tile for NoTexture {
    type Point = Self;
    type Plane = Self;
    const REUSABLE: bool = true;

    fn bounds(&self) -> GridAab {
        match *self {}
    }

    fn slice(&self, _: GridAab) -> Self::Plane {
        match *self {}
    }

    fn write(&mut self, _data: &[Texel]) {
        match *self {}
    }
}

impl Plane for NoTexture {
    type Point = Self;

    fn grid_to_texcoord(&self, _: Point3D<TextureCoordinate, TexelUnit>) -> Self::Point {
        match *self {}
    }
}

impl CustomFormat<ConciseDebug> for NoTexture {
    fn fmt(&self, _: &mut fmt::Formatter<'_>, _: ConciseDebug) -> fmt::Result {
        match *self {}
    }
}

/// [`Allocator`] which discards all input except for counting calls; for testing.
///
/// This type is public so that it may be used in benchmarks and such, but not intended to be used
/// outside of All is Cubes itself.
#[doc(hidden)]
#[derive(Debug)]
pub struct TestAllocator {
    capacity: usize,
    count_allocated: AtomicUsize,
}

impl TestAllocator {
    pub const fn new() -> Self {
        Self {
            capacity: usize::MAX,
            count_allocated: AtomicUsize::new(0),
        }
    }

    /// Fail after allocating this many tiles. (Currently does not track deallocations.)
    pub fn set_capacity(&mut self, capacity: usize) {
        self.capacity = capacity;
    }

    /// Number of tiles allocated. Does not decrement for deallocations.
    pub fn count_allocated(&self) -> usize {
        self.count_allocated.load(SeqCst)
    }
}

impl Default for TestAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl Allocator for TestAllocator {
    type Tile = TestTile;
    type Point = TestPoint;

    fn allocate(&self, bounds: GridAab) -> Option<Self::Tile> {
        self.count_allocated
            .fetch_update(SeqCst, SeqCst, |count| {
                if count < self.capacity {
                    Some(count + 1)
                } else {
                    None
                }
            })
            .ok()
            .map(|_| ())?;
        Some(TestTile { bounds })
    }
}

/// Tile type for [`TestAllocator`].
///
/// This type is public so that it may be used in benchmarks and such.
#[derive(Clone, Debug, Eq, PartialEq)]
#[doc(hidden)]
pub struct TestTile {
    bounds: GridAab,
}

impl Tile for TestTile {
    type Point = TestPoint;
    type Plane = TestTile;
    const REUSABLE: bool = true;

    fn bounds(&self) -> GridAab {
        self.bounds
    }

    fn slice(&self, bounds: GridAab) -> Self::Plane {
        validate_slice(self.bounds, bounds);
        self.clone()
    }

    fn write(&mut self, data: &[Texel]) {
        // Validate data size.
        assert_eq!(
            data.len(),
            self.bounds.volume(),
            "tile data did not match resolution"
        );
    }
}
impl Plane for TestTile {
    type Point = TestPoint;

    fn grid_to_texcoord(&self, in_tile: Point3D<TextureCoordinate, TexelUnit>) -> Self::Point {
        in_tile
    }
}

/// Texture point for [`TestAllocator`]
#[doc(hidden)]
pub type TestPoint = Point3D<TextureCoordinate, TexelUnit>;

#[cfg(test)]
mod tests {
    use super::*;
    use all_is_cubes::block::Resolution::*;

    /// Test the [`TestAllocator`].
    #[test]
    fn test_texture_allocator() {
        let bounds = GridAab::for_block(R8);
        let mut allocator = TestAllocator::new();
        assert_eq!(allocator.count_allocated(), 0);
        assert!(allocator.allocate(bounds).is_some());
        assert!(allocator.allocate(bounds).is_some());
        assert_eq!(allocator.count_allocated(), 2);
        allocator.set_capacity(3);
        assert!(allocator.allocate(bounds).is_some());
        assert!(allocator.allocate(bounds).is_none());
    }
}
