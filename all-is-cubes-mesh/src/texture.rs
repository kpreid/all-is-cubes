//! Traits for textures used by the meshes this library generates.

use std::fmt;

use all_is_cubes::block::{Evoxel, Evoxels};
use all_is_cubes::content::palette;
use all_is_cubes::euclid::Point3D;
use all_is_cubes::math::{Axis, Cube, GridAab, Vol};
use all_is_cubes::util::{ConciseDebug, Fmt};

#[cfg(doc)]
use all_is_cubes::math::GridArray;

/// Numeric type used to calculate texture coordinates and store them in [`BlockVertex`].
///
/// Note that this type is only exposed publicly within [`texture::Tile::grid_to_texcoord()`];
/// dependents’ texture coordinates are not required to be f32.
pub(crate) type TextureCoordinate = f32;

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

    /// Type of points within the texture, that vertices store (or at least, that are
    /// used to construct vertices).
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
///
/// When all clones of this value are dropped, the texture allocation may be released and
/// the texture coordinate region may be reused for different data.
///
/// Implement this along with [`Allocator`] and [`Plane`] to provide appropriate texture
/// storage for a particular graphics system.
pub trait Tile: Clone + PartialEq {
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

    /// Copy the given voxels' color into this texture volume.
    ///
    /// [`data.bounds()`](Vol::bounds) must be equal to [`self.bounds()`](Self::bounds).
    ///
    /// If `Self::REUSABLE` is false, this may not be called more than once; the implementation
    /// may panic, overwrite, or ignore additional calls.
    //---
    // TODO: Make the input be ordered in X-major order so a bulk copy is feasible in typical
    // texturing systems.
    // TODO: `REUSABLE` is a lousy API because it isn't statically checked
    fn write(&mut self, data: Vol<&[Evoxel]>);
}

/// 2D texture slice to use for texturing the surface of a voxel mesh.
///
/// Implement this along with [`Allocator`] and [`Tile`] to provide appropriate texture
/// storage for a particular graphics system.
pub trait Plane: Clone {
    /// Type of points within this texture, that are to be used in vertices.
    type Point: Copy;

    /// Transform a point in the coordinate system of, and within, the `bounds` given to
    /// create this plane (that is, 1 unit = 1 texel) into texture coordinates suitable for
    /// the target [`GfxVertex`](super::GfxVertex) type.
    ///
    /// The returned texture coordinates are guaranteed to be valid only as long as
    /// the parent [`Tile`] (or a clone of it) has not been dropped.
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
/// * If valid, return the axis on which the slice is flat.
///     * If there are multiple axes on which the size is 1, the highest one is returned
///       (i.e. Z is preferred).
/// * If invalid, panic.
///
/// This function may be useful to [`Tile`] implementors.
#[track_caller]
pub fn validate_slice(tile_bounds: GridAab, slice_bounds: GridAab) -> Axis {
    assert!(
        tile_bounds.contains_box(slice_bounds),
        "Tile::slice() bounds {slice_bounds:?} are not within the tile bounds {tile_bounds:?}"
    );
    match Into::<[i32; 3]>::into(slice_bounds.size()) {
        [_, _, 1] => Axis::Z,
        [_, 1, _] => Axis::Y,
        [1, _, _] => Axis::X,
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
            texture.write(voxels.as_vol_ref());
            texture
        })
}

/// Helper function to implement the typical case of copying voxels into an X-major, sRGB, RGBA
/// texture.
#[doc(hidden)]
pub fn copy_voxels_into_xmaj_texture(voxels: Vol<&[Evoxel]>, texture: &mut [[u8; 4]]) {
    let bounds = voxels.bounds();
    assert_eq!(bounds.volume(), texture.len());

    // TODO: Consider changing `Evoxels`'s ordering so that this can be a straight copy instead
    // of a shuffle. Or at least implement the shuffle more efficiently.
    let mut i = 0;
    for z in bounds.z_range() {
        for y in bounds.y_range() {
            for x in bounds.x_range() {
                texture[i] = voxels
                    .get(Cube { x, y, z })
                    .copied()
                    .unwrap_or(Evoxel::from_color(palette::MISSING_VOXEL_ERROR))
                    .color
                    .to_srgb8();
                i += 1;
            }
        }
    }
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

    fn write(&mut self, _data: Vol<&[Evoxel]>) {
        match *self {}
    }
}

impl Plane for NoTexture {
    type Point = Self;

    fn grid_to_texcoord(&self, _: Point3D<TextureCoordinate, TexelUnit>) -> Self::Point {
        match *self {}
    }
}

impl Fmt<ConciseDebug> for NoTexture {
    fn fmt(&self, _: &mut fmt::Formatter<'_>, _: &ConciseDebug) -> fmt::Result {
        match *self {}
    }
}
