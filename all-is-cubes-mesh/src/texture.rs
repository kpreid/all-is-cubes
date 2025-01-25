//! Traits for textures used by the meshes this library generates.

// TODO: texture tile bounds should be `Vol<(), XMaj>` instead of `GridAab`, but we don't have `XMaj` ordering yet.

use core::fmt;

use all_is_cubes::block::{Evoxel, Evoxels};
use all_is_cubes::content::palette;
use all_is_cubes::euclid::Point3D;
use all_is_cubes::math::{Axis, Cube, GridAab, GridSizeCoord, Rgb, Vol};
use all_is_cubes::util::{ConciseDebug, Fmt};

/// Numeric type used to calculate texture coordinates and store them in [`BlockVertex`].
///
/// Note that this type is only exposed publicly within [`texture::Tile::grid_to_texcoord()`];
/// dependents’ texture coordinates are not required to be f32.
pub(crate) type TextureCoordinate = f32;

/// Unit-of-measure identifier used with [`euclid`](all_is_cubes::euclid) for “whole texels”.
#[expect(clippy::exhaustive_enums)]
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
    //---
    // Design note: The bounds beyond `Tile` are not required for allocators to function, but
    // for how this type is used in the rest of the mesh library.
    type Tile: Tile<Point = Self::Point> + fmt::Debug + 'static;

    /// Type of points within the texture, that vertices store (or at least, that are
    /// used to construct vertices).
    //---
    // Design note: The bounds are not required for allocators to function, but
    // for how this type is used in the rest of the mesh library.
    type Point: Copy + PartialEq + fmt::Debug + Send + Sync + 'static;

    /// Allocate a tile, whose range of texels will be reserved for use as long as the
    /// [`Tile`] value, and its clones, are not dropped.
    ///
    /// * `bounds` specifies the desired size of the allocation;
    ///   its translation does not affect the size but may be used to make the resulting
    ///   texture coordinate transformation convenient for the caller.
    ///   It must not be empty (zero volume).
    /// * `channels` specifies what types of data the texture should capture from the
    ///   [`Evoxel`]s that will be provided later to [`Tile::write()`].
    ///   The allocator may choose to ignore some channels if this suits the
    ///   limitations of the intended rendering; an allocation should not fail due to
    ///   unsupported channels.
    ///
    /// Returns [`None`] if no space is available for another region.
    fn allocate(&self, bounds: GridAab, channels: Channels) -> Option<Self::Tile>;
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

    /// Returns the [`Channels`] that this tile is capable of storing or intentionally discarding.
    /// This should be equal to, or a superset of, the [`Channels`] requested when the texture was
    /// allocated.
    ///
    /// This will be used by mesh algorithms to avoid reallocating unless necessary when new texel
    /// data is to be displayed.
    fn channels(&self) -> Channels;

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
    fn allocate(&self, bounds: GridAab, channels: Channels) -> Option<Self::Tile> {
        <T as Allocator>::allocate(self, bounds, channels)
    }
}
impl<T: Allocator> Allocator for alloc::sync::Arc<T> {
    type Tile = T::Tile;
    type Point = T::Point;
    #[mutants::skip] // trivial
    fn allocate(&self, bounds: GridAab, channels: Channels) -> Option<Self::Tile> {
        <T as Allocator>::allocate(self, bounds, channels)
    }
}
impl<T: Allocator> Allocator for alloc::rc::Rc<T> {
    type Tile = T::Tile;
    type Point = T::Point;
    #[mutants::skip] // trivial
    fn allocate(&self, bounds: GridAab, channels: Channels) -> Option<Self::Tile> {
        <T as Allocator>::allocate(self, bounds, channels)
    }
}

/// Specifies a combination of data stored per texel that may be requested of an [`Allocator`].
///
/// Design note: This is an `enum` rather than a bitmask so that allocators and shaders do not
/// have to support a large number of cases, but only typical ones.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[expect(clippy::exhaustive_enums)]
pub enum Channels {
    /// RGBA color (or perhaps RGB if the target does not support transparency) representing
    /// reflectance.
    Reflectance,
    /// Reflectance as defined above and also RGB light emission.
    ReflectanceEmission,
}

impl Channels {
    /// Returns whether `self` can store everything that `other` can.
    pub(crate) fn is_superset_of(self, other: Self) -> bool {
        use Channels::*;
        match (self, other) {
            (ReflectanceEmission, _) => true,
            (Reflectance, Reflectance) => true,
            (Reflectance, ReflectanceEmission) => false,
        }
    }

    /// Returns whether this includes RGB emission channels.
    pub fn has_emission(self) -> bool {
        match self {
            Channels::Reflectance => false,
            Channels::ReflectanceEmission => true,
        }
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
/// This function may be useful to [`Tile::slice()`] implementors.
#[track_caller]
pub fn validate_slice(tile_bounds: GridAab, slice_bounds: GridAab) -> Axis {
    assert!(
        tile_bounds.contains_box(slice_bounds),
        "Tile::slice() bounds {slice_bounds:?} are not within the tile bounds {tile_bounds:?}"
    );
    match Into::<[GridSizeCoord; 3]>::into(slice_bounds.size()) {
        [_, _, 1] => Axis::Z,
        [_, 1, _] => Axis::Y,
        [1, _, _] => Axis::X,
        _ => panic!("Tile::slice() bounds {slice_bounds:?} are not flat enough"),
    }
}

/// `voxels` must not be empty (zero volume).
pub(super) fn copy_voxels_to_new_texture<A: Allocator>(
    texture_allocator: &A,
    voxels: &Evoxels,
) -> Option<A::Tile> {
    texture_allocator
        .allocate(voxels.bounds(), needed_channels(voxels))
        .map(|mut texture| {
            texture.write(voxels.as_vol_ref());
            texture
        })
}

/// Determine which [`Channels`] are necessary to store all relevant characteristics of the block.
pub(super) fn needed_channels(voxels: &Evoxels) -> Channels {
    // This has false positives because it includes obscured voxels, but that is probably not
    // worth fixing with a more complex algorithm.
    if voxels
        .as_vol_ref()
        .as_linear()
        .iter()
        .any(|voxel| voxel.emission != Rgb::ZERO)
    {
        Channels::ReflectanceEmission
    } else {
        Channels::Reflectance
    }
}

/// Helper function to implement the typical case of copying voxels into an X-major, sRGB, RGBA
/// texture.
#[doc(hidden)]
#[expect(
    clippy::needless_pass_by_value,
    reason = "<https://github.com/rust-lang/rust-clippy/issues/7456>"
)]
#[expect(clippy::module_name_repetitions)]
pub fn copy_voxels_into_xmaj_texture(
    voxels: Vol<&[Evoxel]>,
    reflectance_texture: &mut [[u8; 4]],
    emission_texture: Option<&mut [[u8; 4]]>,
) {
    let bounds = voxels.bounds();
    assert_eq!(voxels.volume(), reflectance_texture.len());
    if let Some(&mut ref mut t) = emission_texture {
        assert_eq!(voxels.volume(), t.len());
    }

    // TODO: Consider changing `Evoxels`'s ordering so that this can be a straight copy instead
    // of a shuffle. Or at least implement the shuffle more efficiently.
    let mut i = 0;
    for z in bounds.z_range() {
        for y in bounds.y_range() {
            for x in bounds.x_range() {
                let voxel = voxels
                    .get(Cube { x, y, z })
                    .copied()
                    .unwrap_or(Evoxel::from_color(palette::MISSING_VOXEL_ERROR));
                reflectance_texture[i] = voxel.color.to_srgb8();

                if let Some(&mut ref mut t) = emission_texture {
                    t[i] = voxel.emission.with_alpha_one().to_srgb8();
                }

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
#[expect(clippy::exhaustive_structs)]
pub struct NoTextures;

impl Allocator for NoTextures {
    type Tile = NoTexture;
    type Point = NoTexture;

    fn allocate(&self, bounds: GridAab, _: Channels) -> Option<Self::Tile> {
        assert!(!bounds.is_empty());
        None
    }
}

/// Uninhabited [`Tile`] type; no instance of this ever exists.
///
/// TODO: this can and should be just ! (never) when that's available in stable Rust
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[expect(clippy::exhaustive_enums)]
#[expect(
    clippy::module_name_repetitions,
    reason = "short name would be confusing"
)]
pub enum NoTexture {}

impl Tile for NoTexture {
    type Point = Self;
    type Plane = Self;
    const REUSABLE: bool = true;

    fn bounds(&self) -> GridAab {
        match *self {}
    }

    fn channels(&self) -> Channels {
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
