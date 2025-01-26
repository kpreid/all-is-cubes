//! Mesh vertices.

use core::fmt;

use all_is_cubes::block::Resolution;
use all_is_cubes::euclid::Point3D;
use all_is_cubes::math::{Cube, Face6, FreeCoordinate, FreePoint, FreeVector, Rgba};
use all_is_cubes::util::{ConciseDebug, Fmt, Refmt as _};

use crate::MeshTypes;

/// Basic vertex data type for a [`BlockMesh`].
/// Implement <code>[`From`]&lt;[`BlockVertex`]&gt;</code> (and usually [`GfxVertex`])
/// to provide a specialized version fit for the target graphics API.
///
/// `T` is the type of texture-coordinate points being used. That is, one `T` value
/// should identify one point in the block's 3D texture, such as `T = Point3<f32>`).
///
/// [`BlockMesh`]: super::BlockMesh
#[expect(clippy::exhaustive_structs)]
#[derive(Clone, Copy, PartialEq)]
pub struct BlockVertex<T> {
    /// Vertex position.
    pub position: FreePoint,
    /// Vertex normal, always axis-aligned.
    pub face: Face6,
    /// Surface color or texture coordinate.
    pub coloring: Coloring<T>,
}

impl<T: Clone> BlockVertex<T> {
    /// Remove the clamp information for the sake of tidier tests of one thing at a time.
    #[cfg(test)]
    pub(crate) fn remove_clamps(mut self) -> Self {
        self.coloring = match self.coloring {
            Coloring::Texture {
                pos,
                clamp_min: _,
                clamp_max: _,
                resolution,
            } => Coloring::Texture {
                clamp_min: pos.clone(),
                clamp_max: pos.clone(),
                pos,
                resolution,
            },
            other @ Coloring::Solid(_) => other,
        };
        self
    }
}

/// Describes the two ways a [`BlockVertex`] may be colored; by a solid color or by a texture.
///
/// `T` is the type of texture-coordinate points being used. That is, one `T` value
/// should identify one point in the block's 3D texture, such as `T = Point3<f32>`).
#[expect(clippy::exhaustive_enums)]
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Coloring<T> {
    /// Solid color.
    Solid(Rgba),
    /// Texture coordinates provided by the [`Allocator`](super::texture::Allocator)
    /// for this vertex.
    Texture {
        /// Texture coordinates for this vertex.
        pos: T,
        /// Lower bounds for clamping the entire surface's texture coordinates.
        /// May be used to avoid texture bleed.
        clamp_min: T,
        /// Upper bounds for clamping the entire surface's texture coordinates.
        /// May be used to avoid texture bleed.
        clamp_max: T,
        /// Resolution of the voxels the texture was created from.
        resolution: Resolution,
    },
}

impl<T> fmt::Debug for BlockVertex<T>
where
    Coloring<T>: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print compactly on single line even if the formatter is in prettyprint mode.
        write!(
            fmt,
            "{{ p: {:?} n: {:?} c: {:?} }}",
            self.position.refmt(&ConciseDebug),
            self.face,
            self.coloring
        )
    }
}
impl<T> fmt::Debug for Coloring<T>
where
    T: Fmt<ConciseDebug>, // TODO: inelegant
{
    // TODO: test formatting of this
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Coloring::Solid(color) => write!(fmt, "Solid({color:?})"),
            Coloring::Texture { pos, .. } => {
                write!(fmt, "Texture({:?})", pos.refmt(&ConciseDebug))
            }
        }
    }
}

/// A custom representation of [`BlockVertex`] suitable for a specific graphics system.
///
/// The life cycle of a [`GfxVertex`]:
///
/// * First, it is constructed by [`BlockMesh::new()`]
///   for a particular [`Block`] value, and stored in a [`BlockMesh`].
/// * Then, wherever that block appears in a [`Space`], the block vertices are copied
///   to become the [`SpaceMesh`]’s vertices, and [`GfxVertex::instantiate_vertex`] is
///   called on each copy to position it at the particular block's location.
///
/// [`Block`]: all_is_cubes::block::Block
/// [`BlockMesh`]: crate::BlockMesh
/// [`BlockMesh::new()`]: crate::BlockMesh::new()
/// [`Space`]: all_is_cubes::space::Space
/// [`SpaceMesh`]: crate::SpaceMesh
pub trait GfxVertex: From<BlockVertex<Self::TexPoint>> + Copy + Sized + 'static {
    /// Whether [`SpaceMesh`]es should provide pre-sorted vertex index slices to allow
    /// back-to-front drawing order based on viewing ranges.
    ///
    /// Design note: Strictly speaking, this doesn't need to be static and could be part
    /// of [`MeshOptions`]. However, we currently have no reason to complicate run-time
    /// data flow that way.
    ///
    /// [`SpaceMesh`]: crate::SpaceMesh
    /// [`MeshOptions`]: crate::MeshOptions
    const WANTS_DEPTH_SORTING: bool;

    /// Number type for the vertex position coordinates.
    type Coordinate: num_traits::float::FloatCore;

    /// Point type identifying a point in the block's texture.
    type TexPoint: Copy;

    /// Type of the data carried from [`Self::instantiate_block()`] to
    /// [`Self::instantiate_vertex()`].
    type BlockInst: Copy;

    /// Prepare the information needed by [`Self::instantiate_vertex()`] for one block.
    /// Currently, this constitutes the location of that block, and hence this function
    /// is responsible for any necessary numeric conversion.
    fn instantiate_block(cube: Cube) -> Self::BlockInst;

    /// Transforms a vertex belonging to a general model of a block to its instantiation
    /// in a specific location in space.
    ///
    /// The `block` value should be obtained by calling [`Self::instantiate_block()`].
    fn instantiate_vertex(&mut self, block: Self::BlockInst);

    /// Returns the position of this vertex.
    ///
    /// Note: This is used to perform depth sorting for transparent vertices.
    fn position(&self) -> Point3D<Self::Coordinate, Cube>;
}

/// Trivial implementation of [`GfxVertex`] for testing purposes. Discards lighting.
impl<T: Copy + 'static> GfxVertex for BlockVertex<T> {
    const WANTS_DEPTH_SORTING: bool = true;
    type Coordinate = FreeCoordinate;
    type TexPoint = T;
    type BlockInst = FreeVector;

    fn position(&self) -> FreePoint {
        self.position
    }

    #[inline]
    fn instantiate_block(cube: Cube) -> Self::BlockInst {
        cube.lower_bounds().to_f64().to_vector()
    }

    #[inline]
    fn instantiate_vertex(&mut self, offset: Self::BlockInst) {
        self.position += offset;
    }
}

/// A vertex type's position using its coordinate type.
pub(crate) type VPos<M> = Point3D<<<M as MeshTypes>::Vertex as GfxVertex>::Coordinate, Cube>;
