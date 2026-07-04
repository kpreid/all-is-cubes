use core::fmt;
use core::marker::PhantomData;

use all_is_cubes_render::camera::{GraphicsOptions, TransparencyOption};

use crate::{Vertex, texture};

// -------------------------------------------------------------------------------------------------

/// Parameters for creating meshes that aren't the block/space data itself
/// (or the texture allocator, since that may need to be mutable).
///
/// Creating this and comparing it against a previous instance is appropriate for
/// determining when to invalidate previously computed meshes. This type is also intended
/// to make the API future-proof against additional configuration being needed.
///
/// See also [`MeshTypes`] for statically chosen properties of meshes.
#[derive(Debug)]
pub struct MeshOptions<M: MeshTypes> {
    /// Input to [`TransparencyOption::limit_alpha()`] applied to all vertex colors and voxels.
    pub(crate) transparency: TransparencyOption,

    /// Determines what geometry should be produced when a mesh contains transparent voxels,
    /// after filtering by [`TransparencyOption::limit_alpha()`].
    pub(crate) transparency_format: TransparencyFormat,

    /// Ignore blocks' [`voxels`] data and use only the overall color.
    ///
    /// [`voxels`]: all_is_cubes::block::EvaluatedBlock::voxels
    pub(crate) ignore_voxels: bool,

    /// Does not own an M but depends on it.
    pub(crate) _mt: PhantomData<fn(&M)>,
}

impl<M: MeshTypes> MeshOptions<M> {
    /// Take the options relevant to mesh generation from the given [`GraphicsOptions`].
    pub fn new(graphics_options: &GraphicsOptions) -> Self {
        let transparency = graphics_options.transparency.clone();

        let transparency_format = match transparency {
            TransparencyOption::Surface => TransparencyFormat::Surfaces,
            // TODO: Do this only if the textures support it (but we don't have MeshTypes here)
            TransparencyOption::Volumetric => TransparencyFormat::BoundingBox,
            TransparencyOption::Threshold(_) => TransparencyFormat::Surfaces,
            ref o => {
                if cfg!(debug_assertions) {
                    unreachable!("missing support for transparency option {o:?}");
                } else {
                    TransparencyFormat::Surfaces
                }
            }
        };

        Self {
            transparency,
            transparency_format,
            ignore_voxels: false,
            _mt: PhantomData,
        }
    }

    /// Placeholder for use in tests which do not care about any of the
    /// characteristics that are affected by options (yet).
    #[doc(hidden)]
    pub fn dont_care_for_test() -> Self {
        Self {
            transparency: TransparencyOption::Volumetric,
            transparency_format: TransparencyFormat::BoundingBox,
            ignore_voxels: false,
            _mt: PhantomData,
        }
    }
}

impl<M: MeshTypes> Eq for MeshOptions<M> {}
impl<M: MeshTypes> PartialEq for MeshOptions<M> {
    fn eq(&self, other: &Self) -> bool {
        let Self {
            transparency,
            transparency_format,
            ignore_voxels,
            _mt: _,
        } = self;
        *transparency == other.transparency
            && *transparency_format == other.transparency_format
            && *ignore_voxels == other.ignore_voxels
    }
}

impl<M: MeshTypes> Clone for MeshOptions<M> {
    fn clone(&self) -> Self {
        Self {
            transparency: self.transparency.clone(),
            transparency_format: self.transparency_format,
            ignore_voxels: self.ignore_voxels,
            _mt: PhantomData,
        }
    }
}

#[cfg(feature = "arbitrary")]
#[mutants::skip]
impl<'a, M: MeshTypes> arbitrary::Arbitrary<'a> for MeshOptions<M> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(MeshOptions {
            // Note: This can produce nonsensical combinations of TransparencyOption and
            // TransparencyFormat, but that is probably harmless.
            transparency: u.arbitrary()?,
            transparency_format: u.arbitrary()?,
            ignore_voxels: u.arbitrary()?,
            _mt: PhantomData,
        })
    }

    fn size_hint(depth: usize) -> (usize, Option<usize>) {
        Self::try_size_hint(depth).unwrap_or_default()
    }

    fn try_size_hint(
        depth: usize,
    ) -> Result<(usize, Option<usize>), arbitrary::MaxRecursionReached> {
        Ok(arbitrary::size_hint::and_all(&[
            TransparencyOption::try_size_hint(depth)?,
            TransparencyFormat::try_size_hint(depth)?,
            bool::try_size_hint(depth)?,
        ]))
    }
}

// -------------------------------------------------------------------------------------------------

/// Bundle of types chosen to support a specific graphics API or other mesh format.
///
/// Implement this trait (using a placeholder type which need not store any data) to choose the
/// vertex format, texture format, and texture coordinates to be used.
///
/// The rationale of this trait existing is to be able to avoid numerous type parameters passed
/// around separately.
pub trait MeshTypes: 'static {
    /// Mesh vertex type.
    type Vertex: Vertex<
            TexPoint = <Self::Alloc as texture::Allocator>::Point,
            SecondaryData: fmt::Debug + PartialEq + Send + Sync + 'static,
        > + fmt::Debug
        + PartialEq
        + Send
        + Sync
        + 'static;

    /// Texture, or texture atlas, allocator.
    ///
    /// If texture support is not desired or possible, use [`texture::NoTextures`] here.
    type Alloc: texture::Allocator<Tile = Self::Tile> + fmt::Debug + 'static;

    /// Texture handle type.
    //--
    // Design note: This `Point` constraint is theoretically redundant with the above `TexPoint`
    // and `Tile` constraints, but the compiler won't infer it for us.
    type Tile: texture::Tile<Point = <Self::Alloc as texture::Allocator>::Point>
        + fmt::Debug
        + 'static;
}

// -------------------------------------------------------------------------------------------------

/// Determines what geometry should be produced when a mesh contains transparent voxels.
///
/// Currently, this is always derived from [`GraphicsOptions::transparency`].
/// If necessary, it may be made a public option.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[non_exhaustive]
pub(crate) enum TransparencyFormat {
    /// Generate triangles for transparent surfaces just like opaque ones.
    Surfaces,

    /// Generate a single bounding box for all transparent voxels.
    /// Assume that shading will take care of rendering all details within that box.
    BoundingBox,
}
