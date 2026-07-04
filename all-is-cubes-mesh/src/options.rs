use core::fmt;

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
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct MeshOptions {
    /// Input to [`TransparencyOption::limit_alpha()`] applied to all vertex colors and voxels.
    pub(crate) transparency: TransparencyOption,

    /// Ignore blocks' [`voxels`] data and use only the overall color.
    ///
    /// [`voxels`]: all_is_cubes::block::EvaluatedBlock::voxels
    pub(crate) ignore_voxels: bool,
}

impl MeshOptions {
    /// Take the options relevant to mesh generation from the given [`GraphicsOptions`].
    pub fn new(graphics_options: &GraphicsOptions) -> Self {
        Self {
            transparency: graphics_options.transparency.clone(),
            ignore_voxels: false,
        }
    }

    /// Placeholder for use in tests which do not care about any of the
    /// characteristics that are affected by options (yet).
    #[doc(hidden)]
    pub fn dont_care_for_test() -> Self {
        Self {
            transparency: TransparencyOption::Volumetric,
            ignore_voxels: false,
        }
    }

    /// Determines what geometry should be produced when a mesh contains transparent voxels,
    /// after filtering by [`TransparencyOption::limit_alpha()`].
    pub(crate) fn transparency_format(&self) -> TransparencyFormat {
        match self.transparency {
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
        }
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
