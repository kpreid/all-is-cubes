//! Data structures for triangle meshes and algorithms for converting blocks/voxels to
//! meshes for rendering.
//!
//! All of the algorithms here are independent of graphics API, but they require providing
//! vertex and texture data types suitable for the API or data format you wish to use.

use crate::camera::{GraphicsOptions, TransparencyOption};
use crate::math::FreeCoordinate;

mod block_vertex;
pub use block_vertex::*;
mod block_mesh;
pub use block_mesh::*;
#[doc(hidden)] // TODO: candidate for being public
pub mod chunked_mesh;
mod space_mesh;
use cgmath::Point3;
pub use space_mesh::*;
mod planar;
use planar::*;
mod texalloc;
pub use texalloc::*;

#[cfg(test)]
mod tests;

/// Parameters for creating meshes that aren't the block/space data itself
/// (or the texture allocator, since that may need to be mutable).
///
/// Creating this and comparing it against a previous instance is appropriate for
/// determining when to invalidate previously computed meshes. This type is also intended
/// to make the API future-proof against additional configuration being needed.
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct MeshOptions {
    /// Input to TransparencyOption::limit_alpha.
    transparency: TransparencyOption,

    /// Ignore blocks' [`voxels`] data and use only the overall color.
    ///
    /// [`voxels`]: crate::block::EvaluatedBlock::voxels
    ignore_voxels: bool,
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
}

/// One end of a line to be drawn.
///
/// Used for debugging visualizations and not for game content, with the current exception
/// of [`Cursor`](crate::character::Cursor).
///
/// The primary way in which these are used in this crate is
/// [`Geometry::wireframe_points()`](crate::math::Geometry::wireframe_points).
#[derive(Clone, Copy, Debug, PartialEq)]
#[non_exhaustive]
pub struct LineVertex {
    /// Position of the vertex.
    pub position: Point3<FreeCoordinate>,

    /// Color in which to draw the line.
    ///
    /// If [`None`], a color set by the context/parent should be used instead.
    ///
    /// If the ends of a line are different colors, color should be interpolated along
    /// the line.
    pub color: Option<crate::math::Rgba>,
}

impl From<Point3<FreeCoordinate>> for LineVertex {
    fn from(position: Point3<FreeCoordinate>) -> Self {
        Self {
            position,
            color: None,
        }
    }
}
