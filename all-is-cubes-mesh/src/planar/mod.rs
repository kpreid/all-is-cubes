//! Construction of 2D planar triangles and polygons from the surfaces of voxel shapes.
//!
//! The algorithms in this module are primarily for internal use by [`BlockMesh`][crate::BlockMesh],
//! and you do not need to use any items in this module to build block meshes.
//! However, they have been made available for separate use if desired.
//!
//! Each of the main algorithms accepts an ordered list of [`Vertex`]es, which contain local
//! vertex/edge/face connectivity but not global connectivity. They produce:
//!
//! * [`Triangulator`] produces a [triangulation] of the shape.
//!   This output is suitable for triangle meshes.
//! * [`Outliner`] produces the boundary of the shape, organized into closed loops.
//!   This output is suitable for translation into vector graphics paths, such as in SVG.
//!
//! [triangulation]: https://en.wikipedia.org/wiki/Polygon_triangulation

use core::fmt;

use all_is_cubes::euclid::{Box2D, Point2D, vec2};
use all_is_cubes::math::{Face, GridPoint};
use all_is_cubes::util::Refmt;

#[cfg(doc)]
use crate::planar;

// -------------------------------------------------------------------------------------------------

mod basis;
pub use basis::Basis;

mod mask;
pub use mask::Mask;

mod outliner;
pub use outliner::Outliner;

#[cfg(test)]
mod svg;

#[cfg(test)]
mod testing;

mod triangulator;
pub use triangulator::Triangulator;

#[cfg(test)]
mod triangulator_tests;

// -------------------------------------------------------------------------------------------------

/// Type of a vertex index.
///
/// TODO: Should this live in `crate::index_vec` next to other code to do with the maximum index
/// integer width we allow?
type Index = u32;

/// A vertex in the form required by [`Triangulator`] or [`Outliner`].
///
/// (Refer to this type as `planar::Vertex` to avoid ambiguity.)
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
#[allow(
    clippy::exhaustive_structs,
    reason = "each field is required input data"
)]
pub struct Vertex {
    /// Position of the vertex.
    pub position: GridPoint,

    /// Bitmask of which areas adjacent to this vertex, in the plane of the triangulation,
    /// should be covered by triangles.
    pub connectivity: Mask,

    /// Value used to refer to this vertex in the output of triangulation.
    pub index: Index,
}

impl fmt::Debug for Vertex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            position,
            connectivity,
            index,
        } = self;
        // Always single-line formatting.
        write!(
            f,
            "Vertex(#{index} at {position} connected {connectivity:?})",
            position = position.refmt(&all_is_cubes::util::ConciseDebug)
        )
    }
}

// -------------------------------------------------------------------------------------------------

/// Given access to a two-dimensional binary image, find its [`planar::Vertex`]es.
///
/// This function is analogous to [`Analysis::analyze()`][crate::Analysis::analyze],
/// but for 2D situations rather than 3D, and without consideration of texturing.
///
/// `get_pixel` should return the value of the pixel whose center is at `point + vec2(0.5, 0.5)`.
/// It will be called four times per pixel.
///
/// The returned [`Basis`] specifies the ordering of the returned vertices, as is needed in order to
/// pass them to [`Triangulator`] or [`Outliner`].
///
/// # Example
///
/// ```
/// use all_is_cubes::euclid::{point2, size2, default::{Box2D, Point2D}};
/// use all_is_cubes_mesh::planar;
///
/// let image: &[&[u8]] = &[
///     b".......",
///     b"#####..",
///     b"..###..",
/// ];
///
/// let (basis, vertices) = planar::analyze_2d(
///     Box2D::from_size(size2(image[0].len() as i32, image.len() as i32)),
///     |point| image[point.y as usize][point.x as usize] != b'.'
/// );
///
/// assert_eq!(
///     Vec::from_iter(vertices.map(|vertex| vertex.position.xy())),
///     [
///         point2(0, 1), // top left
///         point2(5, 1), // top right
///         point2(0, 2),
///         point2(2, 2),
///         point2(2, 3),
///         point2(5, 3), // bottom right
///     ]
/// );
/// ```
//---
// TODO: Consider making this able to proceed in different orderings.
pub fn analyze_2d<U>(
    bounds: Box2D<i32, U>,
    get_pixel: impl Fn(Point2D<i32, U>) -> bool,
) -> (Basis, impl Iterator<Item = Vertex>) {
    let mut index_counter = 0;

    let vertices = itertools::iproduct!(bounds.min.y..=bounds.max.y, bounds.min.x..=bounds.max.x)
        .filter_map(move |(y, x)| {
            let vertex_position = Point2D::new(x, y);

            let mut connectivity = Mask::Empty;
            for (dx, dy, mask) in [
                (-1, -1, Mask::Bsbp),
                (0, -1, Mask::Bsfp),
                (-1, 0, Mask::Fsbp),
                (0, 0, Mask::Fsfp),
            ] {
                let pixel = vertex_position + vec2(dx, dy);
                if bounds.contains(pixel) && get_pixel(pixel) {
                    connectivity |= mask;
                }
            }
            if connectivity.is_corner() {
                let index = index_counter;
                index_counter += 1;

                Some(Vertex {
                    position: vertex_position.extend(0).cast_unit(),
                    connectivity,
                    index,
                })
            } else {
                None
            }
        });

    (Basis::new(Face::PZ, Face::PY, Face::PX), vertices)
}
